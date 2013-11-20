## Copyright 2013 Elliot Chow

## Licensed under the Apache License, Version 2.0 (the "License")
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at

## http://www.apache.org/licenses/LICENSE-2.0

## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.

import('utils',
       'math',
       'gdata',
       'stringr',
       'plyr',
       'doMC',
       'gbm',
       'betareg',
       'glmnet',
       'infotheo',
       'rjson',
       'ggplot2')

##############################################
#### Model Building
##############################################

is.model.def <- function(x){
  attrs <- list(
             "id" = is.character, # name of model
             "target.gen" = function(tg) is.function(tg) || is.null(tg), # function that takes in a superset of the training data  and returns the target
             "fit" = is.function, # function for fitting the model of the same form as gbm.fit
             "features" = function(x) is.character(x) || is.numeric(x), # vector of feature names to be used by the model
             "predict" = is.function, # function for computing a prediction of the same form as gbm.predict
             "params" = is.list, # extra parameters for the fitting function
             "check" = is.function, # function that takes in a model definition, target, and data and checks if there are any issues
             "weights" = function(w) is.function(w) || is.null(w), # weights on training examples
             "report" = is.function# function that generates report for model
             )

  (length(x) == length(attrs)) && all(names(x) == names(attrs)) && all(unlist(lapply(names(attrs), function(a) attrs[[a]](x[[a]]) )))
}

mdls.target <- function(expr){
  t <- substitute(expr)
  function(d)
    eval(t, envir=d)
}

mdls.fit <- function(datasets, ..., mapping = list(".*"=".*"), log.level=SimpleLog.ERROR, .parallel=FALSE){
  ## train models on datasets
  ## datasets: single or list of data.frames/paths to data.frames/functions that load a data.frame
  ## ... : model definitions
  ## mapping : mapping by matching model def id pattern (value in list) to dataset id pattern (name in list)

  #### EXAMPLE: targets aren't all meaningful
  ## import('mdls')
  ## mdls.fit(iris[,1:4],
  ##          gbm.model.def("gbmmodel", mdls.target(Sepal.Length),
  ##                        c('Sepal.Width','Petal.Length','Petal.Width'),
  ##                        distribution='gaussian',train.fraction=0.8,interaction.depth=6,weights=function(data) runif(nrow(data))),
  ##          lm.model.def('lmmodel', mdls.target(Sepal.Length),
  ##                       c('Sepal.Width','Petal.Length','Petal.Width')),
  ##          glm.model.def('glmmodel', mdls.target(Sepal.Length),
  ##                       c('Sepal.Width','Petal.Length','Petal.Width'), family = 'gaussian'),
  ##          betareg.model.def("betaregmodel", mdls.target(Sepal.Width / Sepal.Length),
  ##                            c('Sepal.Width','Petal.Length','Petal.Width'),
  ##                            phi.features='Sepal.Width'),
  ##          .parallel=F) -> ms

  logger <- SimpleLog('mdls.fit',log.level)

  datasets <- if(is.data.frame(datasets) || !is.list(datasets)) list(datasets) else datasets
  model.defs <- list(...)
  dataset.ids <- if(!is.null(names(datasets))) names(datasets) else sapply(1:length(datasets),int.to.char.seq)

  valid.model.defs <- unlist(lapply(model.defs, is.model.def))
  stop.if.not(all(valid.model.defs), 'invalid model defs (%s)', paste(which(valid.model.defs),collapse=','))

  timer <- Timer(logger)
  flatten(lapply(lzip(dataset.ids, datasets),
                 function(x){
                   ds.id <- x[[1]]
                   data <- x[[2]]
                   if(is.character(data)){
                     t0 <- start.timer(timer,'loading dataset "%s"', ds.id)
                     data <- load.data(data)
                     stop.timer(timer)
                   }else if(is.function(data)){
                     data <- data()
                   }

                   model.defs.filtered <- Filter(function(md){
                     any(sapply(lzip(names(mapping), mapping),
                                function(mp)
                                str_detect(ds.id, mp[[1]]) && str_detect(md$id, mp[[2]])
                                ))
                   }, model.defs)

                   start.timer(timer, sprintf('train models on "%s"', ds.id))
                   models <- flatten(llply(model.defs.filtered,
                                           function(md){
                                             id <- sprintf('%s%s', md$id,
                                                           if(length(datasets) > 1) sprintf("__%s", ds.id) else "")

                                             t <- NULL
                                             if(!is.null(md$target.gen)){
                                               write.msg(logger, sprintf('building target for "%s"', id))
                                               t <- tryCatch(md$target.gen(data),
                                                             error=function(e){
                                                               write.msg(logger,str_trim(as.character(e)),
                                                                         level=SimpleLog.ERROR)
                                                               NA
                                                             })
                                             }

                                             w <- NULL
                                             if(!is.null(md$weights)){
                                               write.msg(logger, sprintf('adding weights for "%s"', id))
                                               w <- tryCatch(md$weights(data),
                                                             error=function(e){
                                                               write.msg(logger,str_trim(as.character(e)),
                                                                         level=SimpleLog.ERROR)
                                                               NA
                                                             })
                                             }
                                             problems <- md$check(md,t,data,w)
                                             if(length(problems) > 0){
                                               lapply(lzip(names(problems),problems),
                                                      function(p){
                                                        write.msg(logger,
                                                                  sprintf('problem with "%s" (%s): %s',
                                                                          id,
                                                                          p[[1]],
                                                                          if(is.na(p[[2]])){''}else{p[[2]]}),
                                                                  level='warning')
                                                      })
                                             }else{
                                               write.msg(logger, 'training "%s"', id)

                                               m <- tryCatch(do.call(md$fit,
                                                                     c(list(data[,grep("^\\|$",md$features,invert=T,value=T)],
                                                                            t,
                                                                            weights=w),
                                                                       md$params)),
                                                             error=function(e){
                                                               write.msg(logger,str_trim(as.character(e)),
                                                                         level=SimpleLog.ERROR)
                                                               NA
                                                             })

                                               if(is.list(m) || !any(is.na(m))){
                                                 z <- named(list(list(target=t,
                                                                      model=m,
                                                                      id=id,
                                                                      predict=md$predict,
                                                                      features=md$features,
                                                                      report=md$report)),
                                                            id)

                                                 return(z)
                                               }
                                             }

                                             write.msg(logger,sprintf('could not train "%s" (skipped)', id,
                                                                      level='warning'))

                                             list()
                                           },
                                           .parallel=.parallel))
                   stop.timer(timer)
                   models
                 }))
}

mdls.predict <- function(models, datasets, mapping=list(".*"=".*"), metric.groups = NULL, metrics.mapping=list('.*'='.*'), log.level=SimpleLog.ERROR, .parallel=FALSE){
  ## predict models (trained with mdls.fit) over a datasets
  ## models: output of mdls.fit
  ## datasets: single or list of data.frames/paths to data.frames/functions that load a data.frame
  ## mapping : mapping by matching model def id pattern (value in list) to dataset id pattern (name in list)
  ## metric.groups: definitions of metrics to compute for each model/dataset (see example below)
  ## metrics.mapping: regex mapping of metric groups to datasets (same format as 'mapping' parameter)

  ##### EXAMPLE - metrics aren't really meaningful here!
  ## import('mdls','infor')
  ## metrics <- list(
  ##                 'basic'=list(
  ##                   'metrics' = list(
  ##                     'mse' = function(p, d) mean(((if(is.matrix(p)) p[,1] else p) - d$Sepal.Length)^2),
  ##                     'foo' = function(p, d) ggplot(data.frame(x=1:10,y=rnorm(10)), aes(x=x,y=y)) + geom_point()
  ##                     ),
  ##                   'baselines' = list('random' = function(d) rnorm(nrow(d)))
  ##                   ),
  ##                 'grouped'=list(
  ##                   'preprocess' = function(s,d){
  ##                     compute.ranks(if(is.matrix(s)) s[,1] else s, d$Species)
  ##                   },
  ##                   'metrics' = list(
  ##                     'mean.at.top' = function(r, d) compute.infor.metric(r, d$Sepal.Length, d$Species, at.top(4))
  ##                     )
  ##                   ))
  ## mdls.predict(ms, iris, metric.groups=metrics) -> ss

  logger <- SimpleLog('mdls.predict',log.level)
  datasets <- if(is.data.frame(datasets) || !is.list(datasets)) list(datasets) else datasets
  dataset.ids <- if(!is.null(names(datasets))) names(datasets) else sapply(1:length(datasets),int.to.char.seq)

  model.ids <- lapply(models, function(m) m$id)
  metric.group.ids <- names(metric.groups)

  timer <- Timer(logger)
  flatten(lapply(lzip(dataset.ids,datasets),
                 function(x){
                   ds.id <- x[[1]]
                   data <- x[[2]]

                   if(is.character(data)){
                     t0 <- start.timer(timer,'loading dataset "%s"', ds.id)
                     data <- load.data(data)
                     stop.timer(timer)
                   }else if(is.function(data)){
                     data <- data()
                   }

                   models.filtered <- Filter(function(m){
                     any(sapply(lzip(names(mapping), mapping),
                                function(mp){
                                  str_detect(ds.id, mp[[1]]) && str_detect(m[[1]], mp[[2]])
                                }))

                   }, lzip(model.ids, models))


                   start.timer(timer,sprintf('computing predictions on "%s"', ds.id))
                   z.pred <- flatten(llply(models.filtered,
                                      function(x){
                                        id <- x[[1]]
                                        m <- x[[2]]$model

                                        features <- x[[2]]$features
                                        predict <- x[[2]]$predict

                                        write.msg(logger,sprintf('predicting with "%s"', id))
                                        pr <- tryCatch(predict(m, data[,features]),
                                                       error=function(e){
                                                         write.msg(logger, str_trim(as.character(e)), level=SimpleLog.ERROR)
                                                         NA
                                                       })

                                        if(all(is.na(pr)))
                                          write.msg(logger, 'failed to predict "%s" (skipped)', id, level=SimpleLog.WARNING)

                                        z <- list(pr)
                                        names(z) <- id
                                        z
                                      }, .parallel=.parallel))

                   metrics.filtered <- Filter(function(m){
                     any(sapply(lzip(names(metrics.mapping), metrics.mapping),
                                function(mp){
                                  str_detect(ds.id, mp[[1]]) && str_detect(m[[1]], mp[[2]])
                                }))

                   }, lzip(metric.group.ids, metric.groups))

                   avg.num.metrics.per.group <- if(is.null(metrics.filtered)) 0 else mean(unlist(lapply(metrics.filtered, function(mg) length(mg[[2]]$metrics))))
                   .parallel.layer <- if(length(z.pred) >= avg.num.metrics.per.group) 1 else 2
                   write.msg(logger, 'parallelizing over %s (# models: %s, average # metrics in group: %s)', if(.parallel.layer == 1) 'models' else 'metrics', length(z.pred), avg.num.metrics.per.group, level=SimpleLog.DEBUG)
                   z.metrics <- flatten(lapply(metrics.filtered,
                                              function(metric.group){
                                                metric.group.id <- metric.group[[1]]
                                                preprocess <- get.or.else(metric.group[[2]], 'preprocess', function(s,d) s)
                                                metrics.lst <- metric.group[[2]]$metrics
                                                baselines.lst <- metric.group[[2]]$baselines

                                                write.msg(logger, 'computing metrics group "%s"', metric.group.id)
                                                z.pred.with.baselines <- c(z.pred,
                                                                           flatten(lapply(names(baselines.lst),
                                                                                  function(b.name){
                                                                                    write.msg(logger, 'adding baseline "%s"', b.name)
                                                                                    b <- tryCatch(baselines.lst[[b.name]](data),
                                                                                                  error = function(e){
                                                                                                    write.msg(logger, str_trim(as.character(e)), level=SimpleLog.ERROR)
                                                                                                    NA
                                                                                                  })
                                                                                    if(all(is.na(b)))
                                                                                      write.msg(logger, 'failed to compute baseline "%s" from "%s" on "%s" (skipped)', b.name, metric.group.id, ds.id, level=SimpleLog.WARNING)

                                                                                    list(b) %named% b.name
                                                                                  })))

                                                z.ms.all <- flatten(llply(names(z.pred.with.baselines),
                                                              function(pred.name){
                                                                write.msg(logger, 'preprocess predictions of "%s"', pred.name, level=SimpleLog.DEBUG)
                                                                processed.pred <- tryCatch(preprocess(z.pred.with.baselines[[pred.name]], data),
                                                                                           error = function(e){
                                                                                             write.msg(logger, str_trim(as.character(e)), level=SimpleLog.ERROR)
                                                                                             NA
                                                                                           })
                                                                if(all(is.na(processed.pred)))
                                                                  write.msg(logger, 'failed to preprocess "%s" for "%s" on "%s" (skipped)', pred.name, metric.group.id, ds.id, level=SimpleLog.WARNING)

                                                                z.ms <- flatten(llply(lzip(names(metrics.lst), metrics.lst),
                                                                                      function(m){
                                                                                        m.id <- m[[1]]
                                                                                        m <- m[[2]]
                                                                                        write.msg(logger, 'computing metric "%s" for "%s" on "%s"', m.id, pred.name, ds.id)

                                                                                        z.m <- tryCatch(m(processed.pred, data),
                                                                                                        error = function(e){
                                                                                                          write.msg(logger, str_trim(as.character(e)), level=SimpleLog.ERROR)
                                                                                                          NA
                                                                                                        })
                                                                                        if(all(is.na(z.m)))
                                                                                          write.msg(logger, 'failed to compute metric "%s" for "%s" on "%s" (skipped)', m.id, pred.name, ds.id, level=SimpleLog.WARNING)

                                                                                        list(z.m) %named% m.id
                                                                                      },
                                                                                      .parallel = .parallel && (.parallel.layer == 2)))

                                                                list(z.ms) %named% pred.name
                                                              },
                                                              .parallel = .parallel && (.parallel.layer == 1)))

                                                list(z.ms.all) %named% metric.group.id
                                              }))

                   z <- list(list(scores=z.pred, metrics=z.metrics)) %named% ds.id
                   stop.timer(timer)
                   z
                 })
          )
}

mdls.report <- function(mdls, root, text.as = 'html', overwrite = FALSE, log.level = SimpleLog.INFO, .parallel=FALSE){
  ## generate reports for each model trained by mdls.fit
  ## mdls: output of mdls.fit
  ## root: root output directory for reports
  ## text.as: output format
  ## overwrite: overwrite output directory or not

  ## import('mdls'); mdls.fit(iris, gbm.model.def("gbmmodel",function(x) x$Sepal.Length + as.integer(x$Species), c('Sepal.Width','Petal.Length','Petal.Width','Species'),distribution='gaussian',weights=function(data) runif(nrow(data)), train.fraction=0.8),glm.model.def('glmmodel', function(x) x$Sepal.Length, c('Sepal.Width','Petal.Length','Petal.Width'), family='gaussian'),lm.model.def('lmmodel', function(x) x$Sepal.Length, c('Sepal.Width','Petal.Length','Petal.Width'))) -> ms; system('rm -r ~/tmp/test/'); mdls.report(ms,'~/tmp/test')
  if(overwrite)
    rrmdir(root)

  stop.if(file.exists(root) && !overwrite, 'output directory "%s" already exists ', root)

  logger <- SimpleLog('mdls.report',log.level)

  dir.create(root, recursive = TRUE)

  invisible(llply(lzip(names(mdls), mdls),
                  function(x){
                    id <- x[[1]]
                    m <- x[[2]]

                    write.msg(logger, sprintf('saving report for "%s"', id))

                    m$report(m$model, file.path(root, id), text.as = text.as, log.level = log.level, .parallel = .parallel)
                  },
                  .parallel=.parallel))
}

#####################################
#### gbm modifications and helpers
#####################################

gbm.model.def <- function(id, target.gen, features, ...,
                          weights=function(data) NULL){
  params <- with.defaults(list(...),
                          list(train.fraction=0.8, shrinkage=0.01,
                               n.trees=100, interaction.depth=8, keep.data=FALSE,
                               verbose=TRUE))

  g <- tryCatch(params$distribution$group, error=function(e) NULL)
  features <- unique(c(features,g))

  list(id=id, target.gen=target.gen, fit=function(...,weights=NULL) gbm.fit.plus(..., group = g, w=weights),
       features=features, predict=gbm.predict, params=params, check=check.gbm.model.def, weights=weights, report=gbm.model.report)
}

gbm.fit.plus <- function(x, y, ..., train.fraction = 0.8, group = NULL, y.label="y"){
  features <- setdiff(names(x), group)
  nTrain <- train.fraction * nrow(x)
  gbm.fit(subset(x, select=features), y, ..., nTrain=nTrain, group=x[,group])
}

gbm.predict <- function(object,newdata,n.trees=NULL,type='response',...){
  trees <- if(is.null(n.trees)) gbm.perf(object,method='test',plot.it=FALSE) else n.trees

  if(is.null(object$num.classes))
    object$num.classes <- 1 ## for compatibility with v1.6.3

  stop.if(length(trees) != 1, 'could not determine optimal number of trees')

  predict.gbm(object,newdata,n.trees=trees,type=type,...)
}

check.gbm.model.def <- function(model.def, target, data, weights){
  problems <- list()

  missing <- setdiff(model.def$features, names(data))
  available <- setdiff(model.def$features, missing)
  if(length(missing) != 0)
    problems$missing.features <- missing

  group.col <- tryCatch(model.def$params$distribution$group, error=function(e) NULL)

  gt1024levels <- sapply(setdiff(available, group.col),
                         function(f){is.factor(data[[f]]) && (length(levels(data[[f]])) > 1024)})


  if(any(gt1024levels)){problems$too.many.levels <- available[gt1024levels]}

  all.na <- sapply(available, function(f){all(is.na(data[[f]]))})
  if(any(all.na))
    problems$all.na <- available[all.na]

  monotonicity <- (('var.monotone' %in% names(model.def$params)) &&
                   ((length(model.def$params$var.monotone) != length(model.def$features)) ||
                    !all(model.def$params$var.monotone %in% (-1:1))))
  if(monotonicity)
    problems$monotonicity <- NA

  na.target <- any(is.na(target))
  if(na.target)
    problems$na.target <- NA

  nan.target <- any(is.nan(target))
  if(nan.target)
    problems$nan.target <- NA

  infinite.target <- any(is.infinite(target))
  if(infinite.target)
    problems$infinite.target <- NA

  no.distribution <- !('distribution' %in% names(model.def$params))
  if(no.distribution)
    problems$no.distribution <- NA
  else{
    invalid.bernoulli.target <- (model.def$params$distribution == 'bernoulli') && (!all(target %in% (0:1)))
    if(invalid.bernoulli.target)
      problems$invalid.bernoulli.target <- NA
  }

  invalid.weights <- any(is.na(weights) | is.nan(weights) | is.infinite(weights))
  if(invalid.weights)
    problems$invalid.weights <- NA
  else{
    negative.weights <- any(weights < 0)
    if(negative.weights)
      problems$negative.weights <- NA
  }
  problems
}

gbm.opt.n.trees <- function(object, method='test')
  ## get optimal trees
  gbm.perf(object,method=method,plot.it=F)

gbm.tree.as.df <- function(object, i.tree = 1){
  ## read a gbm tree into a data.frame
  stop.if((i.tree < 1) || (i.tree > object$n.trees),
          "i.tree %d is out of range (%d)", i.tree, object$n.trees)
  tree <- named(data.frame(object$trees[[i.tree]]),
                c("SplitVar", "SplitCodePred", "LeftNode",
                  "RightNode", "MissingNode", "ErrorReduction", "Weight",
                  "Prediction")) ## tree from gbm

  df <- data.frame(node.id = 1:nrow(tree))
  df$is.leaf <- tree$SplitVar == -1 ## leaf node marked as -1
  df$var.id <- ifelse(df$is.leaf, NA , tree$SplitVar + 1) ## var id
  df$is.cat <- ifelse(df$is.leaf, NA, c(NA,object$var.type > 0)[df$var.id + 1]) ## is variable categorical
  df$var.name <- ifelse(df$is.leaf, NA , object$var.names[df$var.id]) ## name of variable
  df$op <- ifelse(df$is.cat, 'in', 'less_than') ## operator - when true, go left
  df$condition <- ifelse(df$is.leaf, NA, ## condition to satisify to go left
                         ifelse(df$is.cat,
                                sapply(lzip(c(NA,object$c.splits)[(tree$SplitCodePred + 1) * df$is.cat + 1], ## look up grouping of categorical factors
                                            df$var.id),
                                       function(x){
                                         var.id <- x[[2]]
                                         level.names <- object$var.levels[[var.id]][x[[1]] < 0] ## get level names to split left

                                         paste('{', paste(level.names, collapse=',') ,'}',sep='')
                                       }),
                                tree$SplitCodePred)
                         )
  df$left.id <- ifelse(df$is.leaf, NA, tree$LeftNode + 1)
  df$right.id <- ifelse(df$is.leaf, NA, tree$RightNode+ 1)
  df$missing.id <- ifelse(df$is.leaf, NA, tree$MissingNode + 1)
  df$prediction <- ifelse(df$is.leaf, tree$Prediction + (i.tree == 1) * object$initF, NA)

  df
}

gbm.tree.row.as.list <- function(tree, node){
  row <- tree[tree$node == node,]
  if(row$is.leaf)
    list(prediction=row$prediction)
  else{
    list(split=list(
           var.name = row$var.name,
           op = row$op,
           val = row$condition
           ),
         if_true = gbm.tree.row.as.list(tree, row$left.id),
         if_false = gbm.tree.row.as.list(tree, row$right.id),
         if_missing = gbm.tree.row.as.list(tree, row$missing.id)
         )
  }
}

gbm.tree.as.list <- function(object, i.tree)
  ## turn a tree into a list
  gbm.tree.row.as.list(gbm.tree.as.df(object, i.tree), 1)

gbm.model.as.list <- function(object, n.trees=object$n.trees, name="", .parallel=FALSE){
  ## turn model into a list - can be easily serializable to JSON or YAML
  usedVariables <- gbm.model.used.variables(object)
  trees <- llply(1:n.trees, function(tree) gbm.tree.as.list(object, i.tree=tree), .parallel=.parallel)

  list(name=name,
       bag.fraction=object$bag.fraction,
       distribution= object$distribution$name,
       interaction.depth=object$interaction.depth,
       n.minobsinnode=object$n.minobsinnode,
       n.trees=as.integer(n.trees),
       shrinkage=object$shrinkage,
       features = usedVariables,
       trees=trees)
}

gbm.model.used.variables <- function(object, trees=object$n.trees)
  ## variables with nonzero importance
  as.character(subset(summary.gbm(object,plotit=F), rel.inf > 0,)$var)

gbm.split.points <- function(object, var.name=1, trees=object$n.trees){
  ## split points using this variable
  if(is.numeric(var.name))
    var.name <- object$var.names[var.name]

  sp <- csplat(c, lapply(1:trees,
                         function(tree){
                           z <- gbm.tree.as.df(object, i.tree=tree)
                           z$condition[ !is.na(z$var.name) & (z$var.name == var.name) ]
                         }))

  sp.n <- as.numeric(sp)
  if(any(is.na(sp.n)))
    sp
  else
    sp.n
}

gbm.feature.importance <- function(object, k=min(10,length(object$var.names)),
                                   n.trees=gbm.opt.n.trees(object), ...){
  ## feature importance displayed with ggplot
  stop.if(k < 1, "k must be >= 1")

  x <- as.data.frame(as.list(summary(object, n.trees=n.trees, plotit=FALSE)[k:1,]))
  names(x) <- c('feature', 'importance')
  x$feature <- ordered(x$feature, x$feature)
  ggplot(x, aes(feature,importance)) +
    geom_bar(stat='identity') +
      coord_flip() +
        ggtitle(paste('Top',k,'of',length(object$var.names),'Features'))
}

gbm.plot <- function (x, i.var = 1, n.trees = x$n.trees, continuous.resolution = list('splits',NA),
                      return.grid = FALSE, type = "link", ...)
  ## customized dependency plots (allow for plotting at split points, displaying as steps)
{
  if (!is.element(type, c("link", "response"))) {
    stop("type must be either 'link' or 'response'")
  }
  if(is.null(x$num.classes))
    x$num.classes <- 1 ## for compatibility with v1.6.3
  if (all(is.character(i.var))) {
    i <- match(i.var, x$var.names)
    if (any(is.na(i))) {
      stop("Plot variables not used in gbm model fit: ",
           i.var[is.na(i)])
    }
    else {
      i.var <- i
    }
  }
  if ((min(i.var) < 1) || (max(i.var) > length(x$var.names))) {
    warning("i.var must be between 1 and ", length(x$var.names))
  }
  if (n.trees > x$n.trees) {
    warning(paste("n.trees exceeds the number of trees in the model, ",
                  x$n.trees, ". Plotting using ", x$n.trees, " trees.",
                  sep = ""))
    n.trees <- x$n.trees
  }
  if (length(i.var) > 3) {
    warning("gbm.int.plot creates up to 3-way interaction plots.\nplot.gbm will only return the plotting data structure.")
    return.grid = TRUE
  }
  grid.levels <- vector("list", length(i.var))
  for (i in 1:length(i.var)) {
    if (is.numeric(x$var.levels[[i.var[i]]])) {
      if(continuous.resolution[[1]] == 'quantile'){
        grid.levels[[i]] <- as.vector(quantile(x$var.levels[[i.var[i]]], probs=continuous.resolution[[2]]))
      }else if (continuous.resolution[[1]] == 'uniform') {
        grid.levels[[i]] <- seq(min(x$var.levels[[i.var[i]]]),
                                max(x$var.levels[[i.var[i]]]), length = continuous.resolution[[2]])
      }else if(continuous.resolution[[1]] == 'splits'){
        var.name <- x$var.names[i.var[i]]
        splitPoints <- sort(unique(gbm.split.points(x, var.name)))
        if(length(splitPoints) == 1){
          d <- 1
        }else{
          d <- min(diff(splitPoints))
        }
        grid.levels[[i]] <- c(splitPoints[1] - d, splitPoints)
      }else{
        stop("unknown range sampling method: ", continuous.resolution[[1]])
      }
    }
    else {
      grid.levels[[i]] <- as.numeric(factor(x$var.levels[[i.var[i]]],
                                            levels = x$var.levels[[i.var[i]]])) - 1
    }
  }
  X <- expand.grid(grid.levels)
  names(X) <- paste("X", 1:length(i.var), sep = "")
  y <- .Call("gbm_plot", X = as.double(data.matrix(X)), cRows = as.integer(nrow(X)),
             cCols = as.integer(ncol(X)), n.class = as.integer(x$num.classes),
             i.var = as.integer(i.var - 1), n.trees = as.integer(n.trees),
             initF = as.double(x$initF), trees = x$trees, c.splits = x$c.splits,
             var.type = as.integer(x$var.type), PACKAGE = "gbm")
  if (x$distribution$name == "multinomial") {
    X$y <- matrix(y, ncol = x$num.classes)
    colnames(X$y) <- x$classes
    if (type == "response") {
      X$y <- exp(X$y)
      X$y <- X$y/matrix(rowSums(X$y), ncol = ncol(X$y),
                        nrow = nrow(X$y))
    }
  }
  else if (is.element(x$distribution$name, c("bernoulli", "pairwise")) &&
           type == "response") {
    X$y <- 1/(1 + exp(-y))
  }
  else if ((x$distribution$name == "poisson") && (type == "response")) {
    X$y <- exp(y)
  }
  else if (type == "response") {
    warning("type 'response' only implemented for 'bernoulli', 'poisson', 'multinomial', and 'pairwise'. Ignoring")
  }
  else {
    X$y <- y
  }
  f.factor <- rep(FALSE, length(i.var))
  for (i in 1:length(i.var)) {
    if (!is.numeric(x$var.levels[[i.var[i]]])) {
      X[, i] <- factor(x$var.levels[[i.var[i]]][X[, i] +
                                                1], levels = x$var.levels[[i.var[i]]])
      f.factor[i] <- TRUE
    }
  }
  if (return.grid) {
    names(X)[1:length(i.var)] <- x$var.names[i.var]
    return(X)
  }
  if (length(i.var) == 1) {
    if (!f.factor) {
      j <- order(X$X1)
      if (x$distribution$name == "multinomial") {
        if (type == "response") {
          ylabel <- "Predicted class probability"
        }
        else {
          ylabel <- paste("f(", x$var.names[i.var], ")",
                          sep = "")
        }
        plot(range(X$X1), range(X$y), type = "n", xlab = x$var.names[i.var],
             ylab = ylabel)
        for (ii in 1:x$num.classes) {
          lines(X$X1, X$y[, ii], xlab = x$var.names[i.var],
                ylab = paste("f(", x$var.names[i.var], ")",
                  sep = ""), col = ii, ...)
        }
      }
      else if (is.element(x$distribution$name, c("bernoulli",
                                                 "pairwise"))) {
        if (type == "response") {
          ylabel <- "Predicted probability"
        }
        else {
          ylabel <- paste("f(", x$var.names[i.var], ")",
                          sep = "")
        }
        plot(X$X1, X$y, type = "s", xlab = x$var.names[i.var],
             ylab = ylabel)
      }
      else if (x$distribution$name == "poisson") {
        if (type == "response") {
          ylabel <- "Predicted count"
        }
        else {
          ylabel <- paste("f(", x$var.names[i.var], ")",
                          sep = "")
        }
        plot(X$X1, X$y, type = "s", xlab = x$var.names[i.var],
             ylab = ylabel)
      }
      else {
        plot(X$X1, X$y, type = "s", xlab = x$var.names[i.var],
             ylab = paste("f(", x$var.names[i.var], ")",
               sep = ""), ...)
      }
    }
    else {
      if (x$distribution$name == "multinomial") {
        nX <- length(X$X1)
        dim.y <- dim(X$y)
        if (type == "response") {
          ylabel <- "Predicted probability"
        }
        else {
          ylabel <- paste("f(", x$var.names[i.var], ")",
                          sep = "")
        }
        plot(c(0, nX), range(X$y), axes = FALSE, type = "n",
             xlab = x$var.names[i.var], ylab = ylabel)
        axis(side = 1, labels = FALSE, at = 0:nX)
        axis(side = 2)
        mtext(as.character(X$X1), side = 1, at = 1:nX -
              0.5)
        segments(x1 = rep(1:nX - 0.75, each = dim.y[2]),
                 y1 = as.vector(t(X$y)), x2 = rep(1:nX - 0.25,
                                           each = dim.y[2]), col = 1:dim.y[2])
      }
      else if (is.element(x$distribution$name, c("bernoulli",
                                                 "pairwise")) && type == "response") {
        ylabel <- "Predicted probability"
        plot(X$X1, X$y, type = "l", xlab = x$var.names[i.var],
             ylab = ylabel)
      }
      else if (x$distribution$name == "poisson" & type ==
               "response") {
        ylabel <- "Predicted count"
        plot(X$X1, X$y, type = "l", xlab = x$var.names[i.var],
             ylab = ylabel)
      }
      else {
        plot(X$X1, X$y, type = "l", xlab = x$var.names[i.var],
             ylab = paste("f(", x$var.names[i.var], ")",
               sep = ""), ...)
      }
    }
  }
  else if (length(i.var) == 2) {
    if (!f.factor[1] && !f.factor[2]) {
      if (x$distribution$name == "multinomial") {
        for (ii in 1:x$num.classes) {
          X$temp <- X$y[, ii]
          print(levelplot(temp ~ X1 * X2, data = X, xlab = x$var.names[i.var[1]],
                          ylab = x$var.names[i.var[2]], ...))
          title(paste("Class:", dimnames(X$y)[[2]][ii]))
        }
        X$temp <- NULL
      }
      else {
        levelplot(y ~ X1 * X2, data = X, xlab = x$var.names[i.var[1]],
                  ylab = x$var.names[i.var[2]], ...)
      }
    }
    else if (f.factor[1] && !f.factor[2]) {
      if (x$distribution$name == "multinomial") {
        for (ii in 1:x$num.classes) {
          X$temp <- X$y[, ii]
          print(xyplot(temp ~ X2 | X1, data = X, xlab = x$var.names[i.var[2]],
                       ylab = paste("f(", x$var.names[i.var[1]],
                         ",", x$var.names[i.var[2]], ")", sep = ""),
                       type = "l", panel = panel.xyplot, ...))
          title(paste("Class:", dimnames(X$y)[[2]][ii]))
        }
        X$temp <- NULL
      }
      else {
        xyplot(y ~ X2 | X1, data = X, xlab = x$var.names[i.var[2]],
               ylab = paste("f(", x$var.names[i.var[1]], ",",
                 x$var.names[i.var[2]], ")", sep = ""), type = "l",
               panel = panel.xyplot, ...)
      }
    }
    else if (!f.factor[1] && f.factor[2]) {
      if (x$distribution$name == "multinomial") {
        for (ii in 1:x$num.classes) {
          X$temp <- X$y[, ii]
          print(xyplot(temp ~ X1 | X2, data = X, xlab = x$var.names[i.var[1]],
                       ylab = paste("f(", x$var.names[i.var[1]],
                         ",", x$var.names[i.var[2]], ")", sep = ""),
                       type = "l", panel = panel.xyplot, ...))
          title(paste("Class:", dimnames(X$y)[[2]][ii]))
        }
        X$temp <- NULL
      }
      else {
        xyplot(y ~ X1 | X2, data = X, xlab = x$var.names[i.var[1]],
               ylab = paste("f(", x$var.names[i.var[1]], ",",
                 x$var.names[i.var[2]], ")", sep = ""), type = "l",
               panel = panel.xyplot, ...)
      }
    }
    else {
      if (x$distribution$name == "multinomial") {
        for (ii in 1:x$num.classes) {
          X$temp <- X$y[, ii]
          print(stripplot(X1 ~ temp | X2, data = X, xlab = x$var.names[i.var[2]],
                          ylab = paste("f(", x$var.names[i.var[1]],
                            ",", x$var.names[i.var[2]], ")", sep = ""),
                          ...))
          title(paste("Class:", dimnames(X$y)[[2]][ii]))
        }
        X$temp <- NULL
      }
      else {
        stripplot(X1 ~ y | X2, data = X, xlab = x$var.names[i.var[2]],
                  ylab = paste("f(", x$var.names[i.var[1]], ",",
                    x$var.names[i.var[2]], ")", sep = ""), ...)
      }
    }
  }
  else if (length(i.var) == 3) {
    i <- order(f.factor)
    X.new <- X[, i]
    X.new$y <- X$y
    names(X.new) <- names(X)
    if (sum(f.factor) == 0) {
      X.new$X3 <- equal.count(X.new$X3)
      if (x$distribution$name == "multinomial") {
        for (ii in 1:x$num.classes) {
          X.new$temp <- X.new$y[, ii]
          print(levelplot(temp ~ X1 * X2 | X3, data = X.new,
                          xlab = x$var.names[i.var[i[1]]], ylab = x$var.names[i.var[i[2]]],
                          ...))
          title(paste("Class:", dimnames(X.new$y)[[2]][ii]))
        }
        X.new$temp <- NULL
      }
      else {
        levelplot(y ~ X1 * X2 | X3, data = X.new, xlab = x$var.names[i.var[i[1]]],
                  ylab = x$var.names[i.var[i[2]]], ...)
      }
    }
    else if (sum(f.factor) == 1) {
      if (x$distribution$name == "multinomial") {
        for (ii in 1:x$num.classes) {
          X.new$temp <- X.new$y[, ii]
          print(levelplot(temp ~ X1 * X2 | X3, data = X.new,
                          xlab = x$var.names[i.var[i[1]]], ylab = x$var.names[i.var[i[2]]],
                          ...))
          title(paste("Class:", dimnames(X.new$y)[[2]][ii]))
        }
        X.new$temp <- NULL
      }
      else {
        levelplot(y ~ X1 * X2 | X3, data = X.new, xlab = x$var.names[i.var[i[1]]],
                  ylab = x$var.names[i.var[i[2]]], ...)
      }
    }
    else if (sum(f.factor) == 2) {
      if (x$distribution$name == "multinomial") {
        for (ii in 1:x$num.classes) {
          X.new$temp <- X.new$y[, ii]
          print(xyplot(temp ~ X1 | X2 * X3, data = X.new,
                       type = "l", xlab = x$var.names[i.var[i[1]]],
                       ylab = paste("f(", paste(x$var.names[i.var[1:3]],
                         collapse = ","), ")", sep = ""), panel = panel.xyplot,
                       ...))
          title(paste("Class:", dimnames(X.new$y)[[2]][ii]))
        }
        X.new$temp <- NULL
      }
      else {
        xyplot(y ~ X1 | X2 * X3, data = X.new, type = "l",
               xlab = x$var.names[i.var[i[1]]], ylab = paste("f(",
                                                  paste(x$var.names[i.var[1:3]], collapse = ","),
                                                  ")", sep = ""), panel = panel.xyplot, ...)
      }
    }
    else if (sum(f.factor) == 3) {
      if (x$distribution$name == "multinomial") {
        for (ii in 1:x$num.classes) {
          X.new$temp <- X.new$y[, ii]
          print(stripplot(X1 ~ temp | X2 * X3, data = X.new,
                          xlab = x$var.names[i.var[i[1]]], ylab = paste("f(",
                                                             paste(x$var.names[i.var[1:3]], collapse = ","),
                                                             ")", sep = ""), ...))
          title(paste("Class:", dimnames(X.new$y)[[2]][ii]))
        }
        X.new$temp <- NULL
      }
      else {
        stripplot(X1 ~ y | X2 * X3, data = X.new, xlab = x$var.names[i.var[i[1]]],
                  ylab = paste("f(", paste(x$var.names[i.var[1:3]],
                    collapse = ","), ")", sep = ""), ...)
      }
    }
  }
}

gbm.loss <- function(target, prediction, distribution, weight=rep(1,length(target)), offset=NA, baseline=0, group = NULL, max.rank = NULL)
  gbm::gbm.loss(target, prediction, weight, offset, if(is.character(distribution)) list(name=distribution) else distribution, baseline, group, max.rank)

gbm.model.report <- function(object, root, text.as = 'html', plot.it = TRUE, log.level = SimpleLog.INFO, .parallel = TRUE){
  ## reporting for gbm model (error, feature importance, dependency plots)
  stop.if.not(text.as %in% c('txt','html','textile'),
              'unknown text format: %s', text.as)
  stop.if(file.exists(root), 'output directory "%s" already exists ', root)

  logger <- SimpleLog('gbm.model.report', log.level)

  dir.create(root, recursive = TRUE)
  dep.plots.dir <- file.path(root, 'dependency-plots')
  dir.create(dep.plots.dir)

  if('txt' == text.as)
    format.fun <- pprint.dataframe
  else if('html' == text.as)
    format.fun <- function(...) paste(sorttable.import(), dataframe.to.html.table(...), sep='\n')
  else if('textile' == text.as)
    format.fun <- dataframe.to.textile

  ## model performance
  if(object$train.fraction < 1){
    png(file.path(root, 'error-test.png'))
    gbm.perf(object, method = 'test')
    dev.off()
  }else{
    png(file.path(root, 'error-oob.png'))
    gbm.perf(object, method = 'OOB')
    dev.off()
  }

  ## feature importance
  write.msg(logger, 'saving feature importance')
  x <- summary(object, plotit=F)
  names(x) <- c('feature', 'importance')
  x$feature <- ordered(x$feature, x$feature)
  row.names(x) <- NULL
  cat(format.fun(x), file=file.path(root, sprintf('feature-importance.%s', text.as)))

  ## dependency plots
  nonzero.features <- as.character(x$feature[x$importance > 0])
  invisible(llply(nonzero.features,
                  function(f){
                    write.msg(logger, sprintf('saving dependency plot for %s', f))
                    dp <- gbm.plot(object, f, return.grid=T)
                    cat(format.fun(dp), file=file.path(dep.plots.dir, sprintf('%s.%s', f, text.as)))

                    if(plot.it){
                      png(file.path(dep.plots.dir, sprintf('%s.png', f)))
                      gbm.plot(object, f, return.grid=F)
                      dev.off()
                    }
                  },
                  .parallel=.parallel))
}



#####################################
#### lm modifications and helpers
#####################################

lm.model.def <- function(id, target.gen, features, ..., weights=function(data) NULL){
  list(id=id, target.gen=target.gen, fit=lm.fit.plus, features=features, predict=predict.lm, params=list(...), check=check.lm.model.def, weights=weights, report=lm.model.report)
}

lm.fit.plus <- function(x, y, ..., y.label="y"){
  features <- names(x)
  x[[y.label]] <- y
  f <- sprintf("%s ~ %s", y.label, paste(features, collapse = " + "))
  m <- lm(formula(f), x, ...)
  m$var.names <- features
  m
}

check.lm.model.def <- function(model.def, target, data, weights){
  problems <- list()

  missing <- setdiff(model.def$features, names(data))
  available <- setdiff(model.def$features, missing)
  if(length(missing) != 0)
    problems$missing.features <- missing

  na.target <- any(is.na(target))
  if(na.target)
    problems$na.target <- NA

  nan.target <- any(is.nan(target))
  if(nan.target)
    problems$nan.target <- NA

  infinite.target <- any(is.infinite(target))
  if(infinite.target)
    problems$infinite.target <- NA

  invalid.weights <- any(is.na(weights) | is.nan(weights) | is.infinite(weights))
  if(invalid.weights)
    problems$invalid.weights <- NA
  else{
    negative.weights <- any(weights < 0)
    if(negative.weights)
      problems$negative.weights <- NA
  }

  problems
}

lm.model.report <- function(object, root, text.as = 'txt', log.level = SimpleLog.INFO, .parallel = TRUE){
  stop.if(file.exists(root), 'output directory "%s" already exists ', root)

  logger <- SimpleLog('lm.model.report', log.level)

  dir.create(root, recursive = TRUE)
  cat(str_replace_all(paste(capture.output(summary(object)),collapse='\n'), '[’‘]', '"'),
      file=file.path(root, 'summary.txt'))
}

#####################################
#### glm modifications and helpers
#####################################

glm.model.def <- function(id, target.gen, features, ..., weights=function(data) NULL){
  params <- list(...)

  list(id=id, target.gen=target.gen, fit=glm.fit.plus, features=features, predict=glm.predict, params=params, check=check.glm.model.def, weights=weights, report=glm.model.report)
}

glm.fit.plus <- function(x, y, family=NA,..., y.label="y"){
  features <- names(x)
  x[[y.label]] <- y
  f <- sprintf("%s ~ %s", y.label, paste(features, collapse =" + "))
  glm(formula(f), family=family, x, ...)
}

check.glm.model.def <- function(model.def, target, data, weights){
  problems <- list()

  missing <- setdiff(model.def$features, names(data))
  available <- setdiff(model.def$features, missing)
  if(length(missing) != 0)
    problems$missing.features <- missing

  na.target <- any(is.na(target))
  if(na.target)
    problems$na.target <- NA

  nan.target <- any(is.nan(target))
  if(nan.target)
    problems$nan.target <- NA

  infinite.target <- any(is.infinite(target))
  if(infinite.target)
    problems$infinite.target <- NA

  no.family <- !('family' %in% names(model.def$params))
  if(no.family)
    problems$no.family <- NA
  else{
    invalid.binomial.target <- (model.def$params$family == 'binomial') && (!all(target %in% (0:1)))
    if(invalid.binomial.target)
      problems$invalid.binomial.target <- NA
  }

  invalid.weights <- any(is.na(weights) | is.nan(weights) | is.infinite(weights))
  if(invalid.weights)
    problems$invalid.weights <- NA
  else{
    negative.weights <- any(weights < 0)
    if(negative.weights)
      problems$negative.weights <- NA
  }

  problems
}

glm.predict <- function(object,newdata,type='response',...)
  predict.glm(object,newdata,type=type,...)

glm.model.report <- function(object, root, text.as = 'txt', log.level = SimpleLog.INFO, .parallel = TRUE){
  stop.if(file.exists(root), 'output directory "%s" already exists ', root)

  logger <- SimpleLog('glm.model.report', log.level)

  dir.create(root, recursive = TRUE)
  cat(str_replace_all(paste(capture.output(summary(object)), collapse = '\n'), '[’‘]', '"'),
      file=file.path(root, 'summary.txt'))
}

#####################################
#### glmnet modifications and helpers
#####################################

glmnet.model.def <- function(id, target.gen, features, ..., weights=function(data) NULL){
  list(id=id, target.gen=target.gen, fit=glmnet.fit, features=features, predict=glmnet.predict, params=list(...), check=check.glmnet.model.def, weights=weights, report=glmnet.model.report)
}

glmnet.fit <- function(x, y, ..., cv = T, weights = NULL){
  fit <- if(cv) cv.glmnet else glmnet
  if(is.null(weights))
    fit(as.matrix(x), y, ...)
  else
    fit(as.matrix(x), y, ..., weights = weights)
}

check.glmnet.model.def <- function(model.def, target, data, weights){
  problems <- list()

  missing <- setdiff(model.def$features, names(data))
  available <- setdiff(model.def$features, missing)
  if(length(missing) != 0)
    problems$missing.features <- missing

  na.target <- any(is.na(target))
  if(na.target)
    problems$na.target <- NA

  nan.target <- any(is.nan(target))
  if(nan.target)
    problems$nan.target <- NA

  infinite.target <- any(is.infinite(target))
  if(infinite.target)
    problems$infinite.target <- NA

  no.family <- !('family' %in% names(model.def$params))
  if(no.family)
    problems$no.family <- NA
  else{
    invalid.binomial.target <- (model.def$params$family == 'binomial') && (!all(target %in% (0:1)))
    if(invalid.binomial.target)
      problems$invalid.binomial.target <- NA
  }

  invalid.weights <- any(is.na(weights) | is.nan(weights) | is.infinite(weights))
  if(invalid.weights)
    problems$invalid.weights <- NA
  else{
    negative.weights <- any(weights < 0)
    if(negative.weights)
      problems$negative.weights <- NA
  }

  problems
}

glmnet.predict <- function(object,newx,type='response',...)
  predict(object,newx,type=type,...)

glmnet.model.report <- function(object, root, text.as = 'txt', log.level = SimpleLog.INFO, .parallel = TRUE){
  ## FIXME
  stop.if(file.exists(root), 'output directory "%s" already exists ', root)

  logger <- SimpleLog('glmnet.model.report', log.level)

  dir.create(root, recursive = TRUE)
  cat(str_replace_all(paste(capture.output(info), collapse = '\n'), '[’‘]', '"'),
      file=file.path(root, 'summary.txt'))
}

#####################################
#### betareg modifications and helpers
#####################################

betareg.model.def <- function(id, target.gen, features, ..., phi.features=NULL, weights=function(data) NULL){
  all.features <- unique(c(features, phi.features))

  list(id=id, target.gen=target.gen, fit=betareg.fit.plus, features=all.features, predict=betareg.predict, params=c(list(...), list(features=features, phi.features = phi.features)), check=check.betareg.model.def, weights=weights, report=betareg.model.report)
}

betareg.fit.plus <- function(x, y, ..., features, phi.features = NULL, y.label="y"){
  ## import('mdls'); mdls.fit(FoodExpenditure,betareg.model.def("br", function(x) x$food / x$income,c('income','persons'),phi.features='persons')) -> ms

  x[[y.label]] <- y
  f <- sprintf("%s ~ %s %s", y.label,
               paste( features, collapse=" + "),
               if(is.null(phi.features)) "" else paste(" |", paste(phi.features, collapse = ' + ')))

  m <- betareg(formula(f), x, ...)
  m$var.names <- features
  m
}

check.betareg.model.def <- function(model.def, target, data, weights){
  problems <- list()

  missing <- setdiff(model.def$features, names(data))
  available <- setdiff(model.def$features, missing)
  if(length(missing) != 0)
    problems$missing.features <- missing

  na.target <- any(is.na(target))
  if(na.target)
    problems$na.target <- NA

  nan.target <- any(is.nan(target))
  if(nan.target)
    problems$nan.target <- NA

  infinite.target <- any(is.infinite(target))
  if(infinite.target)
    problems$infinite.target <- NA

  invalid.target <- !any(is.between(target, c(0,1), F))
  if(invalid.target)
    problems$invalid.target <- NA

  invalid.weights <- any(is.na(weights) | is.nan(weights) | is.infinite(weights))
  if(invalid.weights)
    problems$invalid.weights <- NA
  else{
    negative.weights <- any(weights < 0)
    if(negative.weights)
      problems$negative.weights <- NA
  }

  problems
}

betareg.predict <- function(object,newdata,type=c('response','variance'), ...)
  named(cbind %wargs% lapply(type, function(t) predict(object, newdata, type=t,...)),
        type, 'col')


betareg.model.report <- function(object, root, text.as = 'txt', log.level = SimpleLog.INFO, .parallel = TRUE){
  stop.if(file.exists(root), 'output directory "%s" already exists ', root)

  logger <- SimpleLog('betareg.model.report', log.level)

  dir.create(root, recursive = TRUE)
  cat(str_replace_all(paste(capture.output(summary(object)), collapse = '\n'), '[’‘]', '"'),
      file=file.path(root, 'summary.txt'))
}

#####################################
#### pca modifications and helpers
#####################################

spectral.pca.fit <- function(X, center=T, scale=F){
  ## perform principal component analysis on x using spectral decomposition
  ## X: matrix-like on which to perform PCA
  ## center: center the columns
  ## scale: scale the columns

  ## head(spectral.pca.predict(spectral.pca.fit(as.matrix(USArrests)),USArrests))

  X <- scale(X,scale=scale, center=center)
  centers <- attr(X, "scaled:center")
  scales <- attr(X, "scaled:scale")
  stop.if(any(scales == 0), 'cannot rescale constant columns to unit variance (%s)', paste(which(scales == 0),collapse=','))

  XXt <- t(X) %*% X

  eig <- eigen(XXt, TRUE)

  P <- t(eig$vectors)

  list(p=P, centers=if(is.null(centers)) NA else centers, scales=if(is.null(scales)) NA else scales)
}

spectral.pca.predict <- function(object,newdata){
  t(object$p %*% t(scale(newdata,
                         center=if(all(is.na(object$centers))) FALSE else object$centers,
                         scale=if(is.na(object$scales)) FALSE else object$scales)))
}

svd.pca.fit <- function(X, center=FALSE, scale=FALSE, tol=function(sds) length(sds), method=NULL, ...){
  ## perform principal component analysis on x using SVD
  ##   decompose into X = U D V' => X == u %*% diag(d) %*% t(v)
  ## X: matrix-like on which to perform PCA
  ## k: number of PCs to compute
  ## center: center the columns
  ## scale: scale the columns
  ## tol: level at which PCs are discarded

  ## head(svd.pca.predict(svd.pca.fit(USArrests,center=T),USArrests))

  if(is.null(method))
    method <- 'svd'

  stop.if.not(method %in% c('svd', 'irlba'), 'unknown method "%s"', method)
  stop.if.not(length(dim(X)) == 2, 'X must be 2-dimensional')

  centers <- NULL
  scales <- NULL
  if(center || scale){
    X <- scale(as.matrix(X), center=center, scale=scale)
    centers <- attr(X, "scaled:center")
    scales <- attr(X, "scaled:scale")
    stop.if(any(scales == 0), 'cannot rescale constant columns to unit variance (%s)', paste(which(scales == 0),collapse=','))
  }

  svd.f <- svd
  if(method == 'irlba'){
    svd.f <- irlba
  }

  z <- svd.f(X, ...)
  ## assume z is of form list(u=, d=, v=, ...)

  z$sd <- z$d/sqrt(max(1, nrow(X) - 1))
  z$r <- length(z$d)
  if(!is.null(tol)){
    r <- tol(z$sd) ## r <- sum(z$sd > (z$sd[1L * tol]))
    z$r <- r
    if(r < ncol(X)){
      z$v <- z$v[, 1L:r, drop=FALSE]
      z$sd <- z$sd[1L:r]
    }
  }
  dimnames(z$v) <- list(colnames(X), paste0("PC", seq_len(ncol(z$v))))

  z$centers <- if(is.null(centers)) NA else centers
  z$scales <- if(is.null(scales)) NA else scales

  z
}

svd.pca.importance <- function(object){
  v <- object$sd^2
  prop.var <- v / sum(v)
  cum.prop <- cumsum(prop.var)

  named(data.frame(sd=object$sd, var=v, prop.var=prop.var, cum.prop=cum.prop),
        colnames(object$v),
        'row')

}

svd.pca.predict <- function(object, newdata, npcs=NULL){
  if(is.null(npcs))
    npcs <- object$r

  stop.if.not(length(dim(newdata)) == 2, 'newdata must be 2-dimensional')

  stop.if.not((!is.null(row.names(object$v)) && all(row.names(object$v) == colnames(newdata))) ||
              (is.null(row.names(object$v)) && (ncol(newdata) == nrow(object$v))),
              'newdata does not have the correct number of columns')

  scaled.newdata <- scale(newdata[,1:npcs],
                          center=if(all(is.na(object$centers))) FALSE else object$centers[1:npcs],
                          scale=if(is.na(object$scales)) FALSE else object$scales[1:npcs])

  scaled.newdata %*% object$v[1:npcs,]
}

svd.pca.model.def <- function(id, features, ..., nv=length(features)){
  ## import('mdls')
  ## mdls.fit(USArrests, pca.model.def("pca", names(USArrests))) -> ms

  params <- list(...)
  params$nv <- nv
  list(id=id, target.gen=NULL, fit=function(x,y,...,weights=NULL) pca.fit(x,...), features=features,
       predict = function(...) pca.predict(...), params = params,
       check=check.svd.pca.model.def, weights=NULL, report=svd.pca.model.report)
}

svd.pca.plot <- function(object, newdata, npcs = NULL, labels = NULL, type='scatter', return.grid=FALSE){
  stop.if.not(type %in% c('scatter'))
  if(type == 'scatter'){
    ret.grid <- as.data.frame(pca.predict(object, newdata, npcs)[, 1:2])
    ret.grid$label <- if(is.null(labels)) row.names(newdata) else labels

    g <- ggplot(ret.grid, aes(PC1, PC2)) + geom_point(aes(color=label)) + theme(legend.position="bottom")
  }

  if(return.grid)
    ret.grid
  else
    g
}

check.svd.pca.model.def <- function(model.def, target, data, weights){
  problems <- list()

  missing <- setdiff(model.def$features, colnames(data))
  available <- setdiff(model.def$features, missing)
  if(length(missing) != 0)
    problems$missing.features <- missing

  na.data <- any(is.na(data))
  if(na.data)
    problems$na.data <- NA

  inf.data <- any(is.infinite(data))
  if(inf.data)
    problems$infinite.data <- NA

  problems
}

svd.pca.model.report <- function(object, root, text.as = 'txt', log.level = SimpleLog.INFO, .parallel = TRUE){
  stop.if(file.exists(root), 'output directory "%s" already exists ', root)

  logger <- SimpleLog('pca.model.report', log.level)

  dir.create(root, recursive = TRUE)

  if('txt' == text.as)
    format.fun <- pprint.dataframe
  else if('html' == text.as)
    format.fun <- function(...) paste(sorttable.import(), dataframe.to.html.table(...), sep='\n')
  else if('textile' == text.as)
    format.fun <- dataframe.to.textile

  cat(format.fun(pca.importance(object)), file=file.path(root, sprintf('pc-importance.%s', text.as)))
}

#####################################
#### optimx modifications and helpers
#####################################

optimx.fit <- function(x, y,
                       f = function(w, x) as.matrix(x) %*% w,
                       loss = function(y, y.hat) mean(y - y.hat)^2,
                       par = vector('double', ncol(x)), ...){
  optimx(par, function(w) loss(y, f(w,x)), ...)
}

optimx.model.def <- function(id, target.gen, features,
                             f = function(w, x) as.matrix(x) %*% w,
                             loss = function(y, y.hat) mean((y - y.hat)^2),
                             par = vector('double', length(features)),
                             method = 'BFGS',
                             report=function(..., log.level=SimpleLog.INFO){logger <- SimpleLog('optimx.model.report', log.level); write.msg(logger, 'nothing to report for optimx model')}, ...){
  params <- list(par=par, ..., method=method)

  stop.if.not(length(method) == 1, 'optimx.model.def "%d" must have length 1', id)

  list(id=id, target.gen=target.gen, fit = function(..., weights=NULL) { optimx.fit(...) },
       features=features,
       predict=function(w, x) f(unlist(w$par), x),
       params=params,
       check=function(...) list(),
       weights=function(...) NULL, report=report)
}

#######################################
#### Feature Helpers ##################
#######################################

feature.contributions <- function(mdl, src, snk, select=which.max, score.idx = 1, log.level=SimpleLog.ERROR, .parallel=FALSE){
  ## compute approximate feature contributions - useful for seeing why an example receives a higher/lower score than another example
  ## mdl: model (one element our mdls.fit output)
  ## src: starting example
  ## snk: target example
  ## select: method of selecting next feature

  logger <- SimpleLog('feature.contributions',log.level)
  ## feature.contributions(ms$gbmmodel,iris[1,],iris[100,],which.max)
  ## feature.contributions(list(id="m",model=ms$gbmmodel$model,features=ms$gbmmodel$model$var.names,predict=gbm.predict), iris[6,], iris[5,])
  features <-   mdl$features
  src <- subset(src,select=features) -> osrc
  snk <- subset(snk,select=features) -> osnk

  predict.log.level <- if('debug' %in% log.level) SimpleLog.DEBUG else c('warning', 'error')

  extract.score <- function(x) if(is.matrix(x)) x[,score.idx] else x

  md <- list(this.model=mdl)
  names(md) <- mdl$id
  srcScore <- extract.score(mdls.predict(md,src,log.level=predict.log.level)[[1]][[1]])
  snkScore <- extract.score(mdls.predict(md,snk,log.level=predict.log.level)[[1]][[1]])

  selected.features <- NA
  scores <- srcScore
  while(length(features) > 0){
    s <- laply(features,
               function(ft){
                 src[[ft]] <- snk[[ft]]
                 extract.score(mdls.predict(md,src,log.level=predict.log.level)[[1]][[1]])
               }, .parallel=.parallel)

    selected <- select(snkScore - s)
    ft <- features[selected]
    write.msg(logger,'feature %d selected: %s',length(selected.features)-1,ft)

    selected.features <- c(selected.features,ft)
    features <- setdiff(features,ft)
    scores <- c(scores,s[selected])

    src[[ft]] <- snk[[ft]]
  }

  z <- data.frame(tail(selected.features,-1),
                  diff(scores),
                  t(osrc[,tail(selected.features,-1)]),
                  t(osnk[,tail(selected.features,-1)]),
                  head(scores,-1),
                  tail(scores,-1)
                  )

  names(z) <- c('var.name','delta','src.var.value','snk.var.value','score.before','score.after')
  row.names(z) <- NULL
  z[order(z$delta,decreasing=TRUE),]
}

feature.selection.by.filter <- function(target, features, initSelected, evaluate, update, n=ncol(features),log.level=SimpleLog.ERROR){
  ## greedy feature selection
  ## target: target of model
  ## features: dataframe with features
  ## initSelected: initially selected features
  ## evaluate: how to evaluate features for selecting the next one
  ## update: update scores
  ## n: number of features to select

  logger <- SimpleLog('feature.selection.by.filter',log.level)
  z <- list(selected = initSelected,
            remaining = setdiff(names(features), initSelected),
            scores = c(),
            complete_scores = list())
  for(i in 1:n){
    if(length(z$remaining) > 0){
      scores <- evaluate(target, subset(features,select=z$selected), subset(features,select=z$remaining),log.level=log.level)
      z <- update(z, scores)
      write.msg(logger,'feature %d: %s',i, tail(z$selected,1))
      z$complete_scores <- c(z$complete_scores, list(scores))
    }
  }
  z
}

forward.filter.feature.selection <- function(target, features, evaluate=cor.feature.selection.filter, choose.best=max, n=ncol(features), .parallel=FALSE, log.level=SimpleLog.ERROR){
  ## forward filter feature selectio
  logger <- SimpleLog('forward.filter.feature.selection',log.level)
  feature.selection.by.filter(target, features, NULL, function(...) evaluate(...,.parallel=.parallel),
                              function(z, scores){
                                are.na <- names(scores)[is.na(scores)]
                                write.msg(logger,'score for features "%s" is na - dropping',paste(are.na, collapse = ','), level='warn')
                                bestScore <- choose.best(na.rm(scores))
                                best <- match(TRUE,scores == bestScore)
                                z$selected <- c(z$selected, names(scores[best]))
                                z$remaining <- z$remaining[-1 * best]
                                z$remaining <- z$remaining[!(z$remaining %in% are.na)]
                                z$scores <- c(z$scores, bestScore)
                                z
                              },
                              n=n
                              )
}

interinfo.feature.selection.filter <- function(t,s,r,.parallel=FALSE,log.level=SimpleLog.ERROR){
  logger <- SimpleLog('interinfo.feature.selection.filter',log.level)
  logger$level <- log.level
  remaining <- names(r)
  scores <- laply(remaining,
                  function(f) interinformation(cbind(s, t, r[[f]])),
                  .parallel=.parallel)
  names(scores) <- remaining
  scores
}

cor.feature.selection.filter <- function(t,s,r,.parallel=FALSE,log.level=SimpleLog.ERROR){
  logger <- SimpleLog('cor.feature.selection.filter',log.level)
  remaining <- names(r)
  scores <- laply(remaining,
                  function(f) abs(cor(t,
                                      r[[f]])) - (if(ncol(s)==0) 0 else max(abs(cor(s, r[[f]]))))^2,
                  .parallel=.parallel)
  names(scores) <- remaining
  scores
}

gbm.feature.selection.filter <- function(..., train.fraction=0.8, verbose=F){
  ## import('mdls'); forward.filter.feature.selection(iris$Sepal.Length,iris[,2:4], gbm.feature.selection.filter(distribution='gaussian',verbose=F,n.trees=200,interaction.depth=6,keep.data=F))
  function(t,s,r,.parallel=FALSE,log.level=SimpleLog.ERROR){
    nTrain <- train.fraction * length(t)
    logger <- SimpleLog('gbm.feature.selection.filter',log.level)
    remaining <- names(r)
    scores <- laply(remaining,
                    function(f) {
                      s.f <- cbind(s, subset(r,select=f))
                      m <- gbm.fit(s.f, t, ..., nTrain=nTrain, verbose=verbose)

                      -min(m$valid.error)
                    },
                    .parallel=.parallel)
    names(scores) <- remaining
    scores
  }
}

glm.feature.selection.filter <- function(..., f=function(x) x^2){
  ## import('mdls'); forward.filter.feature.selection(iris$Sepal.Length,iris[,2:4], glm.feature.selection.filter(family='gaussian'))
  function(t,s,r,.parallel=FALSE,log.level=SimpleLog.ERROR){
    logger <- SimpleLog('glm.feature.selection.filter',log.level)
    remaining <- names(r)
    scores <- laply(remaining,
                    function(f) {
                      s.f <- cbind(s, subset(r,select=f))
                      m <- glm.fit.plus(s.f, t, ...)

                      -mean(f(residuals(m)))
                    },
                    .parallel=.parallel)
    names(scores) <- remaining
    scores
  }
}

######################
#### Metrics
######################

clsfy.confusion <- function(prediction, label){
  ## make confusion matrix
  p <- as.logical(prediction)
  l <- as.logical(label)
  data.frame(true.positive = sum(p & l),
             false.positive = sum(p & !l),
             true.negative = sum(!p & !l),
             false.negative = sum(!p & l))
}

clsfy.confusion.scan <- function(score, label, to.prediction = function(t){ function(s) s > t }, params.list=as.list(quantile(score,seq(0,1,0.01))), .parallel=FALSE){
  ## confusion matrix at specified thresholds
  rbind %wargs% llply(named(parameter.scan(to.prediction, params.list), names(params.list)),
                      function(f)
                      clsfy.confusion(f(score),label),
                      .parallel=.parallel)
}

## see http://en.wikipedia.org/wiki/Receiver_operating_characteristic for definitions

clsfy.precision <- function(confusion)
  confusion$true.positive / (confusion$true.positive + confusion$false.positive)

clsfy.recall <- function(confusion)
  confusion$true.positive / (confusion$true.positive + confusion$false.negative)

clsfy.accuracy <- function(confusion)
  (confusion$true.positive + confusion$true.negative) / (confusion$true.positive + confusion$false.positive + confusion$true.negative + confusion$false.negative)

clsfy.fallout <- function(confusion)
  confusion$false.positive / (confusion$false.positive + confusion$true.negative)





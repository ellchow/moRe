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
  all(names(x) == c(
             "id", # name of model
             "target.gen", # function that takes in a superset of the training data  and returns the target
             "fit", # function for fitting the model of the same form as gbm.fit
             "features", # vector of feature names to be used by the model
             "predict", # function for computing a prediction of the same form as gbm.predict
             "params", # extra parameters for the fitting function
             "check", # function that takes in a model definition, target, and data and checks if there are any issues
             "weights", # weights on training examples
             "report" # function that generates report for model
             ))
}

mdls.fit <- function(datasets, ..., mapping = list(".*"=".*"), log.level=SimpleLog.ERROR, .parallel=FALSE){
  ## import('mdls')
  ## mdls.fit(iris[,1:4],
  ##          gbm.model.def("gbmmodel", Sepal.Length,
  ##                        c('Sepal.Width','Petal.Length','Petal.Width'),
  ##                        distribution='gaussian',train.fraction=0.8,interaction.depth=6,weights=function(data) runif(nrow(data))),
  ##          lm.model.def('lmmodel', Sepal.Length,
  ##                       c('Sepal.Width','Petal.Length','Petal.Width')),
  ##          glm.model.def('glmmodel', Sepal.Length,
  ##                       c('Sepal.Width','Petal.Length','Petal.Width'), family = 'gaussian'),
  ##          betareg.model.def("betaregmodel", Sepal.Width / Sepal.Length,
  ##                            c('Sepal.Width','Petal.Length','Petal.Width'),
  ##                            phi.features='Sepal.Width'),
  ##          .parallel=F) -> ms

  logger <- SimpleLog('mdls.fit',log.level)

  datasets <- if(is.data.frame(datasets) || !is.list(datasets)) list(datasets) else datasets
  model.defs <- list(...)
  dataset.ids <- if(!is.null(names(datasets))) names(datasets) else sapply(1:length(datasets),int.to.char.seq)

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
                   }else{
                     data <- as.data.frame(data)
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
                                             write.msg(logger, sprintf('building target for "%s"', id))

                                             t <- tryCatch(md$target.gen(data),
                                                           error=function(e){
                                                             write.msg(logger,str_trim(as.character(e)),
                                                                       level='error')
                                                             NA
                                                           })
                                             write.msg(logger, sprintf('adding weights for "%s"', id))
                                             w <- tryCatch(md$weights(data),
                                                           error=function(e){
                                                             write.msg(logger,str_trim(as.character(e)),
                                                                       level='error')
                                                             NA
                                                           })
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
                                               write.msg(logger, sprintf('training "%s"', id))

                                               m <- tryCatch(do.call(md$fit,
                                                                     c(list(subset(data,
                                                                                   select=grep("^\\|$",md$features,invert=T,value=T)),
                                                                            t,
                                                                            weights=w),
                                                                       md$params)),
                                                             error=function(e){
                                                               write.msg(logger,str_trim(as.character(e)),
                                                                         level='error')
                                                               NA
                                                             })
                                               if(!any(is.na(m))){
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

mdls.predict <- function(models, datasets, mapping=list(".*"=".*"), log.level=SimpleLog.ERROR, .parallel=FALSE){
  logger <- SimpleLog('mdls.predict',log.level)
  datasets <- if(is.data.frame(datasets) || !is.list(datasets)) list(datasets) else datasets
  dataset.ids <- if(!is.null(names(datasets))) names(datasets) else sapply(1:length(datasets),int.to.char.seq)

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
                   }else{
                     data <- as.data.frame(data)
                   }

                   models.filtered <- Filter(function(m){
                     any(sapply(lzip(names(mapping), mapping),
                                function(mp) str_detect(ds.id, mp[[1]]) && str_detect(m[[1]], mp[[2]]) ))

                   }, lzip(names(models), models))

                   start.timer(timer,sprintf('computing predictions on "%s"', ds.id))
                   z <- flatten(llply(models.filtered,
                                      function(x){
                                        id <- x[[1]]
                                        m <- x[[2]]$model

                                        features <- x[[2]]$features
                                        predict <- x[[2]]$predict

                                        write.msg(logger,sprintf('predicting with "%s"', id))
                                        pr <- predict(m, subset(data,select=features))

                                        z <- list(pr)
                                        names(z) <- id
                                        z
                                      }, .parallel=.parallel))




                   z <- named(list(z), ds.id)
                   stop.timer(timer)
                   z
                 })
          )
}

mdls.report <- function(mdls, root, text.as = 'html', overwrite = FALSE, log.level = SimpleLog.INFO, .parallel=FALSE){
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


gbm.model.def <- function(id, target.expr, features, ..., weights=function(data) NULL){
  params <- list(...)
  g <- tryCatch(params$distribution$group, error=function(e) NULL)
  features <- unique(c(features,g))

  t <- substitute(target.expr)

  list(id=id, target.gen=function(data) eval(t, envir=data), fit=function(...,weights=NULL) gbm.fit.plus(...,group = g, w=weights), features=features, predict=gbm.predict, params=params, check=check.gbm.model.def, weights=weights, report=gbm.model.report)
}

gbm.fit.plus <- function(x, y, ..., group = NULL, y.label="y"){
  features <- setdiff(names(x), group)
  x[[y.label]] <- y
  f <- sprintf("%s ~ %s", y.label, paste(features, collapse=" + "))
  gbm(formula(f), x, ...)
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

  gt1024levels <- sapply(available,
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
  gbm.perf(object,method=method,plot.it=F)

gbm.tree.as.df <- function(object, i.tree = 1){
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
  gbm.tree.row.as.list(gbm.tree.as.df(object, i.tree), 1)

gbm.model.as.list <- function(object, n.trees=object$n.trees, name="", .parallel=FALSE){
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
  as.character(subset(summary.gbm(object,plotit=F), rel.inf > 0,)$var)

gbm.split.points <- function(object, var.name=1, trees=object$n.trees){
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

gbm.model.report <- function(object, root, text.as = 'html', plot.it = TRUE, log.level = SimpleLog.INFO, .parallel = TRUE){

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

lm.model.def <- function(id, target.expr, features, ..., weights=function(data) NULL){
  t <- substitute(target.expr)

  list(id=id, target.gen=function(data) eval(t, envir=data), fit=lm.fit.plus, features=features, predict=predict.lm, params=list(...), check=check.lm.model.def, weights=weights, report=lm.model.report)
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

glm.model.def <- function(id, target.expr, features, ..., weights=function(data) NULL){
  params <- list(...)
  t <- substitute(target.expr)

  list(id=id, target.gen=function(data) eval(t, envir=data), fit=glm.fit.plus, features=features, predict=glm.predict, params=params, check=check.glm.model.def, weights=weights, report=glm.model.report)
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

glmnet.model.def <- function(id, target.expr, features, ..., weights=function(data) NULL){
  t <- substitute(target.expr)

  list(id=id, target.gen=function(data) eval(t, envir=data), fit=glmnet.fit, features=features, predict=glmnet.predict, params=list(...), check=check.glmnet.model.def, weights=weights, report=glmnet.model.report)
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

betareg.model.def <- function(id, target.expr, features, ..., phi.features=NULL, weights=function(data) NULL){
  all.features <- unique(c(features, phi.features))

  t <- substitute(target.expr)

  list(id=id, target.gen=function(data) eval(t, envir=data), fit=betareg.fit.plus, features=all.features, predict=betareg.predict, params=c(list(...), list(features=features, phi.features = phi.features)), check=check.betareg.model.def, weights=weights, report=betareg.model.report)
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

#######################################
#### Feature Helpers ##################
#######################################

feature.contributions <- function(mdl, src, snk, select=which.max, score.idx = 1, log.level=SimpleLog.ERROR, .parallel=FALSE){
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

forward.filter.feature.selection <- function(target, features, evaluate=cor.feature.selection.filter, choose.best=max, n=ncol(features), .parallel=FALSE, log.level=SimpleLog.ERROR){
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

feature.selection.by.filter <- function(target, features, initSelected, evaluate, update, n=ncol(features),log.level=SimpleLog.ERROR){
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

######################
#### Metrics
######################

clsfy.confusion <- function(prediction, label){
  p <- as.logical(prediction)
  l <- as.logical(label)
  data.frame(true.positive = sum(p & l),
             false.positive = sum(p & !l),
             true.negative = sum(!p & !l),
             false.negative = sum(!p & l))
}

clsfy.confusion.scan <- function(score, label, to.prediction = function(t){ function(s) s > t }, params.list=as.list(quantile(score,seq(0,1,0.01))), .parallel=FALSE){
  rbind %wargs% llply(named(parameter.scan(params.list, to.prediction), names(params.list)),
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










source('utils.R', chdir=TRUE)
dump <- sapply(c('gdata',
                 'stringr',
                 'plyr',
                 'doMC',
                 'gbm',
                 'infotheo',
                 'R.oo',
                 get_parallel_library()$lib
                 ), better.library)

get_parallel_library()$activate()

##############################################
#### Model Building
##############################################

setConstructorS3('ModelDef',
                 function(id=gsub('^..','',runif(1)),
                          target_gen=function(data){data$target},
                          fit=gbm.fit,
                          features=NULL,
                          predict=gbm_predict,
                          params=list(),
                          check=function(md,d,l){list()}){
                   extend(Object(), 'ModelDef',
                          id=id, # name of model
                          target_gen=target_gen, # function that takes in a superset of the training data  and returns the target
                          fit=fit, # function for fitting the model of the same form as gbm.fit
                          features=features, # vector of feature names to be used by the model
                          params=params, # extra parameters for the fitting function
                          predict=predict, # function for computing a prediction of the same form as gbm.predict
                          check=check # function that takes in a model definition, target, and data and checks if there are any issues
                          )
                 })
is_model_def <- function(x){
  'ModelDef' %in% class(x)
}

mdls_build <- function(datasets, modelDefs, logger=NULL, .parallel=TRUE){
  datasets <- if(is.data.frame(datasets))(list(datasets))else{datasets}
  modelDefs <- if(is_model_def(modelDefs)){list(modelDefs)}else{modelDefs}
  timer <- Timer(logger)
  flatten(lapply(lzip(if(!is.null(names(datasets))){names(datasets)}else{1:length(datasets)},
                      datasets),
                 function(x){
                   dsId <- x[[1]]
                   data <- x[[2]]

                   if(typeof(data) == 'character'){
                     t0 <- start_timer(timer,'loading dataset "%s"', dsId)
                     data <- get(load(data))
                     timer_stop(timer)
                   }else{
                     data <- as.data.frame(data)
                   }

                   start_timer(timer, sprintf('train models on "%s"', dsId))
                   models <- flatten(llply(modelDefs,
                                           function(md){
                                             id <- sprintf('%s%s', md$id,
                                                           if(length(datasets) > 1){
                                                             sprintf("__%s", dsId)
                                                           }else{""})
                                             write_msg(logger, sprintf('building target for "%s"', id))
                                             t <- tryCatch(md$target_gen(data),
                                                           error=function(e){
                                                             write_msg(logger,str_trim(as.character(e)),
                                                                       level='error')
                                                             NA
                                                           })
                                             problems <- md$check(md,t,data)
                                             if(length(problems) > 0){
                                               lapply(lzip(names(problems),problems),
                                                      function(p){
                                                        write_msg(logger,
                                                                  sprintf('problem with "%s" (%s): %s',
                                                                          id,
                                                                          p[[1]],
                                                                          if(is.na(p[[2]])){''}else{p[[2]]}),
                                                                  level='warning')
                                                      })
                                             }else{
                                               write_msg(logger, sprintf('training "%s"', id))
                                               m <- tryCatch(do.call(md$fit,
                                                                     c(list(subset(data,select=md$features),
                                                                            t),
                                                                       md$params)),
                                                             error=function(e){
                                                               write_msg(logger,str_trim(as.character(e)),
                                                                         level='error')
                                                               NA
                                                             })
                                               if(!any(is.na(m))){
                                                 z <- list(list(target=t,
                                                                model=m,
                                                                predict=md$predict,
                                                                features=md$features))
                                                 names(z) <- id
                                                 return(z)
                                               }
                                             }
                                             write_msg(logger,sprintf('could not train "%s" (skipped)', id,
                                                       level='warning'))

                                             list()
                                           },
                                           .parallel=.parallel))
                   stop_timer(timer)
                   models
                 }))
}

mdls_predict <- function(models, datasets, logger=NULL){
  models <- if(!is.list(models)){list(models)}else{models}
  datasets <- if(is.data.frame(datasets)){list(datasets)}else{datasets}
  timer <- Timer(logger)
  flatten(lapply(lzip(if(!is.null(names(datasets))){names(datasets)}else{sapply(1:length(datasets),int_to_char_seq)},
                      datasets),
                 function(x){
                   dsId <- x[[1]]
                   data <- x[[2]]
                   if(typeof(data) == 'character'){
                     start_timer(timer,sprintf('loading dataset "%s"', dsId))
                     data <- get(load(data))
                     stop_timer(timer)
                   }else{
                     data <- as.data.frame(data)
                   }
                   start_timer(timer,sprintf('computing predictions on "%s"', dsId))
                   z <- flatten(llply(lzip(names(models), models),
                                      function(x){
                                        id <- x[[1]]
                                        m <- x[[2]]$model

                                        features <- x[[2]]$features
                                        predict <- x[[2]]$predict
                                        if(is.null(x[[2]]$predictions)){
                                          x[[2]]$predictions <- list()
                                        }

                                        write_msg(logger,sprintf('predicting with "%s"', id))
                                        pr <- predict(m, subset(data,select=features))

                                        x[[2]]$predictions[[as.character(dsId)]] <- pr
                                        z <- list(x[[2]])
                                        names(z) <- id
                                        z
                                      }))
                   stop_timer(timer)
                   z
                 })
          )
}

#####################################
#### gbm modifications and helpers
#####################################

gbm_predict <- function(object,newdata,n.trees=NULL,type='response',...){
  trees = if(is.null(n.trees)) gbm.perf(object,method='test',plot.it=FALSE) else n.trees
  predict.gbm(object,newdata,n.trees=trees,type=type,...)
}

check_gbm_model_def <- function(modelDef, target, data){
  problems <- list()

  missing <- setdiff(modelDef$features, names(data))
  available <- setdiff(modelDef$features, missing)
  if(length(missing) != 0){problems$missing_factors <- missing}

  gt1024levels <- sapply(available,
                         function(f){is.factor(data[[f]]) && (length(levels(data[[f]])) > 1024)})
  if(any(gt1024levels)){problems$too_many_levels <- available[gt1024levels]}

  all_na <- sapply(available, function(f){all(is.na(data[[f]]))})
  if(any(all_na)){problems$all_na <- available[all_na]}

  monotonicity <- (('var.monotone' %in% names(modelDef$params)) &&
                   ((length(modelDef$params$var.monotone) != length(modelDef$features)) ||
                    !all(modelDef$params$var.monotone %in% (-1:1))))
  if(monotonicity){problems$monotonicity <- NA}

  invalid_target <- any(is.na(target)) || ((!('distribution' %in% names(modelDef$params)) || modelDef$params$distribution == 'bernoulli') && (!all(target %in% (0:1))))
  if(invalid_target){problems$invalid_target <- NA}

  problems
}

setConstructorS3('GbmModelDef',
                 function(id, target_gen, features, params=list()){
                   extend(ModelDef(id, target_gen, gbm.fit, features, gbm_predict, params=params, check=check_gbm_model_def), 'ModelDef')
                 })

## make_gbm_model_def <- function(id, target_gen, features, params=list()){
  ## make_model_def(id, target_gen, gbm.fit, features, gbm_predict, params=params, check=check_gbm_model_def)
## }

gbm_dependency_plot <- function (x, i.var = 1, n.trees = x$n.trees, continuous.resolution = 100,
                             return.grid = FALSE, ...){
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
    warning("plot.gbm creates up to 3-way interaction plots.\nplot.gbm will only return the plotting data structure.")
    return.grid = TRUE
  }
  ## error check for invalid quantiles
  if(length(continuous.resolution) > 1 && any(continuous.resolution < 0 | continuous.resolution > 1)){
    stop("Invalid quantiles specified in continuous.resolution: ", continuous.resolution)
  }
  grid.levels <- vector("list", length(i.var))
  for (i in 1:length(i.var)) {
    if (is.numeric(x$var.levels[[i.var[i]]])) {
      ## allow for quantile selection
      if(length(continuous.resolution) > 1){
        grid.levels[[i]] <- as.vector(quantile(x$var.levels[[i.var[i]]], probs=continuous.resolution))
      }else{
        grid.levels[[i]] <- seq(min(x$var.levels[[i.var[i]]]),
                                max(x$var.levels[[i.var[i]]]), length = continuous.resolution)
      }
    }
    else {
      grid.levels[[i]] <- as.numeric(factor(x$var.levels[[i.var[i]]],
                                            levels = x$var.levels[[i.var[i]]])) - 1
    }
  }
  X <- expand.grid(grid.levels)
  names(X) <- paste("X", 1:length(i.var), sep = "")
  X$y <- .Call("gbm_plot", X = as.double(data.matrix(X)), cRows = as.integer(nrow(X)),
               cCols = as.integer(ncol(X)), i.var = as.integer(i.var -
                                              1), n.trees = as.integer(n.trees), initF = as.double(x$initF),
               trees = x$trees, c.splits = x$c.splits, var.type = as.integer(x$var.type),
               PACKAGE = "gbm")
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
      plot(X$X1, X$y, type = "l", xlab = x$var.names[i.var],
           ylab = paste("f(", x$var.names[i.var], ")", sep = ""),
           ...)
    }
    else {
      plot(X$X1, X$y, xlab = x$var.names[i.var], ylab = paste("f(",
                                                   x$var.names[i.var], ")", sep = ""), ...)
    }
  }
  else if (length(i.var) == 2) {
    if (!f.factor[1] && !f.factor[2]) {
      levelplot(y ~ X1 * X2, data = X, xlab = x$var.names[i.var[1]],
                ylab = x$var.names[i.var[2]], ...)
    }
    else if (f.factor[1] && !f.factor[2]) {
      xyplot(y ~ X2 | X1, data = X, xlab = x$var.names[i.var[2]],
             ylab = paste("f(", x$var.names[i.var[1]], ",",
               x$var.names[i.var[2]], ")", sep = ""), type = "l",
             ...)
    }
    else if (!f.factor[1] && f.factor[2]) {
      xyplot(y ~ X1 | X2, data = X, xlab = x$var.names[i.var[1]],
             ylab = paste("f(", x$var.names[i.var[1]], ",",
               x$var.names[i.var[2]], ")", sep = ""), type = "l",
             ...)
    }
    else {
      stripplot(y ~ X1 | X2, data = X, xlab = x$var.names[i.var[2]],
                ylab = paste("f(", x$var.names[i.var[1]], ",",
                  x$var.names[i.var[2]], ")", sep = ""), ...)
    }
  }
  else if (length(i.var) == 3) {
    i <- order(f.factor)
    X.new <- X[, i]
    X.new$y <- X$y
    names(X.new) <- names(X)
    if (sum(f.factor) == 0) {
      X.new$X3 <- equal.count(X.new$X3)
      levelplot(y ~ X1 * X2 | X3, data = X.new, xlab = x$var.names[i.var[i[1]]],
                ylab = x$var.names[i.var[i[2]]], ...)
    }
    else if (sum(f.factor) == 1) {
      levelplot(y ~ X1 * X2 | X3, data = X.new, xlab = x$var.names[i.var[i[1]]],
                ylab = x$var.names[i.var[i[2]]], ...)
    }
    else if (sum(f.factor) == 2) {
      xyplot(y ~ X1 | X2 * X3, data = X.new, type = "l",
             xlab = x$var.names[i.var[i[1]]], ylab = paste("f(",
                                                paste(x$var.names[i.var[1:3]], collapse = ","),
                                                ")", sep = ""), ...)
    }
    else if (sum(f.factor) == 3) {
      stripplot(y ~ X1 | X2 * X3, data = X.new, xlab = x$var.names[i.var[i[1]]],
                ylab = paste("f(", paste(x$var.names[i.var[1:3]],
                  collapse = ","), ")", sep = ""), ...)
    }
  }
}

#####################################
#### glm modifications and helpers
#####################################

glm_predict <- function(object,newdata,type='response',...){
  predict.glm(object,newdata,type=type,...)
}

check_glm_model_def <- function(modelDef, target, data){
  problems <- list()

  missing <- setdiff(modelDef$features, names(data))
  available <- setdiff(modelDef$features, missing)
  if(length(missing) != 0){problems$missing_factors <- missing}

  has_na <- sapply(available, function(f){any(is.na(data[[f]]))})
  if(any(has_na)){problems$has_na <- available[has_na]}

  problems
}

make_glm_model_def <- function(id, target_gen, features, params=list()){
  make_model_def(id, target_gen, glm.fit, features, glm_predict, params=params, check=check_glm_model_def)
}

## logger <- make_logger('mylogger')
## x <- get(load('data0.rda'))
## md <- list(make_glm_model_def('glm',function(x)x$x, c('b')),
##           make_gbm_model_def('gbm',function(x)x$x, c('a','b'),params=list(distribution='gaussian',train.frac=0.8))
##           )
## mdls_build(x,md,logger=logger) -> m
## mdls_predict(m,x,logger=logger) -> y


##############################################
#### Feature Selection
##############################################

pairwise_compare_vectors <- function(data, pairs, cmp=function(x,y){cor(x,y)}, .parallel=FALSE){
  z <- adply(pairs,2,
             function(i){
               i <- sort(i)
               data.frame(factor_a=i[1], factor_b=i[2], value=cmp(data[[i[1]]], data[[i[2]]]))
             },
             .parallel=.parallel)

  z[[1]] <- NULL
  z
}


mrmr_feature_selection_filter <- function(t,s,r){
  ## http://penglab.janelia.org/papersall/docpdf/2005_TPAMI_FeaSel.pdf
  remaining <- names(r)
  scores <- sapply(remaining,
                     function(f){
                       ## mi <- as.data.frame(mutinformation(sx))
                       mi <- if(length(s) > 0){
                         sapply(c(names(s)),
                                function(ff){
                                  mutinformation(r[[f]], s[[ff]])
                                })
                       }else{
                         0
                       }
                       relevance <- mutinformation(t,r[[f]])
                       redundancy <- mean(mi)
                       z <- relevance - redundancy
                       z
                     }
                     )
  names(scores) <- remaining
  scores
}

cor_feature_selection_filter <- function(t,s,r){
  remaining <- names(r)
  scores <- sapply(remaining,
                   function(f){
                     z <- abs(cor(t, r[[f]])) - (if(ncol(s)==0){0}else{max(abs(cor(s, r[[f]])))})^2
                     z
                   }
                   )
  names(scores) <- remaining
  scores
}

forward_filter_feature_selection <- function(target, features, evaluate=cor_feature_selection_filter, choose_best=max, n=ncol(features)){
  feature_selection_by_filter(target, features, NULL, evaluate,
                              function(z, scores){
                                scores <- scores[!is.na(scores)]
                                if(length(scores) == 0){
                                  z$remaining <- NULL
                                }else{
                                  bestScore <- choose_best(scores)
                                  best <- match(TRUE,scores == bestScore)
                                  z$selected <- c(z$selected, names(scores[best]))
                                  z$remaining <- z$remaining[-1 * best]
                                  z$scores <- c(z$scores, bestScore)
                                }
                                z
                              },
                              n=n
                              )
}

feature_selection_by_filter <- function(target, features, initSelected, evaluate, update, n=ncol(features)){
  z <- list(selected = initSelected,
            remaining = setdiff(names(features), initSelected),
            scores = c(),
            complete_scores = list())
  for(i in 1:n){
    scores <- evaluate(target, subset(features,select=z$selected), subset(features,select=z$remaining))
    z <- update(z, scores)
    z$complete_scores <- c(z$complete_scores, list(scores))
  }
  z
}

feature_selection_by_gbm <- function(target, features, data,
                                     gbmSettings=list(n.trees=100,shrinkage=0.05,interaction.depth=8,train.fraction=0.8,distribution='gaussian', keep.data=FALSE),
                                     keep=function(importance,iter){importance > quantile(importance,0.05)},
                                     stop_when=function(remaining,importance){
                                       (length(remaining) <= 10) || (min(importance) > 5)}){
  result <- list(list(remaining=features,
                      model=NA,
                      importance=NA
                      ))
  while(is.na(tail(result,1)[[1]]$importance) ||
        !stop_when(tail(result,1)[[1]]$remaining, tail(result,1)[[1]]$importance)){

    f <- sort(tail(result,1)[[1]]$remaining)
    model <- do.call(function(...) gbm.fit(subset(data,select=f), target,...), gbmSettings)
    optNumTrees <- max(1, gbm.perf(model,plot.it=F,method='test'))

    importance <- summary(model, n.trees=optNumTrees, plotit=F)
    importance <- importance[order(importance[[1]]),][[2]]

    toKeep <- keep(importance, length(result))

    result <- c(result,list(list(remaining = f[toKeep],
                                 model = model,
                                 importance = importance
                                 )))
  }
  result
}

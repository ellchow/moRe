#### Elliot Chow

source('utils.R', chdir=TRUE)
source('math.R', chdir=TRUE)
better.library('gdata',
               'stringr',
               'plyr',
               'doMC',
               'gbm',
               'infotheo',
               'rjson',
               'ggplot2',
               get.parallel.library()$lib
               )

get.parallel.library()$activate()

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
             "weights" # weights on training examples
             ))
}

mdls.fit <- function(datasets, ..., log.level=c('info','warning','error'), .parallel=TRUE){
  ## mdls.fit(iris[,1:4],
  ##          gbm.model.def("gbmmodel",function(x) x$Sepal.Length,
  ##                        c('Sepal.Width','Petal.Length','Petal.Width'),
  ##                        distribution='gaussian',train.fraction=0.8,weights=function(data) runif(nrow(data))),
  ##          lm.model.def('lmmodel', function(x) x$Sepal.Length,
  ##                       c('Sepal.Width','Petal.Length','Petal.Width')) ) -> ms
  ## mdls.predict(ms,iris[,1:4]) -> s

  logger <- SimpleLog('mdls.fit',log.level)

  datasets <- if(is.data.frame(datasets))(list(datasets))else{datasets}
  modelDefs <- list(...) ##if(is.model.def(modelDefs)){list(modelDefs)}else{modelDefs}

  timer <- Timer(logger)
  ## pair dataset with id
  flatten(lapply(lzip(if(!is.null(names(datasets))){names(datasets)}else{1:length(datasets)},
                      datasets),
                 function(x){
                   dsId <- x[[1]]
                   data <- x[[2]]

                   if(typeof(data) == 'character'){
                     t0 <- start.timer(timer,'loading dataset "%s"', dsId)
                     data <- load.data(data)
                     stop.timer(timer)
                   }else{
                     data <- as.data.frame(data)
                   }

                   start.timer(timer, sprintf('train models on "%s"', dsId))
                   models <- flatten(llply(modelDefs,
                                           function(md){
                                             id <- sprintf('%s%s', md$id,
                                                           if(length(datasets) > 1){
                                                             sprintf("__%s", dsId)
                                                           }else{""})
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
                                                                     c(list(subset(data,select=md$features),
                                                                            t,weights=w),
                                                                       md$params)),
                                                             error=function(e){
                                                               write.msg(logger,str_trim(as.character(e)),
                                                                         level='error')
                                                               NA
                                                             })
                                               if(!any(is.na(m))){
                                                 z <- list(list(target=t,
                                                                model=m,
                                                                id=id,
                                                                predict=md$predict,
                                                                features=md$features))
                                                 names(z) <- id
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

mdls.predict <- function(models, datasets, log.level=c('info','warning','error')){
  logger <- SimpleLog('mdls.predict',log.level)
  datasets <- if(is.data.frame(datasets)){list(datasets)}else{datasets}

  timer <- Timer(logger)
  flatten(lapply(lzip(if(!is.null(names(datasets))){names(datasets)}else{sapply(1:length(datasets),int.to.char.seq)},
                      datasets),
                 function(x){
                   dsId <- x[[1]]
                   data <- x[[2]]
                   if(typeof(data) == 'character'){
                     start.timer(timer,sprintf('loading dataset "%s"', dsId))
                     data <- load.data(data)
                     stop.timer(timer)
                   }else{
                     data <- as.data.frame(data)
                   }
                   start.timer(timer,sprintf('computing predictions on "%s"', dsId))
                   z <- flatten(llply(lzip(names(models), models),
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
                                      }))
                   z <- list(z)
                   names(z) <- dsId
                   stop.timer(timer)
                   z
                 })
          )
}

#####################################
#### gbm modifications and helpers
#####################################


gbm.model.def <- function(id, target.gen, features, ..., weights=function(data) NULL){
  list(id=id, target.gen=target.gen, fit=function(...,weights=NULL)gbm.fit(...,w=weights), features=features, predict=gbm.predict, params=list(...), check=check.gbm.model.def, weights=weights)
}


gbm.predict <- function(object,newdata,n.trees=NULL,type='response',...){
  trees = if(is.null(n.trees)) gbm.perf(object,method='test',plot.it=FALSE) else n.trees
  if(length(trees) != 1){
    stop('could not determine optimal number of trees')
  }
  predict.gbm(object,newdata,n.trees=trees,type=type,...)
}

check.gbm.model.def <- function(modelDef, target, data, weights){
  problems <- list()

  missing <- setdiff(modelDef$features, names(data))
  available <- setdiff(modelDef$features, missing)
  if(length(missing) != 0){problems$missing.factors <- missing}

  gt1024levels <- sapply(available,
                         function(f){is.factor(data[[f]]) && (length(levels(data[[f]])) > 1024)})
  if(any(gt1024levels)){problems$too.many.levels <- available[gt1024levels]}

  all.na <- sapply(available, function(f){all(is.na(data[[f]]))})
  if(any(all.na)){problems$all.na <- available[all.na]}

  monotonicity <- (('var.monotone' %in% names(modelDef$params)) &&
                   ((length(modelDef$params$var.monotone) != length(modelDef$features)) ||
                    !all(modelDef$params$var.monotone %in% (-1:1))))
  if(monotonicity){problems$monotonicity <- NA}

  na.target <- any(is.na(target))
  if(na.target){problems$na.target <- NA}

  no.distribution <- !('distribution' %in% names(modelDef$params))

  if(no.distribution){problems$no.distribution <- NA}
  else{
    invalid.bernoulli.target <- (modelDef$params$distribution == 'bernoulli') && (!all(target %in% (0:1)))
    if(invalid.bernoulli.target){problems$invalid.bernoulli.target <- NA}
  }

  invalid.weights <- any(is.na(weights) | is.nan(weights) | is.infinite(weights))
  if(invalid.weights){problems$invalid.weights <- NA}
  else{
    negative.weights <- any(weights < 0)
    if(negative.weights){problems$negative.weights <- NA}
  }
  problems
}

gbm.opt.n.trees <- function(object, method='test'){
  gbm.perf(object,method='test',plot.it=F)
}

gbm.tree.as.df <- function(object, i.tree = 1){
  if ((i.tree < 1) || (i.tree > length(object$trees))) {
    stop("i.tree is out of range ", length(object$trees))
  }else{
    tree <- data.frame(object$trees[[i.tree]])
    names(tree) <- c("SplitVar", "SplitCodePred", "LeftNode",
                     "RightNode", "MissingNode", "ErrorReduction", "Weight",
                     "Prediction")
    tree$LeftNode <- tree$LeftNode + 1
    tree$RightNode <- tree$RightNode + 1
    tree$MissingNode <- tree$MissingNode + 1
    tree$SplitVarName <- c("",object$var.names)[tree$SplitVar + 2]
    tree$SplitCodePred <- ifelse(i.tree == 1 & tree$SplitVarName=='', tree$SplitCodePred + object$initF, tree$SplitCodePred)
    tree$node <- 1:nrow(tree)
    tree[, c("node", "SplitVarName", "SplitCodePred", "LeftNode", "RightNode", "MissingNode")]
  }
}

gbm.tree.row.as.json <- function(tree.df, node){
  row <- tree.df[tree.df$node == node,]
  if (row$SplitVarName != "") {
    list(cond=list(var=row$SplitVarName,op="<", val=row$SplitCodePred),
         if_true = gbm.tree.row.as.json(tree.df, row$LeftNode),
         if_false = gbm.tree.row.as.json(tree.df, row$RightNode),
         if_missing = gbm.tree.row.as.json(tree.df, row$MissingNode))
  } else {
    list(prediction=row$SplitCodePred)
  }
}

gbm.tree.as.json <- function(object, n.trees=object$n.trees, i.tree=1){
  gbm.tree.row.as.json(gbm.tree.as.df(object, i.tree), 1)
}

gbm.model.json <- function(object, trees=object$n.trees, name=""){
  usedVariables <- gbm.model.used.variables(object)
  jsonTree <- lapply(1:trees, function(tree) gbm.tree.as.json(object, i.tree=tree, n.trees=trees))

  toJSON(list(name=name,
              bag.fraction=object$bag.fraction,
              distribution= object$distribution$name,
              interaction.depth=object$interaction.depth,
              n.minobsinnode=object$n.minobsinnode,
              n.trees=trees,
              shrinkage=object$shrinkage,
              factors = usedVariables,
              trees=jsonTree))
}

gbm.model.used.variables <- function(object, trees=object$n.trees){
  as.character(subset(summary.gbm(object,plotit=F), rel.inf > 0,)$var)
}

gbm.split.points <- function(object, var.name=1, trees=object$n.trees){
  if(is.numeric(var.name)){
    var.name <- object$var.names[var.name]
  }
  subset(do.call(rbind, lapply(1:trees, function(tree) gbm.tree.as.df(object, i.tree=tree))), SplitVarName == var.name)$SplitCodePred
}

gbm.factor.importance <- function(object, k=min(10,length(object$var.names)),
                                  n.trees=gbm.opt.n.trees(object), ...){
  x <- as.data.frame(as.list(summary(object, n.trees=n.trees, plotit=FALSE)[k:1,]))
  names(x) <- c('factor', 'importance')
  x$factor <- ordered(x$factor, x$factor)
  ggplot(x, aes(factor,importance)) +
    geom_bar() +
      coord_flip() +
        ggtitle(paste('Top',k,'of',length(object$var.names),'Factors'))
}

gbm.plot <- function (x, i.var = 1, n.trees = x$n.trees, continuous.resolution = list('splits',NA),
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
  grid.levels <- vector("list", length(i.var))
  for (i in 1:length(i.var)) {
    if (is.numeric(x$var.levels[[i.var[i]]])) {
      if(continuous.resolution[[1]] == 'quantile'){
        grid.levels[[i]] <- as.vector(quantile(x$var.levels[[i.var[i]]], probs=continuous.resolution[[2]]))
      }else if (continuous.resolution[[1]] == 'uniform'){
        grid.levels[[i]] <- seq(min(x$var.levels[[i.var[i]]]),
                                max(x$var.levels[[i.var[i]]]), length = continuous.resolution[[2]])
      }else if(continuous.resolution[[1]] == "splits"){
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
      ggplot(X,aes(x=X1,y=y)) + geom_step(direction="h") + scale_x_continuous(name=x$var.names[i.var]) + scale_y_continuous(name=sprintf('f(%s)',(x$var.names[i.var])))
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




feature.contributions <- function(mdl, src, snk, select=which.max, log.level=c('info','warning','error')){
  logger <- SimpleLog('factor.contributions',log.level)
  ## feature.contributions(ms$gbmmodel,iris[1,],iris[100,],which.max)
  features <- mdl$model$var.names
  src <- subset(src,select=features) -> osrc
  snk <- subset(snk,select=features) -> osnk

  md <- list(model=mdl)
  names(md) <- mdl$id
  srcScore <- mdls.predict(md,src,log.level=log.level)[[1]][[1]]
  snkScore <- mdls.predict(md,snk,log.level=log.level)[[1]][[1]]
  selected.features <- NA
  scores <- srcScore

  while(length(features) > 0){
    s <- sapply(features,
                function(ft){
                  src[[ft]] <- snk[[ft]]
                  mdls.predict(md,src,log.level=log.level)[[1]][[1]]
                })
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
  names(z) <- c('var','delta','src.feature','snk.feature','score.before','score.after')
  row.names(z) <- NULL
  z
}













#####################################
#### (g)lm modifications and helpers
#####################################

lm.model.def <- function(id, target.gen, features, ..., weights=function(data) NULL){
  list(id=id, target.gen=target.gen, fit=lm.fit.plus, features=features, predict=predict.lm, params=list(...), check=check.lm.model.def, weights=weights)
}



lm.fit.plus <- function(x, y, ..., y.label="y"){
  features <- names(x)
  x[[y.label]] <- y
  f <- sprintf("%s ~ %s", y.label, do.call(paste,c(as.list(features),sep=" + ")))
  m <- lm(formula(f), x, ...)
  m$var.names <- features
  m
}

check.lm.model.def <- function(modelDef, target, data, weights){
  problems <- list()

  invalid.weights <- any(is.na(weights) | is.nan(weights) | is.infinite(weights))
  if(invalid.weights){problems$invalid.weights <- NA}
  else{
    negative.weights <- any(weights < 0)
    if(negative.weights){problems$negative.weights <- NA}
  }

  problems
}

glm.model.def <- function(id, target.gen, features, ...){
  list(id=id, target.gen=target.gen, fit=glm.fit.plus, features=features, predict=glm.predict, params=list(...), check=check.glm.model.def)
}


glm.fit.plus <- function(x, y, family=gaussian,..., y.label="y"){
  features <- names(x)
  x[[y.label]] <- y
  f <- sprintf("%s ~ %s", y.label, do.call(paste,c(as.list(features),sep=" + ")))
  glm(formula(f), family=family, x, ...)
}

check.glm.model.def <- function(modelDef, target, data){list()}

glm.predict <- function(object,newdata,type='response',...){
  predict.glm(object,newdata,type=type,...)
}


#######################################
#### Feature Helpers ##################
#######################################

interinfo.feature.selection.filter <- function(t,s,r,.parallel=FALSE,log.level=c('info','warning','error')){
  logger <- SimpleLog('interinfo.feature.selection.filter',log.level)
  logger$level <- log.level
  remaining <- names(r)
  scores <- laply(remaining,
                  function(f){
                    z <- interinformation(cbind(s, t, r[[f]]))
                    z
                  },
                  .parallel=.parallel)
  names(scores) <- remaining
  scores
}

cor.feature.selection.filter <- function(t,s,r,.parallel=FALSE,log.level=c('info','warning','error')){
  logger <- SimpleLog('cor.feature.selection.filter',log.level)
  remaining <- names(r)
  scores <- laply(remaining,
                  function(f){
                    z <- abs(cor(t, r[[f]])) - (if(ncol(s)==0){0}else{max(abs(cor(s, r[[f]])))})^2
                    z
                  },
                  .parallel=.parallel)
  names(scores) <- remaining
  scores
}

forward.filter.feature.selection <- function(target, features, evaluate=interinfo.feature.selection.filter, choose.best=max, n=ncol(features), .parallel=FALSE, log.level=c('info','warning','error')){
  logger <- SimpleLog('forward.filter.feature.selection',log.level)
  feature.selection.by.filter(target, features, NULL, function(...) evaluate(...,.parallel=.parallel),
                              function(z, scores){
                                are.na <- names(scores)[is.na(scores)]
                                write.msg(logger,'score for factors "%s" is na - dropping',csplat(paste,are.na,sep=','),level='warn')
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

feature.selection.by.filter <- function(target, features, initSelected, evaluate, update, n=ncol(features),log.level=c('info','warning','error')){
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

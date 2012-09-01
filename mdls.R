#### Elliot Chow

source('utils.R', chdir=TRUE)
source('math.R', chdir=TRUE)
dump <- sapply(c('gdata',
                 'stringr',
                 'plyr',
                 'doMC',
                 'gbm',
                 'infotheo',
                 'R.oo',
                 'rjson',
                 'ggplot2',
                 get.parallel.library()$lib
                 ), better.library)

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
             "check" # function that takes in a model definition, target, and data and checks if there are any issues
             ))
}

mdls.fit <- function(datasets, ..., logger=NULL, .parallel=TRUE){
  datasets <- if(is.data.frame(datasets))(list(datasets))else{datasets}
  modelDefs <- list(...) ##if(is.model.def(modelDefs)){list(modelDefs)}else{modelDefs}

  if(is.null(logger)){ logger <- SimpleLog()}
  timer <- Timer(logger)
  flatten(lapply(lzip(if(!is.null(names(datasets))){names(datasets)}else{1:length(datasets)},
                      datasets),
                 function(x){
                   dsId <- x[[1]]
                   data <- x[[2]]

                   if(typeof(data) == 'character'){
                     t0 <- start.timer(timer,'loading dataset "%s"', dsId)
                     data <- get(load(data))
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
                                             problems <- md$check(md,t,data)

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
                                                                            t),
                                                                       md$params)),
                                                             error=function(e){
                                                               write.msg(logger,str_trim(as.character(e)),
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
                                             write.msg(logger,sprintf('could not train "%s" (skipped)', id,
                                                                      level='warning'))

                                             list()
                                           },
                                           .parallel=.parallel))
                   stop.timer(timer)
                   models
                 }))
}

mdls.predict <- function(models, datasets, logger=NULL){
  models <- if(!is.list(models)){list(models)}else{models}
  datasets <- if(is.data.frame(datasets)){list(datasets)}else{datasets}
  timer <- Timer(logger)
  flatten(lapply(lzip(if(!is.null(names(datasets))){names(datasets)}else{sapply(1:length(datasets),int.to.char.seq)},
                      datasets),
                 function(x){
                   dsId <- x[[1]]
                   data <- x[[2]]
                   if(typeof(data) == 'character'){
                     start.timer(timer,sprintf('loading dataset "%s"', dsId))
                     data <- get(load(data))
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
                                        if(is.null(x[[2]]$predictions)){
                                          x[[2]]$predictions <- list()
                                        }

                                        write.msg(logger,sprintf('predicting with "%s"', id))
                                        pr <- predict(m, subset(data,select=features))

                                        x[[2]]$predictions[[as.character(dsId)]] <- pr
                                        z <- list(x[[2]])
                                        names(z) <- id
                                        z
                                      }))
                   stop.timer(timer)
                   z
                 })
          )
}

#####################################
#### gbm modifications and helpers
#####################################


gbm.model.def <- function(id, target.gen, features, params=list()){
  list(id=id, target.gen=target.gen, fit=gbm.fit, features=features, predict=gbm.predict, params=params, check=check.gbm.model.def)
}


gbm.predict <- function(object,newdata,n.trees=NULL,type='response',...){
  trees = if(is.null(n.trees)) gbm.perf(object,method='test',plot.it=FALSE) else n.trees
  if(length(trees) != 1){
    stop('could not determine optimal number of trees')
  }
  predict.gbm(object,newdata,n.trees=trees,type=type,...)
}

check.gbm.model.def <- function(modelDef, target, data){
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
  if(na.target){problems$invalid.target <- NA}

  no.distribution <- !('distribution' %in% names(modelDef$params))
  if(no.distribution){problems$no.distribution <- NA}
  else{
    invalid.bernoulli.target <- (modelDef$params$distribution == 'bernoulli') && (!all(target %in% (0:1)))
    if(invalid.bernoulli.target){problems$invalid.bernoulli.target <- NA}
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
  print(x)
  ggplot(x, aes(factor,importance)) +
    geom_bar() +
      coord_flip() +
        opts(title=paste('Top',k,'of',length(object$var.names),'Factors'))
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
      qtls <- quantile(x$var.levels[[i.var[i]]], probs=seq(0,1,0.1))
      qtls <- qtls[qtls <= max(X$X1) & qtls >= min(X$X1)]
      n <- nrow(X)
      ggplot(cbind(rbind(cbind(X,lbl=NA), data.frame(X1=qtls,y=max(X$y),lbl=names(qtls))),n=n), aes(x=X1[1:n[1]],y=y[1:n[1]])) + geom_step(color='steelblue',direction='hv') + geom_point(color='red',alpha=0.65) +
        geom_vline(aes(xintercept=X1[-(1:n[1])]),alpha=0.3) +
          geom_text(aes(x=X1[-(1:n[1])],y=max(y[-(1:n[1])]),label=lbl[-(1:n[1])]),angle=90,size=3,vjust=-1,alpha=0.5) +
            scale_x_continuous(x$var.names[i.var]) +
              scale_y_continuous(paste("f(", x$var.names[i.var], ")", sep = ""))
    }
    else {
      ggplot(X, aes(x=X1,y=y)) + geom_boxplot(color='steelblue') + scale_x_discrete(x$var.names[i.var]) + scale_y_continuous(paste("f(", x$var.names[i.var], ")", sep = ""))
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
#### (g)lm modifications and helpers
#####################################

lm.model.def <- function(id, target.gen, features, params=list()){
  list(id=id, target.gen=target.gen, fit=lm.fit.plus, features=features, predict=predict.lm, params=params, check=check.lm.model.def)
}



lm.fit.plus <- function(x, y, ..., y.label="y"){
  features <- names(x)
  x[[y.label]] <- y
  f <- sprintf("%s ~ %s", y.label, do.call(paste,c(as.list(features),sep=" + ")))
  lm(formula(f), x, ...)
}

check.lm.model.def <- function(modelDef, target, data){list()}



glm.model.def <- function(id, target.gen, features, params=list()){
  list(id=id, target.gen=target.gen, fit=glm.fit.plus, features=features, predict=glm.predict, params=params, check=check.glm.model.def)
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


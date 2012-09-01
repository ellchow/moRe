#### Elliot Chow

source('utils.R', chdir=TRUE)
source('math.R', chdir=TRUE)
source('mdls.R', chdir=TRUE)


pairwise.target <- function(model, x, y, predict, tau=0.1, flipped.tol=1){
  scores <- predict(model, x)
  names(scores) <- names(y) ## names of target identify the pairs (expects 1 better, >=1 worse)

  good <- y == 1

  scores.good<- scores[good]
  scores.bad <- scores[!good]

  gap <- scores.good[names(scores.bad)] - scores.bad

  flipped <- names(gap)[gap < (flipped.tol * tau)]

  ## cat(length(flipped), "   ", length(names(gap)[gap < 0]), '\n')


  scores0 <- scores
  scores[y == 1 & (names(scores) %in% flipped)] <- scores[y == 1 & (names(scores) %in% flipped)] + tau
  scores[y == 0 & (names(scores) %in% flipped)] <- scores[y == 0 & (names(scores) %in% flipped)] - tau

  ## print(cbind(y,scores0,scores))

  scores
}

## lm.add.predict <- function(models, x, ..., shrinkage=0.1){
##   Reduce(function(a,b) a + b,
##          lapply(1:length(models),
##                 function(i){
##                   ifelse(i > 1, shrinkage, 1) * predict.lm(models[[i]], x)
##                 }),
##          init = 0)
## }

## source('mdls.R');offset <- c(1,2,3,4); do.call(rbind,lapply(offset,
##                                                                 function(o){
## rbind(data.frame(y=1,a=1.2*o+rnorm(1),pair.id=paste("id",o,sep="_")),data.frame(y=0,a=o+rnorm(1),pair.id=paste("id",o,sep="_")))
## })) -> x

## train <- subset(x,select=a)
## target <- x$y
## names(target) <- x$pair.id
## m <- list(lm.fit.plus(train,target))
## target <- pairwise.target(m, train, target, lm.add.predict, tau = 0.01)
## m <- c(m, list(lm.fit.plus(train,target)))
## target <- pairwise.target(m, train, target, lm.add.predict, tau = 0.01)

## train <- subset(x,select=a)
## target <- x$y
## names(target) <- x$pair.id
## m <- gbm.fit(train,target,n.trees=1)
## target <- pairwise.target(m, train, target, function(...) gbm.predict(...,n.trees=1), tau = 0.01)
## m <- gbm.more(m, 1, weights=target)
## target <- pairwise.target(m, train, target, function(...) gbm.predict(...,n.trees=1), tau = 0.01)
## m <- gbm.more(m, 1, weights=target)
## target <- pairwise.target(m, train, target, function(...) gbm.predict(...,n.trees=1), tau = 0.01)


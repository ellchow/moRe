#### Elliot Chow

source('utils.R', chdir=TRUE)
source('math.R', chdir=TRUE)
source('mdls.R', chdir=TRUE)

############### EXPERIMENTAL

pairwise.target <- function(model, x, y, predict, tau=0.1, flipped.tol=1){
  scores <- predict(model, x)
  names(scores) <- names(y) ## names of target identify the pairs (expects 1 better, >=1 worse)

  good <- y == 1
  scores.good<- scores[good]
  scores.bad <- scores[!good]

  gap <- scores.good[names(scores.bad)] - scores.bad

  is.flipped <- names(scores) %in% (names(gap)[gap < (flipped.tol * tau)])
  is.too.small <- vector('logical',length(scores))
  is.too.small[!good] <- gap < tau

  scores0 <- scores
  scores[y == 1 & is.flipped] <- scores[y == 1 & is.flipped] + tau
  scores[y == 0 & is.too.small] <- scores[y == 0 & is.too.small] - tau
  keep <- (y == 1 & is.flipped) | (y == 0 & is.too.small)

  ## print(cbind(y,scores0,y == 1 & is.flipped,y == 0 & is.too.small,scores))

  list(target=scores[keep], x=subset(x,keep), y=y[keep])
}

lm.add.predict <- function(models, x, ..., shrinkage=0.01){
  z <- Reduce(function(a,b) a + b,
         lapply(1:length(models),
                function(i){
                  ifelse(i > 1, shrinkage, 1) * predict.lm(models[[i]], x)
                }),
         init = 0)
  z
}

## source('mdls.R');offset <- c(1,2,3,4); do.call(rbind,lapply(offset,
##                                                                 function(o){
## rbind(data.frame(y=1,a=1.2*o+rnorm(1),pair.id=paste("id",o,sep="_")),data.frame(y=0,a=o+rnorm(1),pair.id=paste("id",o,sep="_")))
## })) -> x

## train <- subset(x,select=a)
## pairs <- x$y
## names(pairs) <- x$pair.id
## pt <- list(target=pairs, x=train, y=pairs)
## trace <- NULL
## n.iter <- 100
## for(i in 1:n.iter){
## m <- c(m,list(lm.fit.plus(pt$x,pt$target)))
## pt <- pairwise.target(m, pt$x, pt$y, lm.add.predict, tau = 0.1)
## trace <- c(trace,length(pt$y))

## if(i == 1 || i == n.iter){
##  tt <- train
##  tt$pairs <- names(pairs)
##  tt$y <- x$y
##  tt$s <- lm.add.predict(m, tt)
##  tt$r <- ave(tt$s,tt$pairs,FUN=rank)
## ## print(tt[order(tt$pairs,tt$s),])
##  print(subset(tt, y == 1))
## }
## }


## n.iter <- 200
## train <- subset(x,select=a)
## pairs <- x$y
## names(pairs) <- x$pair.id
## pt <- list(target=pairs, x=train, y=pairs)
## trace <- NULL

## m <- gbm.fit(pt$x,pt$target,n.trees=1)
## pt <- pairwise.target(m, pt$x, pt$y, function(...) gbm.predict(...,n.trees=1), tau = 0.1)

## for(i in 2:n.iter){
##   m <- gbm.more(m, pt$x, pt$target,  n.new.trees=1)
##   pt <- pairwise.target(m, pt$x, pt$y, function(...) gbm.predict(...,n.trees=1), tau = 0.01)
##   if(i == 2 || i == n.iter){
##  tt <- train
##  tt$s <- gbm.predict(m,train,n.trees=i)
##  tt$pairs <- names(pairs)
##  tt$y <- x$y
##  tt$r <- ave(tt$s,tt$pairs,FUN=rank)
## ## print(tt[order(tt$pairs,tt$s),])
##  print(subset(tt, y == 1))
## }

## }



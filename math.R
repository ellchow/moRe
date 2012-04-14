source('utils.R', chdir=T)

stat.sum.df <- function(fun, geom="crossbar", colour='steelblue',...) {
  stat_summary(fun.data=fun, colour=colour, geom=geom, width=0.4, ...)
}

linear.norm <- function(x, lb, ub, clipMin=FALSE, clipMax=FALSE, rm.na=TRUE, displayLevel=0){
  if(clipMin){
    x <- max(x,lb)
  }
  if(clipMax){
    x <- min(x,ub)
  }
  y <- (x - lb) / (ub - lb)

  y
}

mean.cl.boot.w <- function(x,w=rep(1,length(x)),rounds=1000,ci=0.95,na.rm=T){
  if(na.rm){
    ok <- !is.na(x) & !is.nan(x) & !is.infinite(x)
    w <- w[ok]
    x <- x[ok]
  }
  w <- w / sum(w)
  z <- c(x %*% w,
         quantile(sapply(1:rounds,
                         function(i,x,m){
                           sum(sample(x,m,replace=T,prob=w))/m
                         },
                           x=x,m=length(x)),
                         c(1-ci,1+ci)/2))
  names(z) <- c('y','ymin','ymax')
  as.data.frame(as.list(z))
}

val.to.quantile <- function(x,q=0.1){
  qtls <- seq(0,1,q)
  vals <- quantile(x, qtls)
  sapply(x,
         function(y) {
           z <- y <= vals
           qtls[z][1]
         })
}

rdiscrete <- function(n, prob, domain=1:length(prob)){
  apply(rmultinom(n,1,prob/sum(prob)), 2, function(x) domain[as.logical(x)])
}

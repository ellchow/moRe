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

val.to.quantile <- function(x,...){
  qtls <- seq(0,1,...)
  vals <- quantile(x, qtls)
  f <- approxfun(vals, qtls)
  f(x)
}

rdiscrete <- function(n, prob, domain=1:length(prob)){
  apply(rmultinom(n,1,prob/sum(prob)), 2, function(x) domain[as.logical(x)])
}

beta.params <- function(a,b){
  list(a=a, b=b, mean=(a / (a + b)), var=(a*b / ((a+b)^2 * (a+b+1))))
}

beta.estimate <- function(x,m=mean,v=var){
  if(is.null(dim(x)) || ncol(x) != 2){
    sampleMean <- m(x)
    sampleVar <- v(x)
  }else{
    sampleMean <- m(x[,1] / x[,2])
    sampleVar <- v(x[,1] / x[,2])
  }
  a <- sampleMean * ((sampleMean * (1 - sampleMean)) / sampleVar - 1)
  b <- (1 - sampleMean) * ((sampleMean * (1 - sampleMean)) / sampleVar - 1)
  beta.params(a,b)
}

beta.update <- function(params,s,n){
  beta.params(s+params$a, n-s+params$b)
}

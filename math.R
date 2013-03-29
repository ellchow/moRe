source('import.R',chdir=T)
import('utils','psych')

linear.norm <- function(x, lb, ub, clipMin=FALSE, clipMax=FALSE, na.rm=FALSE, displayLevel=0){
  if(clipMin)
    x <- pmax(x,lb,na.rm=na.rm)

  if(clipMax)
    x <- pmin(x,ub,na.rm=na.rm)

  (x - lb) / (ub - lb)
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

trim <- function(x, lb, ub)
  pmin(ub,pmax(lb,x))

bucketize <- function(x,buckets=quantile(x,seq(0,1,0.1)),label=!is.null(names(buckets))){
  ub <- max(buckets)
  buckets[which.max(buckets)] <- ub + 1
  ub <- ub + 1
  lb <- min(buckets)
  f <- approxfun(cbind(sort(buckets),1:length(buckets)))
  x <- trim(x, lb, ub)

  b <- floor(f(x))
  if(label)
    factor(names(buckets)[b],levels=names(buckets), ordered=T)
  else
    b
}

rdiscrete <- function(n, prob, domain=1:length(prob))
  bucketize(runif(n), c(0,cumsum(prob / sum(prob))))

beta.params <- function(a,b)
  c(a=a, b=b, mean=(a / (a + b)), var=(a*b / ((a+b)^2 * (a+b+1))))

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

beta.update <- function(params, s, n){
  beta.params(s + params['a'], n + params['b'])
}

ffilter <- function(x,w,indexes=1:length(x),sides=2){
  stop.if(length(w) %% 2 == 0 && sides == 2,'filter must have odd length if two-sided')
  offset <-  if(sides == 2){-as.integer(length(w)/2)}else{0}
  bw <- length(w)
  n <- length(x)
  sapply(indexes,
         function(i){
           is <- (offset + i):(offset + i + bw - 1)
           keep <- is > 0 & is <= n
           is <- is[keep]
           w <-  w[keep]
           w %*% x[is]
         })
}

cumdist.df <- function(x,buckets=seq(min(x),max(x),length=1000),values=rep(1,length(x))){
  lookup<-approxfun(1:length(buckets),sort(buckets))
  bucketize(x,buckets) -> b
  dist <- tapply(values,b,sum)/sum(values)
  data.frame(x=sort(lookup(as.integer(names(dist)))),'F(x)'=cumsum(dist[order(lookup(as.integer(names(dist))))]))
}

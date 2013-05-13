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

import('utils')

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

cap <- function(x, lb, ub)
  pmin(ub,pmax(lb,x))

winsor.mean <- function(x, trim=0.02){
  stop.if.not(trim >= 0 && trim <= 1, "trim must be between 0 and 1")

  mean(cap(x,quantile(x,trim), quantile(x,1 - trim)))
}

winsor.var <- function(x, trim=0.02){
  stop.if.not(trim >= 0 && trim <= 1, "trim must be between 0 and 1")

  var(cap(x,quantile(x,trim), quantile(x,1 - trim)))
}

skewness <- function(x, ...)
  mean((x - mean(x, ...)) ^ 3, ...) / (mean((x - mean(x, ...)) ^ 2) ^ 1.5)


kurtosis <- function(x, ...)
  -3 + mean((x - mean(x, ...)) ^ 4, ...) / (mean((x - mean(x, ...)) ^ 2, ...) ^ 2)

bucketize <- function(x, buckets=head(quantile(x,seq(0,1,0.1)),-1), label='names',
                      uniq.boundaries = TRUE){
  if(uniq.boundaries)
    stop.if(length(unique(buckets)) != length(buckets),
            "bucket boundaries must be unique")
  else{
    z <- tapply(if(!is.null(names(buckets))) names(buckets) else indices(buckets), buckets, function(x) csplat(paste,x,sep=', '))
    buckets <- named(as.numeric(names(z)), z)
  }

  f <- approxfun(sort(buckets),indices(buckets), yleft = 1, yright = length(buckets))
  b <- floor(f(x))
  if(label == 'names')
    factor(names(buckets)[b],levels=names(buckets), ordered=T)
  else if(label == 'buckets')
    factor(buckets[b],levels=buckets, ordered=T)
  else
    b
}

rdiscrete <- function(n, prob, domain=1:length(prob))
  bucketize(runif(n), c(0,cumsum(prob / sum(prob))))

beta.params <- function(a,b)
  c(a=a, b=b, mean=(a / (a + b)), var=(a*b / ((a+b)^2 * (a+b+1))))

beta.estimate <- function(x, m=mean, v=var){
  if(is.null(dim(x)) || ncol(x) != 2){
    sampleMean <- m(x)
    sampleVar <- v(x)
  }else{
    sampleMean <- m(ifelse(x[,2] == 0, 0, x[,1] / x[,2]))
    sampleVar <- v(ifelse(x[,2] == 0, x[,2], x[,1] / x[,2]))
  }

  a <- sampleMean * ((sampleMean * (1 - sampleMean)) / sampleVar - 1)

  b <- (1 - sampleMean) * ((sampleMean * (1 - sampleMean)) / sampleVar - 1)

  beta.params(a,b)
}

beta.update <- function(params, s, n)
  beta.params(s + params['a'], n + params['b'])

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


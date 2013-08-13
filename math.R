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

linear.norm <- function(x, lb, ub, clip = c(FALSE,FALSE), na.rm = FALSE){
  y <- cap(x, if(clip[1]) lb else -Inf, if(clip[2]) ub else Inf)

  (y - lb) / (ub - lb)
}

is.between <- function(x, bounds, inclusive=T){
  if(inclusive)
    (x >= bounds[1]) & (x <= bounds[2])
  else
    (x > bounds[1]) & (x < bounds[2])
}

cap <- function(x, lb, ub, na.rm = FALSE)
  pmin(ub,pmax(lb, x, na.rm=na.rm), na.rm=na.rm)

bucketize <- function(x, buckets = quantile(x,seq(0,1,0.1)), label='names',
                      uniq.boundaries = TRUE, drop.ub = TRUE){
  if(drop.ub)
    buckets <- head(buckets, -1)

  if(uniq.boundaries)
    stop.if(length(unique(buckets)) != length(buckets),
            "bucket boundaries must be unique")
  else{
    z <- tapply(if(!is.null(names(buckets))) names(buckets) else indices(buckets),
                buckets,
                function(x) paste(x, collapse=', '))
    buckets <- named(as.numeric(names(z)), z)
  }
  if(is.null(names(buckets)))
    names(buckets) <- buckets

  f <- approxfun(sort(buckets),indices(buckets), yleft = 1, yright = length(buckets))
  b <- floor(f(x))
  if(label == 'names')
    factor(names(buckets)[b],levels=names(buckets), ordered=T)
  else if(label == 'buckets')
    factor(buckets[b],levels=buckets, ordered=T)
  else
    b
}

rdiscrete <- function(n, prob, domain = indices(prob))
  bucketize(runif(n), named(c(0,cumsum(prob / sum(prob))), domain))

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

######################
#### statistics
######################

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

beta.params <- function(a,b,method='ab'){
  stop.if.not(method %in% c('ab','md'), 'unknown method: %s', method)
  if(method == 'md'){
    alpha <- a * b
    beta <-  b - alpha
    beta.params(alpha, beta, 'ab')
  }else{
    c(a=a, b=b, mean=(a / (a + b)), var=(a*b / ((a+b)^2 * (a+b+1))))
  }
}


beta.estimate <- function(x, m=mean, v=var){
  if(is.null(dim(x)) || ncol(x) != 2){
    sample.mean <- m(x)
    sample.var <- v(x)
  }else{
    sample.mean <- m(ifelse(x[,2] == 0, 0, x[,1] / x[,2]))
    sample.var <- v(ifelse(x[,2] == 0, x[,2], x[,1] / x[,2]))
  }

  a <- sample.mean * ((sample.mean * (1 - sample.mean)) / sample.var - 1)

  b <- (1 - sample.mean) * ((sample.mean * (1 - sample.mean)) / sample.var - 1)

  beta.params(a,b)
}

beta.update <- function(params, s, n)
  beta.params(s + params['a'], n + params['b'])

######################
#### optimization
######################

has.converged <- function(xs, stat = max, tol = 1e-6, n = 4, is.relative.tol = FALSE){
  xs <- tail(xs, n)

  if(length(xs) < n){
    return(FALSE)
  }else{
    if(is.relative.tol)
      stat(abs(diff(xs)) - head(xs,1)) < tol
    else
      stat(abs(diff(xs))) < tol
  }
}


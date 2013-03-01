source('import.R',chdir=T)

import('utils')


dcg <- function(value=I, discount=function(i) pmax(1, log(i,2))){
  function(r, x){
    sum(value(x[r]) / discount(1:length(x)))
  }
}

ndcg <- function(...){
  function(r, x){
    f <- dcg(...)
    f(r, x) / f(order(x, decreasing=TRUE), x)
  }
}

at.top <- function(n,f=mean){
  function(r, x){
    f(x[r <= n])
  }
}

compute.ranks <- function(s, g, ties.method='random'){
  stop.if.not(length(s) == length(g), "length of s must equal length of g")
  x <- vector('integer',length=length(s))
  split(x,g) <- tapply(s, g,function(x) rank(-x, ties.method=ties.method))
  x
}

compute.skips <- function(rnk, pos, g){
  stop.if.not(is.logical(pos), "pos must be boolean")
  stop.if.not(length(rnk) == length(pos), "length of rnk must equal length of pos")

  x <- vector(length=length(rnk))
  split(x, g) <- tapply(list(rnk,pos), g,
         function(r,p){
           rp <- if(any(p)) r[p] else -Inf
           lowest.pos <- max(rp)
           !p & (r < lowest.pos)
         })
  x
}

compute.metric <- function(rnk, values, g, metric){
  tapply(list(rnk, values), g,
         function(r, x){
           metric(r, x)
         })
}




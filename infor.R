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

source('import.R',chdir=T)

import('utils', 'mdls')


dcg <- function(value=I, discount=function(i) pmax(1, log(i,2))){
  function(r, x)
    sum(value(x[r]) / discount(1:length(x)))
}

ndcg <- function(...){
  function(r, x){
    f <- dcg(...)
    f(r, x) / f(order(x, decreasing=TRUE), x)
  }
}

at.top <- function(n,f=mean){
  stop.if.not(n > 0, "n must be > 0")

  function(r, x)
    f(x[r <= n])
}

pos.rank <- function(select=mean, f=function(x) 1 / x){
  function(r, x){
    stop.if.not(is.logical(x), "x must be logical")

    f(select(r[x]))
  }
}

rel.to <- function(ref, f=cor){
  function(r, ...)
    f(ref, r, ...)
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

compute.infor.metric <- function(rnk, values, g, metric){
  tapply(list(rnk, values), g,
         function(r, x){
           metric(r, x)
         })
}


feature.contributions.infor.metric <- function(mdl, d, iss, values, g, metric, log.level=SimpleLog.INFO, .parallel=TRUE){
  logger <- SimpleLog('feature.contributions.infor.metric',log.level)
  named(llply(iss,
        function(is){
          write.msg(logger,sprintf('randomizing features: %s',csplat(paste,is,sep=',')))
          d.r <- d
          for(i in is)
            d.r[[i]] <- sample(d.r[[i]])

          write.msg(logger,sprintf('scoring model with randomized %s',csplat(paste,is,sep=',')))
          s.r <- mdl$predict(mdl$model, d.r[,mdl$features])
          r.r <- compute.ranks(s.r, d.r$QueryID)

          compute.infor.metric(r.r, values, g, metric)
        },
        .parallel=.parallel),
        sapply(iss, function(is) csplat(paste,is,sep=',')))
}



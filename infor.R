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

pos.rank <- function(select=mean, f=I){
  function(r, x){
    stop.if.not(is.logical(x), "x must be logical")

    f(select(r[x]))
  }
}

mean.reciprocal.rank <- pos.rank(min, function(x) 1 / x)

rel.to <- function(f=cor){
  function(r, x)
    f(r, x)
}

overlap <- function(n=10)
  rel.to(function(r, r.ref) sum((r <= n) & (r.ref <= n)))

compute.ranks <- function(s, g, ties.method='random', envir=NULL){
  if(!is.null(envir)){
    s <- eval(substitute(s),envir)
    g <- eval(substitute(g),envir)
  }

  tapply(s, g, function(x) rank(-x, ties.method=ties.method), ret.type='par')
}

compute.skips <- function(rnk, pos, g, envir=NULL){
  if(!is.null(envir)){
    rnk <- eval(substitute(rnk),envir)
    pos <- eval(substitute(pos),envir)
    g <- eval(substitute(g),envir)
  }

  stop.if.not(is.logical(pos), "pos must be boolean")
  stop.if.not(length(rnk) == length(pos), "length of rnk must equal length of pos")

  tapply(list(rnk,pos), g,
         function(r,p){
           rp <- if(any(p)) r[p] else -Inf
           lowest.pos <- max(rp)
           !p & (r < lowest.pos)
         }, ret.type = 'par')
}

compute.infor.metric <- function(rnk, values, g, metric, envir=NULL){
  if(!is.null(envir)){
    rnk <- eval(substitute(rnk),envir)
    values <- eval(substitute(values),envir)
    g <- eval(substitute(g),envir)
  }

  tapply(list(rnk, values), g,
         function(r, x){
           metric(r, x)
         })
}


feature.contributions.infor.metric <- function(mdl, d, values, g, metric, featureSets = mdl$features,
                                               agg = smean.cl.boot,
                                               log.level=SimpleLog.INFO, .parallel=TRUE){
  logger <- SimpleLog('feature.contributions.infor.metric', log.level)

  rnk <- eval(substitute(rnk),envir)
  values <- eval(substitute(values),d)
  g <- eval(substitute(g),d)

  named(llply(featureSets,
        function(featureSet){
          write.msg(logger,sprintf('randomizing features: %s', paste(featureSet, collapse=',')))

          d.r <- d
          for(i in featureSet)
            d.r[[i]] <- sample(d.r[[i]])

          write.msg(logger,sprintf('scoring model with randomized %s', paste(featureSet, collapse=',')))
          s.r <- mdl$predict(mdl$model, d.r[,mdl$features])
          r.r <- compute.ranks(s.r, d.r[[g]])

          agg(compute.infor.metric(r.r, values, g, metric))
        },
        .parallel=.parallel),
        sapply(featureSets, function(featureSet) paste(featureSet, collapse=',')))
}



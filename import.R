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

moRe.ROOTDIR <- getwd()

better.library <- function(...,repos='http://cran.cnr.Berkeley.edu/',lib=.Library,attempt=1){
  dots <- list(...)
  loaded <- vector('logical',length(dots))
  for(i in 1:length(dots)){
    pkg <- dots[[i]]
    ## set number of attempts for installation
    attempt <- max(0,attempt)
    ## try to load package, return false if failed or error
    check <- tryCatch(suppressMessages(suppressWarnings(do.call(library,list(pkg,logical.return=T,quietly=T,verbose=F)))),
                      error=function(e){FALSE})
    if(!check){
      if(attempt > 0){
        ## try to install and then import again
        cat(sprintf('Trying to install "%s"\n', pkg))
        check <- tryCatch(is.null(suppressWarnings(install.packages(pkg,repos=repos,dependencies=T))),
                          error=function(e){FALSE}) && better.library(pkg,repos=repos,lib=lib,attempt=attempt-1)
      }else{
        ## stop if failed to install properly
        stop(sprintf('Could not load or install package "%s"', pkg))
      }
    }
    loaded[i] <- check
  }
  invisible(loaded)
}

system.os <- function(){.Platform$OS.type}

import <- function(..., as.library=NULL){
  available <- c('utils', 'mdls', 'yahoofin', 'finance', 'cmdargs', 'math', 'plots', 'sql', 'infor', 'postgres', 'quickcheck')
  from.source <- c('rstan')

  selected <- c(...)

  not.in.moRe <- setdiff(selected, available)
  found.in.moRe <- setdiff(selected, not.in.moRe)

  ## RStan http://code.google.com/p/stan/wiki/RStanGettingStarted#RStan_Getting_Started
  options(repos = c(getOption("repos"), rstan = "http://wiki.stan.googlecode.com/git/R"))


  invisible(sapply(found.in.moRe,
         function(s) source(paste(moRe.ROOTDIR, '/', s, '.R', sep=''), chdir=T) ))

  invisible(sapply(union(setdiff(not.in.moRe, from.source),as.library), better.library))

  invisible(sapply(intersect(from.source, selected), function(s) install.packages(s, type='source')))

}

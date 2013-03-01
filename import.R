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

get.parallel.library <- function(){
  if(system.os() == 'windows'){
    list(lib='doSMP',activate=function(cores){eval(parse(text=sprintf('registerDoSMP(%d)',cores)))})
  }else{
    list(lib='doMC',activate=function(cores="NULL"){eval(parse(text=sprintf('registerDoMC(%s)',cores)))})
  }
}

import <- function(..., from.moRe=TRUE){
  available <- c('utils', 'mdls', 'yahoofin', 'cmdargs', 'math', 'plots', 'sql', 'infor')
  selected <- c(...)
  not.in.moRe <- setdiff(selected, available)
  found.in.moRe <- setdiff(selected, not.in.moRe)

  invisible(sapply(found.in.moRe,
         function(s) source(paste(moRe.ROOTDIR, '/', s, '.R', sep=''), chdir=T) ))
  invisible(sapply(not.in.moRe, better.library))
}


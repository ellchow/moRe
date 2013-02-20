source('utils.R')

moRe.ROOTDIR <- getwd()

import <- function(...){
  available <- c('utils', 'mdls', 'yahoofin', 'cmdargs', 'math', 'plots', 'sql')
  selected <- c(...)
  unknown <- setdiff(selected, available)

  stop.if(length(unknown) > 0,
          sprintf("unknown libraries in moRe: %s",
                  csplat(paste, unknown)))

  invisible(sapply(selected,
         function(s){
           source(paste(moRe.ROOTDIR, '/', s, '.R', sep=''), chdir=T)
         }))
}

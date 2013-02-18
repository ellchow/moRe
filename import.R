source('utils.R')

import <- function(...){
  available <- c('utils', 'mdls', 'yahoofin', 'cmdargs', 'math', 'plots', 'sql')
  selected <- c(...)

  unknown <- setdiff(selected, available)

  stop.if(length(unknown) > 0,
          sprintf("unknown libraries in moRe: %s",
                  csplat(paste, unknown)))

  csplat(source, paste(selected, '.R', sep=''))
}

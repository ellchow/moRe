#!/usr/bin/env Rscript
source('../../import.R', chdir=TRUE)
import('utils','cmdargs','shiny')


main <- function(raw.args){
  parsed <-parse.args('start.R',
                    list(
                         list(name='data',
                              desc='path to dataset'),
                         list(name='browser',
                              desc='open default browser',
                              flag=TRUE)
                         ),
                    raw.args)
  args <- parsed$args
  if(is.null(args))
    return(args$help)

  shiny::runApp(launch.browser=args$browser)
  return(TRUE)
}


main(commandArgs(TRUE))

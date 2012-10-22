#!/usr/bin/env Rscript

source('utils.R', chdir=TRUE)
source('cmdargs.R', chdir=TRUE)
better.library('Rook')

main <- function(argsString){
  parsedArgs <- parse.args('start_rook.R',
                           list(list(name='app',desc='path to app file',required=TRUE),
                                list(name='port',desc='port',default=9080,parser=as.integer)
                                ),
                           argsString)

  if(is.null(parsedArgs))
    return(NULL)
  else
    args <- parsedArgs$args

  ROOK_SERVER <- Rhttpd$new()
  ROOK_SERVER$add(name=args$app,app=args$app)
  ROOK_SERVER$start(quiet=FALSE,port=args$port)
  ROOK_SERVER$print()

  while (TRUE) Sys.sleep(24 * 60 * 60)
}

invisible(main(commandArgs(T)))

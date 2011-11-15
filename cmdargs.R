source('utils.R', chdir=TRUE)

better.library('stringr')

parse_args <- function(filename, arglist, args){
  args <- do.call(paste,as.list(c(args,'')))
  success <- TRUE
  ## add help option
  arglist <- c(arglist,list(list(name='help',desc='show this help message',flag=TRUE)))
  ## get names
  argnames <- lapply(arglist,
                     function(x){
                       x$name
                     })
  ## process required
  required <- do.call(c,lapply(arglist,
                               function(x){
                                 if(('required' %in% names(x))){
                                   if(x$required){
                                     x$name
                                   }
                                 }else{NULL}
                               }))
  ## process flags
  flag <- do.call(c,lapply(arglist,
                           function(x){
                             if(('flag' %in% names(x))){
                               if(x$flag){
                                 z<-list(FALSE)
                                 names(z)<- x$name
                                 z
                               }
                             }else{NULL}
                           }))
  ## process defaults
  defaults <- c(do.call(c,lapply(arglist,
                                 function(x){
                                   if(('default' %in% names(x))){
                                     z <- list(x$default)
                                     names(z) <- x$name
                                     z
                                   }else{NULL}
                                 })),flag)
  ## process arg parsers
  parsers <- do.call(c,lapply(arglist,
                              function(x){
                                if(('parser' %in% names(x))){
                                  y <- list(x$parser)
                                  names(y) <- x$name
                                  y
                                }else{NULL}
                              }))
  options(warn=-1)
  ## parse args
  parsed <- do.call(c,lapply(str_split(args,'(\\s+|^\\s*)\\-')[[1]],
                             function(x){
                               if(str_length(gsub('\\s','',x)) == 0){
                                 NULL
                               }else{
                                 y<-str_split(x,' ')[[1]]
                                 z <- list(str_trim(do.call(paste,as.list(y[2:length(y)]))))
                                 if(!(y[1] %in% argnames)){
                                   ## cat(sprintf('ERROR: unknown argument "%s"\n', y[1]))
                                   return(NA)
                                 }
                                 if(y[1] %in% names(flag)){
                                   z <- list(TRUE)
                                 }else if(y[1] %in% names(parsers)){
                                   z <- list(parsers[[y[1]]](y[2]))
                                 }
                                 names(z)<-y[1]
                                 z
                               }
                             }))
  allNames <- unique(c(names(defaults),names(parsed)))
  parsed <- lapply(allNames,
                   function(i){
                     if(i %in% names(parsed)){
                       z <- parsed[[i]]
                     }else{
                       z <- defaults[[i]]
                     }
                     z
                   })
  names(parsed) <- allNames
  success <- !any(is.na(parsed))
  options(warn=0)
  ## check
  if(!parsed$help && success){
    ## check required
    missing <- setdiff(required, names(parsed))
    if(length(missing) > 0){
      ## cat(sprintf('ERROR: missing arguments %s\n', do.call(paste,as.list(c(missing,sep=', ')))))
      success <- FALSE
    }

    ## check types
    invalidTypes <- is.na(parsed)
    success <- !any(invalidTypes)
  }
  z <- list(success=success,args=parsed,usage=make_usage_string(filename, arglist))
  if(parsed$help || (!success)){
    cat(z$usage)
  }
  z
}

make_usage_string <- function(filename, arglist){
  sprintf('USAGE: %s %s\n  ARGS  %s\n', filename,
          do.call(paste, c(lapply(arglist,
                                  function(x){
                                    v <- x$name
                                    if(('required' %in% names(x)) && x$required){
                                      sprintf('-%s',v)
                                    }else{
                                      sprintf('[-%s]',v)
                                    }}))),
          do.call(paste, c(lapply(arglist,
                                  function(x){
                                    paste(x$name,'\t',x$desc,sep='')
                                  }),sep='\n        '))
          )
}



parse_list <- function(v,sep=',',def='=', requireNames=FALSE){
  z <- do.call(c,lapply(str_split(v,sep)[[1]],
                   function(x){
                     y <- str_split(x,def)[[1]]
                     if(length(y) == 1){
                       list(y[1])
                     }else{
                       z <- list(y[2])
                       names(z) <- y[1]
                       z
                     }
                   }
                 ))
  if(!requireNames || !is.null(names(z))){
    z
  }else{
    NULL
  }
}

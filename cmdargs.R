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

import('utils', 'stringr')

parse.args <- function(filename, arglist, args, prologue = '', epilogue = '', skip.undefined = FALSE, exit.on.help=TRUE){
  ## parse_args('foo.R',list(list(name='a',desc='arg a'),list(name='b',desc='arg b', required=T),list(name='c',desc='arg c',required=F,flag=T),list(name='d',desc='arg d',parser=as.integer),list(name='e',desc='arg e',parser=function(x){str_split(x,'\\s*,\\s*')[[1]]})), '-a qux -b zonk -c -d 1.2 -e e,d,x -help')

  args <- paste(args, collapse = ' ')
  failures <- list()
  ## add help option
  arglist <- c(arglist,list(list(name='-help',desc='show this help message',flag=TRUE)))
  ## get names
  argnames <- lapply(arglist,
                     function(x){
                       x$name
                     })
  ## process required
  required <- flatten(lapply(arglist,
                             function(x){
                               if(('required' %in% names(x))){
                                 if(x$required){
                                   x$name
                                 }
                               }else{NULL}
                             }))
  ## process flags
  flag <- flatten(lapply(arglist,
                         function(x){
                           if(('flag' %in% names(x))){
                             if(x$flag){
                               z<-list(!is.null(x$negate) && x$negate)
                               names(z)<- x$name
                               z
                             }
                           }else{NULL}
                         }))

  ## process defaults
  defaults <- c(flatten(lapply(arglist,
                               function(x){
                                 if(('default' %in% names(x))){
                                   z <- list(x$default)
                                   names(z) <- x$name
                                   z
                                 }else{NULL}
                               })),
                flag)
  ## process arg parsers
  parsers <- flatten(lapply(arglist,
                            function(x){
                              if(('parser' %in% names(x))){
                                y <- list(x$parser)
                                names(y) <- x$name
                                y
                              }else{NULL}
                            }))
  options(warn=-1)
  ## parse args
  undefined.args <- NULL
  parsed <- flatten(lapply(str_split(args,'(\\s+|^\\s*)\\-')[[1]],
                           function(x){
                             if(str_length(gsub('\\s','',x)) == 0){
                               NULL
                             }else{
                               y <- str_split(x, ' ')[[1]]

                               argname <- y[1]
                               argval <- paste(y[2:length(y)], collapse = ' ')

                               z <- list(str_trim(argval))

                               if(!(argname %in% argnames)){
                                 undefined.args <<- c(undefined.args, argname)
                                 NULL
                               }else{
                                 if(argname %in% names(flag))
                                   z <- list(!flag[[argname]])
                                 else if(argname %in% names(parsers))
                                   z <- list(parsers[[argname]](argval))

                                 z %named% argname
                               }
                             }
                           }))

  if(!skip.undefined && length(undefined.args) > 0)
    failures <- c(failures, sprintf('undefined args %s', paste(undefined.args, collapse=',')))

  allNames <- unique(c(names(defaults),names(parsed)))
  parsed <- lapply(allNames,
                   function(i){
                     if(i %in% names(parsed))
                       z <- parsed[[i]]
                     else
                       z <- defaults[[i]]

                     z
                   })
  names(parsed) <- allNames
  options(warn=0)
  ## check
  if(!parsed[['-help']]){
    ## check required
    missing <- setdiff(required, names(parsed))
    if(length(missing) > 0){
      failures <- c(failures, sprintf("missing args %s",
                                      paste('-',missing, sep='', collapse = ', ')))
    }

    ## check types
    invalidTypes <- is.na(parsed)
    if(any(invalidTypes)){
      failures <- sprintf('invalid values for %s\n',
                          paste('-',names(parsed)[invalidTypes], sep='', collapse = ', '))
    }
  }

  stop.if.not(length(failures) == 0,
              sprintf('failed to parse args (run with --help for usage)\n    PROBLEMS:\n        %s',
                      paste(failures, collapse = '\n        ')))

  if(exit.on.help && parsed[['-help']]){
    cat(make.usage.string(filename, arglist, prologue, epilogue))
    q(save='no',runLast=FALSE)
  }

  parsed
}

make.usage.string <- function(filename, arglist, prologue, epilogue){
  std.indent <- '\n        '
  sprintf('%s\nUSAGE: %s %s\n  ARGS  %s\n%s\n',
          prologue,
          filename,
          paste(lapply(arglist,
                       function(x){
                         v <- x$name
                         if(('required' %in% names(x)) && x$required){
                           sprintf('-%s',v)
                         }else{
                           sprintf('[-%s]',v)
                         }}),
                collapse = ' '),
          paste(lapply(arglist,
                       function(x){
                         ret.line <- NULL
                         if(str_length(x$name) >= 6)
                           ret.line <- paste(std.indent,'       ',sep='')

                         paste(c('-', x$name, ret.line, '\t', x$desc), collapse = '')
                       }),
                collapse = std.indent),
          epilogue)
}



parse.list <- function(v, sep=',', def='=', useDefaultNames=TRUE){
  if(!is.null(v)){
    vv <- str_split(v,sep)[[1]]

    z <- flatten(sapply(lzip(1:length(vv), vv),
                        function(e){
                          i <- e[[1]]
                          x <- e[[2]]
                          y <- str_split(x,def)[[1]]

                          if(length(y) == 1){
                            key <- if(useDefaultNames){i}else{''}
                            val <- y[1]
                          }else{
                            key <- y[1]
                            val <- y[2]
                          }
                          z <- list(val)
                          names(z) <- key
                          z
                        }
                        ))
  }else{
    z <- NULL
  }
  z
}

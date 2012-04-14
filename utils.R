system.os <- function(){.Platform$OS.type}

get.parallel.library <- function(){
  if(system.os() == 'windows'){
    list(lib='doSMP',activate=function(cores){eval(parse(text=sprintf('registerDoSMP(%d)',cores)))})
  }else{
    list(lib='doMC',activate=function(cores="NULL"){eval(parse(text=sprintf('registerDoMC(%s)',cores)))})
  }
}

better.library <- function(pkg,repos='http://cran.cnr.Berkeley.edu/',lib=.Library,attempt=1){
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
  check
}

dump <- sapply(c('gdata',
                 'stringr',
                 'plyr',
                 'hash',
                 'R.oo',
                 get.parallel.library()$lib
                 ),
               better.library)

####################
#### Logging
####################
options(warn=-1)
setConstructorS3('SimpleLog',
                 function(id='log', level=c('info','warning','error'), availableLevels=NULL, outputs=stderr(), overwrite=TRUE){
                   availableLevels <- union(level,availableLevels)
                   if(overwrite){
                     sapply(outputs[outputs != ""],
                            function(x){
                              if(is.character(x)){
                                file.remove(x)
                              }
                            }
                            )
                   }
                   extend(Object(), 'SimpleLog',
                          id=id,
                          level=level,
                          availableLevels=availableLevels,
                          outputs=outputs)
                 })

setMethodS3('write.msg','SimpleLog',
            function(log,...,level='info',sep=' - '){
              check <- TRUE
              if(level %in% log$level){
                check <- all(sapply(log$outputs,
                                    function(o){
                                      sapply(intersect(level,log$level),
                                             function(lvl){
                                               msg <- do.call(paste,c(as.list(keep.if(c(log$id, lvl, sprintf(...)),
                                                                                      function(i){!is.null(i)})), sep=sep))
                                               tryCatch(is.null(cat(msg,'\n', file=o, append=TRUE)), error=function(e){FALSE})
                                             })
                                    }))
              }
            })

setConstructorS3('Timer',
                 function(log=NULL){
                   if(is.null(log)){
                     log <- SimpleLog('timerLog')
                   }
                   extend(Object(), 'Timer',
                          log=log)
                 })
setMethodS3('start.timer', 'Timer',
            function(self,msg=NULL,...){
              if(!is.null(msg)){
                write.msg(self$log,msg)
              }
              self$startTime <- proc.time()[3]
            })
setMethodS3('stop.timer', 'Timer',
            function(self,...){
              self$stopTime <- proc.time()[3]
              dt <- self$stopTime - self$startTime
              m <- as.integer(dt / 60)
              s <- round(dt - 60 * m,1)
              write.msg(self$log,
                        sprintf('elapsed time: %s', paste(m, 'm', s, 's')))
            })
options(warn=0)
####################
#### Files
####################

rrmdir <- function(path,rmContentsOnly=FALSE,displayLevel=0){
  path <- gsub('/( )*$','',path)
  isDir <- file.info(path)$isdir
  if(!is.na(isDir) && isDir){
    for(i in dir(path)){
      rrmdir(file.path(path,i),FALSE,displayLevel=displayLevel)
    }
  }
  if(!rmContentsOnly){
    file.remove(path)
  }
}

####################
#### Lists/Vectors
####################

csplat <- function(f,a,...){
  do.call(f,c(as.list(a),...))
}

tmapply <- function(f,g,...){
  do.call(mapply,c(f,lapply(list(...),function(x) split(x,g))))
}

lzip <- function(...){
  args <- list(...)
  n <- min(sapply(args,length))
  if(n == 0){
    return(NULL)
  }
  lapply(1:n,
         function(i){
           lapply(1:length(args),
                  function(j){
                    y <- args[[j]]
                    if(typeof(y) == 'list')
                      y[[i]]
                    else
                      y[i]
                  })
         })
}

zip.to.named <- function(x,nameCol=1,valCol=2){
  do.call(c,lapply(x,
                   function(y){
                     z <- list(y[[valCol]])
                     names(z) <- y[[nameCol]]
                     z
                   }))
}

keep.if <- function(x,f){
  mask <- sapply(x,f)
  x[mask]
}

flatten <- function(x){
  do.call(c,x)
}

save.plots <-function(plots,outputPath,ext='png',...,.parallel=FALSE){
  llply(plots,function(x){
    tryCatch(ggsave(filename=paste(outputPath,'/',x$name,'.',ext,sep=''),plot=x$plot,...),
             error=function(e){
               NA
             })
  }, .parallel=.parallel)
}

merge.lists <- function(all,FUN=function(n,x){x}){
  allNames <- unique(do.call(c,lapply(all,names)))
  z <- lapply(allNames,
              function(n){
                z <- FUN(n,lapply(all,
                                  function(x){
                                    tryCatch(x[[n]],
                                             error=function(e){NULL})
                                  }))
                z
              })
  names(z) <- allNames
  z
}



####################
#### Dataframe
####################

max.element.str.length <- function(data,.parallel=FALSE){
  maxLengths <- llply(names(data),
                       function(i){
                         max(str_length(i),
                             max(sapply(data[[i]],
                                        function(j) str_length(j)
                                        )))},
                      .parallel=.parallel)
  names(maxLengths) <- names(data)
  maxLengths
}

rename.cols <- function(data, old, new){
  cbind(data,
        do.call(c,
                lapply(1:length(old),
                       function(i){
                         z <- list(data[[old[i]]])
                         names(z) <- new[i]
                         z
                       }
                       )
                )
        )
}

str.align <- function(data, maxLengths, .parallel=FALSE){
  result <- llply(names(maxLengths),
                  function(i){
                    sapply(data[[i]],
                           function(j){
                             x <- as.character(j)
                             if(is.na(x)){x <- 'NA'}
                             n <- maxLengths[[i]] - str_length(x)
                             if(is.na(n)){
                               n <- 2
                               x <- 'NA'
                             }
                             fmtd <- x
                             if(n > 0){
                               fmtd <- sprintf('%s%s',x,do.call(paste,c(as.list(rep(' ',n)),sep='')))
                             }
                             fmtd
                           })},
                  .parallel=.parallel)
  names(result) <- names(maxLengths)
  result
}


pprint.dataframe <- function(data,sep='  |  ',.parallel=FALSE){
  maxLengths <- max.element.str.length(data,.parallel=.parallel)
  header <- as.list(names(maxLengths))
  names(header) <- header
  result <- str.align(data,maxLengths,.parallel=.parallel)
  result <- as.data.frame(result)
  header <- do.call(paste,as.list(c(str.align(header,maxLengths),sep=sep)))
  paste(header,
        do.call(paste,as.list(c(rep('-',str_length(header)),sep=''))),
        do.call(paste, as.list(c(apply(result,1,
                                       function(x){
                                         do.call(paste,as.list(c(x,sep=sep)))
                                       }),
                                 sep='\n'))),
        '',
        sep='\n'
        )
}

dataframe.to.html.table <- function(x,
                                    table.attrs='border="1"',
                                    th.attrs='style=font-size:24px',
                                    add.tr.attr=function(x,i){''},
                                    add.td.attr=function(x,i,j){''}){
  if(nrow(x) == 0){
    rows <- ''
  }else{
    rows <- do.call(paste,
            lapply(1:nrow(x),
                   function(i){
                     z <- sprintf('<tr %s>%s</tr>',
                                  add.tr.attr(x,i),
                                  do.call(paste,
                                          lapply(1:ncol(x),
                                                 function(j){
                                                   sprintf('<td %s>%s</td>',
                                                           add.td.attr(x,i,j),
                                                           x[i,j])
                                                 })
                                          ))
                     z
                   }))
  }
  headers <- sprintf('<tr>%s</tr>',
                       do.call(paste,lapply(colnames(x), function(c){sprintf('<th %s>%s</th>', th.attrs, c)})))
  z <- sprintf('<table %s>\n%s\n%s\n</table>',
               table.attrs,
               headers,
               rows
               )
  z
}


####################
#### Misc
####################


int.to.char.map <- hash(keys=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126"),values=c("NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL","BS","TAB","LF","VT","FF","CR","SO","SI","DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB","CAN","EM","SUB","ESC","FS","GS","RS","US"," ","!","\"","#","$","%","&","\'","(",")","*","+",",","-",".","/","0","1","2","3","4","5","6","7","8","9",":",";","<","=",">","?","@","A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","[","]","^","_","`","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","{","|","}","~"))

int.to.char <- function(i){
  int.to.char.map[[as.character(i)]]
}

char.to.int.map <- hash(values=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126"),keys=c("NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL","BS","TAB","LF","VT","FF","CR","SO","SI","DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB","CAN","EM","SUB","ESC","FS","GS","RS","US"," ","!","\"","#","$","%","&","\'","(",")","*","+",",","-",".","/","0","1","2","3","4","5","6","7","8","9",":",";","<","=",">","?","@","A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","[","]","^","_","`","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","{","|","}","~"))

char.to.int <- function(c){
  as.integer(char.to.int.map[[as.character(c)]])
}

int.to.char.seq <- function(x, offset=0){
  n <- as.integer((x-1) / 26) + 1
  m <- as.integer((x-1) %% 26)
  do.call(paste,c(as.list(rep(int.to.char(m + 97),n)),sep=''))
}

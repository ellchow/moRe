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
                 'hash'),
               better.library)

####################
#### Logging
####################
make_log <- function(id, level=c('info','warning','error'), availableLevels=NULL, outputs="", overwrite=TRUE){
  availableLevels <- union(level,availableLevels)
  if(overwrite){
    sapply(outputs[outputs != ""], file.remove)
  }
  hash(id=id,
       level=level,
       availableLevels=availableLevels,
       outputs=outputs
       )
}

write_log <- function(log,...,level='info',sep=' - '){
  if(!is.null(log) && !suppressWarnings(is.na(log))){
    check <- TRUE
    if(level %in% log$level){
      check <- all(sapply(log$outputs,
                          function(o){
                            sapply(intersect(level,log$level),
                                   function(lvl){
                                     msg <- do.call(paste,c(as.list(keep_if(c(log$id, lvl, sprintf(...)),
                                                                            function(i){!is.null(i)})), sep=sep))
                                     tryCatch(is.null(cat(msg,'\n', file=o, append=TRUE)), error=function(e){FALSE})
                                   })
                          }))
    }
  }
  log
}

timer_start <- function(...,level='info'){
  write_log(...)
  proc.time()[3]
}

timer_stop <- function(t0, log, level='info'){
  t1 <- proc.time()[3]
  dt <- t1 - t0
  m <- as.integer(dt / 60)
  s <- round(dt - 60 * m,1)
  write_log(log, "elapsed time: %s", paste(m, 'm', s, 's'), level)
}

####################
#### Files
####################

rrmdir <- function(path,rmContentsOnly=FALSE,displayLevel=0){
  path <- gsub('/( )*$','',path)
  isDir <- file.info(path)$isdir
  printv(paste('At', path), msgLevel=-1, displayLevel=displayLevel)
  printv(paste(path,'is dir?',isDir), msgLevel=-1, displayLevel=displayLevel)
  if(!is.na(isDir) && isDir){
    for(i in dir(path)){
      printv("Recurse %s",file.path(path,i),msgLevel=-1,displayLevel=displayLevel)
      rrmdir(file.path(path,i),FALSE,displayLevel=displayLevel)
    }
  }
  if(!rmContentsOnly){
    file.remove(path)
  }
  printv(paste('Removed', path), msgLevel=-1, displayLevel=displayLevel)
}

####################
#### Misc
####################

system_os <- function(){.Platform$OS.type}

get_parallel_library <- function(){
  if(system_os() == 'windows'){
    list(lib='doSMP',activate=function(cores){eval(parse(text=sprintf('registerDoSMP(%d)',cores)))})
  }else{
    list(lib='doMC',activate=function(cores="NULL"){eval(parse(text=sprintf('registerDoMC(%s)',cores)))})
  }
}

stat_sum_df <- function(fun, geom="crossbar", colour='steelblue',...) {
  stat_summary(fun.data=fun, colour=colour, geom=geom, width=0.4, ...)
}

linear_norm <- function(x, lb, ub, clipMin=FALSE, clipMax=FALSE, rm_na=TRUE, displayLevel=0){
  if(any(lb == ub)){
    printv('WARNING: denominator is 0 in linear_norm\n',msgLevel=1,displayLevel=displayLevel)
  }
  if(clipMin){
    x <- max(x,lb)
  }
  if(clipMax){
    x <- min(x,ub)
  }
  y <- (x - lb) / (ub - lb)

  y
}

lzip <- function(x,y){
  lapply(1:min(length(x),length(y)),
         function(i){
           list(x[i],if(typeof(y) == 'list') y[[i]] else y[i])
         })
}

zip_to_named <- function(x){
  do.call(c,lapply(x,
                   function(y){
                     z <- list(y[[2]])
                     names(z) <- y[[1]]
                     z
                   }))
}

keep_if <- function(x,f){
  mask <- sapply(x,f)
  x[mask]
}

flatten <- function(x){
  do.call(c,x)
}

save_plots <-function(plots,outputPath,ext='png',...,.parallel=FALSE){
  llply(plots,function(x){
    tryCatch(ggsave(filename=paste(outputPath,'/',x$name,'.',ext,sep=''),plot=x$plot,...),
             error=function(e){
               printv('WARNING: failed to save plot "%s" - skipped\n', x)
             })
  }, .parallel=.parallel)
  NA
}

merge_lists <- function(all,FUN=function(n,x){x}){
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

max_element_length <- function(data){
  maxLengths <- lapply(names(data),
                       function(i){
                         max(str_length(i),
                             max(sapply(data[[i]],
                                        function(j) str_length(j)
                                        )))})
  names(maxLengths) <- names(data)
  maxLengths
}

str_align <- function(data, maxLengths){
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
                           })})
  names(result) <- names(maxLengths)
  result
}


pprint_dataframe <- function(data,sep='  |  '){
  maxLengths <- max_element_length(data)
  header <- as.list(names(maxLengths))
  names(header) <- header
  result <- str_align(data,maxLengths)
  result <- as.data.frame(result)
  header <- do.call(paste,as.list(c(str_align(header,maxLengths),sep=sep)))
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

rdiscrete <- function(dist, domain=NULL, n=1){
  dist <- cumsum(dist)
  m <- length(dist)
  if(is.null(domain)){domain <- 1:m}
  sapply(runif(n), function(x) domain[(x <= dist)][1])
}

explode_dataframe <- function(all, dist, groupBy=NULL, outputPath=NULL){
  if(is.null(groupBy)){
    groupIds <- 1:nrow(all)
  }else{
    groupIds <- unique(all[[groupBy]])
  }
  nShards <- length(dist)
  shardIds <- as.list(rdiscrete(dist,n=length(groupIds)))
  names(shardIds) <- groupIds
  shards <- lapply(1:nShards,
                   function(s){
                     check <- if(is.null(groupBy)){groupIds}else{all[[groupBy]]}
                     drop.levels(all[check %in% names(shardIds)[shardIds == s],])
                   })
  if(!is.null(outputPath)){
    sapply(1:nShards,
           function(s){
             sh<-shards[[s]];
             save(sh,file=sprintf('%s_%d_%.2f.rda',outputPath,s,dist[s]))
           })
    NULL
  }else{
    shards
  }
}

int_to_char_map <- hash(keys=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126"),values=c("NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL","BS","TAB","LF","VT","FF","CR","SO","SI","DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB","CAN","EM","SUB","ESC","FS","GS","RS","US"," ","!","\"","#","$","%","&","\'","(",")","*","+",",","-",".","/","0","1","2","3","4","5","6","7","8","9",":",";","<","=",">","?","@","A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","[","]","^","_","`","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","{","|","}","~"))

int_to_char <- function(i){
  int_to_char_map[[as.character(i)]]
}

char_to_int_map <- hash(values=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126"),keys=c("NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL","BS","TAB","LF","VT","FF","CR","SO","SI","DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB","CAN","EM","SUB","ESC","FS","GS","RS","US"," ","!","\"","#","$","%","&","\'","(",")","*","+",",","-",".","/","0","1","2","3","4","5","6","7","8","9",":",";","<","=",">","?","@","A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","[","]","^","_","`","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","{","|","}","~"))

char_to_int <- function(c){
  as.integer(char_to_int_map[[as.character(c)]])
}

int_to_char_seq <- function(x, offset=0){
  n <- as.integer((x-1) / 26) + 1
  m <- as.integer((x-1) %% 26)
  do.call(paste,c(as.list(rep(int_to_char(m + 97),n)),sep=''))
}

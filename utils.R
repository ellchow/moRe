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

import('doMC',
       'gdata',
       'stringr',
       'plyr',
       'hash',
       'R.oo',
       'digest',
       as.library='utils')


options(width=110,scipen=6)

####################
#### Logging
####################
options(warn=-1)
setConstructorS3('SimpleLog',
                 function(id='log', level=c('info','warning','error'), availableLevels=NULL, outputs=stderr(), overwrite=TRUE){
                   availableLevels <- union(level,availableLevels)
                   if(overwrite){
                     sapply(outputs[is.character(outputs) & outputs != ""],
                            function(x){
                              if(is.character(x)){
                                file.remove(x)
                              }
                            })
                   }
                   extend(Object(), 'SimpleLog',
                          id=id,
                          level=level,
                          availableLevels=availableLevels,
                          outputs=outputs)
                 })

SimpleLog.INFO <- 'info'
SimpleLog.WARNING <- c(SimpleLog.INFO, 'warning')
SimpleLog.ERROR <- c(SimpleLog.WARNING, 'error')
SimpleLog.DEBUG <- c(SimpleLog.ERROR, 'debug')

setMethodS3('write.msg','SimpleLog',
            function(log,...,level=SimpleLog.INFO,sep=' - '){
              check <- TRUE
              if(tail(level,1) %in% log$level){
                check <- all(sapply(log$outputs,
                                    function(o){
                                      sapply(intersect(tail(level,1),log$level),
                                             function(lvl){
                                               msg <- csplat(paste, list(format(Sys.time(), "%Y/%m/%d %H:%M:%S"), lvl, log$id, sprintf(...)), sep=sep)
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
            function(self, msg=NULL, ...){
              if(!is.null(msg)){
                write.msg(self$log,msg, ...)
              }
              self$startTime <- proc.time()[3]
            })
setMethodS3('stop.timer', 'Timer',
            function(self, ...){
              self$stopTime <- proc.time()[3]
              dt <- self$stopTime - self$startTime
              m <- as.integer(dt / 60)
              s <- round(dt - 60 * m,1)
              write.msg(self$log,
                        sprintf('elapsed time: %s', paste(m, 'm', s, 's')),
                        ...)
            })
options(warn=0)

stop.if <- function(x,msg,cleanup = function(){}){
  if(x){
    cleanup()
    stop(msg)
  }
}
stop.if.not <- function(x,msg,cleanup = function(){})
  stop.if(!x,msg,cleanup)


####################
#### Files
####################

rrmdir <- function(path,rm.contents.only=FALSE){
  path <- gsub('/( )*$','',path)
  isDir <- file.info(path)$isdir
  if(!is.na(isDir) && isDir){
    for(i in dir(path)){
      rrmdir(file.path(path,i),FALSE)
    }
  }
  if(!rm.contents.only){
    file.remove(path)
  }
}

py.urlencode <- function(p){
  csplat(paste,system(paste('python -c "import urllib; print urllib.urlencode({',
               csplat(paste,lapply(lzip(names(p),p),
                                   function(kv)  sprintf("'%s':'%s'",
                                                         str_replace_all(kv[[1]],"'","\\\\'"),
                                                         str_replace_all(kv[[2]],"'","\\\\'")) ),
                      sep=','),' })"'),
         intern = T),
         sep='')
}

curl.cmd <- function(url, output.path, params = NULL, method = 'get', show.progress = NULL, custom.opts = ''){
  stop.if.not(method %in% c('get','post'), 'method must be get or post')
  stop.if.not(is.null(show.progress) || show.progress %in% c('bar','text'), 'progress must be bar or text')

  if(!is.null(params))
    ps <- py.urlencode(params)
  else
    ps <- ''


  if(is.null(show.progress))
    progress.opt <- '-s'
  else if (show.progress == 'bar')
    progress.opt <- '-#'
  else if(show.progress == 'text')
    progress.opt <- ''



  method.opt <- if(method == 'get') '-X GET' else '-X POST'

  sprintf('curl %s %s --data "%s" -o %s %s "%s"',
          method.opt,
          custom.opts,
          ps,
          output.path,
          progress.opt,
          url)

}

cache.data <- function(path, ..., cache.path='.cache', force=FALSE, log.level = SimpleLog.INFO){
  logger <- SimpleLog('cache.data', log.level)

  if(str_detect(path,'http[s]?://')){
    path.hash <- digest(csplat(paste, list(...), path), 'md5')
    cached.file <- file.path(cache.path, path.hash)
    cmd <- curl.cmd(path, cached.file, ...)

    write.msg(logger, 'curl command:  %s', cmd, level = SimpleLog.DEBUG)
    write.msg(logger, 'cached file at %s', cached.file)

    dir.create(cache.path, showWarnings = FALSE)
    exit.code <- 0
    if(!file.exists(cached.file) || force)
      exit.code <- system(cmd)
    else
      write.msg(logger, 'reading from cache', cmd)

    stop.if.not(exit.code == 0, 'failed to download file', function() file.remove(cached.file) )
    conn <- cached.file
  }else{
    conn <- path
  }
  conn
}

load.data <- function(path,...,sep='\t',header=T,comment.char='',quote='',cache.path='.cache', show.progress = NULL, force=FALSE, log.level = SimpleLog.INFO){
  options(warn=-1)

  if(is.list(path)){
    path <- c(path, cache.path = cache.path, show.progress = show.progress, force = force, list(log.level = log.level))
    conn <- csplat(cache.data, path)
  }else{
    conn <- cache.data(path, cache.path = cache.path, force = force)
  }

  x <- tryCatch(get(load(conn)),
                error=function(e){
                  read.table(conn,sep=sep,header=header,comment.char=comment.char,quote=quote,...)
                })
  options(warn=0)
  x
}

load.data.many <- function(paths,...,.parallel=FALSE){
  as.data.frame(llply(csplat(rbind,
                             llply(system(sprintf('ls %s', csplat(paste,paths)),intern=T),
                                   function(z) load.data(z,stringsAsFactors=F,...),.parallel=.parallel)),
                      function(col){if(is.character(col)) factor(col) else col},
                      .parallel=.parallel))
}

file.to.string <- function(file)
  readChar(file, file.info(file)$size)


brew.string <- function(s,...){
  dots <- list(...)
  e <- if(length(dots) == 0) new.env() else list2env(dots)
  brewedSql <- tempfile()
  brew(text=s,output=brewedSql,envir=e)
  sql <- file.to.string(brewedSql)
  sql
}

####################
#### Lists/Vectors
####################

get.or.else <- function(x, field, default){
  z <- x[[field]]
  if(is.null(z))
    z <- default
  z
}

csplat <- function(f,a,...)
  do.call(f,c(as.list(a),...))


indices <- function(xs, type){
  if(missing(type))
    f <- length
  else if(type == 'col')
    f <- ncol
  else if(type == 'row')
    f <- nrow

  len <- f(xs)
  if(len > 0) 1:len else NULL
}

na.rm <- function(x, required, discard = is.na) {
  if(is.data.frame(x)){
    if(missing(required))
      required <- names(x)
    keep <- Reduce(function(a,b) a & b, lapply(subset(x,select=required), function(y) !discard(y)), init=TRUE)
    x[keep,]
  }else
  x[!discard(x)]
}

inf.rm <- function(...) na.rm(..., discard = is.infinite)

nan.rm <- function(...) na.rm(..., discard = is.nan)

invalid.rm <- function(...) na.rm(..., discard = function(z) is.na(z) | is.nan(z) | is.infinite(z))

tapply <- function (X, INDEX, FUN = NULL, simplify = TRUE, ret.type='list') {
  FUN <- if(!is.null(FUN))
    match.fun(FUN)
  if(!is.list(INDEX))
    INDEX <- list(INDEX)
  if(!is.list(X))
    X <- list(X)
  names(X) <- NULL
  nI <- length(INDEX)
  namelist <- vector("list", nI)
  names(namelist) <- names(INDEX)
  extent <- integer(nI)
  nx <- length(X[[1]])
  one <- 1L
  group <- rep.int(one, nx)
  ngroup <- one
  for(i in seq_along(INDEX)){
    index <- as.factor(INDEX[[i]])
    if(length(index) != nx)
      stop("arguments must have same length")
    namelist[[i]] <- levels(index)
    extent[i] <- nlevels(index)
    group <- group + ngroup * (as.integer(index) - one)
    ngroup <- ngroup * nlevels(index)
  }
  if(is.null(FUN))
    return(group)
  ans <- do.call(mapply, c(FUN, lapply(X, function(x) split(x, group)), SIMPLIFY=FALSE))
  index <- as.integer(names(ans))

  if(simplify && all(unlist(lapply(ans, length)) == 1L)){
    ansmat <- array(dim = extent, dimnames = namelist)
    ans <- unlist(ans, recursive = FALSE)
  }
  else{
    ansmat <- array(vector("list", prod(extent)), dim = extent,
                    dimnames = namelist)
  }
  if(length(index)){
    names(ans) <- NULL
    ansmat[index] <- ans
  }

  if(ret.type == 'df'){
    ansmat <- data.frame(expand.grid(dimnames(ansmat)),y=do.call(rbind,as.list(ansmat)))
  }else if(ret.type == 'join'){
    stop.if(length(INDEX) != 1, 'INDEX must have length 1 for ret.type == "par"')
    ansmat <- ansmat[as.character(INDEX[[1]])]
  }else if(ret.type == 'par'){
    z <- vector('list',length(X[[1]]))
    split(z, group) <- ansmat
    ansmat <- unlist(z)
  }else{
    stop.if(ret.type != 'list', sprintf('unknown ret.type "%s"', ret.type))
  }

  ansmat
}

lzip <- function(...){
  args <- lapply(list(...), as.list)
  n <- min(sapply(args,length))
  if(n <= 0)
    return(NULL)

  lapply(1:n,
         function(i){
           zip.to.named(lapply(indices(args),
                  function(j){
                    y <- args[[j]]
                    if(typeof(y) == 'list')
                      list(names(y)[i], y[[i]])
                    else
                      list(names(y)[i], y[[i]])
                  }))
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

named <- function(x, n, type=''){
  if(type == 'row')
    row.names(x) <- n
  else if(type == 'col')
    colnames(x) <- n
  else
    names(x) <- n
  x
}

remove.names <- function(x, type=''){
  named(x, NULL, type)
}

keep.if <- function(x,f){
  mask <- sapply(x,f)
  x[mask]
}

flatten <- function(x){
  do.call(c,x)
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

setdiff2 <- function(x,y){
  list(setdiff(x,y), setdiff(y, x))
}

make.combinations <- function(...){
  dots <- list(...)
  apply(do.call(expand.grid, dots), 1,
        function(z) as.list(z))
}

parameter.scan <- function(params.list, f){
  named(lapply(params.list,
               function(params) do.call(f, as.list(params)) ),
        names(params.list))
}

sample.by <- function(x,...,as.filter=TRUE){
  if(is.factor(x))
    g <- levels(x)
  else
    g <- unique(x)

  g.s <- sample(g, ...)

  if(as.filter)
    x %in% g.s
  else
    x[x %in% g.s]
}

####################
#### Dataframe
####################

rbind.fill.par <- function(lst, m = 1000) {
  n <- length(lst)
  ldply(1:ceiling(n / m),
        function(offset){
          select <- 1 + ((m * (offset - 1)) : (m * offset - 1))
          select <- select[select <= n]

          ds <- lst[select]

          rbind.fill(ds)
        },
        .parallel = TRUE)
}

max.element.str.length <- function(data,.parallel=FALSE){
  maxLengths <- llply(names(data),
                      function(i){
                        max(str_length(i),
                            max(str_length(data[[i]])))
                      },
                      .parallel=.parallel)
  names(maxLengths) <- names(data)
  maxLengths
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


pprint.dataframe <- function(data, sep='  |  ', prepend.row.names = ' ', .parallel=FALSE){
  if(is.matrix(data))
    data <- as.data.frame(data)

  stop.if.not(is.data.frame(data), 'input must be data.frame')

  if(!is.null(prepend.row.names) && !is.null(row.names(data)))
    data <- named(cbind(row.names(data), data), c(prepend.row.names, names(data)))

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

dataframe.to.textile <- function(x, attr.for = function(e, i, j) NA, header = T, prepend.row.names = ' ', .parallel=FALSE){
  row.to.tt <- function(row) paste('|', csplat(paste, as.character(row), sep = ' |'), ' |', sep='')

  add.attr <- function(e, i, j) {
    attr <- attr.for(e, i, j)

    if(is.na(attr)) paste(' ', e, sep='') else paste(attr, e, sep='. ')
  }

  if(is.matrix(x))
    x <- as.data.frame(x)

  if(!is.null(prepend.row.names) && !is.null(row.names(x)))
    x <- named(cbind(row.names(x), x), c(prepend.row.names, names(x)))


  zz <- if(header) paste('_', names(x), sep='. ') else NULL
  csplat(paste,
         list(if(header) row.to.tt(paste('_', names(x), sep='. ')) else NULL),
         llply(indices(x, 'row'),
               function(i){
                 row.to.tt(sapply(indices(x, 'col'),
                                  function(j) add.attr(x[i,j], i, j)))
               },
               .parallel=.parallel),
         sep = '\n')
}

dataframe.to.html.table <- function(x,
                                    table.attrs='class="sortable" border="1"',
                                    th.attrs='style=font-size:24px',
                                    add.tr.attr=function(x,i){''},
                                    add.td.attr=function(x,i,j){''},
                                    prepend.row.names = ' ',
                                    .parallel=FALSE){
  if(is.matrix(x))
    x <- as.data.frame(x)

  if(!is.null(prepend.row.names) && !is.null(row.names(x)))
    x <- named(cbind(row.names(x), x), c(prepend.row.names, names(x)))

  if(nrow(x) == 0){
    rows <- ''
  }else{
    rows <- do.call(paste,
                    llply(1:nrow(x),
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
                          },.parallel=.parallel))
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

sorttable.import <- function(loc='http://www.kryogenix.org/code/browser/sorttable/sorttable.js'){
  sprintf('<script src="%s"></script>
<style media="screen" type="text/css">

table.sortable thead {
    background-color:#eee;
    color:#666666;
    font-weight: bold;
    cursor: default;
}

</style>',loc)
}

###################
#### Date
###################

STANDARD.TIMESTAMP.FORMAT <- "%Y-%m-%d %H:%M:%S"
EPOCH <- strptime("1970-01-01 00:00:00", STANDARD.TIMESTAMP.FORMAT, tz="UTC")
MIN.SEC <- 60
HOUR.SEC <- 60 * MIN.SEC
DAY.SEC <-  24 * HOUR.SEC
unix.timestamp.to.fmt <- function(ts,fmt=STANDARD.TIMESTAMP.FORMAT){
  as.POSIXct(ts,fmt,origin=EPOCH)
}

####################
#### Misc
####################

str.fmt <- function(s,...){
  dots <- list(...)
  named.pat <- '(^|[^%])(%)\\(([A-Za-z0-9_.]+?)\\)(([0-9.]+)?[sdf])'
  unnamed.pat <- '(^|[^%])(%[sdf])'
  named <- str_detect(s,named.pat)
  ss <- s
  params <- dots
  if(named){
    n <- as.vector(sapply(str_extract_all(s,named.pat),
                          function(x) gsub(named.pat,'\\3', x)))

    stop.if(is.null(names(dots)) || any(str_length(names(dots))==0),
            'requires named parameters')

    stop.if(any(!n %in% names(dots)), sprintf('missing params %s', csplat(paste,setdiff(n,names(dots)),sep=',')))

    ## first escape things that percent symbols; then replace named params with unnamed params
    ss <- gsub(named.pat,'\\1\\2\\4',
               gsub(unnamed.pat,'\\1%\\2',s))

    ## get params in order of appearance
    params <- dots[n]
  }
  csplat(sprintf, ss, params)
}



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


file.size.gb <- function(path)
  file.info(path)$size / (1024^3)

object.size.gb <- function(x)
  object.size(x) / (8 * 1024^3)

estimate.parallelism <- function(sizes.gb, default.memory.gb = 4, num.nested.par = 1, max.procs.per.core = 2, par.limit = Inf, log.level = SimpleLog.INFO){

  floor.8 <- function(x) {
    f.x <- floor(x)
    f.x + ((x -f.x) > 0.8)
  }

  log <- SimpleLog('estimate.parallelism', log.level)

  num.cores <- multicore:::detectCores()
  max.num.procs <- num.cores * max.procs.per.core
  mul <- 100
  max.size.gb <- max(unlist(sizes.gb))
  est.proc.mem.usage <- max.size.gb * mul
  write.msg(log, sprintf('estimated memory usage: %.2f Gb', est.proc.mem.usage))

  mem.free.gb <- get.free.mem.gb(default.memory.gb)
  write.msg(log, sprintf('free memory: %.2f Gb', mem.free.gb))

  max.par <- floor(mem.free.gb / est.proc.mem.usage)
  max.par.with.nested <- pmin(max.par, floor.8(max.par ^ (1 / num.nested.par)))

  pmax(1,pmin(par.limit, max.par.with.nested))
}

get.free.mem.gb <- function(default.memory.gb = 1, log.level=SimpleLog.INFO){
  log <- SimpleLog('get.free.mem.gb', log.level)

  uname <- system("uname", intern=TRUE)
  if(uname == 'Linux'){
    cmd <- "free -k | grep 'buffers/cache' | sed -r 's/\\s+/\\t/g' |cut -f4"
    write.msg(log,str.fmt('OS detected: Linux - command to get free memory\n  #> %(cmd)s', cmd=cmd))

    as.double(system(cmd, intern=TRUE)) / 1024^2
  }else if(uname == 'Darwin'){
    cmd <- "top -l 1 | grep PhysMem| awk -F', ' '{ print $NF }' | sed 's/\\s*free.*//g'"
    write.msg(log,str.fmt('OS detected: Mac - command to get free memory\n  #> %(cmd)s', cmd=cmd))

    mem.str <- str_trim(system(cmd, intern=TRUE))
    mem.unit <- str_sub(mem.str,start=str_length(mem.str))
    mem.n <- as.double(str_sub(mem.str,end=str_length(mem.str)-1))
    d <- if(mem.unit == 'G') 1 else if(mem.unit == 'M') 1024 else if(mem.unit == 'K') 1024^2 else 1024^3

    mem.n / d
  }else
  default.memory.gb

}

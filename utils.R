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
       'Hmisc',
       'digest',
       'sfsmisc',
       'rjson',
       'testthat',
       as.library='utils')

## options(width=110,scipen=6)
options(scipen=6)

####################
#### Logging
####################
options(warn=-1)
setConstructorS3('SimpleLog',
                 function(id='log',
                          level=c('info','warning','error'),
                          colors = c('info'='light gray','warning'='yellow','error'='red','debug'='dark gray')
                          ## outputs=stderr(),
                          ##overwrite=TRUE
                          ){
                   ## if(overwrite){
                   ##   sapply(outputs[is.character(outputs) & outputs != ""],
                   ##          function(x){
                   ##            if(is.character(x)){
                   ##              file.remove(x)
                   ##            }
                   ##          })
                   ## }
                   extend(Object(), 'SimpleLog',
                          id=id,
                          level=level,
                          colors=colors,
                          ## outputs=outputs
                          outputs=stderr()
                          )
                 })

SimpleLog.INFO <- 'info'
SimpleLog.WARNING <- c(SimpleLog.INFO, 'warning')
SimpleLog.ERROR <- c(SimpleLog.WARNING, 'error')
SimpleLog.DEBUG <- c(SimpleLog.ERROR, 'debug')

if(!exists('SimpleLog.CONFIG', envir = globalenv())){
  assign('SimpleLog.CONFIG', new.env(), envir=globalenv())
}

setMethodS3('write.msg','SimpleLog',
            function(log,...,level=SimpleLog.INFO,sep=' - '){
              check <- TRUE

              lvl <- intersect(tail(level,1), log$level)
              if(length(lvl) > 0){
                msg <- paste(list(format(Sys.time(), "%Y/%m/%d %H:%M:%S"), lvl, log$id, sprintf(...)), collapse = sep)

                all(sapply(log$outputs,
                           function(o) {
                             if((!is.null(globalenv()$SimpleLog.CONFIG$colorize) && globalenv()$SimpleLog.CONFIG$colorize) && (o %in% c(stderr(), stdout()))){
                               color <- log$colors[lvl]
                               if(!is.na(color))
                                 msg <- colourise(msg, color)
                             }

                             tryCatch(is.null(cat(msg, '\n', file=o, append=TRUE)),
                                      error = function(e) FALSE)
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

stop.if <- function(x, msg, ..., tag = NULL, cleanup = function(){}, failed.cond = substitute(x)){
  call <- sys.call(1)
  if(x){
    err <- simpleError(paste(c(sprintf(msg, ...),
                               '\n  Failed condition: ', failed.cond),
                             collapse=''),
                       call)
    attr(err, 'tag') <- tag

    cleanup()

    stop(err)
  }
}
stop.if.not <- function(x, ...){
  failed.cond <- substitute(!(x))
  stop.if(!x, ..., failed.cond = failed.cond)
}

dump.frames.on.failure <- function(on = TRUE){
  if(on)
    options(error = quote({ dump.frames('dump-frames', to.file = TRUE)
                            system('echo "frames dumped to dump-frames.rda"')
                            q()
                          }
              ))
  else
    options(error = NULL)
}
####################
#### URL encoding
####################
## http://svn.python.org/view/*checkout*/python/tags/r265/Lib/urllib.py?revision=79064&content-type=text%2Fplain

url.always.safe.chars <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","0","1","2","3","4","5","6","7","8","9","_",".","-")
url.reserved.chars <- c(";", "/", "?", ":", "@", "&", "=", "+", "$", ",")

url.quote <- function(s, reserved = url.reserved.chars,  plus.spaces = T){
  chars <- int.to.char(1:255)
  safe <- named(ifelse(chars %in% c(url.always.safe.chars, reserved), chars, sprintf('%%%X', 1:255)),
                chars)

  if(plus.spaces)
    safe[[' ']] <- '+'

  unlist(lapply(strcodes(s), function(chars) paste(safe[chars], collapse = '')))
}

url.unquote <- function(s, reserved = NULL, plus.spaces = T){
  chars <- int.to.char(1:255)
  safe <- named(chars,
                ifelse(chars %in% c(url.always.safe.chars, reserved), chars, sprintf('%X', 1:255)))

  z <- lapply(strsplit(s, '%'),
         function(xs){
           y <- paste(safe[str_sub(xs[-1], end = 2)],
                 str_sub(xs[-1], start = 3),
                 sep = '')

           z <- paste(c(xs[1], y), collapse = '')

           if(plus.spaces)
             gsub('\\+',' ',z)
           else
             z
         })

  unlist(z)
}

url.encode.params <- function(params){
  params <- unlist(params)
  paste(paste(url.quote(names(params), reserved = NULL, plus.spaces=T),
              url.quote(params, reserved = NULL, plus.spaces=T),
              sep = '='),
         collapse = '&')
}

####################
#### Functions
####################

"%within%" <- function(expr, envir) eval(substitute(expr), envir=envir)

tag <- function(x,...) {
  dots <- list(...)
  tagged <- x
  attr(tagged, names(dots)[1]) <- dots[[1]]

  tagged
}

var.name <- function(x) deparse(substitute(x))

is.global.env <- function(env) environmentName(env) == 'R_GlobalEnv'

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

curl.cmd <- function(url, output.path, params = NULL, method = 'get', show.progress = NULL, custom.opts = ''){
  stop.if.not(method %in% c('get','post'), 'method must be get or post')
  stop.if.not(is.null(show.progress) || show.progress %in% c('bar','text'), 'progress must be bar or text')

  if(!is.null(params))
    ps <- url.encode.params(params)
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

    stop.if.not(exit.code == 0, 'failed to download file', cleanup = function() file.remove(cached.file) )
    conn <- cached.file
  }else{
    conn <- path
  }
}

load.data <- function(path, load.fun, ..., cache.path = '.cache', show.progress = NULL, force=FALSE, log.level = SimpleLog.INFO){
  logger <- SimpleLog('load.data', log.level)

  if(missing(load.fun)){
    write.msg(logger, 'missing "load.fun" function - calling load.table', level = SimpleLog.DEBUG)
    return(load.table(path, ..., cache.path = cache.path, show.progress = show.progress, force = force, log.level = log.level))
  }

  if(is.list(path)){
    path <- c(path, cache.path = cache.path, show.progress = show.progress, force = force, list(log.level = log.level))
    conn <- cache.data %wargs% path
  }else{
    conn <- cache.data(path, cache.path = cache.path, force = force, log.level=log.level)
  }

  tryCatch({
    options(warn=-1)
    z <- get(load(conn))
    options(warn=0)
    z
  },
           error = function(e) load.fun(conn, ...))
}

load.lines <- function(path, parser = NULL, cache.path='.cache', show.progress = NULL, force=FALSE, log.level = SimpleLog.INFO){
  load <- function(conn)
    readLines(conn, warn = F)

  z <- load.data(path, load, cache.path = cache.path, show.progress = show.progress, force = force, log.level = log.level)

  if(!is.null(parser))
    lapply(z, parser)
  else
    z
}

load.string <- function(path, parser = NULL, cache.path='.cache', show.progress = NULL, force=FALSE, log.level = SimpleLog.INFO){
  load <- function(conn)
    file.to.string(conn)

  z <- load.data(path, load, cache.path = cache.path, show.progress = show.progress, force = force, log.level = log.level)

  if(!is.null(parser))
    parser(z)
  else
    z
}

load.table <- function(path, ..., sep='\t', header=T, comment.char='', quote='', cache.path='.cache', show.progress = NULL, force=FALSE, log.level = SimpleLog.INFO){
  load.fun <- function(conn)
    read.table(conn, sep=sep, header=header, comment.char=comment.char, quote=quote, ...)

  load.data(path, load.fun, cache.path = cache.path, show.progress = show.progress, force = force, log.level = log.level)
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
  do.call(f, c(as.list(a),...))

"%wargs%" <- function(f, a)
  do.call(f, as.list(a))

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

tapply <- function (X.expr, INDEX.expr, FUN = NULL, simplify = TRUE, ret.type='list', envir = NULL) {
  ## lookup inputs in envir if supplied
  if(is.null(envir)){
    X <- X.expr
    INDEX <- INDEX.expr
  }else{
    X <- eval(substitute(X.expr), envir = envir)
    INDEX <- eval(substitute(INDEX.expr), envir = envir)
  }

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
  ## use mapply/split to allow for multipl inputs
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

  ## return types
  if(ret.type == 'df'){
    ## create data.frame by expanding grid
    ansmat <- data.frame(expand.grid(dimnames(ansmat)),y=do.call(rbind,as.list(ansmat)))
  }else if(ret.type == 'par'){
    ## put output values into original spots
    z <- vector('list',length(X[[1]]))
    split(z, group) <- ansmat
    ansmat <- unlist(z)
  }else{
    stop.if(ret.type != 'list', 'unknown ret.type "%s"', ret.type, tag = 'unknown.type')
  }

  ansmat
}

lzip <- function(...){
  delayedAssign('args', lapply(list(...), as.list))

  n <- min(sapply(args,length))
  if(n <= 0)
    return(NULL)

  lapply(1:n,
         function(i){
           zip.to.named(lapply(indices(args),
                  function(j){
                    y <- args[[j]]

                    list(names(y)[i], y[[i]])
                  }))
         })
}

"%zip%" <- function(a,b) lzip(a, b)

zip.to.named <- function(x,nameCol=1,valCol=2){
  flatten(lapply(x,
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

"%named%" <- function(x, n) named(x, n)

"%rnamed%" <- function(x, n) named(x, n, 'row')

"%cnamed%" <- function(x, n) named(x, n, 'col')

remove.names <- function(x, type='')
  named(x, NULL, type)

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

## merge.lists <- function(...){
## }

setdiff2 <- function(x,y){
  list(setdiff(x,y), setdiff(y, x))
}

make.combinations <- function(...){
  dots <- list(...)
  apply(expand.grid %wargs% dots, 1,
        function(z) as.list(z))
}

parameter.scan <- function(params.list, f, .parallel=FALSE){
  named(llply(params.list,
               function(params) f %wargs% params,
              .parallel=.parallel),
        names(params.list),)
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
                               fmtd <- sprintf('%s%s', x, paste(rep(' ',n), collapse = ''))
                             }
                             fmtd
                           })},
                  .parallel=.parallel)
  names(result) <- names(maxLengths)
  result
}

dataframe.to.tsv <- function(x, file, sep='\t'){
  write.table(x, file=file , sep=sep, row.names=F, quote=FALSE)
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
  header <- paste(str.align(header, maxLengths), collapse = sep)

  paste(header,
        paste(rep('-',str_length(header)), collapse = ''),
        paste(apply(result, 1,
                    function(x) paste(x, collapse = sep)
                  ),
              collapse = '\n'),
        '',
        sep = '\n')
}

dataframe.to.textile <- function(x, attr.for = function(e, i, j) NA, header = T, prepend.row.names = ' ', .parallel=FALSE){
  row.to.tt <- function(row) paste('|', paste(as.character(row), collapse = ' |'), ' |', sep='')

  add.attr <- function(e, i, j) {
    attr <- attr.for(e, i, j)

    if(is.na(attr)) paste(' ', e, sep='') else paste(attr, e, sep='. ')
  }

  if(is.matrix(x))
    x <- as.data.frame(x)

  if(!is.null(prepend.row.names) && !is.null(row.names(x)))
    x <- named(cbind(row.names(x), x), c(prepend.row.names, names(x)))


  zz <- if(header) paste('_', names(x), sep='. ') else NULL
  paste(
        list(if(header) row.to.tt(paste('_', names(x), sep='. ')) else NULL),
        llply(indices(x, 'row'),
              function(i){
                row.to.tt(sapply(indices(x, 'col'),
                                 function(j) add.attr(x[i,j], i, j)))
              },
              .parallel=.parallel),
        collapse = '\n')
}

dataframe.to.html.table <- function(x,
                                    table.attrs='class="sortable" border="1"',
                                    th.attrs='style=font-size:24px',
                                    add.tr.attr=function(i){''},
                                    add.td.attr=function(i,j){''},
                                    prepend.row.names = ' ',
                                    .parallel=FALSE){
  if(is.matrix(x))
    x <- as.data.frame(x)

  if(!is.null(prepend.row.names) && !is.null(row.names(x)))
    x <- named(cbind(row.names(x), x), c(prepend.row.names, names(x)))

  if(nrow(x) == 0){
    rows <- ''
  }else{
    rows <- paste(llply(1:nrow(x),
                        function(i){
                          z <- sprintf('<tr %s>%s</tr>',
                                       add.tr.attr(i),
                                       paste(lapply(1:ncol(x),
                                                    function(j){
                                                      sprintf('<td %s>%s</td>',
                                                              add.td.attr(i,j),
                                                              x[i,j])
                                                    }),
                                             collapse = ''))
                          z
                        },.parallel=.parallel),
                  collapse = '\n')
  }
  headers <- sprintf('<tr>%s</tr>',
                     paste(lapply(colnames(x), function(c){sprintf('<th %s>%s</th>', th.attrs, c)}),
                           collapse = ''))
  sprintf('<table %s>\n%s\n%s\n</table>',
          table.attrs,
          headers,
          rows)
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

unix.timestamp.to.fmt <- function(ts, fmt=STANDARD.TIMESTAMP.FORMAT, tz='UTC')
  strptime(as.POSIXct(ts,origin=EPOCH), fmt, tz=tz)

unix.timestamp.now <- function() unclass(Sys.time())

today <- function(sep='-') unix.timestamp.to.fmt(unix.timestamp.now(), paste('%Y','%m','%d', sep='-'))



####################
#### Misc
####################

str.fmt <- function(s,...){
  dots <- list(...)
  named.pat <- '(^|[^%])(%)\\(([A-Za-z0-9_.]+?)\\)(([0-9.]+)?[sdf])' ## name params can contain alphanumeric chars, underscores, and periods
  unnamed.pat <- '(^|[^%])(%[sdf])'
  named <- str_detect(s,named.pat)
  ss <- s
  params <- dots
  if(named){
    n <- as.vector(sapply(str_extract_all(s,named.pat),
                          function(x) gsub(named.pat,'\\3', x)))

    stop.if(is.null(names(dots)) || any(str_length(names(dots))==0),
            'requires named parameters')

    stop.if(any(!n %in% names(dots)), 'missing params %s', paste(setdiff(n,names(dots)), collapse = ','))

    ## first escape things that percent symbols; then replace named params with unnamed params
    ss <- gsub(named.pat,'\\1\\2\\4',
               gsub(unnamed.pat,'\\1%\\2',s))

    ## get params in order of appearance
    params <- dots[n]
  }
  csplat(sprintf, ss, params)
}

int.to.char <- chars8bit

char.to.int <- AsciiToInt

int.to.char.seq <- function(x, offset=0){
  n <- as.integer((x-1) / 26) + 1
  m <- as.integer((x-1) %% 26)
  paste(rep(int.to.char(m + 97), n), collapse = '')
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

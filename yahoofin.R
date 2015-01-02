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

source('import.R',chdir=T)
import('utils',
       'finance',
       'gdata',
       'stringr',
       'plyr',
       'doMC',
       'foreach',
       'Hmisc',
       'ggplot2'
       )

yfin.tags <- named(c('t8','m4','m3','k','j','w','c8','g3','a','b2','a5','a2','b','b3','b6','b4','c1','m5','m7','k4','j5','p2','c','k2','c6','c3','h','g','m','m2','w1','w4','r1','d','e','j4','e7','e9','e8','e1','q','f6','l2','g4','g1','g5','g6','v1','v7','d1','l1','k1','k3','t1','l','l3','j1','j3','i','n','n4','o','i5','r5','r','r2','k5','m6','m8','j6','p','p6','r6','r7','p1','p5','s1','s7','x','s','t7','d2','t6','v'),
                   c('1yrtargetprice','200daymovingaverage','50daymovingaverage','52weekhigh','52weeklow','52weekrange','afterhourschangerealtime','annualizedgain','ask','askrealtime','asksize','averagedailyvolume','bid','bidrealtime','bidsize','bookvalue','change','changefrom200daymovingaverage','changefrom50daymovingaverage','changefrom52weekhigh','changefrom52weeklow','changeinpercent','changepercentchange','changepercentrealtime','changerealtime','commission','dayshigh','dayslow','daysrange','daysrangerealtime','daysvaluechange','daysvaluechangerealtime','dividendpaydate','dividendshare','earningsshare','ebitda','epsestimatecurrentyear','epsestimatenextquarter','epsestimatenextyear','errorindicationreturnedforsymbolchangedinvalid','exdividenddate','floatshares','highlimit','holdingsgain','holdingsgainpercent','holdingsgainpercentrealtime','holdingsgainrealtime','holdingsvalue','holdingsvaluerealtime','lasttradedate','lasttradepriceonly','lasttraderealtimewithtime','lasttradesize','lasttradetime','lasttradewithtime','lowlimit','marketcapitalization','marketcaprealtime','moreinfo','name','notes','open','orderbookrealtime','pegratio','peratio','peratiorealtime','percentchangefrom52weekhigh','percentchangefrom200daymovingaverage','percentchangefrom50daymovingaverage','percentchangefrom52weeklow','previousclose','pricebook','priceepsestimatecurrentyear','priceepsestimatenextyear','pricepaid','pricesales','sharesowned','shortratio','stockexchange','symbol','tickertrend','tradedate','tradelinks','volume'))

yfin.date.fmt <- '%Y-%m-%d'
yfin.header <- c('date', 'open', 'high', 'low', 'close', 'volume', 'adj.close')

yfin.yearly <- 'y'
yfin.monthly <- 'm'
yfin.weekly <- 'w'
yfin.daily <- 'd'

yfin.num.days.in <- function(x) {
  x <- tolower(x)
  if (x %in% c(yfin.yearly, 'year')) {
    365
  } else if (x %in% c(yfin.monthly, 'month')) {
    30
  } else if (x %in% c(yfin.weekly, 'week')) {
    7
  }
}

yfin.standard.symbol.groups <- list(
    market = c('^FTSE','^GSPC','^HSI','^MID','^SML','VAW','VB','^VIX'),
    bond = c('AGG','BIV','BLV','BND','BSV','EDV'),
    other = c('GLD','SLV','IAU'),
    sector = c('VCR','VDC','VDE','VEU','VFH','VGK','VGT','VHT','VIS','VNQ','VOX','VPL','VPU','VSS','VTI','VUG','VWO','VXF'),
    tech=c('AAPL','GOOG','FB','EBAY','AMZN','YHOO','MSFT','LNKD','TSLA','TRLA','YELP','Z','NFLX','ORCL','TDC','IBM','HPQ','INTC','AMD','NVDA','SSNLF'),
    consumer=c('PG','JNJ','PEP','KO','WMT','TGT','KSS','K','M')
)

yfin.standard.symbols <- unique(flatten(yfin.standard.symbol.groups))

yfin.url <- function(symbol, start.date = '1900-01-01',
                     end.date = format(Sys.time(), yfin.date.fmt),
                     frequency = yfin.weekly){
  u <- 'http://ichart.yahoo.com/table.csv'
  start.date <- as.integer(strsplit(start.date,'-')[[1]])
  end.date <- as.integer(strsplit(end.date,'-')[[1]])


  params <- list(s = symbol[1],
                 a = start.date[2] - 1, ## start month - 1
                 b = start.date[3], ## start day
                 c = start.date[1], ## start year
                 d = end.date[2] - 1, ## end month - 1
                 e = end.date[3], ## end day
                 f = end.date[1], ## end month
                 ignore = '.csv'
                 )

  paste(u, url.encode.params(params), sep='?')
}

yfin.download <- function(symbol, ..., start.date = '1900-01-01',
                          end.date = format(Sys.time(), yfin.date.fmt),
                          frequency = yfin.daily,
                          cache.path = '.yfin-download',
                          log.level = SimpleLog.ERROR){
  logger <- SimpleLog('yfin.download', log.level)

  u <- yfin.url(symbol, start.date, end.date, frequency)

  write.msg(logger, 'yahoo finance url: %s', u)

  x <- tryCatch(load.table(u, ..., sep=',', cache.path = cache.path, log.level = log.level) %named% yfin.header,
                error = function(e){
                  write.msg(logger, 'failed to download data for %s', symbol)
                  NA
                })
  if(is.na(x)){
    NA
  }else{
    x$date <- as.Date(x$date, yfin.date.fmt)
    x$symbol <- factor(symbol)
    x
  }
}

yfin.archive <- function(symbols, path, ...,
                         frequency = yfin.daily,
                         log.level = SimpleLog.DEBUG,
                         .parallel = FALSE){
  ## import('yahoofin'); registerDoMC(2); yfin.archive(yfin.standard.symbols, 'yfin-archive', log.level = SimpleLog.DEBUG, .parallel=T)
  logger <- SimpleLog('yfin.archive', log.level)

  end.date <- format(Sys.time(), yfin.date.fmt)
  current.timestamp <- format(Sys.time(), '%Y%m%d%H%M%S')
  cache.path <- paste('.yfin-download', current.timestamp, sep='-')
  dir.create(cache.path, recursive = TRUE)

  write.msg(logger, 'temporary data directory %s', cache.path)
  if(file.exists(path))
    z <- load.data(path)
  else
    z <- NA

  y <- llply(symbols,
             function(symbol){
               start.date <- if(any(is.na(z))) as.Date('1900-01-01', yfin.date.fmt) else max(z$date[z$symbol == symbol])
               write.msg(logger, 'download %s starting from %s', symbol, start.date)

               if(start.date < end.date){
                 x <- yfin.download(symbol, ..., start.date = as.character(start.date), end.date = as.character(end.date), frequency = frequency, cache.path = cache.path, log.level = log.level)

                 if(!is.na(x) && nrow(x) > 0)
                   subset(x, date > start.date)
                 else
                   NA
               }else
                 NA
             },
             .parallel = .parallel)

  y <- rbind.fill %wargs% y[!is.na(y)]

  if(is.null(y) || nrow(y) == 0){
    write.msg(logger, 'no data added')
    y <- z
  }else{
    write.msg(logger, 'adding %d rows', nrow(y))
    backup.path <- paste(path, 'bak', current.timestamp, sep='.')
    write.msg(logger, 'backup previous data to %s', backup.path)
    file.rename(path, backup.path)
    y <- if(!any(is.na(z))) rbind.fill(z,y) else y

    y <- y[order(y$symbol, y$date),]
    write.msg(logger, 'saving to %s', path)
    save(y, file = path)
  }
  rrmdir(cache.path)
  y
}

yfin.wide.format <- function(x, symbol.list = yfin.standard.symbols, value.var = 'adj.close') {
  dcast(subset(x, symbol %in% symbol.list), date ~ symbol, value.var = value.var)
}

yfin.ggplot.symbol.values <- function(x, symbols.list,
                                      start.date = end.date - 3 * yfin.num.days.in('m'),
                                      end.date = max(x$date[x$symbol %in% symbols.list]),
                                      layout = '',
                                      normalize = FALSE,
                                      value.col = 'adj.close',
                                      as.return = FALSE,
                                      log.level = SimpleLog.INFO) {
  y <- x[(as.character(x$symbol) %in% symbols.list) & !((x$date < start.date) | (x$date > end.date)), ]
  y <- drop.levels(y)
  if (as.return) {
    y[[value.col]] <- tapply(y[[value.col]], y$symbol, compute.returns, ret.type = 'par')
    y <- y[!is.invalid(y[[value.col]]),]
  }
  if (normalize && !as.return) {
    min.idx <- tapply(y$date, y$symbol, which.min, ret.type = 'par')
    init.value <- tapply(list(min.idx, y[[value.col]]), y$symbol, function(i, z) z[i], ret.type = 'par')
    y$value <- y[[value.col]] / init.value
  } else {
    y$value <- y[[value.col]]
  }

  p <- ggplot(y, aes(x = date, y = value, color = symbol))

  if (layout == 'line.single') {
    p <- p + geom_line(size=0.2, aes(group = symbol))
  } else if (layout == 'smooth.single') {
    p <- p + stat_smooth(alpha=0.1, size=0.2,aes(fill=symbol))
  } else if (layout == 'line.facet') {
    p <- p + geom_line(aes(group = symbol)) + facet_grid(symbol ~ ., scale = 'free_y')
  } else if (layout == 'point.smooth.facet') {
    p <- p + geom_smooth(size=0.2, aes(group = symbol)) + geom_point(alpha=0.2, size=1) + facet_grid(symbol ~ ., scale = 'free_y')
  }

  p
}


## yfin.report <- function(root.dir = 'data',
##                         symbols.list = yfin.standard.symbol.groups,
##                         update.symbols.list = yfin.standard.symbols,
##                         time.intervals = list('03-months' = 90, '01-year' = 260),
##                         log.level = SimpleLog.INFO,
##                         file.name = 'historical-data',
##                         .parallel = FALSE){
##   ## import('yahoofin'); registerDoMC(2); out <- yfin.report() ;system(sprintf('open %s && open `ls %s/plots/*.png`', out, out))
##   logger <- SimpleLog('yfin.download', log.level)

##   ## get data
##   dir.create(root.dir)
##   archive.path <- file.path(root.dir, sprintf('%s.rda', file.name))
##   write.msg(logger, 'archive to %s', archive.path)
##   x <- yfin.archive(update.symbols.list, archive.path, log.level = log.level, .parallel=.parallel)

##   ## calculate returns
##   write.msg(logger, 'calculating returns')
##   x$ret <- tapply(x$adj.close, x$symbol, compute.returns, ret.type = 'par')

##   ## create output dir
##   today <- max(x$date)
##   write.msg(logger, 'most recent data from %s', today)
##   output.dir <- file.path(root.dir, today, 'plots')

##   if(!file.exists(file.path(root.dir, today, '_SUCCESS'))){
##     write.msg(logger,'saving plots to %s', output.dir)
##     dir.create(output.dir, recursive = TRUE)

##     ## market and sector plots
##     foreach(s = names(symbols.list)) %dopar% {
##       foreach(t = names(time.intervals)) %dopar% {
##         symbols <- symbols.list[[s]]
##         time.interval <- time.intervals[[t]]
##         ## value
##         ggsave(ggplot(subset(x, date > (today - time.interval) & symbol %in% symbols), aes(x = date, y = adj.close, color = symbol)) + geom_line() + geom_smooth() + facet_grid(symbol ~ ., scale = 'free_y'), file = file.path(output.dir,sprintf('%s-value-past-%s.png',s,t)))
##         ## returns
##         ggsave(ggplot(subset(x, date > (today - time.interval) & symbol %in% symbols), aes(x = date, y = ret)) + geom_line(aes(color = symbol)) + geom_smooth() + geom_hline(size=0.2,aes(yintercept=0)) + facet_grid(symbol ~ ., scale = 'free_y'), file = file.path(output.dir,sprintf('%s-return-past-%s.png',s,t)))
##       }
##     }
##   } else {
##     write.msg(logger,'%s already exists', output.dir)
##   }

##   file.create(file.path(root.dir,today,'_SUCCESS'))

##   file.path(root.dir, today)
## }

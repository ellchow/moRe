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
       'ggplot2',
       'reshape2'
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
    market = c('^FTSE','^GSPC','^HSI','^MID','^SML','VB','^VIX'),
    bond = c('AGG','BIV','BLV','BND','BSV','EDV'),
    other = c('GLD','SLV','IAU'),
    sector = c('VCR','VDC','VDE','VEU','VFH','VGK','VGT','VHT','VIS','VNQ','VOX','VPL','VPU','VSS','VTI','VUG','VWO','VXF','VAW'),
    tech=c('AAPL','GOOG','FB','EBAY','AMZN','YHOO','MSFT','LNKD','TSLA','TRLA','YELP','Z','NFLX','ORCL','TDC','IBM','HPQ','INTC','AMD','NVDA','SSNLF'),
    consumer=c('PG','JNJ','PEP','KO','WMT','TGT','KSS','K','M'),
    mutual.funds=c(
        'VLCS', # large cap
        'ARTQX', # mid cap
        'MFLLX', # small cap
        'VBTIX', # total bond

        'VBMPX', # total bond
        'VWNAX', # windsor admiral
        'NSCRX', # small cap
        'VTPSX' # total intl stock
    )
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
                         start.date = as.Date('1900-01-01', yfin.date.fmt),
                         end.date = format(Sys.time(), yfin.date.fmt),
                         .parallel = FALSE) {
  logger <- SimpleLog('yfin.archive', log.level)
  current.timestamp <- format(Sys.time(), '%Y%m%d%H%M%S')
  cache.path <- paste('.yfin-download', current.timestamp, sep='-')
  dir.create(cache.path, recursive = TRUE)
  write.msg(logger, 'temporary data directory %s', cache.path)

  y <- llply(symbols,
             function(symbol){
               x <- yfin.download(symbol, ..., start.date = as.character(start.date), end.date = as.character(end.date), frequency = frequency, cache.path = cache.path, log.level = log.level)
               if(!is.na(x) && nrow(x) > 0) x else NA
             },
             .parallel = .parallel)
  y <- rbind.fill %wargs% y[!is.na(y)]

  write.msg(logger, '%d rows downloaded', nrow(y))
  if (file.exists(path)) {
    backup.path <- paste(path, 'bak', current.timestamp, sep='.')
    write.msg(logger, 'backup previous data to %s', backup.path)
    file.rename(path, backup.path)
  }

  y <- y[order(y$symbol, y$date),]
  write.msg(logger, 'saving to %s', path)
  save(y, file = path)
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

yfin.ggplot.values.and.returns <- function(x, symbols.list,
                                           start.date = end.date - 3 * yfin.num.days.in('m'),
                                           end.date = max(x$date[x$symbol %in% symbols.list]),
                                           normalize = TRUE,
                                           value.col = 'adj.close',
                                           as.return = FALSE,
                                           log.level = SimpleLog.INFO) {
  pv <- yfin.ggplot.symbol.values(x, symbols.list, normalize=normalize, start.date=start.date, end.date=end.date) + geom_hline(yintercept=1,size=0.2) + geom_line(size=0.5) ## + facet_grid(symbol ~ ., )
  pr <- yfin.ggplot.symbol.values(x, symbols.list, as.return=T, start.date=start.date, end.date=end.date) + geom_hline(yintercept=0,size=0.2) + geom_smooth(size=0.5) + geom_line(alpha=0.5,size=0.2) ## + facet_grid(symbol ~ ., )

  multi.plot(pv,pr,nrow=2,ncol=1)
}

yfin.portfolio.return <- function(x, allocation,
                                  start.date = end.date - 3 * yfin.num.days.in('m'),
                                  end.date = max(x$date[x$symbol %in% symbols.list]),
                                  value.col = 'adj.close',
                                  return.col = '.return'
                                  ) {
  symbols.list <- names(allocation)
  allocation <- allocation / sum(allocation)
  y <- x[(as.character(x$symbol) %in% symbols.list) & !((x$date < start.date) | (x$date > end.date)), ]
  y <- drop.levels(y)
  y[[return.col]] <- tapply(y[[value.col]], y$symbol, compute.returns, ret.type = 'par')
  y <- y[!is.invalid(y[[return.col]]),]
  y <- yfin.wide.format(y, symbols.list, value.var = return.col)

  pr <- Reduce(function(a,b) a + b,
               lapply(names(allocation), function(s) {
                 allocation[[s]] * ifelse(is.na(y[[s]]), 0, y[[s]])
               }))

  data.frame(date = y$date, 'return' = pr)
}

yfin.portfolio.returns <- function(x,
                                   allocations,
                                   value.col = 'adj.close',
                                   return.col = '.return'
                                   ) {
  dates.ok <- unlist(lapply(lzip(allocations, tail(allocations,-1)), function(a) {
    (a[[1]]$start.date <= a[[1]]$end.date) &&
        (a[[2]]$start.date <= a[[2]]$end.date) &&
            (a[[2]]$start.date > a[[1]]$end.date)
  }))
  stop.if.not(all(dates.ok), 'invalid date ranges in allocations')
  y <- lapply(allocations, function(a) {
    yfin.portfolio.return(x, a$allocation, a$start.date, a$end.date, value.col, return.col)
  })

  csplat(rbind, y)
}

yfin.portfolio.value <- function(x,
                                  allocations,
                                  value.col = 'adj.close',
                                  return.col = '.return',
                                  symbol = 'portfolio',
                                  init = 1
                                  ) {
  r <- yfin.portfolio.returns(x,allocations,value.col,return.col)
  v <- compute.values(r[['return']], init = init)
  y <- data.frame(date = c(r$date[1] - 1, r$date))
  y[[value.col]] <- v
  y$symbol <- symbol
  y
}

yfin.portfolio.values <- function(x,
                                  portfolio.allocations,
                                  value.col = 'adj.close',
                                  return.col = '.return',
                                  init = 1,
                                  log.level = SimpleLog.INFO) {
  pvs <- lapply(names(portfolio.allocations),
                function(p) yfin.portfolio.value(x, portfolio.allocations[[p]], value.col = value.col, return.col = return.col, init = init, symbol = p))
  symbols.list <- unique(unlist(lapply(portfolio.allocations, function(a) unlist(lapply(a, function(b) names(b$allocation))))))
  start.date <- min(unlist(lapply(portfolio.allocations, function(a) unlist(lapply(a, function(b) b$start.date)))))
  end.date <- max(unlist(lapply(portfolio.allocations, function(a) unlist(lapply(a, function(b) b$end.date)))))
  y <- x[(as.character(x$symbol) %in% symbols.list) & !((x$date < start.date) | (x$date > end.date)), ]
  y <- csplat(rbind.fill, c(list(y), pvs))
  y
}

yfin.ggplot.portfolio.values.with.components <- function(x,
                                                         portfolio.allocations,
                                                         value.col = 'adj.close',
                                                         return.col = '.return',
                                                         init = 1,
                                                         layout = '',
                                                         as.return = FALSE,
                                                         log.level = SimpleLog.INFO) {
  y <- yfin.portfolio.values(x, portfolio.allocations, value.col = value.col, return.col = return.col, init = 1)
  start.date <- min(unlist(lapply(portfolio.allocations, function(a) unlist(lapply(a, function(b) b$start.date)))))
  end.date <- max(unlist(lapply(portfolio.allocations, function(a) unlist(lapply(a, function(b) b$end.date)))))

  p <- yfin.ggplot.symbol.values(y, unique(y$symbol), start.date = start.date, end.date = end.date, normalize = TRUE, value.col = value.col, as.return = as.return, layout = layout)
  p
}

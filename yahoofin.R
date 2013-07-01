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
       'gdata',
       'stringr',
       'plyr',
       'doMC',
       'RSQLite'
       )

yfin.tags <- named(c('t8','m4','m3','k','j','w','c8','g3','a','b2','a5','a2','b','b3','b6','b4','c1','m5','m7','k4','j5','p2','c','k2','c6','c3','h','g','m','m2','w1','w4','r1','d','e','j4','e7','e9','e8','e1','q','f6','l2','g4','g1','g5','g6','v1','v7','d1','l1','k1','k3','t1','l','l3','j1','j3','i','n','n4','o','i5','r5','r','r2','k5','m6','m8','j6','p','p6','r6','r7','p1','p5','s1','s7','x','s','t7','d2','t6','v'),
                   c('1yrtargetprice','200daymovingaverage','50daymovingaverage','52weekhigh','52weeklow','52weekrange','afterhourschangerealtime','annualizedgain','ask','askrealtime','asksize','averagedailyvolume','bid','bidrealtime','bidsize','bookvalue','change','changefrom200daymovingaverage','changefrom50daymovingaverage','changefrom52weekhigh','changefrom52weeklow','changeinpercent','changepercentchange','changepercentrealtime','changerealtime','commission','dayshigh','dayslow','daysrange','daysrangerealtime','daysvaluechange','daysvaluechangerealtime','dividendpaydate','dividendshare','earningsshare','ebitda','epsestimatecurrentyear','epsestimatenextquarter','epsestimatenextyear','errorindicationreturnedforsymbolchangedinvalid','exdividenddate','floatshares','highlimit','holdingsgain','holdingsgainpercent','holdingsgainpercentrealtime','holdingsgainrealtime','holdingsvalue','holdingsvaluerealtime','lasttradedate','lasttradepriceonly','lasttraderealtimewithtime','lasttradesize','lasttradetime','lasttradewithtime','lowlimit','marketcapitalization','marketcaprealtime','moreinfo','name','notes','open','orderbookrealtime','pegratio','peratio','peratiorealtime','percentchangefrom52weekhigh','percentchangefrom200daymovingaverage','percentchangefrom50daymovingaverage','percentchangefrom52weeklow','previousclose','pricebook','priceepsestimatecurrentyear','priceepsestimatenextyear','pricepaid','pricesales','sharesowned','shortratio','stockexchange','symbol','tickertrend','tradedate','tradelinks','volume'))

yfin.date.fmt <- '%Y-%m-%d'
yfin.header <- c('date', 'open', 'high', 'low', 'close', 'volume', 'adj.close')

yfin.monthly <- 'm'
yfin.weekly <- 'w'
yfin.daily <- 'd'

yfin.standard.symbols <- strsplit('AGG,BIV,BLV,BND,BSV,EDV,^FTSE,GLD,^GSPC,^HSI,IAU,^MID,SLV,^SML,VAW,VB,VCR,VDC,VDE,VEU,VFH,VGK,VGT,VHT,VIS,^VIX,VNQ,VOX,VPL,VPU,VSS,VTI,VUG,VWO,VXF',',')[[1]]

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

  x <- tryCatch(named(load.data(u, ..., sep=',', cache.path = cache.path, log.level = log.level), yfin.header),
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
                         log.level = SimpleLog.ERROR,
                         .parallel = FALSE){
  logger <- SimpleLog('yfin.archive', log.level)

  end.date <- format(Sys.time(), yfin.date.fmt)
  current.timestamp <- format(Sys.time(), '%Y%m%d%H%M%S')
  cache.path <- paste('.yfin-download', current.timestamp, sep='-')

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

  y <- csplat(rbind.fill, y[!is.na(y)])
  write.msg(logger, 'adding %d rows', nrow(y))

  if(any(is.na(z))){
    y
  }else{
    backup.path <- paste(path, 'bak', current.timestamp, sep='.')
    write.msg(logger, 'backup previous data to %s', backup.path)
    file.rename(path, backup.path)

    y <- rbind.fill(z,y)
  }
  y <- y[order(y$symbol, y$date),]

  write.msg(logger, 'saving to %s', path)
  save(y, file = path)
  rrmdir(cache.path)

  y
}

compute.returns <- function(z){
  zz <- cbind(tail(z$date,-1),
              as.data.frame(do.call(cbind, lapply(setdiff(names(z),'date'),
                                                  function(i) diff(z[[i]]) / head(z[[i]],1)
                                                  ))))
  names(zz) <- names(z)
  zz
}

compute.portfolio.returns <- function(z, allocation){
  total <- Reduce(function(x,y) x + y, allocation, init = 0)
  Reduce(function(x,y) x + y,
         lapply(lzip(names(allocation), allocation),
                function(a){
                  z[[a[[1]]]] * as.numeric(a[[2]]) / total
                }),
         init = 0
         )
}

compute.values <- function(z, init=1){
  cols <- setdiff(names(z),'date')
  values <- list(rep(init, length(cols)))
  for(i in 1:nrow(z)){
    values <- c(values, list((z[,cols][i,] + 1) * tail(values,1)[[1]]))
  }
  zz <- do.call(rbind,values)
  zz$date <- c(seq(z$date[1], y$date[1] - 31,by="-1 month")[2],
               z$date)
  zz
}


function(){
  ## get data
  root.dir <- '~/Documents/investments/data/'
  dir.create(root.dir)
  x <- yfin.archive(yfin.standard.symbols, file.path(root.dir, 'historical-data-standard.rda'), .parallel=T)

  ## calculate returns
  x$ret <- tapply(x$adj.close, x$symbol, function(y)  c(NA, diff(y)) / tail(y,-1), ret.type = 'par')

  ## create output dir
  today <- as.Date(format(Sys.time(),'%Y-%m-%d'))
  output.dir <- file.path(root.dir,format(Sys.time(),'plots-%Y-%m-%d'))
  dir.create(output.dir)

  ## market and sector plots
  symbols.list <- list(market=c('VTI','VB','VEU','BND','EDV','IAU'),
                       sector=c('VDC','VCR','VDE','VNQ','VGT'))
  for(s in names(symbols.list)){
    cat(sprintf('group: %s\n\n',s))
    symbols <- symbols.list[[s]]

    ggsave(ggplot(subset(x, date > (today - 365) & symbol %in% symbols), aes(x = date, y = adj.close, color = symbol)) + geom_line() + geom_smooth() + facet_grid(symbol ~ ., scale = 'free_x'), file = file.path(output.dir,sprintf('%s-value-past-01-year.png',s)))
    ggsave(ggplot(subset(x, date > (today - 90) & symbol %in% symbols), aes(x = date, y = adj.close, color = symbol)) + geom_line() + geom_smooth() + facet_grid(symbol ~ ., scale = 'free_y'), file = file.path(output.dir,sprintf('%s-value-past-03-months.png',s)))

    ggsave(ggplot(subset(x, date > (today - 365) & symbol %in% symbols), aes(x = date, y = ret)) + geom_line(aes(color = symbol)) + geom_smooth() + geom_hline(size=0.2,aes(yintercept=0)) + facet_grid(symbol ~ ., scale = 'free_y'), file = file.path(output.dir,sprintf('%s-return-past-01-year.png',s)))
    ggsave(ggplot(subset(x, date > (today - 90) & symbol %in% symbols), aes(x = date, y = ret)) + geom_line(aes(color = symbol)) + geom_smooth() + geom_hline(size=0.2,aes(yintercept=0)) + facet_grid(symbol ~ ., scale = 'free_y'), file = file.path(output.dir,sprintf('%s-return-past-03-months.png',s)))
  }

}

source('utils.R', chdir=TRUE)
dump <- sapply(c('gdata',
                 'stringr',
                 'plyr',
                 'doMC',
                 'hash',
                 get_parallel_library()$lib
                 ), better.library)

get_parallel_library()$activate()

YAHOO_TAGS <- hash(keys=c('1yrtargetprice','200daymovingaverage','50daymovingaverage','52weekhigh','52weeklow','52weekrange','afterhourschangerealtime','annualizedgain','ask','askrealtime','asksize','averagedailyvolume','bid','bidrealtime','bidsize','bookvalue','change','changefrom200daymovingaverage','changefrom50daymovingaverage','changefrom52weekhigh','changefrom52weeklow','changeinpercent','changepercentchange','changepercentrealtime','changerealtime','commission','dayshigh','dayslow','daysrange','daysrangerealtime','daysvaluechange','daysvaluechangerealtime','dividendpaydate','dividendshare','earningsshare','ebitda','epsestimatecurrentyear','epsestimatenextquarter','epsestimatenextyear','errorindicationreturnedforsymbolchangedinvalid','exdividenddate','floatshares','highlimit','holdingsgain','holdingsgainpercent','holdingsgainpercentrealtime','holdingsgainrealtime','holdingsvalue','holdingsvaluerealtime','lasttradedate','lasttradepriceonly','lasttraderealtimewithtime','lasttradesize','lasttradetime','lasttradewithtime','lowlimit','marketcapitalization','marketcaprealtime','moreinfo','name','notes','open','orderbookrealtime','pegratio','peratio','peratiorealtime','percebtchangefrom52weekhigh','percentchangefrom200daymovingaverage','percentchangefrom50daymovingaverage','percentchangefrom52weeklow','previousclose','pricebook','priceepsestimatecurrentyear','priceepsestimatenextyear','pricepaid','pricesales','sharesowned','shortratio','stockexchange','symbol','tickertrend','tradedate','tradelinks','volume'),
                   values=c('t8','m4','m3','k','j','w','c8','g3','a','b2','a5','a2','b','b3','b6','b4','c1','m5','m7','k4','j5','p2','c','k2','c6','c3','h','g','m','m2','w1','w4','r1','d','e','j4','e7','e9','e8','e1','q','f6','l2','g4','g1','g5','g6','v1','v7','d1','l1','k1','k3','t1','l','l3','j1','j3','i','n','n4','o','i5','r5','r','r2','k5','m6','m8','j6','p','p6','r6','r7','p1','p5','s1','s7','x','s','t7','d2','t6','v')
                   )

DATE_FORMAT <- '%Y-%m-%d'

YFIN_COLUMNS <- c('date', 'open', 'high', 'low', 'close', 'volume', 'adj_close')

yfin_make_url <- function(syms,
                          startDate=format(Sys.time(),DATE_FORMAT),
                          endDate=format(Sys.time(),DATE_FORMAT),
                          tags=names(YAHOO_TAGS),
                          freq='weekly'){
  s <- do.call(paste, c(as.list(syms), sep='+'))
  t <- do.call(paste, c(as.list(tags), sep=''))
  start <- strsplit(startDate, '-')[[1]]
  end <- strsplit(endDate, '-')[[1]]

  url <- NULL
  if(startDate < endDate){
    if(length(syms) == 1)
      url <- sprintf('http://ichart.yahoo.com/table.csv?s=%s&d=%s&e=%s&f=%s&g=%s&a=%s&b=%s&c=%s&ignore=.csv',
                     s,
                     as.integer(end[2])-1,
                     end[3],
                     end[1],
                     if('weekly' == freq[1]){'w'}
                     else if('monthly' == freq[1]){'m'}
                     else{'d'},
                     as.integer(start[2])-1,
                     start[3],
                     start[1]
                     )
  }else{
    url <- sprintf('http://finance.yahoo.com/d/quotes.csv?s=%s&f=%s', s, t)
  }
  url
}

get_historical_data <- function(...){
  z <- read.table(yfin_make_url(...),sep=',',comment.char='',quote='',header=T)
  names(z) <- tolower(names(z))
  z$date <- factor(z$date, levels=sort(levels(z$date)), ordered=TRUE)
  z
}

## yfin_archive('~/Documents/investments/data/tsv/yd.rda',strsplit('AGG,BIV,BLV,BND,BSV,^DJI,EDV,^FTSE,GLD,^GSPC,^HSI,IAU,^MID,SLV,^SML,VAW,VB,VCR,VDC,VDE,VEU,VFH,VGK,VGT,VHT,VIS,^VIX,VNQ,VOX,VPL,VPU,VSS,VTI,VUG,VWO,VXF',',')[[1]],freq='daily',asSingleTable='~/Documents/investments/data/tsv/yd_single.rda',verbose=TRUE) -> z
yfin_archive <- function(path,syms,startDate=format(Sys.time(),DATE_FORMAT),freq='weekly',update=TRUE, asSingleTable=NULL, .parallel=TRUE, verbose=FALSE){
  verbose <- if(verbose){'info'}else{NULL}
  log <- make_log('yfin_archive',level=verbose)

  archive <- if(file.exists(path)){
    log <- write_log(log, 'loading saved archive')
    get(load(path))
  }else{
    log <- write_log(log, 'creating new archive')
    list()
  }
  today <- format(Sys.time(),DATE_FORMAT)

  if(update){ syms <- union(syms, names(archive))}
  log <- write_log(log, 'getting data for %s', plop(paste,syms,sep=','))
  z <- llply(syms,
             function(s){
               if(s %in% names(archive)){
                 x <- archive[[s]]
                 startDate <- max(levels(x$date))
               }else{
                 x <- data.frame()
                 startDate <- '1900-01-01'
               }
               log <- write_log(log, 'update %s from %s to %s', s, startDate, today)
               if(startDate < today){
                 y <- get_historical_data(s,
                                          startDate=startDate,
                                          endDate=today,
                                          freq=freq)
                 x <- rbind(x,subset(y, as.character(date) > startDate))
               }else{
                 x
               }
               names(x) <- YFIN_COLUMNS
               x
             },
             .parallel=.parallel)
  names(z) <- syms
  log <- write_log(log, 'saving to %s', path)
  save(z, file=path)

  if(!is.null(asSingleTable)){
    log <- write_log(log, 'converting to single table', path)
    z <- yfin_archive_to_single_table(z)
    if(is.character(asSingleTable)){
      log <- write_log(log, 'saving to %s', asSingleTable)
      save(z, file=asSingleTable)
    }
  }
  z
}

yfin_archive_to_single_table <- function(z){
  Reduce(function(x,y) merge(x,y,by='date',all=TRUE) ,
         lapply(lzip(z,names(z)),
                function(x){
                  sym <- x[[2]]
                  d <- x[[1]]
                  i <- grep('date', names(d))
                  names(d)[-i] <- paste(sym,names(d)[-i],sep='_')
                  d
                }))
}

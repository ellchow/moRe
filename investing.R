source('utils.R', chdir=TRUE)
source('yahoofin.R', chdir=TRUE)

better.library('ggplot2',
               'reshape2'
               )

allSyms <- strsplit('AGG,BIV,BLV,BND,BSV,^DJI,EDV,^FTSE,GLD,^GSPC,^HSI,IAU,^MID,SLV,^SML,VAW,VB,VCR,VDC,VDE,VEU,VFH,VGK,VGT,VHT,VIS,^VIX,VNQ,VOX,VPL,VPU,VSS,VTI,VUG,VWO,VXF',',')[[1]]

yfin.archive('~/Documents/investments/data/yd.rda',allSyms,freq='daily',asSingleTable='~/Documents/investments/data/yd_single.rda',.parallel=TRUE) -> z

## plot returns
syms <- c('BIV','BLV','BND','BSV','VTI','EDV')
startDate <- '2011-01-01'
melt(compute.returns(single.table.valid.subset(z, syms)[,single.table.col.names(syms, 'adj_close')]),id.vars='date') -> y
y$pos <- y$value > 0
ggplot(subset(y, date > startDate), aes(date, value)) + geom_line(aes(color=variable)) + stat_smooth(span=0.05) + geom_hline(yintercept=0) + facet_grid(variable ~ .,scale='free_y')

## plot normalized values
## alloc <- list(c(VTI=15,VB=5,VEU=10),
              ## c(BND=10,EDV=10,IAU=15),
              ## c(VDC=10,VCR=0),
              ## c(VWO=5,VPL=5,VSS=0),
              ## c(VNQ=5,VGT=10,VDE=0,VIS=0))
alloc <- list(c(VTI=25,VB=10,VEU=10,EDV=15,IAU=20,  BND=0),
              c(VDC=10,VCR=10)
              ## c(VDC=0,VCR=0,VWO=0,VNQ=10,VGT=0,VDE=0,VIS=0)
              )
## alloc <- list(c(VTI=25,VB=10,VEU=10,EDV=15,IAU=20, BND=0))

## alloc <- list(c(VTI=15,EDV=10))

startDate <- '2012-01-01'
allocAll <- do.call(c,alloc)
print(sum(allocAll))
print(allocAll / sum(allocAll) * 22000)
syms <- names(allocAll)
compute.returns(single.table.valid.subset(z, syms)[,single.table.col.names(syms, 'adj_close')]) -> y
names(allocAll) <- paste(names(allocAll),'adj_close',sep='_')
allocReturns <- compute.portfolio.returns(y, allocAll / sum(allocAll))
y$portfolio_adj_close <- allocReturns
print(sapply(y,mean))
print(sapply(y,sd))
list(mean=mean(y$portfolio_adj_close),
     sd=sd(y$portfolio_adj_close)) -> pstat
melt(compute.values(subset(y,date >= startDate & date <= max(date))),id.vars='date') -> y
ggplot(y, aes(date, value, color=variable, group=variable)) + stat_smooth(span=0.5)
## ggplot(y, aes(date, value, color=variable, group=variable)) + geom_line(alpha=0.4,aes(size=factor(as.integer(variable=='portfolio_adj_close')*0.5 + .3))) + stat_smooth(span=0.5)

                                        #+ scale_color_brewer('Set1')
as.list(tapply(y$value,y$variable,function(zz) tail(zz,1)[[1]]))$portfolio_adj_close -> pstat$final_value
as.list(tapply(y$value,y$variable,summary))$portfolio_adj_close -> pstat$value_summary
pstat


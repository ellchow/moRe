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
       'math',
       'plyr',
       'doMC',
       'grid',
       'ggplot2')

#### basic plotting

save.plot <- function(...,file=NULL,size=c(1024,1024),plot.fun=plot,plot.format=png){
  plot.format(file,width=size[1],height=size[2])
  plot.fun(...)
  dev.off()
}

plot.mean.boot <- function(x,g=NULL,...){
  if(is.null(g)){
    y <- as.list(smean.cl.boot(x,...))
    y$Var1 <- factor('x')
    names(y) <- c('y.Mean','y.Lower','y.Upper','Var1')
  }else{
    y <- tapply(x,g,function(z) smean.cl.boot(z,...),as.df=T)
  }
  plot(y$Var1,y$y.Mean,ylim=c(min(y$y.Lower),max(y$y.Upper)))
  points(y$Var1,y$y.Upper,pch=25,col='blue')
  points(y$Var1,y$y.Lower,pch=24,col='blue')
}

plot.points.with.smoother <- function(x,y,
                                      ...,
                                      model.fit=loess,
                                      p.params=list(xlab='x',ylab='y'),
                                      u.params=list(col='blue'),
                                      l.params=list(col='blue'),
                                      a.params=list(col=rgb(0,100,0,50,maxColorValue=255),
                                        border=rgb(0,100,0,0,maxColorValue=255))
                                      ){
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]
  do.call(plot, c(list(x=x,y=y),p.params))
  sm <- predict(model.fit(y ~ x,...), se=T)
  sm$fit-2*sm$s -> lb
  sm$fit+2*sm$s ->ub
  do.call(polygon, c(list(c(x,x[length(x):1]),c(lb,ub[length(ub):1])),a.params))
  lines(x,sm$fit,col='red')
  do.call(lines,c(list(x=x,y=sm$fit-2*sm$s),l.params))
  do.call(lines,c(list(x=x,y=sm$fit+2*sm$s),u.params))
}

#### ggplot

save.ggplots <-function(plots,outputPath,ext='png',...,.parallel=FALSE){
  llply(plots,function(x){
    tryCatch(ggsave(filename=paste(outputPath,'/',x$name,'.',ext,sep=''),plot=x$plot,...),
             error=function(e){
               NA
             })
  }, .parallel=.parallel)
}

stat.sum.df <- function(fun = 'mean_cl_boot', geom='crossbar', colour='steelblue', width=0.4,...) {
  stat_summary(fun.data=fun, colour=colour, geom=geom, width=width, ...)
}

vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)

multi.plot <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
  dots <- list(...)
  n <- length(dots)

  if(is.null(nrow) & is.null(ncol)){
    nrow <- floor(n/2)
    ncol = ceiling(n/nrow)
  }
  if(is.null(nrow))
    nrow <- ceiling(n/ncol)
  if(is.null(ncol))
    ncol <- ceiling(n/nrow)

  ## NOTE see n2mfrow in grDevices for possible alternative
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow,ncol)))
  ii.p <- 1
  for(ii.row in seq(1, nrow)){
    ii.table.row <- ii.row
    if(as.table)
      ii.table.row <- nrow - ii.table.row + 1

    for(ii.col in seq(1, ncol)){
      ii.table <- ii.p

      if(ii.p > n)
        break

      print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))

      ii.p <- ii.p + 1
    }
  }
}

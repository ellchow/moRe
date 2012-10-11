#### Elliot Chow

source('utils.R', chdir=TRUE)
source('math.R', chdir=TRUE)
dump <- sapply(c('gdata',
                 'stringr',
                 'plyr',
                 'doMC',
                 'gbm',
                 'infotheo',
                 'rjson',
                 'Hmisc',
                 get.parallel.library()$lib
                 ), better.library)

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
                                      p.params=list(xlab='x',ylab='y'),
                                      u.params=list(col='blue'),
                                      l.params=list(col='blue')
                                      ){
  do.call(plot, c(list(x=x,y=y),p.params))
  sm <- predict(loess(y ~ x,...), se=T)
  ## sm$fit-2*sm$s -> lb
  ## sm$fit+2*sm$s -> ub
  ## polygon(c(x,x[length(x):1]),c(lb,ub[length(ub):1]),border='grey',col='grey')
  lines(x,sm$fit,col='red')
  do.call(lines,c(list(x=x,y=sm$fit-2*sm$s),l.params))
  do.call(lines,c(list(x=x,y=sm$fit+2*sm$s),u.params))
}

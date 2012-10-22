source('utils.R',chdir=T)
source('cmdargs.R', chdir=TRUE)
better.library('ggplot2','plyr','yaml')

get.parallel.library()$activate(1)

LOG <- SimpleLog('ggserve')

parsedArgs <- parse.args('ggserve.R',
                         list(),
                         commandArgs(T))
if(is.null(parsedArgs))
  stop("cannot initialize ggserve.R")
args <- parsedArgs$args


DATASETS <- new.env()

app <- URLMap$new(
                  'home'= function(env){
                    req <-Rook::Request$new(env)
                    res <- Rook::Response$new()
                    write.msg(LOG,'/home')

                    res$write('<h2>ggserve</h2>a simple web plotting app (ggplot2)<p>by: Elliot Chow<p>')
                    res$write(sprintf('<ul><li><a href="%s">get dataset info</a></li></ul>',
                                      req$to_url(file.path('/ls'))))
                    res$finish()
                  },
                  'ls'=function(env){
                    req <-Rook::Request$new(env)
                    res <- Rook::Response$new()
                    query <- req$GET()
                    write.msg(LOG,'/ls')

                    if(!is.null(query$path))
                      res$write(csplat(paste,system(sprintf("ls -lh %s", query$path),intern=T),sep='<p>'))
                    else
                      res$write("no path supplied")

                    res$finish()
                  },
                  'summarize'=function(env){
                    req <-Rook::Request$new(env)
                    res <- Rook::Response$new()
                    query <- req$GET()
                    write.msg(LOG,'/summarize')

                    if(!is.null(query$path)){
                      data <- get.dataset(query$path,
                                          get.or.else(query,'type','data.frame'))
                      if(!is.null(data))
                        res$write(eval(parse(text=get.or.else(query,'fun','function(z) dataframe.to.html.table(z[1:3,])')))(data))
                      else
                        res$write(sprintf("could not load dataset %s (%s)",
                                          query$path,get.or.else(query,'type','data.frame')))
                    }
                    res$finish()
                  },
                  'plot'=function(env){
                    req <-Rook::Request$new(env)
                    res <- Rook::Response$new()
                    query <- req$GET()
                    write.msg(LOG,'/plot')
                    if(all(!is.null(c(query$path,query$gg)))){
                      data <- get.dataset(query$path,
                                          get.or.else(query,'type','data.frame'))
                      if(!is.null(data)){
                        res$header('Content-type','image/png')
                        output <- paste(tempfile(),'png',sep='.')
                        write.msg(LOG,'ggplot command: %s',query$gg)
                        pl <- eval(parse(text=query$gg))
                        ggsave(pl,
                               file=output,
                               height=as.integer(get.or.else(query,'height','6')),
                               width=as.integer(get.or.else(query,'width','6')),
                               dpi=as.integer(get.or.else(query,'dpi','200')),
                               )
                        res$body <- readBin(output,'raw',n=file.info(output)$size)
                        unlink(output)
                      }else{
                        res$write(sprintf("could not load dataset %s (%s)",
                                          query$path,get.or.else(query,'type','data.frame')))
                      }
                    }else{
                      res$write("path and gg parameters required")

                    }
                    res$finish()
                  }
                )

######################################
get.dataset <- function(path,type='data.frame',...,max.n.datasets=2){
  ds <- ls(DATASETS)
  i <- length(ds)
      write.msg(LOG,'%d dataset(s) in buffer', i)
  if(!path %in% ds){
    if(i >= max.n.datasets){
      x <- ds[which.min(sapply(ds,function(d) d$i))]
      write.msg(LOG,'dropping dataset %s from buffer', x)
      DATASETS[[x]] <- NULL
    }
    if(type == 'data.frame'){
      write.msg(LOG,'loading %s',path)
      DATASETS[[path]] <- tryCatch(list(data=load.data(path),i=i),
                                   error=function(e){
                                     write.msg(LOG,'failed to load %s',path,level='error')
                                     list()
                                   })
    }else{
      write.msg(LOG,'unknown dataset type "%s"',type,msgLevel='error')
      list()
    }
  }
  DATASETS[[path]]$data
}



# http://localhost:9080/custom/ggserve.R/plot?path=/home/elliot/tmp/iris.rda&gg=ggplot%28data,aes%28x=Petal.Length,y=Sepal.Width%29%29%252Bgeom_point%28%29

source('utils.R')
better.library('RJDBC','brew','stringr')

#options(java.parameters = "-Xmx4g")

SAMPLE_DB_CONFIG <- list(class='com.teradata.jdbc.TeraDriver',
                         protocol='jdbc:teradata',
                         jar='/usr/lib/java/terajdbc4.jar',
                         type='teradata'
                         )

connect.to.db <- function(host,jdbc.config,...,log.level=c('info','warning','error'),log.output=stderr()){
  driver <- JDBC(jdbc.config$class,jdbc.config$jar)
  uri <- paste(jdbc.config$protocol,host,sep="://")
  conn <- dbConnect(driver, uri, ...)
  type <- jdbc.config$type
  logger <- SimpleLog('jdbc.connect',level=log.level,outputs=log.output)
  timer <- Timer(logger)
  list(driver=driver,
       connection=conn,
       type=jdbc.config$type,

       get.query=function(s,...,pretty=TRUE){
         write.msg(logger,'on %s, execute query and get:\n%s',uri,if(pretty) pprint.sql(s) else s)
         start.timer(timer)
         z <- dbGetQuery(conn, s, ...)
         stop.timer(timer)
         z
       },
       exec.query=function(s,...){
         write.msg(logger,'on %s, execute query:\n%s',uri,if(pretty) pprint.sql(s) else s)
         start.timer(timer)
         z <- dbSendQuery(conn, s, ...)
         stop.timer(timer)
         z
       },
       exec.update=function(s,...,pretty=TRUE){
         write.msg(logger,'on %s, execute update:\n%s',uri,if(pretty) pprint.sql(s) else s)
         start.timer(timer)
         z <- dbSendUpdate(conn,s,...)
         stop.timer(timer)
         z
       },

       list.tables=function(...,pretty=TRUE){
         write.msg(logger,'on %s, list tables',uri)
         start.timer(timer)
         z <- dbListTables(conn,...)
         stop.timer(timer)
         z
       },
       list.fields=function(s,...,pretty=TRUE){
         write.msg(logger,'on %s, list fields on %s',uri,if(pretty) pprint.sql(s) else s)
         start.timer(timer)
         z <- dbListFields(conn,s,...)
         stop.timer(timer)
         z
       },

       exists.table=function(s,...,pretty=TRUE){
         write.msg(logger,'on %s, check if table %s exists',uri,if(pretty) pprint.sql(s) else s)
         start.timer(timer)
         z <- dbExistsTable(conn,s,...)
         stop.timer(timer)
         z
       },
       read.table=function(s,...,pretty=TRUE){
         write.msg(logger,'on %s, read table %s',uri,if(pretty) pprint.sql(s) else s)
         start.timer(timer)
         z <- dbReadTable(conn,s,...)
         stop.timer(timer)
         z
       },
       write.table=function(s,...,pretty=TRUE){
         write.msg(logger,'on %s, write table %s',uri,if(pretty) pprint.sql(s) else s)
         start.timer(timer)
         z <- dbWriteTable(conn,s,...)
         stop.timer(timer)
         z
       },
       remove.table=function(s,...,pretty=TRUE){
         write.msg(logger,'on %s, remove table %s',uri,if(pretty) pprint.sql(s) else s)
         start.timer(timer)
         z <- dbRemoveTable(conn,s,...)
         stop.timer(timer)
         z
       },

       explain=function(s,...,pretty=TRUE){
         write.msg(logger,'on %s, explain SQL:\n %s',uri,if(pretty) pprint.sql(s) else s)
         start.timer(timer)
         cat(pprint.dataframe(dbGetQuery(conn,paste('explain ',if(pretty) pprint.sql(s) else s),...)))
         stop.timer(timer)
       },

       nrows=function(t,...){
         write.msg(logger,'on %s, number of rows in %s',uri,t)
         start.timer(timer)
         z <- dbGetQuery(conn, sprintf('select count(*) from %s',t), ...)
         stop.timer(timer)
         z
       }
       )
}


pprint.sql <- function(s){
  tmp <- tempfile()
  fmt <- sprintf('%s.fmt',tmp)
  cat(s,file=tmp)
  system(sprintf("python -c \"import sqlparse; s = open('%s').read(); print sqlparse.format(s,reindent=True,keyword_case='upper')\" > %s",tmp,fmt))
  z <- file.to.string(fmt)
  file.remove(tmp)
  file.remove(fmt)
  z
}

sql.mk.tmp.table <- function(...,db.type='teradata'){
  if(db.type == 'teradata'){
    str.fmt('create volatile table %(name)s as (%(body)s) with data primary index (%(index.attr)s) on commit preserve rows',...)
  }else{
    stop(str.fmt("unknown database '%s'",db.type))
  }
}

sql.contains <- function(e,set){
  sprintf('%s in (%s)',e,csplat(paste,set,sep=','))
}

sql.sample <- function(s,n,db.type='teradata'){
  if(db.type == 'teradata'){
    sprintf('%s\n sample %s', s, as.character(n))
  }else{
    stop(str.fmt("unknown database '%s'",db.type))
  }
}

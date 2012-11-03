source('utils.R')
better.library('RJDBC','brew','stringr')

#options(java.parameters = "-Xmx4g")

SAMPLE_DB_CONFIG <- list(class='com.teradata.jdbc.TeraDriver',
                         protocol='jdbc:teradata',
                         jar='/usr/lib/java/terajdbc4.jar',
                         type='teradata'
                         )

SQL_SYNTAX <- list(teradata=
                   list('tmp.table'='create volatile table <% cat(name) %> as (<% cat(body) %>) with data primary index (<% cat(indexattr) %>) on commit preserve rows'
                        )
                   )
connect.to.db <- function(host,jdbc.config,...,log.level=c('info','warning','error')){
  driver <- JDBC(jdbc.config$class,jdbc.config$jar)
  uri <- paste(jdbc.config$protocol,host,sep="://")
  conn <- dbConnect(driver, uri, ...)
  type <- jdbc.config$type
  logger <- SimpleLog('jdbc.connect',log.level)
  timer <- Timer(logger)
  list(driver=driver,
       connection=conn,
       type=jdbc.config$type,

       get.query=function(s,...){
         write.msg(logger,'on %s, execute query and get:\n%s',uri,s)
         start.timer(timer)
         dbGetQuery(conn, s, ...)
         stop.timer(timer)
       },
       exec.query=function(s,...){
         write.msg(logger,'on %s, execute query:\n%s',uri,s)
         start.timer(timer)
         dbSendQuery(conn, s, ...)
         stop.timer(timer)
       },
       exec.update=function(s,...){
         write.msg(logger,'on %s, execute update:\n%s',uri,s)
         start.timer(timer)
         dbSendUpdate(conn,s,...)
         stop.timer(timer)
       },

       list.tables=function(...){
         write.msg(logger,'on %s, list tables',uri)
         start.timer(timer)
         dbListTables(conn,...)
         stop.timer(timer)
       },
       list.fields=function(s,...){
         write.msg(logger,'on %s, list fields on %s',uri,s)
         start.timer(timer)
         dbListFields(conn,s,...)
         stop.timer(timer)
       },

       exists.table=function(s,...){
         write.msg(logger,'on %s, check if table %s exists',uri,s)
         start.timer(timer)
         dbExistsTable(conn,s,...)
         stop.timer(timer)
       },
       read.table=function(s,...){
         write.msg(logger,'on %s, read table %s',uri,s)
         start.timer(timer)
         dbReadTable(conn,s,...)
       },
       write.table=function(s,...){
         write.msg(logger,'on %s, write table %s',uri,s)
         start.timer(timer)
         dbWriteTable(conn,s,...)
         stop.timer(timer)
       },
       remove.table=function(s,...){
         write.msg(logger,'on %s, remove table %s',uri,s)
         start.timer(timer)
         dbRemoveTable(conn,s,...)
         stop.timer(timer)
       },

       explain=function(s,...){
         write.msg(logger,'on %s, explain SQL:\n %s',uri,s)
         start.timer(timer)
         cat(pprint.dataframe(dbGetQuery(conn,paste('explain ',s),...)))
         stop.timer(timer)
       },

       tmp.table=function(name,s,indexattr) brew.string(SQL_SYNTAX[[type]]$tmp.table,name=name,body=s,indexattr=indexattr)
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

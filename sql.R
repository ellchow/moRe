source('utils.R')
better.library('RJDBC','brew','stringr')

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
  list(driver=driver,
       connection=conn,
       type=jdbc.config$type,

       get.query=function(s,...){
         write.msg(logger,'on %s, execute query and get:\n%s',uri,s)
         dbGetQuery(conn, s, ...)
       },
       exec.query=function(s,...){
         write.msg(logger,'on %s, execute query:\n%s',uri,s)
         dbSendQuery(conn, s, ...)
       },
       exec.update=function(s,...){
         write.msg(logger,'on %s, execute update:\n%s',uri,s)
         dbSendUpdate(conn,s,...)
       },

       list.tables=function(...){
         write.msg(logger,'on %s, list tables',uri)
         dbListTables(conn,...)
       },
       list.fields=function(s,...){
         write.msg(logger,'on %s, list fields on %s',uri,s)
         dbListFields(conn,s,...)
       },

       exists.table=function(s,...){
         write.msg(logger,'on %s, check if table %s exists',uri,s)
         dbExistsTable(conn,s,...)
       },
       read.table=function(s,...){
         write.msg(logger,'on %s, read table %s',uri,s)
         dbReadTable(conn,s,...)
       },
       write.table=function(s,...){
         write.msg(logger,'on %s, write table %s',uri,s)
         dbWriteTable(conn,s,...)
       },
       remove.table=function(s,...){
         write.msg(logger,'on %s, remove table %s',uri,s)
         dbRemoveTable(conn,s,...)
       },

       explain=function(s){
         write.msg(logger,'on %s, explain SQL:\n EXPLAIN %s',uri,s)
         dbGetQuery(conn,s,...)
       },

       tmp.table=function(name,s,indexattr) brew.string(SQL_SYNTAX[[type]]$tmp.table,name=name,body=s,indexattr=indexattr)
       )
}

load.sql.fragments <- function(...,split.by='--\\*'){
  csplat(c,lapply(list(...),function(file){
    s <- file.to.string(file)
    as.list(csplat(c,lapply(str_split(s,split.by)[[1]],
                            function(x){
                              if(str_length(x) == 0) NULL
                              else{
                                i <- str_locate(x,'\n')[1]
                                z <- list(function(...) brew.string(str_trim(str_sub(x,i+1)[[1]]),...))
                                names(z) <- str_trim(str_sub(x,1,i-1)[[1]])
                                z
                              }
                            })))
  }))
}

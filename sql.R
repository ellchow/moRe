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
connect.to.db <- function(host,jdbc.config,...){
  driver <- JDBC(jdbc.config$class,jdbc.config$jar)
  uri <- paste(jdbc.config$protocol,host,sep="://")
  conn <- dbConnect(driver, uri, ...)
  type <- jdbc.config$type
  list(driver=driver,
       connection=conn,
       type=jdbc.config$type,

       get.query=function(s,...) dbGetQuery(conn, s, ...),
       exec.query=function(s,...) dbSendQuery(conn, s, ...),
       exec.update=function(s,...) dbSendUpdate(conn,s,...),

       list.tables=function(...) dbListTables(conn,...),
       list.fields=function(s,...) dbListFields(conn,s,...),

       exists.table=function(s,...) dbExistsTable(conn,s,...),
       read.table=function(s,...) dbReadTable(conn,s,...),
       write.table=function(s,...) dbWriteTable(conn,s,...),
       remove.table=function(s,...) dbRemoveTable(conn,s,...),

       tmp.table=function(name,s,indexattr) brew.string(SQL_SYNTAX[[type]]$tmp.table,name=name,body=s,indexattr=indexattr)
       )
}

load.sql.fragments <- function(file,split.by='--\\*'){
  s <- file.to.string(file)
  as.list(csplat(c,lapply(str_split(s,split.by)[[1]],
         function(x){
           if(str_length(x) == 0) NULL
           else{
             i <- str_locate(x,'\n')[1]
             z <- str_sub(x,i+1)
             names(z) <- str_trim(str_sub(x,1,i-1)[[1]])
             z
           }
         })))
}

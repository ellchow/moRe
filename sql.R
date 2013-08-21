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
import('utils', 'RJDBC')

#options(java.parameters = "-Xmx4g")
#set JAVA_HOME environment variable (MAC -> export JAVA_HOME=$(/usr/libexec/java_home), ...)

TERADATA_JDBC_CONFIG <- list(class='com.teradata.jdbc.TeraDriver',
                         protocol='jdbc:teradata:',
                         jar='/usr/lib/java/terajdbc4.jar',
                         type='teradata'
                         )

MYSQL_JDBC_CONFIG <- list(class='com.mysql.jdbc.Driver',
                          protocol='jdbc:mysql',
                          jar='/usr/lib/java/mysql-connector-java-3.1.14-bin.jar',
                          type='mysql'
                          )

ORACLE_JDBC_CONFIG <- list(class='oracle.jdbc.OracleDriver',
                           protocol='jdbc:oracle:thin:@',
                           jar='/usr/lib/java/ojdbc6.jar',
                           type='mysql')

connect.to.db <- function(host,jdbc.config,...,log.level=c('info','warning','error'),log.output=stderr()){
  driver <- JDBC(jdbc.config$class,jdbc.config$jar)
  uri <- paste(jdbc.config$protocol,host,sep="//")
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
  sprintf('%s in (%s)',e, paste(set, collapse = ','))
}

sql.sample <- function(s,n,db.type='teradata'){
  if(db.type == 'teradata'){
    sprintf('%s\n sample %s', s, as.character(n))
  }else{
    stop(str.fmt("unknown database '%s'",db.type))
  }
}

sql.quote <- function(s,char='\''){
  sprintf('%s%s%s',char,s,char)
}


sql.seq.stmts <- function(stmts,table.gen=function(...) sql.mk.tmp.table(...), actions=rep('exec.update',length(stmts))){
  ## stmts of form list(list( args for table.gen  ))
  tmp.tables <- lapply(stmts,
         function(x){
           table.gen %wargs% x
         })
  function(db=NULL,...,delay=5,pretty=TRUE){
    no.action <- is.null(db)
    for(i in 1:length(tmp.tables)){
      if(i > 1 && !no.action) system(sprintf('sleep %ds',delay))
      if(!no.action){
        db[[actions[i]]](tmp.tables[[i]],pretty=pretty)
      }else{
        cat(if(pretty) pprint.sql(tmp.tables[[i]]) else tmp.tables[[i]])
      }
    }
  }
}


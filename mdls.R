source('utils.R', chdir=TRUE)
dump <- sapply(c('gdata',
                 'stringr',
                 'plyr',
                 'doMC',
                 'gbm',
                 get_parallel_library()$lib
                 ), better.library)

get_parallel_library()$activate()

preprocess_data <- function(x,log=NULL){x}

make_model_def <- function(id, target_gen, fit, features, params, predict){
  hash(id=id,
       target_gen=target_gen,
       fit=fit,
       features=features,
       params=params,
       predict=predict)
}

mdls_build <- function(datasets, modelDefs, log=NULL, .parallel=TRUE){
  models <- flatten(lapply(lzip(if(!is.null(names(datasets))){names(datasets)}else{1:length(datasets)},
                                datasets),
                           function(x){
                             dsId <- x[[1]]
                             data <- x[[2]]

                             if(typeof(data) == 'character'){
                               t0 <- timer_start(log,'loading dataset "%s"', dsId)
                               data <- get(load(data))
                               timer_stop(t0,log)
                             }else{
                               data <- as.data.frame(data)
                             }

                             write_log(log,'preprocessing data')
                             data <- preprocess_data(data,log)

                             write_log(log,'randomize data')
                             data <- data[sample(1:nrow(data)),]

                             t0 <- timer_start(log, 'train models on "%s"', dsId)
                             models <- flatten(llply(modelDefs,
                                                     function(md){
                                                       id <- sprintf('%s%s', md$id,
                                                                     if(length(datasets) > 1){
                                                                       sprintf("__%s", dsId)
                                                                     }else{""})
                                                       write_log(log, 'building target for "%s"', id)
                                                       t <- tryCatch(md$target_gen(data),
                                                                     error=function(e){
                                                                       write_log(log,str_trim(as.character(e)),
                                                                                 level='error')
                                                                       NA
                                                                     })
                                                       if(!any(is.na(t))){
                                                       write_log(log, 'training "%s"', id)
                                                         m <- tryCatch(do.call(md$fit,
                                                                               c(list(data[,md$features],
                                                                                      t),
                                                                                 md$params)),
                                                                       error=function(e){
                                                                         write_log(log,str_trim(as.character(e)),
                                                                                   level='error')
                                                                         NA
                                                                       })
                                                         if(!any(is.na(m))){
                                                           z <- list(hash(target=t,
                                                                          model=m,
                                                                          predict=md$predict,
                                                                          features=md$features))
                                                           names(z) <- id
                                                           return(z)
                                                         }
                                                       }
                                                       write_log(log,'could not train "%s" (skipped)', id,
                                                                 level='warning')

                                                       list()
                                                     },
                                                     .parallel=.parallel))
                             timer_stop(t0,log)
                             models
                           }))
  models
}

mdls_predict <- function(models, datasets, log=NULL){
  flatten(lapply(lzip(if(!is.null(names(datasets))){names(datasets)}else{sapply(1:length(datasets),int_to_char_seq)},
                      datasets),
                 function(x){
                   dsId <- x[[1]]
                   data <- x[[2]]
                   if(typeof(data) == 'character'){
                     t0 <- timer_start(log,'loading dataset "%s"', dsId)
                     data <- get(load(data))
                     timer_stop(t0,log)
                   }else{
                     data <- as.data.frame(data)
                   }
                   t0 <- timer_start(log,'computing predictions on "%s"', dsId)
                   z <- flatten(llply(lzip(names(models), models),
                                 function(x){
                                   id <- x[[1]]
                                   m <- x[[2]]$model
                                   features <- x[[2]]$features
                                   predict <- x[[2]]$predict
                                   if(is.null(x[[2]]$predictions)){
                                     x[[2]]$predictions <- hash()
                                   }

                                   write_log(log,'predicting with "%s"', id)
                                   pr <- predict(m, data[,features])
                                   x[[2]]$predictions[[as.character(dsId)]] <- pr
                                   z <- list(x[[2]])
                                   names(z) <- id
                                   z
                                 }))
                   timer_stop(t0, log)
                   z
                 })
          )
}

mdls_evaluate <- function(){
}







#####################################
#### gbm modifications
#####################################

better.predict.gbm <- function(object,newdata,n.trees=NULL,type='response',...){
  trees = if(is.null(n.trees)) gbm.perf(object,method='test',plot.it=FALSE) else n.trees
  predict.gbm(object,newdata,n.trees=trees,type=type,...)
}




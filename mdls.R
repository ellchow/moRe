source('utils.R', chdir=TRUE)
dump <- sapply(c('gdata',
                 'stringr',
                 'plyr',
                 'doMC',
                 'gbm',
                 get_parallel_library()$lib
                 ), better.library)

get_parallel_library()$activate()

##############################################
#### Model Building
##############################################

preprocess_data <- function(x,log=NULL){x}

make_model_def <- function(id, target_gen, fit, features, params, predict, check=function(md,d,l){list()}){
  list(id=id,
       target_gen=target_gen,
       fit=fit,
       features=features,
       params=params,
       predict=predict,
       check=check)
}

model_def_properties <- function(){
  names(make_model_def(NA,NA,NA,NA,NA,NA))
}

is_model_def <- function(md){
  x <- setdiff(union(names(md),model_def_properties()),
               intersect(names(md),model_def_properties()))
  (length(x)==0)
}

mdls_build <- function(datasets, modelDefs, log=NULL, .parallel=TRUE){
  datasets <- if(is.data.frame(datasets))(list(datasets))else{datasets}
  modelDefs <- if(is_model_def(modelDefs)){list(modelDefs)}else{modelDefs}

  flatten(lapply(lzip(if(!is.null(names(datasets))){names(datasets)}else{1:length(datasets)},
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
                                             problems <- md$check(md,t,data)
                                             if(length(problems) > 0){
                                               lapply(lzip(names(problems),problems),
                                                      function(p){
                                                        write_log(log,'problem with "%s" (%s)',
                                                                  id,
                                                                  p[[1]],
                                                                  if(is.na(p[[2]])){''}else{p[[2]]},
                                                                  level='warning')
                                                      })
                                             }else{
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
                                                 z <- list(list(target=t,
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
}

mdls_predict <- function(models, datasets, log=NULL){
  models <- if(!is.list(models)){list(models)}else{models}
  datasets <- if(is.data.frame(datasets)){list(datasets)}else{datasets}

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
                                          x[[2]]$predictions <- list()
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

#####################################
#### gbm modifications and helpers
#####################################

gbm_predict <- function(object,newdata,n.trees=NULL,type='response',...){
  trees = if(is.null(n.trees)) gbm.perf(object,method='test',plot.it=FALSE) else n.trees
  predict.gbm(object,newdata,n.trees=trees,type=type,...)
}

check_gbm_model_def <- function(modelDef, target, data){
  problems <- list()

  missing <- setdiff(modelDef$features, names(data))
  available <- setdiff(modelDef$features, missing)
  if(length(missing) != 0){problems$missing_factors <- missing}

  gt1024levels <- sapply(available,
                         function(f){is.factor(data[[f]]) && (length(levels(data[[f]])) > 1024)})
  if(any(gt1024levels)){problems$too_many_levels <- available[gt1024levels]}

  all_na <- sapply(available, function(f){all(is.na(data[[f]]))})
  if(any(all_na)){problems$all_na <- available[all_na]}

  monotonicity <- (('var.monotone' %in% names(modelDef$params)) &&
                   ((length(modelDef$params$var.monotone) != length(modelDef$features)) ||
                    !all(modelDef$params$var.monotone %in% (-1:1))))
  if(monotonicity){problems$monotonicity <- NA}

  invalid_target <- any(is.na(target)) || ((modelDef$params$distribution == 'bernoulli') && (!all(target %in% (0:1))))
  if(invalid_target){problems$invalid_target <- NA}

  problems
}


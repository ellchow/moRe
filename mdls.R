source('utils.R', chdir=TRUE)
dump <- sapply(c('gdata',
                 'stringr',
                 'plyr',
                 'doMC',
                 'gbm',
                 'infotheo',
                 get_parallel_library()$lib
                 ), better.library)

get_parallel_library()$activate()

##############################################
#### Model Building
##############################################

make_model_def <- function(id, target_gen, fit, features, predict, params=list(), check=function(md,d,l){list()}){
  list(id=id, # name of model
       target_gen=target_gen, # function that takes in a superset of the training data  and returns the target
       fit=fit, # function for fitting the model of the same form as gbm.fit
       features=features, # vector of feature names to be used by the model
       params=params, # extra parameters for the fitting function
       predict=predict, # function for computing a prediction of the same form as gbm.predict
       check=check # function that takes in a model definition, target, and data and checks if there are any issues
       )
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
                                                                     c(list(subset(data,select=md$features),
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
                                        pr <- predict(m, subset(data,select=features))
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

##############################################
#### Feature Selection
##############################################

pairwise_compare_vectors <- function(data, pairs, cmp=function(x,y){cor(x,y)}, .parallel=FALSE){
  z <- adply(pairs,2,
             function(i){
               i <- sort(i)
               data.frame(factor_a=i[1], factor_b=i[2], value=cmp(data[[i[1]]], data[[i[2]]]))
             },
             .parallel=.parallel)

  z[[1]] <- NULL
  z
}


interinfo_feature_selection_filter <- function(t,s,r){
  remaining <- names(r)
  scores <- sapply(remaining,
                   function(f){
                     z <- interinformation(cbind(s, t, r[[f]]))
                     z
                   }
                 )
  names(scores) <- remaining
  scores
}

cor_feature_selection_filter <- function(t,s,r){
  remaining <- names(r)
  scores <- sapply(remaining,
                   function(f){
                     z <- abs(cor(t, r[[f]])) - (if(ncol(s)==0){0}else{max(abs(cor(s, r[[f]])))})^2
                     z
                   }
                   )
  names(scores) <- remaining
  scores
}

forward_filter_feature_selection <- function(target, features, evaluate=interinfo_feature_selection_filter, choose_best=max, n=ncol(features)){
  feature_selection_by_filter(target, features, NULL, evaluate,
                              function(z, scores){
                                bestScore <- choose_best(scores)
                                best <- match(TRUE,scores == bestScore)
                                z$selected <- c(z$selected, names(scores[best]))
                                z$remaining <- z$remaining[-1 * best]
                                z$scores <- c(z$scores, bestScore)
                                z
                              },
                              n=n
                              )
}

feature_selection_by_filter <- function(target, features, initSelected, evaluate, update, n=ncol(features)){
  z <- list(selected = initSelected,
            remaining = setdiff(names(features), initSelected),
            scores = c(),
            complete_scores = list())
  for(i in 1:n){
    scores <- evaluate(target, subset(features,select=z$selected), subset(features,select=z$remaining))
    z <- update(z, scores)
    z$complete_scores <- c(z$complete_scores, list(scores))
  }
  z
}

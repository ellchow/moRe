#!/usr/bin/env Rscript

source('../import.R', chdir = TRUE)
import('utils','cmdargs','mdls')

configure <- function(){
#### define datasets
  training.datasets <- list(tds1 = 'some/training/dataset/01.RData',
                            tds2 = 'some/training/dataset/02.RData')

  evaluation.datasets <- list(eds1 = 'some/evaluation/dataset/01.RData',
                              eds2 = 'some/training/dataset/02.RData')

#### define models
  model.definitions <- list(m1 =
                            list(model.def = gbm.model.def('gbm1',
                                   function(x) as.integer(x$target > 0),
                                   c('feature1', 'feature2'),
                                   distribution = 'bernoulli',
                                   train.fraction = 0.8),
                                 training.datasets = c('tds1'),
                                 evaluation.datasets = c('eds1','eds2')
                                 ),
                            m2 =
                            list(model.def = lm.model.def('lm1',
                                   function(x) as.integer(x$target > 0),
                                   c('feature1', 'feature2'),
                                   link='binomial'),
                                 training.datasets = c('tds.*'),
                                 evaluation.datasets = c('eds2'))
                            )

#### define metrics
  metrics.definitions <- list(
                              precision = function(score, data){
                                pred <- as.integer(score > 0.5)
                                tpos <- as.integer(data$target > 0)
                                list(sum(pred * tpos) / sum(pred))
                              },
                              grouped = function(score, data){
                                r <- tapply(list(score, as.integer(data$target > 0)),
                                            data$group.id,
                                            function(x){
                                              s <- x[[1]]
                                              t <- x[[2]]
                                              min(rank(-s,ties.method='random')[t])
                                            })
                                list(r, smean.cl.boot(r))
                              })


#### make config object
  list(training.datasets = training.datasets,
       evaluation.datasets = evaluation.datasets,
       model.definitions = model.definitions,
       metrics.definitions = metrics.definitions)
}


main <- function(raw.args){
  args <- parse.args('run.R',
                     list(list(name = 'o',
                               desc = 'output directory',
                               required = T),
                          list(name = 'log.level',
                               desc = 'logging verbosity',
                               default = c('info','warn','error'),
                               parser = function(x) strsplit(x,',')[[1]])
                          ),
                     raw.args)
  logger <- SimpleLog('run.R',args$log.level)
  write.msg(logger, sprintf('command line arguments: "%s"', csplat(paste,raw.args)))

  if(file.exists(file.path(args$o,'config.RData'))){}

}


main(commandArgs(T))


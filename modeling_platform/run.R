#!/usr/bin/env Rscript

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


source('../import.R', chdir = TRUE)
import('utils','cmdargs','mdls', 'infor')

configure <- function(logger){
#### define datasets
  training.datasets <- list(tds1 = 'some/training/dataset/01.RData',
                            tds2 = 'some/training/dataset/02.RData')

  evaluation.datasets <- list(eds1 = 'some/evaluation/dataset/01.RData',
                              eds2 = 'some/training/dataset/02.RData')

#### define models
  model.definitions <- list(m1 =
                            list(
                                 model.def =
                                 gbm.model.def('gbm1',
                                   function(x) as.integer(x$target > 0),
                                   c('feature1', 'feature2'),
                                   distribution = 'bernoulli',
                                   train.fraction = 0.8),
                                 training.datasets = c('tds1'),
                                 evaluation.datasets = c('eds1','eds2')
                                 ),
                            m2 =
                            list(model.def =
                                 lm.model.def('lm1',
                                   function(x) as.integer(x$target > 0),
                                   c('feature1', 'feature2'),
                                   link='binomial'),
                                 training.datasets = c('tds.*'),
                                 evaluation.datasets = c('eds2'))
                            )

#### define metrics
  metrics.definitions <- list(
                              'eds.*' =
                              list(
                                   'raw' =
                                   list(
                                        metrics =
                                        list(
                                             squared.error =
                                             list(
                                                  f = function(score, data) (score - data$target)^2,
                                                  report = function(..., path){}
                                                  )
                                             )
                                        )
                                   ),
                              'eds2' =
                              list(
                                   'ranking' =
                                   list(
                                        preprocess =
                                        function(score, data){
                                          list(rnk = compute.ranks(score, data$group.id),
                                               group.id = data$group.id)
                                        },
                                        metrics =
                                        list(
                                             click.rank =
                                             list(
                                                  f = function(score, data, rnk, group.id){
                                                    cr <- compute.metric(rnk, data$clicked == 1, group.id, pos.rank())
                                                    list(mean.cl.boot(cr))
                                                  },
                                                  report = function(..., path) {}
                                                  )
                                             )
                                        )
                                   )
                              )

#### make config object
  list(training.datasets = training.datasets,
       evaluation.datasets = evaluation.datasets,
       model.definitions = model.definitions,
       metrics.definitions = metrics.definitions)
}

make.config.path <- function(root,filename='config.RData')
  file.path(root, filename)

main <- function(raw.args){
  args <- parse.args('run.R',
                     list(list(name = 'o',
                               desc = 'output directory',
                               required = T),
                          list(name = 'c',
                               desc = 'config path (default to <output directory>/config.RData)',
                               default = NULL),
                          list(name = 'sources',
                               desc = 'extra R files to source',
                               default = NULL,
                               parser = function(x) strsplit(x,',')[[1]]),
                          list(name='overwrite',
                               desc = 'overwrite output directory (force execution)',
                               flag = T),
                          list(name = 'log.level',
                               desc = 'logging verbosity',
                               default = c('info','warn','error'),
                               parser = function(x) strsplit(x,',')[[1]])
                          ),
                     raw.args)
  logger <- SimpleLog('run.R',args$log.level)
  timer <- Timer(logger)
  write.msg(logger, sprintf('command line arguments: %s', csplat(paste,raw.args)))

  ## source extra files
  sapply(args$sources,
         function(f){
           write.msg(logger, sprintf('sourcing R file: %s', f))
           source(f, chdir=T)
         })

  ## load/run config
  config.path <- if(is.null(args$c)) make.config.path(args$o) else args$c
  if(!args$overwrite && file.exists(config.path)){
    start.timer(timer,sprintf('loading configuration from %s', config.path))
    config <- load.data(config.path)
    stop.timer(timer)
  }else{
    write.msg(logger, 'creating configuration')
    config <- configure(logger)
    start.timer(timer,sprintf('saving configuration to %s', config.path))
    dir.create(dirname(config.path),recursive=T)
    save(config, file=config.path)
    stop.timer(timer)
  }

}


main(commandArgs(T))


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

import('shiny',
       'utils',
       'infor',
       'cmdargs',
       'mdls',
       'doMC')

#### parse command line args
raw.args <- commandArgs(T)
args <- parse.args('infor-vis.R',
                   list(
                        list(name='-config',
                             desc='path to config file',
                             default='config.R'),
                        list(name='-port',
                             desc='port number',
                             default=8100,
                             parser = as.integer),
                        list(name='-loglevel',
                             desc='log level',
                             default = SimpleLog.ERROR,
                             parser = function(s){
                               if(s == 'info') SimpleLog.INFO
                               else if (s == 'error') SimpleLog.ERROR
                               else if (s == 'debug') SimpleLog.DEBUG
                               else NA
                             })
                        ),
                   raw.args)

logger <- SimpleLog('infor-vis', args[['-loglevel']])
timer <- Timer(logger)

#### load config
.config <- list()
source(args[['-config']], chdir=T)

if(.config$parallel > 0)
  registerDoMC(.config$parallel)

## check config blocks present
required.config.blocks <- c('models', 'data', 'query', 'query.id', 'display')
missing.config.blocks <- required.config.blocks[!required.config.blocks %in% names(.config)]
stop.if.not(length(missing.config.blocks) == 0, sprintf('missing configuration blocks: %s', paste(missing.config.blocks, collapse=',')))

## check required columns, compute new columns, subset
.config$required.columns <- c(.config$query, .config$query.id)
missing.required.columns <- setdiff(.config$required.columns, names(.config$data))
stop.if.not(length(missing.required.columns) == 0, 'missing required columns %s', paste(missing.required.columns, collapse=','))

missing.display.columns <- setdiff(.config$display, names(.config$data))
write.msg(logger, 'missing columns for display %s', paste(missing.display.columns, collapse=','), level = 'warning')
for(col in missing.display.columns)
  .config$data[[col]] <- NA

## check features
.config$models <- Filter(function(m){
  missing.features <- setdiff(m$features, names(.config$data))
  can.evaluate <- length(missing.features) == 0

  if(!can.evaluate)
    write.msg(logger, 'model %s cannot be evaluated (missing features %s)', m$id, paste(missing.features, collapse=','), level='warning')

  can.evaluate
}, .config$models)

if(.config$drop){
  keep <- unique(c(.config$required.columns, .config$display, .config$display.compare, unlist(lapply(.config$models, function(m) m$features))))
  write.msg(logger, 'only keeping columns %s', paste(keep, collapse=','))
  .config$data <- subset(.config$data, select = keep)
}

.config$data[['[row.id]']] <- 1:nrow(.config$data)

gc()


#### start server
runApp('./', port=args[['-port']])

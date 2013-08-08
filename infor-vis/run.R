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
       'cmdargs',
       'yaml')

#### parse command line args
raw.args <- commandArgs(T)
args <- parse.args('run.R',
                   list(
                        list(name='-config',
                             desc='path to config file',
                             default='config.R')
                        ),
                   raw.args)

#### load/process configuration
source(args[['-config']],chdir=T)

## check config blocks present
required.config.blocks <- c('models', 'data', 'query', 'query.id', 'display')
missing.config.blocks <- required.config.blocks[!required.config.blocks %in% names(.config)]
stop.if.not(length(missing.config.blocks) == 0, sprintf('missing configuration blocks: %s', paste(missing.config.blocks, collapse=',')))

## load models

#### start server
runApp('./')

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

source('../../import.R', chdir=TRUE)
import('utils','cmdargs','shiny')


main <- function(raw.args){
  parsed <-parse.args('start.R',
                    list(
                         list(name='data',
                              desc='path to dataset'),
                         list(name='browser',
                              desc='open default browser',
                              flag=TRUE)
                         ),
                    raw.args)
  args <- parsed$args
  if(is.null(args))
    return(args$help)

  shiny::runApp(launch.browser=args$browser)
  return(TRUE)
}


main(commandArgs(TRUE))

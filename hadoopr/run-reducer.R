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

source('import.R')

## by convention, stdin is placed into variable .input
.input <- pipe('cat /dev/stdin', open='r')


.chunk.size <- 5000
.key.column <- 1

on.dataframe <- function(f){
  streaming.group.by.key(function(lines) f(read.table(textConnection(unlist(lines)), sep='\t', header=F)),
                         function(x) strsplit(x,'\t')[[.key.column]][1])(.input, .chunk.size)
}

## runs your mapper script in reducer.R
## output should be printed to stdout
source('reducer.R',echo=T)

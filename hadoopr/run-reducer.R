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
.output <- NULL

## function to produce output; overwrite in your script as desired
.produce.output <- function() cat(.output)

## runs your mapper script in mapper.R
sink(stderr())
source('reducer.R',echo=T)
sink()

## produce
.produce.output()

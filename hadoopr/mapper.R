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

tryCatch({
  .output <- read.table(.input,sep='\t',header=F,comment.char='',quote='')

  .output <- cbind(NEW='aaaa',.output)

  .produce.output <- function() write.table(.output,sep=',',row.names=F,quote=FALSE)

}, error=function(e) print(e))

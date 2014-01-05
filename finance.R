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

source('import.R',chdir=T)
import('utils', 'yahoofin', 'quadprog', 'reshape2')

x <- load.data('~/Documents/investments/data/historical-data-standard.rda')

stats <- yfin.stats(x)

A <- cbind(
  matrix(rep(1,stats$n), nr=stats$n),
  diag(stats$n)
)
b <- c(1, rep(0,stats$n))
r <- solve.QP(cov2cor(stats$cov), 100 * stats$mean, A, b, meq=1)
w <- round(r$solution,4) %named% stats$symbols
w %*% stats$mean

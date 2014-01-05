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

mpt.minvar.portfolio <- function(r, sigma, target.return, labels=names(r), return.qp = FALSE) {
  n <- length(r)
  A <- cbind(matrix(rep(1, n), nr=n), ## weights sum to 1
             matrix(r, nr=n), ## achieves target return
             diag(n) ## lower bounds of weights - currently, no shorting allowed
             )
  b <- c(1,
         target.return,
         rep(0,n)
         )

  y <- solve.QP(sigma, rep(0, n), A, b, meq=2)

  w <- round(y$solution,4) %named% labels
  result <- list(w = w,
                 mean = w %*% r,
                 sd= sqrt(t(w) %*% sigma %*% w)
                 )
  if(return.qp)
    result$qp <- y

  result
}

mpt.efficient.frontier <- function(r, sigma, labels=names(r)) {
  ws <- lapply(seq(max(min(stats$mean), 0), max(stats$mean), length=10), function(t) {
    tryCatch(mpt.minvar.portfolio(stats$mean, stats$cov, t, stats$symbols, return.qp = F), error=function(e) NA)
  })

  as.data.frame(csplat(rbind, lapply(na.rm(ws), function(x) c(x$w, mean=x$mean, sd = x$sd))))
}



duration <- 260
m <- 1
stats <- yfin.stats(x, start.date=max(x$date) - duration * (m + 1), end.date=max(x$date) - duration * m)
d <- mpt.efficient.frontier(stats$mean, stats$sigma)
ggplot(d, aes(260 * mean, sd)) + geom_point() + geom_line() + coord_flip()


####
## A <- cbind(
##            matrix(rep(1,stats$n), nr=stats$n),
##            diag(stats$n)
##            )
## b <- c(1, rep(0,stats$n))
## r <- solve.QP(cov2cor(stats$cov), 100 * stats$mean, A, b, meq=1)
## w <- round(r$solution,4) %named% stats$symbols
## w %*% stats$mean
####

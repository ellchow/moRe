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

mpt.efficient.frontier <- function(r, sigma, labels=names(r), target.returns=seq(max(min(r), 0), max(r), length=10)) {
  ws <- lapply(target.returns, function(t) {
    tryCatch(mpt.minvar.portfolio(r, sigma, t, labels, return.qp = F), error=function(e) NA)
  })
  as.data.frame(csplat(rbind, lapply(na.rm(ws), function(x) c(x$w, mean=x$mean, sd = x$sd))))
}

x <- load.data('~/Documents/investments/data/historical-data-standard.rda')

duration <- 180
ms <- 0:8
symbols <- c('BND','EDV','IAU','VAW','VB',
             'VCR','VDC','VDE','VEU','VFH',
             'VGK','VGT','VHT','VIS','VNQ',
             'VOX','VPL','VPU','VSS','VTI',
             'VUG','VWO','VXF')
target.returns <- yfin.stats(x, symbols, start.date=max(x$date) - duration * (max(ms) + 1), end.date=max(x$date) - duration * min(ms))$mean
target.returns <- c(0.1 / 260, seq(max(min(target.returns),0), max(target.returns), length=10))
selected <<- NA

d <- csplat(rbind,
       lapply(sort(ms,decreasing=T), function(m) {
         stdt <- max(x$date) - duration * (m + 1)
         eddt <- max(x$date) - duration * m
         stats <- yfin.stats(x, symbols, start.date=stdt, end.date=eddt)
         dd <- cbind(mpt.efficient.frontier(stats$mean, stats$cov, target.returns=target.returns), type='ef')

         baseline.70.30 <- ifelse(stats$symbols == 'VTI', 0.7, ifelse(stats$symbols == 'BND', 0.3, 0))
         baseline.70.30 <- as.data.frame(rbind(c(baseline.70.30, baseline.70.30 %*% stats$mean, sqrt(t(baseline.70.30) %*% stats$cov %*% baseline.70.30))))
         baseline.70.30 <- cbind(baseline.70.30, type='70-30') %named% names(dd)

         baseline.30.70 <- ifelse(stats$symbols == 'VTI', 0.3, ifelse(stats$symbols == 'BND', 0.7, 0))
         baseline.30.70 <- as.data.frame(rbind(c(baseline.30.70, baseline.30.70 %*% stats$mean, sqrt(t(baseline.30.70) %*% stats$cov %*% baseline.30.70))))
         baseline.30.70 <- cbind(baseline.30.70, type='30-70') %named% names(dd)

         baseline.selected <- NULL
         if (!any(is.na(selected))) {
           baseline.selected <- as.data.frame(rbind(c(selected, selected %*% stats$mean, sqrt(t(selected) %*% stats$cov %*% selected))))
           baseline.selected <- cbind(baseline.selected, type='selected') %named% names(dd)
         }

         selected <<- unlist(as.vector(dd[which.min(abs(dd$mean - 0.1 / 260)), stats$symbols]))

         dd <- rbind(dd, baseline.70.30,baseline.30.70,baseline.selected)

         cbind(date=rep(paste(stdt,eddt,sep=' : '), nrow(dd)), dd)
       }))
d$date <- factor(d$date, levels=sort(unique(d$date)), ordered=T)
d$type <- factor(d$type)
d[grep('^2013-07-07',as.character(d$date),value='logical') & (d$type != 'ef'),]
ggplot(d, aes(260 * mean, sqrt(260) * sd, group=type, color=type)) + geom_point() + geom_line() + facet_wrap(~ date) + coord_flip()







         ## if (!is.na(selected)) {
         ##   baseline.selected <- as.data.frame(rbind(c(selected, selected %*% stats$mean, sqrt(t(selected) %*% stats$cov %*% selected))))
         ##   baseline.selected <- cbind(selected, type='selected') %named% names(dd)
         ## }

         ## next.selected <- which.min(abs(dd$mean - 0.1))
         ## print(dd[next.selected,])
         ## selected <- dd[next.selected,]



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

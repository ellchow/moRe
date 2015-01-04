#!/usr/bin/env Rscript

import('cmdargs','yahoofin')
registerDoMC(4)
raw.args <- commandArgs(TRUE)
args <- parse.args('update-yfin-data.R',
                   list(
                       list(name = 'd',
                            desc = 'data directory',
                            default = 'data')
                   ),
                   raw.args)
invisible(yfin.archive(yfin.standard.symbols, sprintf('%s/historical-data.rda', args[['d']]), log.level = SimpleLog.INFO, .parallel=TRUE))

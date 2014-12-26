#!/usr/bin/env Rscript

import('yahoofin')
registerDoMC(2)
out <- yfin.report()
if ("openplots" %in% commandArgs(T)) {
    system(sprintf('open %s && open `ls %s/plots/*.png`', out, out))
}

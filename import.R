#!/usr/bin/env Rscript

do.call(function(x) source(x,chdir=TRUE),
        list('utils.R','cmdargs.R', 'mdls.R', 'yahoofin.R')
        )

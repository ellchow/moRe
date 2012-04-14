#!/usr/bin/env Rscript

lapply(list('utils.R',
            'cmdargs.R',
            'mdls.R',
            'math.R',
            'yahoofin.R'),
       function(x) source(x,chdir=TRUE))


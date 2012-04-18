#!/usr/bin/env Rscript

#### Elliot Chow

lapply(list('utils.R',
            'cmdargs.R',
            'mdls.R',
            'math.R',
            'yahoofin.R'),
       function(x) source(x,chdir=TRUE))


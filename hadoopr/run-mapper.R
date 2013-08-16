## by convention, stdin is placed into variable .input
.input <- pipe('cat /dev/stdin')
.output <- NULL

## function to produce output; overwrite in your script as desired
.produce.output <- function() cat(.output)

## runs your mapper script in mapper.R
sink(stderr())
source('mapper.R',echo=T)
sink()

## produce
.produce.output()

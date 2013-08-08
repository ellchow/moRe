

.config$models <- c('../models.rda')


.config$data <- load.table('../iris.rda')

.config$query <- 'q'

.config$query.id <- .config$query

.config$truth <- 'true.order'

.config$display <- list('a',
                        'b',
                        'a plus b' = list(c('a','b'), function(a, b) a + b)
                        )

.config$drop <- TRUE

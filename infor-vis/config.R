import('utils')

.config <- list()

.config$models <- c('/path/to/models1.rda',
                    '/path/to/models2.rda')

.config$data <- '/path/to/dataset.rda'

.config$query <- 'q'

.config$query.id <- .config$query

.config$display <- list('a',
                        'b',
                        'a plus b' = function(data) (a + b) %within% data
                        )






.config$models <- {
  mdls.fit(iris,
           lm.model.def('lmmodel', Sepal.Length,
                        c('Sepal.Width','Petal.Length','Petal.Width'))
           )
}

.config$data <- {
  z <- iris

  z$order.by.sepal.length <- compute.ranks(Sepal.Length, Species, envir=z)

  z
}

.config$query <- 'Species'

.config$query.id <- .config$query

.config$truth <- 'order.by.sepal.length'

.config$display <- c('Sepal.Width','Petal.Length','Petal.Width')

.config$display.limit <- Inf

.config$drop <- TRUE


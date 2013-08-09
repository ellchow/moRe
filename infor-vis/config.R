
#### list of models, as trained by mdls.fit
.config$models <- {
  mdls.fit(iris,
           lm.model.def('lmmodel', Sepal.Length,
                        c('Sepal.Width','Petal.Length','Petal.Width'))
           )
}


#### data.frame containing all queries
.config$data <- iris


#### query column
.config$query <- 'Species'

#### unique query id
.config$query.id <- .config$query

#### display columns
.config$display <- c('Sepal.Width','Petal.Length','Petal.Width')

#### display info on compare items
.config$display.compare <- c('Sepal.Length', 'Sepal.Width')

#### row/cell formatting

.config$row.format <- function(x, i){
  if(x[i,]$rank %% 2 == 0) '' else ''
}

#### max number of items to display per query
.config$display.limit <- Inf

#### drop columns upfront
.config$drop <- TRUE

#### parallelization
.config$parallel <- 3

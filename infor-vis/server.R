## Copyright 2013 Elliot Chow

## Licensed under the Apache License, Version 2.0 (the "License")
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at

## http://www.apache.org/licenses/LICENSE-2.0

## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.


import('shiny')

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

  select.model <- function(id)
    Filter(function(m) m$id == id, .config$models)[1]

  single.query <- reactive(function(){
    if(!is.null(input$query.id) && !is.null(input$sort.by) && !is.null(input$order.decr)){
      q <- .config$data[.config$data[[.config$query.id]] == input$query.id, ]

      sort.score <- NULL
      if(input$sort.by %in% names(q))
        sort.score <- q[[input$sort.by]]
      else{
        m <- select.model(input$sort.by)
        if(!is.na(m)){
          sort.score <- mdls.predict(m, q)[[1]][[1]]
        }
      }

      if(!is.null(sort.score)){
        q[['[sort.score]']] <- sort.score
        q <- head(cbind(rank=1:length(sort.score), q[order(sort.score, decreasing = input$order.decr == '1'), ]),
                  .config$display.limit)
      }
    }else
      NULL
  })

  output$results.table <- reactive(function(){
    q <- single.query()
    if(!is.null(q))
      dataframe.to.html.table(q, table.attrs = 'class="data table table-bordered table-condensed" style="color:#555555"',
                              prepend.row.names = NULL, .parallel=.config$parallel > 0)
  })





})


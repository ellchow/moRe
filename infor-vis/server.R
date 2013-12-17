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

shinyServer(function(input, output) {

  available.models <- unlist(lapply(.config$models, function(m) m$id))
  select.model <- function(id){
    i <- which(available.models == id)

    if(length(i) == 0) NA else .config$models[i]
  }


  single.query <- reactive({
    if(!is.null(input$query.id) && !is.null(input$sort.by) && !is.null(input$order.decr)){
      q <- .config$data[.config$data[[.config$query.id]] == input$query.id, ]

      sort.score <- NULL
      if(input$sort.by %in% names(q))
        sort.score <- q[[input$sort.by]]
      else{
        m <- select.model(input$sort.by)

        if(!is.na(m))
          sort.score <- mdls.predict(m, q)[[1]]$scores[[input$sort.by]]
      }

      if(!is.null(sort.score)){
        ord <- order(sort.score, decreasing = input$order.decr == '1')

        r <- vector('integer', length(ord))
        r[ord] <- indices(r)

        z <- cbind(rank=r,
                   ## '[Sort Score]' = sort.score,
                   q[, unique(.config$display)],
                   'Compare (Source)' = sprintf('<input type="radio" %s value="%s" name="compare.item.src">', '', q[['[row.id]']]), # ifelse(r == 2, 'checked="checked"', '')
                   'Compare (Sink)' = sprintf('<input type="radio" %s value="%s" name="compare.item.snk">', '', q[['[row.id]']]) # ifelse(r == 1, 'checked="checked"', '')
                   )[ord, ]

        head(z, .config$display.limit)
      }
    }else
    NULL
  })

  output$results.table <- reactive({
    q <- single.query()
    if(!is.null(q)){
      dataframe.to.html.table(q, table.attrs = 'class="data table table-bordered table-condensed" style="color:#555555"',
                              add.tr.attr = function(i) .config$row.format(q, i),
                              add.td.attr = function(i,j){
                                if(names(q)[j] %in% c('rank', 'Compare (Source)', 'Compare (Sink)'))
                                  align <- 'text-align:center'
                                else
                                  align <- 'text-align:right'

                                ## if(names(q)[j] == '[Sort Score]')
                                ##   highlight.sort <- 'background-color:#EEEEEE'
                                ## else
                                ##   highlight.sort <- ''

                                sprintf('style="%s"', paste(align, sep=';')) ## paste(highlight.sort, align, sep=';')
                              },
                              prepend.row.names = NULL, .parallel=.config$parallel > 0)
    }
  })

  output$compare.items <- reactive({
    m <- select.model(input$sort.by)
    if(input$tabs == 'compare'){
      if(is.na(m))
        '<span style="color:red">ERROR: cannot compare using fixed values - please select a model</span>'
      else if(is.null(input$compare.item.src) && is.null(input$compare.item.snk))
        '<span style="color:red">ERROR: please select source and sink for comparison</span>'
      else{
        x.1 <- .config$data[input$compare.item.src,]
        x.2 <- .config$data[input$compare.item.snk,]

        x <- cbind(t(x.1[.config$display.compare]), t(x.2[.config$display.compare]))

        colnames(x) <- c('Source', 'Sink')

        item.info <- dataframe.to.html.table(x, table.attrs = 'class="data table table-bordered table-condensed" style="color:#555555"')


          fc <- feature.contributions(m[[1]], x.1, x.2, .parallel = .config$parallel > 0)

        contribs <- dataframe.to.html.table(as.data.frame(lapply(fc, function(x) if(is.numeric(x)) signif(x, digits = .config$compare.signif) else x)),
                                            table.attrs = 'class="data table table-bordered table-condensed" style="color:#555555"',
                                            prepend.row.names = NULL)

        paste(item.info,contribs, sep='\n')
      }
    }


  })



})


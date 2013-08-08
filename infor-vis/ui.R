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

shinyUI(pageWithSidebar(
                        headerPanel("infor-vis"),

                        sidebarPanel(
                                     selectInput("query.id", "Query",
                                                 {
                                                   qs <- tapply(as.character(.config$data[[.config$query]]),
                                                                as.character(.config$data[[.config$query.id]]),
                                                                function(x) x[1])

                                                   named(names(qs), if(.config$query.id == .config$query) names(qs) else sprintf('%s (%s)', qs, names(qs)))
                                                 }),

                                     selectInput("sort.by", "Sort By:",
                                                 {
                                                   ms <- named(lapply(.config$models, function(m) m$id),
                                                               unlist(lapply(.config$models, function(m) m$id)))

                                                   cs <- named(names(.config$data), names(.config$data))

                                                   c(ms, cs)
                                                 }),

                                     radioButtons("order.decr", "Ordering:",
                                                  list('High-to-Low' = '1',
                                                       'Low-to-High' = '0'))
                                     ),

                        mainPanel(
                                  htmlOutput('main.panel.title'),

                                  tabsetPanel(id='tabs',
                                              tabPanel(
                                                       title='Results',
                                                       value='results',
                                                       htmlOutput('results.table')
                                                       )
                                              )
                                  )
                        )
        )


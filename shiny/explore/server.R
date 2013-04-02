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

source('../../import.R', chdir=TRUE)
import('utils', 'shiny')




# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

  # Function that generates a plot of the distribution. The function
  # is wrapped in a call to reactivePlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  #
  output$distPlot <- reactivePlot(function() {

    # generate an rnorm distribution and plot it
    dist <- rnorm(input$obs)
    hist(dist)
  })
})

source('../../import.R', chdir=TRUE)
import('utils', 'shiny', 'ggplot2')

# Define UI for application that plots random distributions
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Hello Shiny!"),

  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("obs",
                "Number of observations:",
                min = 0,
                max = 1000,
                value = 500)
  ),

  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot")
  )
))


## overview: column names, types, summaries ?
## single column stats/plots: quantiles, mean, stdev, bootstrapped, histogram/density
## paired plots/stats: scatterplots, covariance, qqplots, smoother

## subsetting, grouping/coloring/faceting,  plot settings, zooming


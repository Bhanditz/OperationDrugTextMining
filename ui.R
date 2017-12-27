#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Miles Per Gallon"),
  
  # Sidebar with a slider input for number of bins 
    sidebarPanel(selectInput("variable", "Variable:",
                             list("Cylinders" = "cyl",
                                  "Transmission" = "am",
                                  "Gears" = "gear")),
  
    checkboxInput("outliers", "Show outliers", FALSE)),
    # Show a plot of the generated distribution
    mainPanel(
      h3(textOutput("caption")),
      
      plotOutput("mpgPlot")
    )
))

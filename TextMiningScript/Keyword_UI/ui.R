
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Word Analytics"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("inputSql", 
                "Input Data SQL:",
                "Insert SQL!!"),
      textInput("mainTerm",
                "Main Term: (word1,word2,word3)"),
      
      textInput("stopTerm",
                "Stop Term: (word1,word2,word3)"),
      
      sliderInput("sparse",
                  "Sparse Ratio:",
                  min = 0.9,  max = 1, value = 0.99, step=0.001),
      
      sliderInput("corlimit",
                  "Cor Limit:",
                  min = 0,  max = 1, value = 0.2, step=0.01),
      
      
      submitButton("Execute")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        column(5,h4("Network Chart")),
        column(5,textOutput("visual"))
        #column(2,downloadButton('downloadData', 'Download'))
      ),
      
      plotOutput("network",height = 800)
    )
  )
))

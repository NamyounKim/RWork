
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(
  fluidPage(
  
  # Application title
  titlePanel("Word Analytics"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput("inputFile" # 객체명
                , "Choose CSV File" # 화면 표시 내용
                , accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                ),
      
      textInput("stopTerm" # 객체명
                , "Stop Term: (word1,word2,word3)" # 화면 표시 내용
                ),
      
      sliderInput("sparse"
                  , "Sparse Ratio:"
                  , min = 0.9
                  , max = 1
                  , value = 0.98
                  , step=0.005
                  ),
      
      sliderInput("corLimit"
                  ,"Cut-off point(cor):"
                  , min = 0.05
                  , max = 0.95
                  , value = 0.25
                  , step=0.05
                  )
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      verbatimTextOutput("displayRow"),
      plotOutput("networkPlot"
                 ,height = 800)
      )
    )
  )
)

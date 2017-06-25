# ui.R

library(shiny)
library(reshape)

shinyUI(
  pageWithSidebar(
  headerPanel('LDA Helper'),
  sidebarPanel(
    radioButtons('inputType', 'Input Type',
                 c(DB ='db',
                   CSV='csv')),
    
    textInput("inputSql", 
              "Input Data SQL:",
              "Insert SQL!!"),
    
    fileInput('inputFile', 'Choose CSV File',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    
    textInput("projectName",
              "Project Name:"),
    sliderInput("sparse",
                "Sparse Ratio:",
                min = 0.9,  max = 1, value = 0.99, step=0.001),
    
    numericInput('clusters', 'Cluster count', 10,
                 min = 2, max = 50),
    
    numericInput('termCount', 'Term count', 50,
                 min = 10, max = 100),

    checkboxInput("visual", "Visualization", value = TRUE),
    
    checkboxInput("MSC", "Manual Spam Check", value = FALSE),
    
    submitButton("Execute LDA")
    
  ),
  mainPanel(
    fluidRow(
      column(3,h4("Console")),
      column(5,textOutput("visual")),
      column(2,downloadButton('downloadData', 'Download(Keyword)')),
      column(2,downloadButton('downloadData2', 'Download(Doc)'))
    ),
    verbatimTextOutput("console"),
    plotOutput("network", height = 800),
    tableOutput("lda")
  )
))
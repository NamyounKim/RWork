
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(lubridate)

shinyUI(
  fluidPage(
    
    # Application title
    titlePanel("Word Analytics"),
    
    dateRangeInput('dateRange',
                   label = 'Date range input: yyyy-mm-dd',
                   start = ceiling_date(Sys.Date() %m-% months(2), unit = "month") # 전월 1일 
                   ,end = ceiling_date(Sys.Date() %m-% months(1), unit = "month") - days(1) # 전월 마지막일
    ),
    
    selectInput("monthYear", "월 선택:",
                choices = format(seq(as.Date("2010-01-01"), ceiling_date(Sys.Date() %m-% months(2), unit = "month"), by = "month"), "%Y-%m"))
    
    ,
    plotOutput("test_plot")

  )
)

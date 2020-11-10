library(shiny)
library(lubridate)
library(shinydashboard)
library(shiny)
library(DT)
library(plotly)

month_vec = c('All','01','02','03','04','05','06','07','08','09','10','11','12')
current_date = Sys.Date()
current_year = substr(current_date, 1, 4)
current_month = substr(current_date, 6, 7)

dashboardPage(
  dashboardHeader(title = "가계부"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("월단위 지표", tabName = "t1"),
      menuItem("트렌드 분석", tabName = "t2")
    ),
    br(),br(),br()
    #selectInput("emoticon_name", "대상 이모티콘 선택:", choices = NULL),
    #sliderInput("last_day", "출시일 이후 N일 선택:", min = 1, max = 240, value = 60, step = 1),
    #selectInput("fan_size_level", "관심유저 규모 수준 선택:", choices = fan_size_level)
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
          .content-wrapper {
            background-color: white !important;
          }
        "))
    ),
    tabItems(
      tabItem(tabName = "t1",
              fluidRow(
                column(width = 4, selectInput("year_id", "년도 선택", choices = seq(2010, 2020, 1), selected = current_year)),
                column(width = 4, selectInput("month_id", "월 선택", choices = month_vec, selected = current_month))
              ),
             fluidRow(
               valueBoxOutput("value1", width = 2)
               ,valueBoxOutput("value2", width = 2)
               ,valueBoxOutput("value3", width = 2)
               ,valueBoxOutput("value4", width = 2)
               ,valueBoxOutput("value5", width = 2)
               ,valueBoxOutput("value6", width = 2)
             ),
              hr(),
              h3("이달의 지출 카테고리 순위"),
               fluidRow(
                 column(width = 6, dataTableOutput("table1", height = 600))
                 ,column(width = 6, plotlyOutput("chart1", height = 600))
               ),
              hr(),
              h3("이달의 수입 항목"),
               fluidRow(
                 column(width = 6, dataTableOutput("table2", height = 600))
              #   ,column(width = 4, plotOutput("chart2", height = 600))
              #   ,column(width = 4, plotOutput("chart3", height = 600))
               )
      ),
      tabItem(tabName = "t2",
              fluidRow(
                column(width = 4, selectInput("t2_year_id", "년도 선택", choices = seq(2010, 2020, 1), selected = current_year))
              ),
              hr(),
              h3("수입과 지출 추이"),
              fluidRow(
                column(width = 12, plotlyOutput("trend_chart1"))ㄴ
              ),
              
              
      )
    )
  )
)
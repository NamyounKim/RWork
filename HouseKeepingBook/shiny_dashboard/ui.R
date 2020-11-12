library(shiny)
library(lubridate)
library(shinydashboard)
library(shiny)
library(DT)
library(plotly)

current_date = Sys.Date()
current_year = as.numeric(substr(current_date, 1, 4))
current_month = substr(current_date, 6, 7)

month_vec = c('All','01','02','03','04','05','06','07','08','09','10','11','12')
year_vec = c('All', seq(2010, current_year, 1))

dashboardPage(
  dashboardHeader(title = "가계부"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("월단위 지표", tabName = "t1"),
      menuItem("트렌드 분석", tabName = "t2")
    ),
    br(),br(),br()
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
                column(width = 4, selectInput("year_id", "년도 선택", choices = year_vec, selected = current_year)),
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
             h4("상세 지출 내역"),
             fluidRow(
               column(width = 12, dataTableOutput("detail_table", height = 300))
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
                column(width = 6, sliderInput("year_slider", "년도 범위 선택", min = 2010, max = current_year, value = c(current_year, current_year), step = 1, width = "100%", post = "년", sep = ""))
              ),
              hr(),
              h3("수입과 지출 추이"),
              fluidRow(
                column(width = 12, plotlyOutput("trend_chart1"))
              ),
              h3("누적 Save 추이"),
              fluidRow(
                column(width = 12, plotlyOutput("trend_chart2"))
              ),
              h3("Save 비율 추이"),
              fluidRow(
                column(width = 12, plotlyOutput("trend_chart3"))
              )
      )
    )
  )
)
library(shiny)
library(lubridate)
library(shinydashboard)
library(shiny)
library(DT)
library(plotly)

current_date = Sys.Date()
current_year = as.numeric(substr(current_date, 1, 4))
before_year = as.numeric(year(Sys.Date())-1)
current_month = substr(current_date, 6, 7)

month_vec = c('All','01','02','03','04','05','06','07','08','09','10','11','12')
year_vec = c('All', seq(2010, current_year, 1))

dashboardPage(
  dashboardHeader(title = "가계부"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("월단위 지표", tabName = "t1"),
      menuItem("월별 트렌드 분석", tabName = "t2"),
      menuItem("연도별 트렌드 분석", tabName = "t3"),
      menuItem("카테고리별 트렌드 분석", tabName = "t4"),
      menuItem("비교 분석", tabName = "t5")
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
               column(width = 6, dataTableOutput("table1"))
               ,column(width = 6, plotlyOutput("chart1", height = 600))
             ),
             h4("상세 지출 내역"),
             fluidRow(
               column(width = 12, dataTableOutput("detail_table"))
             ),
             h4("올해 월 평균 생활비"),
             fluidRow(
               column(width = 12, verbatimTextOutput("value7"))
             ),
            hr(),
            h3("이달의 수입 항목"),
            fluidRow(
              column(width = 6, dataTableOutput("table2"))
            #   ,column(width = 4, plotOutput("chart2", height = 600))
            #   ,column(width = 4, plotOutput("chart3", height = 600))
            ),
            h4("상세 수입 내역"),
            fluidRow(
              column(width = 12, dataTableOutput("detail_table2"))
            ),
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
      ),
      tabItem(tabName = "t3",
              # fluidRow(
              #   column(width = 6, sliderInput("year_slider", "년도 범위 선택", min = 2010, max = current_year, value = c(current_year, current_year), step = 1, width = "100%", post = "년", sep = ""))
              # ),
              hr(),
              h3("수입과 지출 추이"),
              fluidRow(
                column(width = 12, plotlyOutput("year_trend_chart1"))
              ),
              h3("월 평균 수입과 지출 추이"),
              fluidRow(
                column(width = 12, plotlyOutput("year_trend_chart1_avg"))
              ),
              h3("누적 Save 추이"),
              fluidRow(
                column(width = 12, plotlyOutput("year_trend_chart2"))
              ),
              h3("Save 비율 추이"),
              fluidRow(
                column(width = 12, plotlyOutput("year_trend_chart3"))
              )
      ),
      tabItem(tabName = "t4",
              fluidRow(
                column(width = 6, sliderInput("year_slider2", "년도 범위 선택", min = 2010, max = current_year, value = c(current_year, current_year), step = 1, width = "100%", post = "년", sep = ""))
              ),
              h3("카테고리별 지출액"),
              fluidRow(
                column(width = 12, dataTableOutput("category_exp_table"))
              ),
              h3("카테고리별 트렌드 차트"),
              fluidRow(
                column(width = 12, plotlyOutput("category_trend_chart"))
              )
      ),
      tabItem(tabName = "t5",
              fluidRow(
                column(width = 2, selectInput("past_year_id", "과거년도 선택", choices = year_vec, selected = before_year)),
                column(width = 2, selectInput("past_month_id", "과거월 선택", choices = month_vec, selected = current_month))
              ),
              fluidRow(
                column(width = 2, selectInput("future_year_id", "기준년도 선택", choices = year_vec, selected = current_year)),
                column(width = 2, selectInput("future_month_id", "기준월 선택", choices = month_vec, selected = current_month))
              ),
              fluidRow(
                valueBoxOutput("value_past_exp", width = 2)
                ,valueBoxOutput("value_future_exp", width = 2)
                ,valueBoxOutput("value_diff_exp", width = 2)
              ),
              hr(),
              h3("월별 비교 차트"),
              fluidRow(
                column(width = 12, plotlyOutput("compare_month_chart"))
              ),
              hr(),
              fluidRow(
                column(width = 12, plotlyOutput("compare_month_chart2"))
              )
      )
    )
  )
)
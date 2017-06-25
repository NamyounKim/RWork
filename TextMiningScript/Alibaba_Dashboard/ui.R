# ui.R

library(shiny)

########### No 1. ###########
weekCombo1<- selectInput("week1",
            "Choose a week: ",
            choices = seq(31,44,1),
            selected = 31)

productCombo1<-selectInput("product1", 
            label = "Choose a product: ",
            choices = c("all", "drum", "bolun"),
            selected = "all")

sentimentCombo1<-selectInput("sentiment1", 
            label = "Choose a sentiment: ",
            choices = c("all", "positive", "negative"),
            selected = "all")

########### No 3. ###########
weekCombo3<- selectInput("week3",
                         "Choose a week: ",
                         choices = seq(31,44,1),
                         selected = 31)

productCombo3<-selectInput("product3", 
                           label = "Choose a product: ",
                           choices = c("all", "drum", "bolun"),
                           selected = "all")

companyCombo3<-selectInput("company3", 
            label = "Choose a company: ",
            choices = c("all", "lg", "haier", "galanz", "siemens", "hisense", "sanyo"),
            selected = "all")

########### No 4. ###########
weekCombo4 <- selectInput("week4",
                         "Choose a week: ",
                         choices = seq(31,44,1),
                         selected = 31)

companyCombo4 <- selectInput("company4", 
                           label = "Choose a company: ",
                           choices = c("all", "lg", "haier", "galanz", "siemens", "hisense", "sanyo"),
                           selected = "all")

weekSlider4 <- sliderInput("n_week4",
            "(Company) N week Period (from this week): ",
            min = 4,
            max = 12,
            value = 4)

bar1<-
  fluidRow(
    column(2, offset=2, weekCombo1),
    column(2,productCombo1),
    column(2,sentimentCombo1)
)

bar3<-
  fluidRow(
    column(2, offset=2, weekCombo3),
    column(2, productCombo3),
    column(2, companyCombo3)
  )

bar4<-
  fluidRow(
    column(2, offset=2, weekCombo4),
    column(2, companyCombo4),
    column(3, weekSlider4)
  )

shinyUI(
  fluidPage(
    navbarPage("Social Buzz & Sales Dashboard",               
               tabPanel("1.Social Buzz & Sales Matrix",
                        bar1,
                        h4("Marketing Intelligence"),
                        verbatimTextOutput("intelligence"),
                        htmlOutput("chart1")
               ),
               tabPanel("2.Social Buzz & Sales Exploration", 
                        br(),
                        fluidRow(
                          column(6, h4("Motion Chart"), htmlOutput("chart2")),
                          column(4, offset=2, h4("Marketing Calender"), dataTableOutput("calendar"))
                          )
                        ),
               
               tabPanel("3.Social Analysis",
                        bar3,
                        htmlOutput("chart3"),
                        h4("Relation Keyword"),
                        htmlOutput("relationKeyword")),
               
               tabPanel("4.LDA Analysis",
                        tags$iframe(src = "http://165.243.188.249:8080/Cosmetics2_997_50",
                                    seamless=NA,
                                    width=1700,
                                    height=900,
                                    frameborder=0)),
               
               tabPanel("5.Sales Analysis",
                        bar4,
                        br(),
                        htmlOutput("chart4")),
               
               tabPanel("6.Knowledge Discovery",
                        br(),
                        htmlOutput("chart5"),
                        br(),
                        fluidRow(
                          column(10, offset = 2, h4("Knowledge Rule"))
                        ),
                        fluidRow(
                          column(10, offset = 2, htmlOutput("knowledge") )
                          )
                        )
    )
  )
  
  
)
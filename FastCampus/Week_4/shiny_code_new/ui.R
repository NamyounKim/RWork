

shinyUI(
    navbarPage(
      "연관 단어 분석",
      tabPanel("특정 단어 분석",
          fluidRow(column(3,
               wellPanel(
                 fileInput("inputFile" # 객체명
                           , "먼저 단어간 동시발생 상관계수 데이터를 업로드 해야합니다." # 화면 표시 내용
                           , accept = ".RDS"
                 )
               ),
               wellPanel(   
                 textInput("oneWord" # 객체명
                           , "중심 단어를 입력해주세요." # 화면 표시 내용
                           , value = NULL
                 ),
                 
                 sliderInput("numberOfWord"
                             , "보여질 단어개수"
                             , min = 5
                             , max = 20
                             , value = 10
                             , step=1
                 )
               )
               ), #column end
               mainPanel(
                 tableOutput("table"),
                 plotOutput("one_word_network", height = 600)
               )
          )#fluidRow end
      ),
      tabPanel("멀티 단어 연관 네트워크", 
               sidebarPanel(
                 textInput("multiWord" # 객체명
                           , "여러 단어를 입력해주세요. (구분자는 쉼표)" # 화면 표시 내용
                           , value = NA
                 ),
                 sliderInput("numberOfAllWord2"
                             , "보여질 단어개수"
                             , min = 50
                             , max = 500
                             , value = 100
                             , step=10
                 )
               ),
               plotOutput("multi_word_network", height = 800)
      ),
      tabPanel("전체 연관 단어 네트워크", 
               sidebarPanel(
                 sliderInput("numberOfAllWord"
                             , "보여질 단어개수"
                             , min = 50
                             , max = 500
                             , value = 100
                             , step=10
                 )
               ),
               plotOutput("all_word_network", height = 800)

      )
  )# navbarPage end
)# shinyUI end
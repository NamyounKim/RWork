library(tm)
library(dplyr)
library(tidyr)
library(igraph)
library(network)
library(sna)
library(ggplot2)
library(GGally)
library(ggraph)
library(data.table)
library(stringr)
library(readr)

options(shiny.maxRequestSize = 30 * 1024 ^ 2) 

function(input, output) {
  
  set_graph_style(family = "AppleGothic", face = "plain", size = 10, text_size = 9, text_colour = "black")
  
  # set uploaded file
  upload_data<-reactive({
    
    inFile = input$inputFile
    
    if (is.null(inFile))
      return(NULL)
    
    #could also store in a reactiveValues
    readRDS(inFile$datapath)
  })
  
  get_data <- function(){
    target_data = upload_data()
  }
  #=============================================================
  # set uploaded file
  upload_data_2<-reactive({
    
    inFile = input$inputFile_2
    
    if (is.null(inFile))
      return(NULL)
    
    #could also store in a reactiveValues
    readRDS(inFile$datapath)
  })
  
  get_data_2 <- function(){
    target_data = upload_data_2()
  }
  
  
  output$table <- renderTable({
    co_occurence_df = get_data()
    one_word = input$oneWord # 중심 단어 세팅
    
    if(nchar(one_word) < 1){
      head(co_occurence_df, 5)
    }else(
      head(co_occurence_df %>% filter(word1 == one_word | word2 == one_word) %>% arrange(-cor_value) %>% top_n(5))
    )
    
  })
  
  # 특정 단어 중심의 네트워크 -------------------------------------------------------------------------------------------------------------------
  output$one_word_network <- renderPlot({
    co_occurence_df = get_data()
    one_word = input$oneWord # 중심 단어 세팅
    number_word = input$numberOfWord # 보여질 연관단어 개수
    
    if(is.null(co_occurence_df)){
      return()
    }else if(nchar(one_word) < 1){
      return()
    }
    
    one_word_graph = co_occurence_df %>%
      filter(word1 == one_word | word2 == one_word) %>% 
      top_n(number_word) %>% 
      graph_from_data_frame(directed = F)# 그래프 형태의 데이터셋으로 변환
    
    ggraph(graph = one_word_graph, layout = "fr") +
      geom_edge_link(aes(edge_alpha = cor_value, edge_width = cor_value), edge_colour = "cyan4", end_cap = circle(.1, 'inches')) +
      geom_node_point(size = 5) +
      geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
      theme_void()
  })
  
  # 전체 네트워크 맵 만들기 --------------------------------------------------------------------------------------------------------------
  output$all_word_network <- renderPlot({
    co_occurence_df = get_data()
    number_word = input$numberOfAllWord # 보여질 연관단어 개수
    
    if(is.null(co_occurence_df)){
      return()
    }
    
    # graph형식 데이터셋 만들기
    all_word_graph = co_occurence_df %>% 
      arrange(desc(cor_value)) %>% 
      top_n(number_word) %>%  
      graph_from_data_frame(directed = F)
    
    # 전체 네트워크 맵 생성
    ggraph(all_word_graph, layout = "fr") +
      geom_edge_link(aes(edge_alpha = cor_value, edge_width = cor_value), edge_colour = "cyan4", show.legend = FALSE, end_cap = circle(.07, 'inches')) +
      geom_node_point(color = "grey", size = centralization.degree(all_word_graph)$res) +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
      theme_void()
  })
  
  # 특정 단어들 네트워크 맵 만들기 --------------------------------------------------------------------------------------------------------------
  output$multi_word_network <- renderPlot({
    co_occurence_df = get_data()
    multi_word = input$multiWord
    multi_word = str_trim(str_split_fixed(multi_word, pattern = ",", n = Inf), side = "both")
    
    number_word = input$numberOfAllWord2 # 보여질 연관단어 개수
    print(length(multi_word))
    print(nchar(multi_word))
    
    if(is.null(co_occurence_df)){
      return()
    }else if(nchar(multi_word) == 0){
      return()
    }
    
    co_occurence_df %>%
      filter(word1 %in% multi_word | word2 %in% multi_word) %>% 
      top_n(number_word) %>% # 보여질 연관단어 개수
      graph_from_data_frame(directed = F) %>% # 그래프 형태의 데이터셋으로 변환
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = cor_value, edge_width = cor_value), edge_colour = "cyan4", end_cap = circle(.07, 'inches')) +
      geom_node_point(size = 5) +
      geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
      theme_void()
    
  })
  
}










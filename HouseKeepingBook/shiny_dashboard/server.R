library(ggplot2)
library(dplyr)
library(data.table)

library(DT)
library(ggthemr)
library(ggsci)
library(ggrepel)
library(readr)
library(scales)
library(stringi)
library(plotly)
source("~/GitHub/RWork/HouseKeepingBook/common_function.R")
ggthemr('fresh')

accountBook = data.table(accountBook)
accountBook$month_id = substr(accountBook$yearMonth, 6,7)

except_life_category = c("세금/이자_세금", "세금/이자_대출이자", "건강/문화_보장성보험","세금/이자_기타")
all_month = c('01','02','03','04','05','06','07','08','09','10','11','12')

c_year = as.numeric(substr(Sys.Date(), 1, 4))
all_year = seq(2010, c_year, 1)

#월 단위 트렌드 차트 만들기 ---------------------------------------------------------------------------------------------------------------------------------
year_trend = accountBook %>% 
  group_by(yearMonth) %>% 
  summarise(t_income = sum(totalIncome)
            ,my_income = sum(ifelse(category2 %in% c('주수입_현아급여', '주수입_주원이') | detail == "퇴직금 찾음", 0, totalIncome))
            ,t_exp = sum(totalExpenditure)
            ,life_exp = sum(ifelse(category2 %in% except_life_category, 0, totalExpenditure))
            ,t_save = sum(totalIncome) - sum(totalExpenditure)) %>% 
  mutate(my_save = my_income - t_exp) %>% #나의 세이브 = 나의 수입 - 전체 지출
  arrange(yearMonth) %>%
  mutate(t_cum_save = cumsum(t_save)) %>%
  data.table()

year_trend$my_save_ratio = year_trend$my_save / year_trend$my_income
year_trend$t_save_ratio = year_trend$t_save / year_trend$t_income

year_trend_melt = melt.data.table(year_trend, id.vars = 'yearMonth'
                                  ,measure.vars = c('t_exp','life_exp','t_income','my_income','t_save','t_cum_save','my_save_ratio','t_save_ratio')
                                  ,variable.name = 'trend_type'
                                  ,value.name = 'trend_value')

year_trend_melt$year = substr(year_trend_melt$yearMonth, 1, 4)

#년 단위 트렌드 차트 만들기 ---------------------------------------------------------------------------------------------------------------------------------
year_year_trend = accountBook %>% 
  group_by(year) %>% 
  summarise(t_income = sum(totalIncome)
            ,my_income = sum(ifelse(category2 %in% c('주수입_현아급여', '주수입_주원이') | detail == "퇴직금 찾음", 0, totalIncome), na.rm = T)
            ,t_exp = sum(totalExpenditure)
            ,life_exp = sum(ifelse(category2 %in% except_life_category, 0, totalExpenditure))
            ,t_save = sum(totalIncome) - sum(totalExpenditure)) %>%  #Total 세이브 = 전체 수입 - 전체 지출
  mutate(my_save = my_income - t_exp) %>% #나의 세이브 = 나의 수입 - 전체지출
  mutate(avg_t_income = t_income / 12
         ,avg_my_income = my_income / 12
         ,avg_t_exp = t_exp / 12
         ,avg_life_exp = life_exp / 12
         ,avg_t_save = t_save / 12
         ,avg_my_save = my_save / 12) %>%
  arrange(year) %>%
  mutate(t_cum_save = cumsum(t_save)) %>%
  data.table()

year_year_trend$my_save_ratio = year_year_trend$my_save / year_year_trend$my_income
year_year_trend$t_save_ratio = year_year_trend$t_save / year_year_trend$t_income

year_year_trend_melt = melt.data.table(year_year_trend, id.vars = 'year'
                                  ,measure.vars = c('t_exp','life_exp','t_income','my_income','t_save','t_cum_save','my_save_ratio','t_save_ratio'
                                                    ,'avg_t_income','avg_my_income','avg_t_exp','avg_life_exp','avg_t_save','avg_my_save')
                                  ,variable.name = 'trend_type'
                                  ,value.name = 'trend_value')


# 공통 함수 ------------------------------------------------------------------------------------------------------------------------------------------------------------
get_input_ym <- function(year_val, month_val){
  if(year_val == 'All' & month_val == 'All'){
    input_ym = apply(expand.grid(all_year, all_month), 1, paste0, collapse = "-")
  }else if(month_val == 'All'){
    input_ym = paste0(year_val, "-", all_month)
  }else if(year_val == 'All'){
    input_ym = paste0(all_year, "-", month_val)
  }else{
    input_ym = paste0(year_val, "-", month_val)
  }
  
  return(input_ym)
}


# shiny server ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
shinyServer(function(input, output, session){
  
  #가공 테이블 결과 담는 객체
  result_obj <- reactiveValues(monthly_exp_cat = NULL, monthly_income_cat = NULL)
  
  #가공 테이블 결과 업데이트 (년월 선택시)
  get_etl_result <- observeEvent(c(input$year_id, input$month_id), {
    input_ym = get_input_ym(input$year_id, input$month_id)
    
    #지출 카테고리2별 집계
    monthly_exp_cat2 = accountBook[yearMonth %in% input_ym & type == 'expenditure'] %>% 
      group_by(category2) %>% 
      summarise(totalExpenditure = sum(totalExpenditure)) %>%
      mutate(expenditure_ratio = totalExpenditure/sum(totalExpenditure)) %>%
      arrange(-totalExpenditure) %>% as.data.table()
    
    
    #수입 카테고리별 집계
    monthly_income_cat = accountBook[yearMonth %in% input_ym & type == 'income'] %>% 
      group_by(category2) %>% 
      summarise(total_income = sum(totalIncome)) %>%
      mutate(income_ratio = total_income/sum(total_income)) %>%
      arrange(-total_income) %>% as.data.table()
    
    
    # 객쳐에 담기 
    result_obj$monthly_exp_cat = monthly_exp_cat2
    result_obj$monthly_income_cat = monthly_income_cat
    
  })
  
  output$value1 <- renderValueBox({
    
    input_ym = get_input_ym(input$year_id, input$month_id)
    
    total_income = sum(accountBook[yearMonth %in% input_ym]$totalIncome)
    
    valueBox(formatC(total_income, digits=0, format="f", big.mark=','), subtitle = "총 수입", color = "navy") 
    #red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  
  })
  
  output$value2 <- renderValueBox({
    
    input_ym = get_input_ym(input$year_id, input$month_id)
    
    my_income = sum(accountBook[yearMonth %in% input_ym & !(category2 %in% c("주수입_현아급여", "주수입_주원이")) & detail != "퇴직금 찾음"]$totalIncome)
    
    valueBox(formatC(my_income, digits=0, format="f", big.mark=','), subtitle = "총 나의 수입", color = "light-blue")
    
  })
  
  output$value3 <- renderValueBox({
    
    input_ym = get_input_ym(input$year_id, input$month_id)
    
    total_expenditure = sum(accountBook[yearMonth %in% input_ym]$totalExpenditure)
    
    valueBox(formatC(total_expenditure, digits=0, format="f", big.mark=','), subtitle = "총 지출", color = "red")  
    
  })
  
  output$value4 <- renderValueBox({
    
    input_ym = get_input_ym(input$year_id, input$month_id)
    
    real_expenditure = sum(accountBook[yearMonth %in% input_ym & !(category2 %in% except_life_category)]$totalExpenditure)
    
    valueBox(formatC(real_expenditure, digits=0, format="f", big.mark=','), subtitle = "총 생활비 지출", color = "purple")
    
  })
  
  output$value5 <- renderValueBox({
    
    input_ym = get_input_ym(input$year_id, input$month_id)
    
    total_income = sum(accountBook[yearMonth %in% input_ym]$totalIncome)
    total_expenditure = sum(accountBook[yearMonth %in% input_ym]$totalExpenditure)
    
    total_save = total_income - total_expenditure
    
    valueBox(formatC(total_save, digits=0, format="f", big.mark=','), subtitle = "총 세이브", color = "olive")
    
  })
  
  output$value6 <- renderValueBox({
    
    input_ym = get_input_ym(input$year_id, input$month_id)
    
    total_income = sum(accountBook[yearMonth %in% input_ym]$totalIncome)
    total_expenditure = sum(accountBook[yearMonth %in% input_ym]$totalExpenditure)
    
    save_ratio = (total_income - total_expenditure) / total_income
    
    valueBox(percent(save_ratio, accuracy = 0.1), subtitle = "수입대비 세이브 비율", color = "olive")
    
  })
  
  output$value7 <- renderText({
    
    input_ym = get_input_ym(input$year_id, input$month_id)
    
    #올해 월 평균 지출액
    this_year_avg_life_exp = accountBook[year == input$year_id & yearMonth <= input_ym & !(category2 %in% except_life_category)] %>% group_by(yearMonth) %>% summarise(sum_exp = sum(totalExpenditure)) %>%
      mutate(mean = mean(sum_exp))
    print(this_year_avg_life_exp)
    formatC(this_year_avg_life_exp$mean[1], digits=0, format="f", big.mark=',')
  })
  
  # 지출 카테고리별 금액 테이블 ---------------------------------------------------------------------------------------------------------------------------------
  output$table1 <- renderDataTable({
    
    temp = result_obj$monthly_exp_cat
    temp$expenditure_ratio = percent(temp$expenditure_ratio, accuracy = 0.1)
    temp$totalExpenditure = formatC(temp$totalExpenditure, digits=0, format="f", big.mark=',')
    
    return(temp)
    
  }, selection = list(mode = 'single', selected = c(1)),
    option=list(columnDefs=list(list(targets=2:3, class="dt-right")), searching = FALSE))
  
  
  # 지출 상세 내역 테이블 ---------------------------------------------------------------------------------------------------------------------------------
  output$detail_table <- renderDataTable({
    
    input_ym = get_input_ym(input$year_id, input$month_id)
    
    exp_dt = result_obj$monthly_exp_cat
    select_cate = exp_dt[input$table1_rows_selected]$category2

    detail_expenditure = accountBook[yearMonth %in% input_ym & type == 'expenditure' & category2 == select_cate]
    
    detail_expenditure = detail_expenditure[,.(날짜, category1, category2, detail, totalExpenditure)]
    detail_expenditure$totalExpenditure = formatC(detail_expenditure$totalExpenditure, digits=0, format="f", big.mark=',')
    
    return(detail_expenditure)
    
  }, selection = list(mode = 'none'),
  option=list(columnDefs=list(list(targets=5, class="dt-right")), searching = FALSE))
  
  ## 지출 카테고리별 막대바 ---------------------------------------------------------------------------------------------------------------------------------
  output$chart1 <- renderPlotly({
    
    temp = result_obj$monthly_exp_cat
    
    p = 
    ggplot(temp, aes(x = reorder(category2, totalExpenditure), y = totalExpenditure)) + 
      geom_bar(stat = "identity") + coord_flip() +
      geom_text(aes(label = percent(expenditure_ratio, accuracy = 0.1)), hjust = 0.5) +
      scale_y_continuous(labels = point) +
      labs(x = "지출 카테고리", y = "지출액") +
      scale_color_startrek() +
      theme(axis.text.x=element_text(size = 11, face = "bold")
            ,axis.text.y=element_text(size = 11, face = "bold")
            , plot.title = element_text(hjust = 0.5, face = "bold")
            , title = element_text(hjust = 0.5, size = 12, face = "bold")
            , legend.position = "top"
            , legend.text = element_text(size = 12, face = "bold")
            , text = element_text(family = "Kakao Regular"))
    
    plotly::ggplotly(p)
  })
  
  ## 수입 내역 ---------------------------------------------------------------------------------------------------------------------------------
  output$table2 <- renderDataTable({
    
    temp = result_obj$monthly_income_cat
    temp$income_ratio = percent(temp$income_ratio, accuracy = 0.1)
    temp$total_income = formatC(temp$total_income, digits=0, format="f", big.mark=',')

    return(temp)
    
  }, selection = list(mode = 'single', selected = c(1)), 
  option=list(columnDefs=list(list(targets=2:3, class="dt-right")), searching = FALSE))

  # 수입 상세 내역 테이블 ---------------------------------------------------------------------------------------------------------------------------------
  output$detail_table2 <- renderDataTable({
    
    input_ym = get_input_ym(input$year_id, input$month_id)
    
    income_dt = result_obj$monthly_income_cat
    select_cate = income_dt[input$table2_rows_selected]$category2
    
    detail_income = accountBook[yearMonth %in% input_ym & type == 'income' & category2 == select_cate]
    
    detail_income = detail_income[,.(날짜, category1, category2, detail, totalIncome)]
    detail_income$totalIncome = formatC(detail_income$totalIncome, digits=0, format="f", big.mark=',')
    print(detail_income)
    return(detail_income)
    
  }, selection = list(mode = 'none'),
  option=list(columnDefs=list(list(targets=5, class="dt-right")), searching = FALSE))
  

# Tab2. 트랜드 분석 -------------------------------------------------------------------------------------------------------------------------------
  output$trend_chart1 <- renderPlotly({
    
    year_range = seq(input$year_slider[1], input$year_slider[2], 1)
    
    trend_dt = year_trend_melt[year %in% year_range]
    
    p = 
      ggplot(trend_dt[trend_type %in% c('t_income','my_income','t_exp','life_exp','t_save')], aes(x = yearMonth, y = trend_value, group = trend_type, color = trend_type)) + 
      geom_line() +
      geom_point() +
      scale_y_continuous(labels = point, breaks = breaks_extended(n = 10)) +
      labs(x = "년월", y = "금액") +
      scale_color_startrek() +
      theme(axis.text.x=element_text(size = 11, face = "bold", angle = 45, hjust = 1)
            ,axis.text.y=element_text(size = 11, face = "bold")
            , plot.title = element_text(hjust = 0.5, face = "bold")
            , title = element_text(hjust = 0.5, size = 12, face = "bold")
            , legend.position = "top"
            , legend.text = element_text(size = 12, face = "bold")
            , text = element_text(family = "Kakao Regular"))
    
    plotly::ggplotly(p)
  })
  
  output$trend_chart2 <- renderPlotly({
    year_range = seq(input$year_slider[1], input$year_slider[2], 1)
    
    trend_dt = year_trend_melt[year %in% year_range]
    
    p = 
      ggplot(trend_dt[trend_type %in% c('t_cum_save')], aes(x = yearMonth, y = trend_value/100000000, group = trend_type, color = trend_type)) + 
      geom_line() +
      geom_point() +
      scale_y_continuous(labels = point, breaks = seq(0, 10, 0.1)) +
      labs(x = "년월", y = "금액 (억)") +
      scale_color_startrek() +
      theme(axis.text.x=element_text(size = 11, face = "bold", angle = 45, hjust = 1)
            ,axis.text.y=element_text(size = 11, face = "bold")
            , plot.title = element_text(hjust = 0.5, face = "bold")
            , title = element_text(hjust = 0.5, size = 12, face = "bold")
            , legend.position = "top"
            , axis.ticks.x = element_blank()
            , legend.text = element_text(size = 12, face = "bold")
            , panel.grid.major.x = element_blank()
            , text = element_text(family = "Kakao Regular"))
    
    plotly::ggplotly(p)
  })
  
  output$trend_chart3 <- renderPlotly({
    year_range = seq(input$year_slider[1], input$year_slider[2], 1)
    
    trend_dt = year_trend_melt[year %in% year_range]
    
    p = 
      ggplot(trend_dt[trend_type %in% c('t_save_ratio', 'my_save_ratio')], aes(x = yearMonth, y = trend_value, group = trend_type, color = trend_type)) + 
      geom_line() +
      geom_point() +
      scale_y_continuous(labels = percent, breaks = extended_breaks(10)) +
      geom_hline(yintercept = 0.5, colour = "blue") +
      geom_hline(yintercept = 0, colour = "black") +
      labs(x = "년월", y = "sav") +
      scale_color_startrek() +
      theme(axis.text.x=element_text(size = 11, face = "bold", angle = 45, hjust = 1)
            ,axis.text.y=element_text(size = 11, face = "bold")
            , plot.title = element_text(hjust = 0.5, face = "bold")
            , title = element_text(hjust = 0.5, size = 12, face = "bold")
            , legend.position = "top"
            , axis.ticks.x = element_blank()
            , legend.text = element_text(size = 12, face = "bold")
            , text = element_text(family = "Kakao Regular"))
    
    plotly::ggplotly(p)
  })
  
  # Tab3. 연도별 비교 -------------------------------------------------------------------------------------------------------------------------------
  output$year_trend_chart1 <- renderPlotly({
    
    p = 
      ggplot(year_year_trend_melt[trend_type %in% c('t_income','my_income','t_exp','life_exp','t_save')], aes(x = year, y = trend_value, group = trend_type, color = trend_type)) + 
      geom_line() +
      geom_point() +
      scale_y_continuous(labels = point, breaks = breaks_extended(n = 10)) +
      labs(x = "년월", y = "금액") +
      scale_color_startrek() +
      theme(axis.text.x=element_text(size = 11, face = "bold", angle = 45, hjust = 1)
            ,axis.text.y=element_text(size = 11, face = "bold")
            , plot.title = element_text(hjust = 0.5, face = "bold")
            , title = element_text(hjust = 0.5, size = 12, face = "bold")
            , legend.position = "top"
            , legend.text = element_text(size = 12, face = "bold")
            , text = element_text(family = "Kakao Regular"))
    
    plotly::ggplotly(p)
  })
  
  output$year_trend_chart1_avg <- renderPlotly({
    
    p = 
      ggplot(year_year_trend_melt[trend_type %in% c('avg_t_income','avg_my_income','avg_t_exp','avg_life_exp','avg_t_save')], aes(x = year, y = trend_value, group = trend_type, color = trend_type)) + 
      geom_line() +
      geom_point() +
      scale_y_continuous(labels = point, breaks = breaks_extended(n = 10)) +
      labs(x = "년월", y = "월평균금액") +
      scale_color_startrek() +
      theme(axis.text.x=element_text(size = 11, face = "bold", angle = 45, hjust = 1)
            ,axis.text.y=element_text(size = 11, face = "bold")
            , plot.title = element_text(hjust = 0.5, face = "bold")
            , title = element_text(hjust = 0.5, size = 12, face = "bold")
            , legend.position = "top"
            , legend.text = element_text(size = 12, face = "bold")
            , text = element_text(family = "Kakao Regular"))
    
    plotly::ggplotly(p)
  })
  
  output$year_trend_chart2 <- renderPlotly({
    
    p = 
      ggplot(year_year_trend_melt[trend_type %in% c('t_cum_save')], aes(x = year, y = trend_value/100000000, group = trend_type, color = trend_type)) + 
      geom_line() +
      geom_point() +
      scale_y_continuous(labels = point, breaks = seq(0, 10, 0.1)) +
      labs(x = "년월", y = "금액 (억)") +
      scale_color_startrek() +
      theme(axis.text.x=element_text(size = 11, face = "bold", angle = 45, hjust = 1)
            ,axis.text.y=element_text(size = 11, face = "bold")
            , plot.title = element_text(hjust = 0.5, face = "bold")
            , title = element_text(hjust = 0.5, size = 12, face = "bold")
            , legend.position = "top"
            , axis.ticks.x = element_blank()
            , legend.text = element_text(size = 12, face = "bold")
            , panel.grid.major.x = element_blank()
            , text = element_text(family = "Kakao Regular"))
    
    plotly::ggplotly(p)
  })
  
  output$year_trend_chart3 <- renderPlotly({
    
    p = 
      ggplot(year_year_trend_melt[trend_type %in% c('t_save_ratio', 'my_save_ratio')], aes(x = year, y = trend_value, group = trend_type, color = trend_type)) + 
      geom_line() +
      geom_point() +
      scale_y_continuous(labels = percent, breaks = extended_breaks(10)) +
      geom_hline(yintercept = 0.5, colour = "blue") +
      geom_hline(yintercept = 0, colour = "black") +
      labs(x = "년월", y = "sav") +
      scale_color_startrek() +
      theme(axis.text.x=element_text(size = 11, face = "bold", angle = 45, hjust = 1)
            ,axis.text.y=element_text(size = 11, face = "bold")
            , plot.title = element_text(hjust = 0.5, face = "bold")
            , title = element_text(hjust = 0.5, size = 12, face = "bold")
            , legend.position = "top"
            , axis.ticks.x = element_blank()
            , legend.text = element_text(size = 12, face = "bold")
            , text = element_text(family = "Kakao Regular"))
    
    plotly::ggplotly(p)
  })
})
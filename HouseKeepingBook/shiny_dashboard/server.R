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
source("/Users/kakao/Documents/GitHub/RWork/common_function.R")
ggthemr('fresh')

accountBook = data.table(accountBook)
accountBook$month_id = substr(accountBook$yearMonth, 6,7)

except_life_category = c("세금/이자_세금", "세금/이자_대출이자", "건강/문화_보장성보험","세금/이자_기타")
all_month = c('01','02','03','04','05','06','07','08','09','10','11','12')

#트렌드 차트 만들기
year_trend = accountBook %>% 
  group_by(yearMonth) %>% 
  summarise(t_income = sum(totalIncome)
            ,my_income = sum(ifelse(category2 %in% c('주수입_현아급여', '주수입_주원이') | detail == "퇴직금 찾음", 0, totalIncome))
            ,t_exp = sum(totalExpenditure)
            ,life_exp = sum(ifelse(category2 %in% except_life_category, 0, totalExpenditure))
            ,t_save = sum(totalIncome) - sum(totalExpenditure)) %>% 
  mutate(my_save = my_income - life_exp) %>% #나의 세이브 = 나의 수입 - 생활비 지출
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


# 공통 함수 ------------------------------------------------------------------------------------------------------------------------------------------------------------
get_input_ym <- function(year_val, month_val){
  if(month_val == 'All'){
    input_ym = paste0(year_val, "-", all_month)
  }else{
    input_ym = paste0(year_val, "-", month_val)
  }
  return(input_ym)
}


# shiny server ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
shinyServer(function(input, output, session){
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
  
  output$table1 <- renderDataTable({
    
    input_ym = get_input_ym(input$year_id, input$month_id)
    
    monthly_exp_cat2 = accountBook[yearMonth %in% input_ym & type == 'expenditure'] %>% 
      group_by(category2) %>% 
      summarise(totalExpenditure = sum(totalExpenditure)) %>%
      mutate(expenditure_ratio = totalExpenditure/sum(totalExpenditure)) %>%
      arrange(-totalExpenditure)
    
    monthly_exp_cat2$expenditure_ratio = percent(monthly_exp_cat2$expenditure_ratio, accuracy = 0.1)
    monthly_exp_cat2$totalExpenditure = formatC(monthly_exp_cat2$totalExpenditure, digits=0, format="f", big.mark=',')
    
    return(monthly_exp_cat2)
    
  }, option=list(columnDefs=list(list(targets=2:3, class="dt-right")), searching = FALSE))
  
  output$chart1 <- renderPlotly({
    
    input_ym = get_input_ym(input$year_id, input$month_id)
    
    monthly_exp_cat2 = accountBook[yearMonth %in% input_ym & type == 'expenditure'] %>% 
      group_by(category2) %>% 
      summarise(totalExpenditure = sum(totalExpenditure)) %>%
      mutate(expenditure_ratio = totalExpenditure/sum(totalExpenditure)) %>%
      arrange(-totalExpenditure)
    
    p = 
    ggplot(monthly_exp_cat2, aes(x = reorder(category2, totalExpenditure), y = totalExpenditure)) + 
      geom_bar(stat = "identity", ) + coord_flip() +
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
  
  output$table2 <- renderDataTable({
    
    input_ym = get_input_ym(input$year_id, input$month_id)
    
    monthly_income_cat = accountBook[yearMonth %in% input_ym & type == 'income'] %>% 
      group_by(category2) %>% 
      summarise(total_income = sum(totalIncome)) %>%
      mutate(income_ratio = total_income/sum(total_income)) %>%
      arrange(-total_income)
    
    monthly_income_cat$income_ratio = percent(monthly_income_cat$income_ratio, accuracy = 0.1)
    monthly_income_cat$total_income = formatC(monthly_income_cat$total_income, digits=0, format="f", big.mark=',')
    
    return(monthly_income_cat)
    
  }, option=list(columnDefs=list(list(targets=2:3, class="dt-right")), searching = FALSE))
  
  # Tab2. 트랜드 분석
  output$trend_chart1 <- renderPlotly({
    
    print(year_trend_melt)
    trend_dt = year_trend_melt[year %in% input$t2_year_id]
    
    p = 
      ggplot(trend_dt[trend_type %in% c('t_income','my_income','t_exp','life_exp','t_save')], aes(x = yearMonth, y = trend_value, group = trend_type, color = trend_type)) + 
      geom_line() +
      geom_point() +
      scale_y_continuous(labels = point, breaks = seq(0, 50000000, 1000000)) +
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
})
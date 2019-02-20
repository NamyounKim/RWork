require(shiny)
require(dplyr)
require(stringi)
require(tm)
require(reshape)
require(slam)
require(SnowballC)
require(igraph)
require(network)
require(sna)
require(ggplot2)
require(GGally)
library(readr)

function(input, output) {
  
  output$test_plot <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    date_Range = as.character(input$dateRange)
    print(date_Range)
    startDate = date_Range[1]
    endDate = date_Range[2]
    
    colnames(accountBook)[1] = "date_id"
    # 월별 지출 항목 보기
    #in_yearMonth = '2018-11'
    monthly_exp_category = accountBook %>% filter(date_id >= startDate & date_id <= endDate)
    monthly_exp_category = monthly_exp_category %>% group_by(category1, category2) %>% summarise(totalExpenditure = sum(totalExpenditure))
    
    ggplot(monthly_exp_category, aes(x=category1, y=totalExpenditure, fill = category2)) + geom_bar(stat = "identity") + scale_y_continuous(labels = point) + th
    
    
  })
}
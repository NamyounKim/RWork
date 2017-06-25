# server.R

library(shiny)
library(googleVis)
library(reshape)
library(wordcloud)

shinyServer(function(input, output) {
  
  sliderValue1_1 <- reactive({
    
    # Chart 1 - Social Buzz & Sales Matrix
    
    dt <- read.csv('View_6_output.csv', header = TRUE)

    if(input$product1 == 'all' && input$sentiment1 =='all'){
      
      sub <- subset(dt, week_num == paste('2015/',input$week1, sep=""), select = c(c_name_eng, total_count, quantity))
      sub <- cbind(as.character(sub[,1]), scale(sub[,-1]))
      colnames(sub) <- c("c_name_eng", "Social", "Sales")
      sub <- data.frame(sub, stringsAsFactors = FALSE)
      
    } else if (input$product1 == 'all' && input$sentiment1 =='positive'){
      
      sub <- subset(dt, week_num == paste('2015/',input$week1, sep=""), select = c(c_name_eng, pos_count, quantity))
      sub <- cbind(as.character(sub[,1]), scale(sub[,-1]))
      colnames(sub) <- c("c_name_eng", "Social", "Sales")
      sub <- data.frame(sub, stringsAsFactors = FALSE)

    } else if (input$product1 == 'all' && input$sentiment1 =='negative'){
      
      sub <- subset(dt, week_num == paste('2015/',input$week1, sep=""), select = c(c_name_eng, neg_count, quantity))
      sub <- cbind(as.character(sub[,1]), scale(sub[,-1]))
      colnames(sub) <- c("c_name_eng", "Social", "Sales")
      sub <- data.frame(sub, stringsAsFactors = FALSE)
      
    } else if (input$product1 == 'drum' && input$sentiment1 =='all'){
      
      sub <- subset(dt, week_num == paste('2015/',input$week1, sep=""), select = c(c_name_eng, t_DRUM_count, DRUM_Q))
      sub <- cbind(as.character(sub[,1]), scale(sub[,-1]))
      colnames(sub) <- c("c_name_eng", "Social", "Sales")
      sub <- data.frame(sub, stringsAsFactors = FALSE)
      
    } else if (input$product1 == 'drum' && input$sentiment1 =='positive'){
      
      sub <- subset(dt, week_num == paste('2015/',input$week1, sep=""), select = c(c_name_eng, p_DRUM_count, DRUM_Q))
      sub <- cbind(as.character(sub[,1]), scale(sub[,-1]))
      colnames(sub) <- c("c_name_eng", "Social", "Sales")
      sub <- data.frame(sub, stringsAsFactors = FALSE)
      
    } else if (input$product1 == 'drum' && input$sentiment1 =='negative'){
      
      sub <- subset(dt, week_num == paste('2015/',input$week1, sep=""), select = c(c_name_eng, n_DRUM_count, DRUM_Q))
      sub <- cbind(as.character(sub[,1]), scale(sub[,-1]))
      colnames(sub) <- c("c_name_eng", "Social", "Sales")
      sub <- data.frame(sub, stringsAsFactors = FALSE)
      
    } else if (input$product1 == 'bolun' && input$sentiment1 =='all'){
      
      sub <- subset(dt, week_num == paste('2015/',input$week1, sep=""), select = c(c_name_eng, t_BOLUN_count, BOLUN_Q))
      sub <- cbind(as.character(sub[,1]), scale(sub[,-1]))
      colnames(sub) <- c("c_name_eng", "Social", "Sales")
      sub <- data.frame(sub, stringsAsFactors = FALSE)
      
    } else if (input$product1 == 'bolun' && input$sentiment1 =='positive'){
      
      sub <- subset(dt, week_num == paste('2015/',input$week1, sep=""), select = c(c_name_eng, p_BOLUN_count, BOLUN_Q))
      sub <- cbind(as.character(sub[,1]), scale(sub[,-1]))
      colnames(sub) <- c("c_name_eng", "Social", "Sales")

    } else if (input$product1 == 'bolun' && input$sentiment1 =='negative'){
      
      sub <- subset(dt, week_num == paste('2015/',input$week1, sep=""), select = c(c_name_eng, n_BOLUN_count, BOLUN_Q))
      sub <- cbind(as.character(sub[,1]), scale(sub[,-1]))
      colnames(sub) <- c("c_name_eng", "Social", "Sales")
      sub <- data.frame(sub, stringsAsFactors = FALSE)
      
    }
    
  }) 
  
  sliderValue1_2 <- reactive({
    
    # Chart 1 - Social Buzz & Sales Matrix
    
    dt2 <- read.csv('View_6_output_overall.csv', header = TRUE, stringsAsFactors = FALSE)
    dt2 <- subset(dt2, sales_type == input$product1 & social_type == input$sentiment1, select = c(c_name_eng, social, sales))
    print(dt2)
    
  })
  
  sliderValue2_1 <- reactive({
    
    # Chart 2 - Social Buzz & Sales Exploration
    
    dt <- read.csv('View_4_output.csv', header = TRUE, stringsAsFactors = FALSE)

  })
  
  sliderValue2_2 <- reactive({
    
    # Chart 2 - Social Buzz & Sales Exploration
    
    dt <- read.csv('promotion_cal.csv', header = TRUE, stringsAsFactors = FALSE)
    
  })
  
  sliderValue3_1 <- reactive({
    
    # Chart 3 - Social Analysis
    
    dt <- read.csv('View_8.csv', header = TRUE)
    
    dtlist <- vector("list", 2)
    subweekdt <- subset(dt, weeknum == paste('2015/',input$week3, sep=""))
    
    productName <- toString(input$product3)
    companyName <- toString(input$company3)
    
    if(productName=="all" && companyName=="all"){
      
      dtlist[[1]] <- aggregate(subweekdt$count, by = list(subweekdt$product, subweekdt$neg_pos), sum)
      colnames(dtlist[[1]]) <- c("product", "neg_pos", "count")
      
      dtlist[[2]] <- cast(subweekdt, media_name + product ~ neg_pos, sum)
      
    } else if(productName=="all" && companyName!="all"){
      
      subCompany <- subset(subweekdt, company == companyName)
      dtlist[[1]] <- aggregate(subCompany$count, by = list(subCompany$product, subCompany$neg_pos), sum)
      colnames(dtlist[[1]]) <- c("product", "neg_pos", "count")
      
      dtlist[[2]] <- cast(subCompany, media_name + product ~ neg_pos, sum)
      
    } else if(productName!="all" && companyName=="all"){
      
      print(productName)
      subProduct <- subset(subweekdt, product == productName)
      dtlist[[1]] <- aggregate(subProduct$count, by = list(subProduct$product, subProduct$neg_pos), sum)
      colnames(dtlist[[1]]) <- c("product", "neg_pos", "count")
      
      dtlist[[2]] <- cast(subProduct, media_name + product ~ neg_pos, sum)
      
    } else if(productName!="all" && companyName!="all"){
      
      subTwo <- subset(subweekdt, product == productName)
      subTwo <- subset(subTwo, company == companyName)
      
      dtlist[[1]] <- aggregate(subTwo$count, by = list(subTwo$product, subTwo$neg_pos), sum)
      colnames(dtlist[[1]]) <- c("product", "neg_pos", "count")
      
      dtlist[[2]] <- cast(subTwo, media_name + product ~ neg_pos, sum)
      
    }
    dtlist[[3]] <- productName
    dtlist[[4]] <- companyName
    print("+++++")
    print(dtlist)

  })
  
  sliderValue3_2 <- reactive({
    keywordset <- read.csv('keyword_summary_Total_v2_1_sampling.csv', header = TRUE)
    colnames(keywordset) <- c("week_num", "product", "neg_pos", "keyword", "role", "numkeyworkd", "numdocs", "company")
    
    productName <- toString(input$product3)
    companyName <- toString(input$company3)
    
    week_keywordset <- subset(keywordset, week_num == paste('2015/', input$week3, sep=""), select = c(keyword,numdocs,neg_pos,company, product))
    
    if(productName=="all" && companyName=="all"){
      
      sub_keywordset <- aggregate(week_keywordset$numdocs, by=list(week_keywordset$keyword, week_keywordset$neg_pos),FUN=sum)
      
    }else if(productName=="all" && companyName!="all"){
      
      print(companyName)
      sub_keywordset <- subset(week_keywordset, company==companyName)
      
      if(nrow(sub_keywordset)>0){
        sub_keywordset <- aggregate(sub_keywordset$numdocs, by=list(sub_keywordset$keyword, sub_keywordset$neg_pos),FUN=sum)
      }
      
      
    }else if(productName!="all" && companyName=="all"){
      
      sub_keywordset <- subset(week_keywordset, product==productName)
      sub_keywordset <- aggregate(sub_keywordset$numdocs, by=list(sub_keywordset$keyword, sub_keywordset$neg_pos),FUN=sum)
      
    }else if(productName!="all" && companyName!="all"){
      
      sub_keywordset <- subset(week_keywordset, product==productName)
      sub_keywordset <- subset(sub_keywordset, company==companyName)
      sub_keywordset <- aggregate(sub_keywordset$numdocs, by=list(sub_keywordset$keyword, sub_keywordset$neg_pos),FUN=sum)
      
    }
   })
  
  sliderValue4 <- reactive({
    
    # Chart 4 - Sales Analysis 
    
    dt <- read.csv('View_2_input2.csv', header = TRUE, stringsAsFactors = FALSE)
    
  })
  
  sliderValue5_1 <- reactive({
    
    dt <- read.csv('View_11_4c.csv', header = TRUE)
    
  })
  
  sliderValue5_2 <- reactive({
    
    dt <- read.csv('view_10_knowledge.csv', header = TRUE, stringsAsFactors = FALSE)
    colnames(dt) <- c('No.', 'Week', 'Date', 'Company', 'Product', 'Social Sentiment', 'Channel', 'Buzz Quantity', '4C Level 1', '4C Level 2', 'Top 5 Keyword', 'Knowledge', 'Export Report')
    print(dt)
  })
  
  output$chart1 <- renderGvis({

    # Chart 1
    
    dt1 <- sliderValue1_1()
    dt1 <- dt1[order(dt1$c_name_eng),]
    
    dt2 <- sliderValue1_2()
    dt2 <- dt2[order(dt2$c_name_eng),]
    
    gvisMerge(
      gvisBubbleChart(dt1,
                      idvar="c_name_eng", 
                      xvar="Sales", yvar="Social",
                      colorvar="c_name_eng",
                      options=list( width = 600
                                    , height = 600
                                    , hAxes="[{viewWindowMode:'explicit', viewWindow:{min:-3, max:3}, title:'Sales', textPosition: 'in'}]"
                                    , vAxes="[{viewWindowMode:'explicit', viewWindow:{min:-3, max:3}, title:'Social Buzz', textPosition: 'in'}]"
                                    , legend.position = 'top'
                                    , title = paste(input$week1,"  WEEK")
                                    , titleTextStyle="{color:'navy', fontSize:16}"
                                    , fontName = 'malgun'))
      , gvisBubbleChart(dt2,
                        idvar="c_name_eng", 
                        xvar="sales", yvar="social",
                        colorvar="c_name_eng",
                        options=list( width = 600
                                      , height = 600
                                      , hAxes="[{viewWindowMode:'explicit', viewWindow:{min:-3, max:3}, title:'Sales', textPosition: 'in'}]"
                                      , vAxes="[{viewWindowMode:'explicit', viewWindow:{min:-3, max:3}, title:'Social Buzz', textPosition: 'in'}]"
                                      , legend.position = 'top'
                                      , title = "3Q"
                                      , titleTextStyle="{color:'navy', fontSize:16}"
                                      , fontName = 'malgun'
                                      )
                         )
      , horizontal = TRUE)
  })
  
  output$chart2 <- renderGvis({
    
    # Chart 2
    
    dt <- sliderValue2_1()
    
    dt$date <- as.Date(dt$date)
    
    gvisMotionChart(dt, idvar = "id", timevar = "date",
                    xvar="x1", yvar="x2", sizevar="quantity",
                    option = list(width = 800, height = 620))
  })
  
  output$chart3 <- renderGvis({
    
    # Chart 3 - Social Analysis
    
    dt <- sliderValue3_1()

    print(dt)
    
    dt1 <- aggregate(dt[[1]][,'count'], by = list(dt[[1]]$neg_pos), sum)
    dt2 <- aggregate(dt[[2]][,c('neg', 'pos')], by = list(dt[[2]]$media_name), sum)
    dt2 <- dt2[, c("Group.1", "pos", "neg")]
    
    gvisMerge(
      gvisPieChart(dt1
                   , options=list(height = 450
                                  , width = 600
                                  , title = toupper(dt[[3]])
                                  , titleTextStyle="{color:'navy', fontSize:16}"
                                  , fontName = 'malgun')
      ),
      gvisBarChart(dt2
                   , xvar = "Group.1"
                   , yvar = c("neg", "pos")
                   , options = list(height = 450
                                    , width = 600
                                    , title = "Buzz Count by Source")
      ),
      horizontal = TRUE
    )
  })
  
  output$chart4 <- renderGvis({
    
    # Chart 4 - Sales Analysis 
    
    dt <- sliderValue4()
    dt$date <- as.Date(dt$date)
    
    print(head(dt))
    
    if ( input$company4 == "all" ){
      
      set <- aggregate(dt[,-c(1:3,ncol(dt))], by = list(dt$c_name_eng, dt$week_num), mean)
      colnames(set)[1:2] <- c('c_name_eng', 'week_num')
      
      subset <- subset(set, week_num == input$week4)
      
      cset <- subset[, c('c_name_eng'
                     , 'online_quantity'
                     , 'offline_quantity')]
      
      cset$sum  <- rowSums(cset[,c(2,3)], na.rm = TRUE)
      cset$avg_online <- mean(cset$online_quantity, na.rm = TRUE)
      cset$avg_offline <- mean(cset$offline_quantity, na.rm = TRUE)
      colnames(cset) <- c('COMPANY', 'ONLINE', 'OFFLINE', 'SUM', 'AVG.ONLINE', 'AVG.OFFLINE')
      
      tset <- subset[, c('c_name_eng'
                     , 'single'
                     , 'dual'
                     , 'cylinder'
                     , 'drum')]
      
      tset$sum  <- rowSums(tset[,c(2:5)], na.rm = TRUE)
      tset[,c(2:5)] <- tset[,c(2:5)]/tset$sum
      tset$avg_single <- mean(tset$single, na.rm = TRUE)
      tset$avg_dual <- mean(tset$dual, na.rm = TRUE)
      tset$avg_cylinder <- mean(tset$cylinder, na.rm = TRUE)
      tset$avg_drum <- mean(tset$drum, na.rm = TRUE)
      colnames(tset) <- c('COMPANY', 'SINGLE', 'DUAL', 'BOLUN', 'DRUM', 'SUM', 'AVG.SINGLE', 'AVG.DOUBLE', 'AVG.BOLUN', 'AVG.DRUM')
      
      pset <- subset[, c('c_name_eng'
                      , 'price_under_200'
                      , 'price_under_400'
                      , 'price_under_600'
                      , 'price_under_800'
                      , 'price_under_1000'
                      , 'price_under_1200'
                      , 'price_under_1400'
                      , 'price_under_1900'
                      , 'price_under_2400'
                      , 'price_under_2900'
                      , 'price_under_3400'
                      , 'price_under_4400'
                      , 'price_over_4401')]
      
      pset$sum  <- rowSums(pset[,c(2:14)], na.rm = TRUE)
      pset[,c(2:14)] <- pset[,c(2:14)]/pset$sum
      colnames(pset) <- c('COMPANY', '200-', '400-', '600-', '800-', '1000-', '1200-', '1400-', '1900-', '2400-', '2900-', '3400-', '4400-', '4400+', 'SUM')
      
      vset <- subset[, c('c_name_eng'
                    , 'vol_under_4.4'
                    , 'vol_under_4.9'
                    , 'vol_under_5.9'
                    , 'vol_under_6.4'
                    , 'vol_under_6.9'
                    , 'vol_under_7.4'
                    , 'vol_under_7.9'
                    , 'vol_over_8.0')]
      
      vset$sum  <- rowSums(vset[,c(2:9)], na.rm = TRUE)
      vset[,c(2:9)] <- vset[,c(2:9)]/vset$sum
      colnames(vset) <- c('COMPANY', '4.4-', '4.9-', '5.9-', '6.4-', '6.9-', '7.4-','7.9-', '8.0+', 'SUM')

      gvisMerge(gvisMerge(gvisColumnChart(cset
                                       , xvar = "COMPANY"
                                       , yvar = c('ONLINE', 'OFFLINE', 'AVG.ONLINE', 'AVG.OFFLINE')
                                       , options=list(height = 400, width = 700
                                                       , title = "ONLINE/OFFLINE SALES VOLUME"
                                                       , titleTextStyle="{color:'navy', fontSize:16}"
                                                       , fontName = 'malgun'
                                                       , vAxes ="[{title:'Quantity', titleTextStyle: {color: 'black'}, textStyle:{color: 'black'}, textPosition: 'out'}]"
                                                       , hAxes = "[{title: 'Date', textPosition: 'out'}]"
                                                       , series="{ 2: {type: 'line', lineDashStyle: [20, 1]}, 3: {type: 'line', lineDashStyle: [4, 1]} }"))
                          , gvisColumnChart(tset
                                         , xvar = "COMPANY"
                                         , yvar = c('SINGLE', 'DUAL', 'BOLUN', 'DRUM', 'AVG.SINGLE', 'AVG.DOUBLE', 'AVG.BOLUN', 'AVG.DRUM')
                                         , options=list(height = 400, width = 700
                                                          , title = "ONLINE SALES VOLUME RATIO BY TYPE"
                                                          , titleTextStyle="{color:'navy', fontSize:16}"
                                                          , fontName = 'malgun'
                                                          , vAxes ="[{title:'Ratio', format:'#,###%', titleTextStyle: {color: 'black'}, textStyle:{color: 'black'}, textPosition: 'out'}]"
                                                          , hAxes = "[{title: 'Date', textPosition: 'out'}]"
                                                          , seriesType = "bars"
                                                          , series="{ 4: {type: 'line', lineDashStyle: [4, 1]}, 5: {type: 'line', lineDashStyle: [4, 1]}
                                                          , 6: {type: 'line', lineDashStyle: [4, 1]}, 7: {type: 'line', lineDashStyle: [4, 1]}}"))
                          , horizontal = TRUE)
                
                , gvisMerge(gvisColumnChart(pset
                                            , xvar = "COMPANY"
                                            , yvar = c('200-', '400-', '600-', '800-', '1000-', '1200-', '1400-', '1900-', '2400-', '2900-', '3400-', '4400-', '4400+')
                                            , options=list(height = 400, width = 700
                                                           , title = "ONLINE SALES VOLUME RATIO BY PRICE"
                                                           , titleTextStyle = "{color:'navy', fontSize:16}"
                                                           , fontName = 'malgun'
                                                           , vAxes ="[{title:'Ratio', format:'#,###%', titleTextStyle: {color: 'black'}, textStyle:{color: 'black'}, textPosition: 'out'}]"
                                                           , hAxes = "[{title: 'Date', textPosition: 'out'}]"))
                            , gvisColumnChart(vset
                                              , xvar = "COMPANY"
                                              , yvar = c('4.4-', '4.9-', '5.9-', '6.4-', '6.9-', '7.4-','7.9-', '8.0+')
                                              , options=list(height = 400, width = 700
                                                             , title = "ONLINE SALES VOLUME RATIO BY VOLUME"
                                                             , titleTextStyle = "{color:'navy', fontSize:16}"
                                                             , fontName = 'malgun'
                                                             , vAxes ="[{title:'Ratio', format:'#,###%', titleTextStyle: {color: 'black'}, textStyle:{color: 'black'}, textPosition: 'out'}]"
                                                             , hAxes = "[{title: 'Date', textPosition: 'out'}]"))
                            , horizontal = TRUE))
      
    } else {
      
      subset_company <- subset(dt, c_name_eng == input$company4)
      nrows <- nrow(subset_company)
      
      cset <- subset_company[seq(nrows-(input$n_week4-1), nrows, 1),c('c_name_eng'
                                                                      , 'online_quantity'
                                                                      , 'offline_quantity'
                                                                      , 'date')]
      cset$sum  <- rowSums(cset[,c(2,3)], na.rm = TRUE)
      cset$avg_online <- mean(cset$online_quantity)
      cset$avg_offnline <- mean(cset$offline_quantity)
      colnames(cset) <- c('COMPANY', 'ONLINE', 'OFFLINE', 'DATE', 'SUM', 'AVG.ONLINE', 'AVG.OFFLINE')
      
      tset <- subset_company[seq(nrows-(input$n_week4-1), nrows, 1),c('c_name_eng'
                                                                     , 'single'
                                                                     , 'dual'
                                                                     , 'cylinder'
                                                                     , 'drum'
                                                                     , 'date')]
      tset$sum  <- rowSums(tset[,c(2:5)], na.rm = TRUE)
      tset[,c(2:5)] <- tset[,c(2:5)]/tset$sum
      tset$avg_single <- mean(tset$single)
      tset$avg_dual <- mean(tset$dual)
      tset$avg_cylinder <- mean(tset$cylinder)
      tset$avg_drum <- mean(tset$drum)
      colnames(tset) <- c('COMPANY', 'SINGLE', 'DUAL', 'BOLUN', 'DRUM', 'DATE', 'SUM', 'AVG.SINGLE', 'AVG.DOUBLE', 'AVG.BOLUN', 'AVG.DRUM')
      
      pset <- subset_company[seq(nrows-(input$n_week4-1), nrows, 1),c('c_name_eng'
                                                                      , 'price_under_200'
                                                                      , 'price_under_400'
                                                                      , 'price_under_600'
                                                                      , 'price_under_800'
                                                                      , 'price_under_1000'
                                                                      , 'price_under_1200'
                                                                      , 'price_under_1400'
                                                                      , 'price_under_1900'
                                                                      , 'price_under_2400'
                                                                      , 'price_under_2900'
                                                                      , 'price_under_3400'
                                                                      , 'price_under_4400'
                                                                      , 'price_over_4401'
                                                                      , 'date')]
      pset$sum  <- rowSums(pset[,c(2:14)], na.rm = TRUE)
      pset[,c(2:14)] <- pset[,c(2:14)]/pset$sum
      colnames(pset) <- c('COMPANY', '200-', '400-', '600-', '800-', '1000-', '1200-', '1400-', '1900-', '2400-', '2900-', '3400-', '4400-', '4400+', 'DATE', 'SUM')
      
      vset <- subset_company[seq(nrows-(input$n_week4 -1), nrows, 1),c('c_name_eng'
                                                                      , 'vol_under_4.4'
                                                                      , 'vol_under_4.9'
                                                                      , 'vol_under_5.9'
                                                                      , 'vol_under_6.4'
                                                                      , 'vol_under_6.9'
                                                                      , 'vol_under_7.4'
                                                                      , 'vol_under_7.9'
                                                                      , 'vol_over_8.0'
                                                                      , 'date')]
      vset$sum  <- rowSums(vset[,c(2:9)], na.rm = TRUE)
      vset[,c(2:9)] <- vset[,c(2:9)]/vset$sum
      colnames(vset) <- c('COMPANY', '4.4-', '4.9-', '5.9-', '6.4-', '6.9-', '7.4-','7.9-', '8.0+', 'DATE', 'SUM')

      gvisMerge(gvisMerge(gvisLineChart(cset
                                        , xvar = "DATE"
                                        , yvar = c('ONLINE', 'OFFLINE', 'AVG.ONLINE', 'AVG.OFFLINE')
                                        , options=list(height = 400, width = 700
                                                       , title = paste(toupper(input$company4), " - ONLINE/OFFLINE SALES VOLUME", sep ="")
                                                       , titleTextStyle="{color:'navy', fontSize:16}"
                                                       , fontName = 'malgun'
                                                       , vAxes ="[{title:'Quantity', titleTextStyle: {color: 'black'}, textStyle:{color: 'black'}, textPosition: 'out'}]"
                                                       , hAxes = "[{title: 'Date', textPosition: 'out'}]"
                                                       , series="{ 2: {lineDashStyle: [20, 1]}, 3: {lineDashStyle: [4, 1]} }"))
                          , gvisComboChart(tset
                                           , xvar = "DATE"
                                           , yvar = c('SINGLE', 'DUAL', 'BOLUN', 'DRUM', 'AVG.SINGLE', 'AVG.DOUBLE', 'AVG.BOLUN', 'AVG.DRUM')
                                           , options=list(height = 400, width = 700
                                                          , title = paste(toupper(input$company4), " - ONLINE SALES VOLUME RATIO BY TYPE", sep ="")
                                                          , titleTextStyle="{color:'navy', fontSize:16}"
                                                          , fontName = 'malgun'
                                                          , vAxes ="[{title:'Ratio', format:'#,###%', titleTextStyle: {color: 'black'}, textStyle:{color: 'black'}, textPosition: 'out'}]"
                                                          , hAxes = "[{title: 'Date', textPosition: 'out'}]"
                                                          , seriesType = "bars"
                                                          , series="{ 4: {type: 'line', lineDashStyle: [4, 1]}, 5: {type: 'line', lineDashStyle: [4, 1]}
                                                          , 6: {type: 'line', lineDashStyle: [4, 1]}, 7: {type: 'line', lineDashStyle: [4, 1]}}"))
                          , horizontal = TRUE)
                
                , gvisMerge(gvisColumnChart(pset
                                            , xvar = "DATE"
                                            , yvar = c('200-', '400-', '600-', '800-', '1000-', '1200-', '1400-', '1900-', '2400-', '2900-', '3400-', '4400-', '4400+')
                                            , options=list(height = 400, width = 700
                                                           , title = paste(toupper(input$company4), " - ONLINE SALES VOLUME RATIO BY PRICE", sep ="")
                                                           , titleTextStyle = "{color:'navy', fontSize:16}"
                                                           , fontName = 'malgun'
                                                           , vAxes ="[{title:'Ratio', format:'#,###%', titleTextStyle: {color: 'black'}, textStyle:{color: 'black'}, textPosition: 'out'}]"
                                                           , hAxes = "[{title: 'Date', textPosition: 'out'}]"))
                            , gvisColumnChart(vset
                                              , xvar = "DATE"
                                              , yvar = c('4.4-', '4.9-', '5.9-', '6.4-', '6.9-', '7.4-','7.9-', '8.0+')
                                              , options=list(height = 400, width = 700
                                                             , title = paste(toupper(input$company4), " - ONLINE SALES VOLUME RATIO BY VOLUME", sep ="")
                                                             , titleTextStyle = "{color:'navy', fontSize:16}"
                                                             , fontName = 'malgun'
                                                             , vAxes ="[{title:'Ratio', format:'#,###%', titleTextStyle: {color: 'black'}, textStyle:{color: 'black'}, textPosition: 'out'}]"
                                                             , hAxes = "[{title: 'Date', textPosition: 'out'}]"))
                            , horizontal = TRUE))
      
    }
    
  })
  
  output$chart5 <- renderGvis({
    
    dt <- sliderValue5_1()
    dt1 <- subset(dt, val2 == TRUE)
    
    
    gvisOrgChart(dt1, idvar = "concept", parentvar = "parent", tipvar = "val",
                 options=list(width=600, height=250))
    
  })
  
  output$calendar <- renderDataTable({
    
    calendarDt <- sliderValue2_2()
    
    #gvisTable(calendarDt, options=list(page='enable', width=600, height='automatic', pageSize=20))
    
  })
  
  # Generate a summary of the dataset
  output$intelligence <- renderPrint({
    
    cat("■ Haier의 긍정 Buzz량이 전체의 53%를 차지하고 있음. \n■ Siemens의 부정 Buzz량은 타사 대비 15%로 높으며 판매량에 영향을 미칠 수 있는 수준임")
  })
  
  output$knowledge <- renderGvis({
    
    dt <- sliderValue5_2()
    gvisTable(dt, options=list(page='enable'))
    
  })
  
  output$relationKeyword <- renderGvis({
    dt2 <- sliderValue3_2()
    df <- data.frame(word=dt2[,1], sentiment=dt2[,2], freq=dt2[,3], stringsAsFactors = FALSE)
    
    sub_pos <- subset(df, df$sentiment=="pos")
    if(nrow(sub_pos) == 0){
      temp = data.frame(word="NO Keyword", freq=10)
      posTotal=data.frame(word="Positive", freq=sum(sub_pos$freq))
      posTotal <- rbind(posTotal,temp)
    }else{
      posTotal=data.frame(word="Positive", freq=sum(sub_pos$freq))
    }
    
    sub_neg <- subset(df, df$sentiment=="neg")
    if(nrow(sub_neg) == 0){
      temp = data.frame(word="NO Keyword", freq=10)
      negTotal=data.frame(word="Negative", freq=sum(sub_neg$freq))
      negTotal <- rbind(negTotal,temp)
    }else{
      negTotal=data.frame(word="Negative", freq=sum(sub_neg$freq))
    }
    
    pos_df <- rbind(posTotal,subset(df, df$sentiment=="pos", select = c("word","freq")))
    pos_df$parent <- "Positive"
    pos_df$parent[pos_df$word=="Positive"] <- NA
    pos_df$color <- log(pos_df$freq)
    pos_df <- pos_df[with(pos_df,order(-freq)),]
    pos_df <- head(pos_df,100)
    
    neg_df <- rbind(negTotal,subset(df, df$sentiment=="neg", select = c("word","freq")))
    neg_df$parent <- "Negative"
    neg_df$parent[neg_df$word=="Negative"] <- NA
    neg_df$color <- log(neg_df$freq)
    neg_df <- neg_df[with(neg_df,order(-freq)),]
    neg_df <- head(neg_df,100)
    
    gvisMerge(
      gvisMerge(
        gvisTreeMap(pos_df, idvar = "word", parentvar="parent", colorvar="freq", sizevar = "freq",
                    options=list(fontSize=16, highlightOnMouseOver=TRUE, maxColor="#F25E7A", midColor="#F2B5C1" ,minColor="#F2DDDE", width=600)),
        gvisTreeMap(neg_df, idvar = "word", parentvar="parent", colorvar="color", sizevar = "freq",
                    options=list(fontSize=16, highlightOnMouseOver=TRUE, maxColor="#396F85", midColor="#8AC0D7" ,minColor="#9DAEB9", width=600)),
        horizontal = TRUE, 
        tableOptions= "cellspacing=\"40\" bgcolor=\"#AABBCC\" left=\"100\" " 
      ),
      
      gvisMerge(
        gvisTable(pos_df, options=list(page="enable", height=400, width=600)),
        gvisTable(neg_df, options=list(page="enable", height=400, width=600)),
        horizontal = TRUE
      )
    )
  })
  
})

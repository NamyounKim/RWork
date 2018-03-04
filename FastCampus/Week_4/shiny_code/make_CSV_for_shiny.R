library(readr)

# shiny 대시보드 입력용 CSV파일 만들기
parsedDataDf = data.frame(parsedContent = parsedData)
write_csv(parsedDataDf, path = "./Week_4/shiny_code/petitions.csv")

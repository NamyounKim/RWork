
parsedData = c(parsedData, NULL)

extract_close_word <- function(parsedData, targetword, distance = 2){
  library(stringi)
  
  close_word_list = NULL
  parsedData = gsub("  "," ",parsedData)
  split_parsed_data = stri_split_fixed(parsedData, pattern = " ")
  
  for(i in 1:length(parsedData)){
    
    target_index_1 = which(split_parsed_data[[i]] == targetword) - distance
    target_index_2 = which(split_parsed_data[[i]] == targetword) - distance + 1
    
    target_index = sort(c(target_index_1, target_index_2))
    
    #인덱스가 1보다 작은 경우 1로 치환하고 중복제거
    if(length(which(target_index < 1))){
      target_index[which(target_index < 1)] = 1
      target_index = unique(target_index)
    }

    print(target_index)
    
    temp = split_parsed_data[[i]][target_index]
    
    if(length(temp) > 0){
      close_word_list = c(close_word_list, temp)
    }
  }
  return(close_word_list)
}

result = extract_close_word(parsedData = parsedData[1:10] #여기서 형태소 분석완료된 결과 넣기 (vector형태로)
                            ,targetword = "대한민국" # 대상단어
                            ,distance = 2 # 얼마나 앞으로 떨어져있는지 (예시2칸)
                            )
result
write_csv(result , "./result.csv")

library(haven)
library(skimr)
library(tidyverse)

#미혼 여성 노동자의 경우 EITC가 노동 공급을 늘이게 될까?
data = haven::read_dta('data/eitc.dta')
skim(data, work, year, children) %>% 
  skimr::kable(digits = 0) 

data2 = data %>% mutate(post93 = year >= 1994, anykids = children >= 1)

model = lm(work ~ anykids*post93, data = data2)
summary(model)


data = read_csv("./data/wage.csv")
data = as.data.table(data)
data$hwage = data$wage / data$hours
head(data)

simple_fit <- lm(log(hwage) ~ educ, data=data)
summary(simple_fit)


multi_fit <- lm(log(hwage) ~ educ + IQ + exper + tenure + age + married + black + south + urban + sibs + brthord + meduc + feduc, data=data)
summary(multi_fit)

install.packages("bayesAB")
library(bayesAB)

testGroup <- rbinom(8050, 1, 757/8050)
controlGroup <- rbinom(8024, 1, 757/8024) # 베이스라인

# First Analysis
test1 <- bayesTest(testGroup, controlGroup, distribution = "bernoulli", priors = c("alpha" = 1, "beta" = 1))
print(test1)
summary(test1)

trendProb[i] = temp$probability

plot(test1)

trendProb = NULL
for(i in 1: 600){
  controlGroup <- rbinom(8024, 1, i/8024)
  testGroup <- rbinom(8050, 1, 355/8050) # 베이스라인
  
  test1 <- bayesTest(controlGroup, testGroup, distribution = "bernoulli", priors = c("alpha" = 1, "beta" = 1))
  temp = summary(test1)
  trendProb[i] = temp$probability
}

plot(1:600,trendProb)


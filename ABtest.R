install.packages("bayesAB")
library(bayesAB)
control_1 <- rbinom(11450, 1, 0.0947)
treatment_1 <- rbinom(11561, 1, 0.1106)

# First Analysis
test1 <- bayesTest(treatment_1, control_1, distribution = "bernoulli", priors = c("alpha" = 1, "beta" = 1))
print(test1)
summary(test1)
plot(test1)

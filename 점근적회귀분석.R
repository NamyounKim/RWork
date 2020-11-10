
library(drc)
library(nlme)
library(aomisc)
library(dplyr)
library(readr)
library(data.table)
library(ggplot2)

#devtools::install_github("OnofriAndreaPG/aomisc")

data_set = read_csv("/Users/kakao/Downloads/ryan_data_set.csv")

options(scipen = 100)

#Asymptotic regression model ------------------
a = -134839
b = 0.1238441
c = 909361.6
xe <- asymReg.fun(data_set$x, a, b, c)

plot(data_set$x, data_set$y)
plot(xe, data_set$y)

model <- drm(data_set$y ~ data_set$x, fct = DRC.asymReg())
plot(model, log="", main = "Asymptotic regression")

summary(model)
str(model)
p_y = c - (c - a) * exp(-b * data_set$x)

coef(model)
plot(data_set$x, p_y)


data_set$y_p = predict(model, newdata = as.data.table(data_set$x))


ggplot(data_set, aes(x = x)) + 
  geom_line(aes(y = y_p, group = 1)) +
  geom_line(aes(y = y, group = 1, color = "red")) + 
  scale_x_continuous(breaks = seq(0,90,1))


x = data_set$x
y = data_set$y
lo <- loess(p_y ~ x, span= 0.2)

xl <- seq(min(x),max(x), (max(x) - min(x))/1000)

out = predict(lo,xl)

plot(x, p_y)
plot(x, y)
lines(xl, out, col='red', lwd=2)

infl <- c(FALSE, diff(diff(p_y)) > 1)
points(x[infl ], p_y[infl ], col="blue")
xl[infl]


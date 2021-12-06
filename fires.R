install.packages("forecast")
install.packages("gtsummary")
library(forecast)
library(patchwork)
library(sandwich)
library(lmtest)
library(tidyverse)
library(gtsummary)
library(Hmisc)
library(corrplot)

set.seed(123)

#read data, subset months
data <- read.csv('lab2new/w203_lab2/forestfires.csv')
data <- subset(data, data$month %in% c("aug", "sep"))

#Define min-max normalize function
normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

#Ordinally Encode Days of Week
days <- factor(data$day, levels = c("mon","tue","wed","thu","fri","sat","sun"))
data$ordinal_day <- as.integer(days)

#Drop non-numeric columns for modeling
data <- subset(data, select = -c(month, day))

#transformations in the search for normality
data$log_area <- normalize(log(data$area + 1))
data$boxcox_ffmc <- normalize(BoxCox(data$FFMC, lambda = "auto"))
data$log_isi <- log(data$ISI + 1)
data$log_RH <- log(data$RH + 1)
data$log_rain <- log(data$rain + 1)

#Write data
write.csv(data, "forestfires_cleaned.csv")

#EDA to visualize how the independent variables change versus how the dependent variable changes
plot(data$log_area, data$log_rain)
plot(data$log_area, data$boxcox_ffmc)
plot(data$log_area, data$log_isi)
plot(data$log_area, data$log_RH)
plot(data$log_area, data$wind)
plot(data$log_area, data$temp)
plot(data$log_area, data$DC)
plot(data$log_area, data$DMC)
plot(data$log_area, data$X)
plot(data$log_area, data$Y)

temp_plot <- ggplot(data, aes(x=temp, y=log_area)) + geom_point() + geom_smooth(method=lm)
print(temp_plot + labs(title="Scatter plot of Burned Area and Air Temperature", y = "Burned Area (logged)(in ha)", x = "Temperature in Celsius"))
rh_plot <- ggplot(data, aes(x=log_RH, y=log_area)) + geom_point() + geom_smooth(method=lm)
print(rh_plot + labs(title="Scatter plot of Burned Area and Relative Humidity", y = "Burned Area (logged)(in ha)", x = "Relative Humidity in % (Logged)"))
wind_plot <- ggplot(data, aes(x=wind, y=log_area)) + geom_point() + geom_smooth(method=lm)
print(wind_plot + labs(title="Scatter plot of Burned Area and Wind", y = "Burned Area (logged)(in ha)", x = "Wind in km/h"))
df = data[,c(2,3,4,5,6,7,9,13,16,17)]
res = cor(df[,-1])
res2 <- rcorr(as.matrix(df[,-1]))
res2
corrplot(res, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)


#model testing

model1 <- lm(log_area ~ wind + temp + log_rain, data=data)
model2 <- lm(log_area ~ log_RH + wind + temp + log_rain, data=data)
model3 <- lm(log_area ~ boxcox_ffmc + log_isi + log_RH + wind + temp + log_rain, data=data)
model4 <- lm(log_area ~ boxcox_ffmc + log_isi + log_RH + wind + temp + DC + DMC + log_rain, data=data)
model5 <- lm(log_area ~ boxcox_ffmc + log_isi + log_RH + wind + temp + DC + DMC + X + Y + log_rain, data=data)

#testing the longest model's efficicacy versus the shorter models
anova(model5, model1)
anova(model5, model2)
anova(model5, model3)
anova(model5, model4)

#coefficient testing to determine significance
coeftest(model1, vcov =  vcovHC)
coeftest(model2, vcov =  vcovHC)
coeftest(model3, vcov =  vcovHC)
coeftest(model4, vcov =  vcovHC)
coeftest(model5, vcov =  vcovHC)

#model regression table
tbl_regression(model5)

#Q-Q plot to evaluate homoscedasticity condition
plot(model5, 2)

#Scale location plot for normal error distribution
plot(model5, 3)

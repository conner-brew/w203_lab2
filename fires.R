#install.packages("forecast")
library(forecast)
library(patchwork)
library(sandwich)
library(lmtest)

set.seed(123)

#read data, subset months
data <- read.csv('forestfires.csv')
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

#Split the data into a test and training set at a 70/30 split
sample_size <- floor(0.7 * nrow(data))
train_indices <- sample(seq_len(nrow(data)), size = sample_size)
train <- data[train_indices,]
test <- data[-train_indices,]


#EDA to visualize how the independent variables change versus how the dependent variable changes
plot(data$log_area, data$log_rain)
plot(data$log_area, data$bboxcox_ffmc)
plot(data$log_area, data$log_isi)
plot(data$log_area, data$log_RH)
plot(data$log_area, data$wind)
plot(data$log_area, data$temp)
plot(data$log_area, data$DC)
plot(data$log_area, data$DMC)
plot(data$log_area, data$X)
plot(data$log_area, data$Y)

#model testing

model1 <- lm(log_area ~ wind + temp + log_rain, data=data)
model2 <- lm(log_area ~ log_RH + wind + temp + log_rain, data=data)
model3 <- lm(log_area ~ boxcox_ffmc + log_isi + log_RH + wind + temp + log_rain, data=data)
model4 <- lm(log_area ~ boxcox_ffmc + log_isi + log_RH + wind + temp + DC + DMC + log_rain, data=data)
model5 <- lm(log_area ~ boxcox_ffmc + log_isi + log_RH + wind + temp + DC + DMC + X + Y + log_rain, data=data)

anova(model1, model2)
anova(model1, model3)
anova(model1, model4)
anova(model1, model5)


coeftest(model1, vcov =  vcovHC)
coeftest(model2, vcov =  vcovHC)
coeftest(model3, vcov =  vcovHC)
coeftest(model4, vcov =  vcovHC)
coeftest(model5, vcov =  vcovHC)

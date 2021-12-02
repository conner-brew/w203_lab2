#install.packages("forecast")
library(forecast)
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

#model testing
model1 <- lm(log_area ~ boxcox_ffmc + log_isi + log_RH + log_rain + wind + temp + DC + DMC + X + Y, data=train)
model2 <- lm(log_area ~ boxcox_ffmc + ISI + log_RH + log_rain + wind + temp + DC + DMC + X + Y, data=train)
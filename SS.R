install.packages('forecast')
library(dplyr)

#setting working directory
setwd("C:/Users/Dell/Desktop/GDM/data")

#reading the csv data
PM2.5 <- read.csv("C:/Users/Dell/Desktop/GDM/data/PM2.5.csv")
NO2 <- read.csv("C:/Users/Dell/Desktop/GDM/data/NO2.csv")
temp <- read.csv("C:/Users/Dell/Desktop/GDM/data/Temp.csv")
PM0.3 <- read.csv("C:/Users/Dell/Desktop/GDM/data/PM0.3.csv")
humidity <- read.csv("C:/Users/Dell/Desktop/GDM/data/RH.csv")

str(PM2.5)

PM2.5_daily <- PM2.5 %>%
  group_by(datetimeLocal) %>%
  summarise (mean_PM2.5 = mean(PM2.5_Value))

PM0.3_daily <- PM0.3_data %>%
  group_by(datetimeLocal) %>%
  summarise (mean_PM0.3 = mean(PM0.3_Value))

humidity_daily <- humidity_data %>%
  group_by(datetimeLocal) %>%
  summarise (mean_RH = mean(RH_Value))



plot(PM2.5_daily$datetimeLocal, PM2.5_daily$mean_PM2.5, type = "l", 
     xlab = "Date", ylab = "PM2.5 Concentration", 
     main = "Daily Mean PM2.5 Concentration")


plot(PM0.3_daily$datetimeLocal, PM0.3_daily$mean_PM0.3, type = "l", 
     xlab = "Date", ylab = "PM0.3 Concentration", 
     main = "Daily Mean PM0.3 Concentration")

plot(humidity_daily$datetimeLocal, humidity_daily$mean_RH, type = "l", 
     xlab = "Date", ylab = "RH Concentration", 
     main = "Daily Mean RH Concentration")

#library(forecast)
#arima_model <- auto.arima(PM2.5_daily$mean_PM2.5)

# Model Evaluation
#summary(arima_model)

model <- lm (PM2.5_Value ~ NO2_Value+temperature+RH_Value+PM0.3_Value, data=merged_data)
summary(model)

plot(model)

plot(modelplot(no2_raster)

# Forecasting
forecast_PM25 <- forecast(arima_model, h = 30)


PM2.5_daily$lagged_PM2.5 <- lag(PM2.5_daily$mean_PM2.5, 1)

# Model Building
regression_model <- lm(mean_PM2.5 ~ lagged_PM2.5 + PM0.3_daily$mean_PM0.3 + humidity_daily$mean_RH, data = PM2.5_daily)

# Model Evaluation
summary(regression_model)

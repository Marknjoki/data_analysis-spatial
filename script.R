#USING MACHINE LEARNING TO TRAIN A MODEL TO PREDICT AIR QUALITY CHANGES IN NAIROBI
install.packages("dplyr")
installed.packages("gdalUtils")
#initialising required libraries
library(raster)
library(sf)
library(randomForest)
library(caTools)
library(dplyr)

#setting working directory
setwd("C:/Users/Dell/Desktop/GDM/data")

#reading the csv data
PM_2.5 <- read.csv("C:/Users/Dell/Desktop/GDM/data/PM2.5.csv")
NO2 <- read.csv("C:/Users/Dell/Desktop/GDM/data/NO2.csv")
temp <- read.csv("C:/Users/Dell/Desktop/GDM/data/Temp.csv")
PM0.3 <- read.csv("C:/Users/Dell/Desktop/GDM/data/PM0.3.csv")
humidity <- read.csv("C:/Users/Dell/Desktop/GDM/data/RH.csv")



#removing unnecessary columns
PM2.5 <- subset(PM_2.5,select = -c(datetimeUtc,location_id,location_name,timezone,parameter,latitude,longitude,country_iso,isMobile,isMonitor,owner_name,provider))
NO_2 <- subset(NO2,select = -c(datetimeUtc,location_id,location_name,timezone,parameter,latitude,longitude,country_iso,isMobile,isMonitor,owner_name,provider))
temp_data <- subset(temp,select = -c(datetimeUtc,location_id,location_name,timezone,parameter,latitude,longitude,country_iso,isMobile,isMonitor,owner_name,provider))
humidity_data <- subset(humidity,select = -c(datetimeUtc,location_id,location_name,timezone,parameter,latitude,longitude,country_iso,isMobile,isMonitor,owner_name,provider))
PM0.3_data <- subset(PM0.3,select = -c(datetimeUtc,location_id,location_name,timezone,parameter,latitude,longitude,country_iso,isMobile,isMonitor,owner_name,provider))
#renaming the columns
colnames(PM2.5) <- c("PM2.5_Value","unit","datetimeLocal")
colnames(NO_2) <- c("NO2_Value","unit","datetimeLocal")
colnames(temp_data) <- c("temperature","unit","datetimeLocal")
colnames(humidity_data) <- c("RH_Value","unit","datetimeLocal")
colnames(PM0.3_data) <- c("PM0.3_Value","unit","datetimeLocal")

#covertiing date column to date type
PM2.5$datetimeLocal <- as.Date(PM2.5$datetimeLocal)
NO_2$datetimeLocal <- as.Date(NO_2$datetimeLocal)
temp_data$datetimeLocal <- as.Date(temp_data$datetimeLocal)
humidity_data$datetimeLocal <- as.Date(humidity_data$datetimeLocal)
PM0.3_data$datetimeLocal <- as.Date(PM0.3_data$datetimeLocal)

PM2.5$day <- as.numeric(format(PM2.5$datetimeLocal, "%d"))
NO_2$day <- as.numeric(format(NO_2$datetimeLocal, "%d"))
temp_data$day <- as.numeric(format(temp_data$datetimeLocal, "%d"))
humidity_data$day <- as.numeric(format(humidity_data$datetimeLocal, "%d"))
PM0.3_data$day <- as.numeric(format(PM0.3_data$datetimeLocal, "%d"))

#PM2.5 <- subset(PM2.5, select = -c(hour))
View(temp)

PM25_daily_mean <- aggregate(PM2.5_Value ~ datetimeLocal, data = PM2.5, FUN = mean)
NO2_daily_mean <- aggregate(NO2_Value ~ datetimeLocal, data = NO_2, FUN = mean)
temp_daily_mean <- aggregate(temperature ~ datetimeLocal, data = temp_data, FUN = mean)
humidity_daily_mean <- aggregate(RH_Value ~ datetimeLocal, data = humidity_data, FUN = mean)
PM0.3_daily_mean <- aggregate(PM0.3_Value ~ datetimeLocal, data = PM0.3_data, FUN = mean)


mergedData <- merge(PM25_daily_mean,NO2_daily_mean, by = "datetimeLocal")

mergedData3 <- merge(temp_daily_mean,humidity_daily_mean, by ="datetimeLocal")

MergedData <- merge (mergedData3,PM0.3_daily_mean, by ="datetimeLocal")

merged_data <- merge(mergedData,MergedData, by ="datetimeLocal")

View(merged_data)#creating a linear regression model
model <- lm(PM2.5_Value ~ NO2_Value, data = mergedData)

# Model summary

summary(model)

plot(model)

#loading raster image of NDVI
ndvi <- raster("C:/Users/Dell/Desktop/GDM/data/NO2_Africa.tif")


#admin boundary
admin_shp <-st_read("C:/Users/Dell/Desktop/GDM/data/nairobi.shp")

no2_stats <- raster::extract(ndvi, admin_shp, fun = mean, na.rm = TRUE)

predictors <- cbind(MergedData, no2_stats)
View(no2_values)

predictions <- predict(model,newdata=predictors)

options(scipen = 999)

plot(mergedData$NO2_Value,mergedData$PM2.5_Value,
     xlab = "NO_2",
     ylab = "PM2.5",
     main = "SCATTER PLOT OF NO_2 VS. PM2.5",
     col="blue")

#regression line
abline(model, col="red")


mergedData2 <- merge(mergedData,temp_daily_mean, by ="datetimeLocal", all.x=TRUE)


spatial_join <- st_join(admin_shp, st_as_sf(mergedData2, coords=c("longitude","latitude")),join = st_intersects)
# Plot NO2 raster data
plot(no2_raster, main = "Tropospheric NO2 Concentrations")


# Overlay raster with shapefile
plot(no2_raster)
plot(admin_shp, add = TRUE)
#centroids <- coordinates(admin_shp)

no2_values <- extract(no2_raster,centroids)

View(no2_values)

PM2.5_NO2 <- cbind(PM25_daily_mean, NO2_Value=no2_values)
PM2.5_NO2 <- na.omit(PM2.5_NO2)
View(PM2.5_NO2)
cor(PM2.5_NO2$PM2.5_Value, PM2.5_NO2$NO2_Value)

rf_model <- randomForest(PM2.5_Value ~ NO2_Value , data= PM2.5_NO2)

summary(new_model)
varImpPlot(rf_model)
#extent_NO2<- extent(c(xmax(36.901133909423365),ymax(-1.3030732919852213),xmin(36.90121974011184),ymin(-1.311868647012887)))

NO2_df <-  as.data.frame(no2_raster,xy=TRUE)
View(NO2_df)

colnames(NO2_df) <- c("longitude","latitude","NO2")

View(NO2_DataFrame)

set.seed(100)

sampleData = sample.split(trainingData,SplitRatio = .80)

View(sampleData)

train = subset(trainingData,sampleData == TRUE)
test = subset(trainingData,sampleData == FALSE)

View(train)
View(test)


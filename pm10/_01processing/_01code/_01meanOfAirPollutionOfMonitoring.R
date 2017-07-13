##mean of each monitoring stations

#remove variable
rm(list=ls())

#package
#install.packages("lubridate")
#install.packages("plyr")
#install.packages("Amelia")
#install.packages("sqldf")
library("plyr")
library("lubridate")
#library("Amelia")
library("sqldf")

#work directory
setwd("D:\\01 Study\\08 pm\\processing\\preprocessing")

#empdy data frame
dataResult = data.frame()

#function definition(50%: 360, 70%: 504, 75%: 540)
narmMean <- function(values){
  if(length(values[values!=NA])>=540){
    mean(values, na.rm=TRUE)
  } else {
    NA
  }
}

narmMedian <- function(values){
  if(length(values[values!=NA])>=540){
    median(values, na.rm=TRUE)
  } else {
    NA
  }
}

narmMax <- function(values){
  if(length(values[values!=NA])>=540){
    max(values, na.rm=TRUE)
  } else {
    NA
  }
}

narmMin <- function(values){
  if(length(values[values!=NA])>=540){
    min(values,na.rm=TRUE)
  } else {
    NA
  }
}

#2016
year <- 2016

#2001-2015
for (year in 2001:2015){
  
  fileName1 <- paste(year,"01.csv",sep='')
  fileName2 <- paste(year,"02.csv",sep='')
  fileName3 <- paste(year,"03.csv",sep='')
  fileName4 <- paste(year,"04.csv",sep='')
  
  data1 <- read.csv(fileName1, header=T, sep=",")  
  data2 <- read.csv(fileName2, header=T, sep=",")
  data3 <- read.csv(fileName3, header=T, sep=",")
  data4 <- read.csv(fileName4, header=T, sep=",")
  
  data <- rbind(data1, data2, data3, data4)
  str(data)
  head(data,5)
  area <- paste(data$지역,data$측정소명)
  data <- cbind(area, data)
  
  #month
  date <- substr(data$측정일시, 1, 8)
  asdate <- as.Date(date, "%Y%m%d")
  month <- month(asdate)
  
  dataMonth <- cbind(data, month)
  head(dataMonth, 5)
  dataMonth[dataMonth == -999] <- NA
  summary(dataMonth, na.rm=TRUE)
  
  #aggregate
  SO2_mean <- aggregate(dataMonth$SO2~dataMonth$month+dataMonth$area, dataMonth, narmMean)
  CO_mean <- aggregate(dataMonth$CO~dataMonth$month+dataMonth$area, dataMonth, narmMean)
  O3_mean <- aggregate(dataMonth$O3~dataMonth$month+dataMonth$area, dataMonth, narmMean)
  NO2_mean <- aggregate(dataMonth$NO2~dataMonth$month+dataMonth$area, dataMonth, narmMean)
  PM10_mean <- aggregate(dataMonth$PM10~dataMonth$month+dataMonth$area, dataMonth, narmMean)
  
  SO2_median <- aggregate(dataMonth$SO2~dataMonth$month+dataMonth$area, dataMonth, narmMedian)
  CO_median <- aggregate(dataMonth$CO~dataMonth$month+dataMonth$area, dataMonth, narmMedian)
  O3_median <- aggregate(dataMonth$O3~dataMonth$month+dataMonth$area, dataMonth, narmMedian)
  NO2_median <- aggregate(dataMonth$NO2~dataMonth$month+dataMonth$area, dataMonth, narmMedian)
  PM10_median <- aggregate(dataMonth$PM10~dataMonth$month+dataMonth$area, dataMonth, narmMedian)
  
  SO2_max <- aggregate(dataMonth$SO2~dataMonth$month+dataMonth$area, dataMonth, narmMax)
  CO_max <- aggregate(dataMonth$CO~dataMonth$month+dataMonth$area, dataMonth, narmMax)
  O3_max <- aggregate(dataMonth$O3~dataMonth$month+dataMonth$area, dataMonth, narmMax)
  NO2_max <- aggregate(dataMonth$NO2~dataMonth$month+dataMonth$area, dataMonth, narmMax)
  PM10_max <- aggregate(dataMonth$PM10~dataMonth$month+dataMonth$area, dataMonth, narmMax)
  
  SO2_min <- aggregate(dataMonth$SO2~dataMonth$month+dataMonth$area, dataMonth, narmMin)
  CO_min <- aggregate(dataMonth$CO~dataMonth$month+dataMonth$area, dataMonth, narmMin)
  O3_min <- aggregate(dataMonth$O3~dataMonth$month+dataMonth$area, dataMonth, narmMin)
  NO2_min <- aggregate(dataMonth$NO2~dataMonth$month+dataMonth$area, dataMonth, narmMin)
  PM10_min <- aggregate(dataMonth$PM10~dataMonth$month+dataMonth$area, dataMonth, narmMin)
  
  
  #월별 측정소의 대기오염 농도 평균 조인
  SO2_mean1 <- paste(SO2_mean$`dataMonth$month`,SO2_mean$`dataMonth$area`)
  CO_mean1 <- paste(CO_mean$`dataMonth$month`,CO_mean$`dataMonth$area`)
  O3_mean1 <- paste(O3_mean$`dataMonth$month`,O3_mean$`dataMonth$area`)
  NO2_mean1 <- paste(NO2_mean$`dataMonth$month`,NO2_mean$`dataMonth$area`)
  PM10_mean1 <- paste(PM10_mean$`dataMonth$month`,PM10_mean$`dataMonth$area`)
  
  SO2_mean2 <- data.frame(cbind(day=SO2_mean1, SO2=SO2_mean$`dataMonth$SO2`))
  CO_mean2 <- data.frame(cbind(day=CO_mean1, CO=CO_mean$`dataMonth$CO`))
  O3_mean2 <- data.frame(cbind(day=O3_mean1, O3=O3_mean$`dataMonth$O3`))
  NO2_mean2 <- data.frame(cbind(day=NO2_mean1, NO2=NO2_mean$`dataMonth$NO2`))
  PM10_mean2 <- data.frame(cbind(day=PM10_mean1, PM10=PM10_mean$`dataMonth$PM10`))
  
  data_2003_mean_bind1 <- merge(SO2_mean2, CO_mean2, by='day', all = TRUE)
  data_2003_mean_bind2 <- merge(data_2003_mean_bind1, O3_mean2, by='day', all = TRUE)
  data_2003_mean_bind3 <- merge(data_2003_mean_bind2, NO2_mean2, by='day', all = TRUE)
  data_2003_mean_bind4 <- merge(data_2003_mean_bind3, PM10_mean2, by='day', all = TRUE)
  head(data_2003_mean_bind4,5)
  str(data_2003_mean_bind4)
  
  #월별 측정소의 대기오염 농도 중앙값 조인
  SO2_median1 <- paste(SO2_median$`dataMonth$month`,SO2_median$`dataMonth$area`)
  CO_median1 <- paste(CO_median$`dataMonth$month`,CO_median$`dataMonth$area`)
  O3_median1 <- paste(O3_median$`dataMonth$month`,O3_median$`dataMonth$area`)
  NO2_median1 <- paste(NO2_median$`dataMonth$month`,NO2_median$`dataMonth$area`)
  PM10_median1 <- paste(PM10_median$`dataMonth$month`,PM10_median$`dataMonth$area`)
  
  SO2_median2 <- data.frame(cbind(day=SO2_median1, SO2=SO2_median$`dataMonth$SO2`))
  CO_median2 <- data.frame(cbind(day=CO_median1, CO=CO_median$`dataMonth$CO`))
  O3_median2 <- data.frame(cbind(day=O3_median1, O3=O3_median$`dataMonth$O3`))
  NO2_median2 <- data.frame(cbind(day=NO2_median1, NO2=NO2_median$`dataMonth$NO2`))
  PM10_median2 <- data.frame(cbind(day=PM10_median1, PM10=PM10_median$`dataMonth$PM10`))
  
  data_2003_median_bind1 <- merge(SO2_median2, CO_median2, by='day', all = TRUE)
  data_2003_median_bind2 <- merge(data_2003_median_bind1, O3_median2, by='day', all = TRUE)
  data_2003_median_bind3 <- merge(data_2003_median_bind2, NO2_median2, by='day', all = TRUE)
  data_2003_median_bind4 <- merge(data_2003_median_bind3, PM10_median2, by='day', all = TRUE)
  head(data_2003_median_bind4,5)
  str(data_2003_median_bind4)
  
  
  #월별 측정소의 대기오염 농도 최대 조인
  SO2_max1 <- paste(SO2_max$`dataMonth$month`,SO2_max$`dataMonth$area`)
  CO_max1 <- paste(CO_max$`dataMonth$month`,CO_max$`dataMonth$area`)
  O3_max1 <- paste(O3_max$`dataMonth$month`,O3_max$`dataMonth$area`)
  NO2_max1 <- paste(NO2_max$`dataMonth$month`,NO2_max$`dataMonth$area`)
  PM10_max1 <- paste(PM10_max$`dataMonth$month`,PM10_max$`dataMonth$area`)
  
  SO2_max2 <- data.frame(cbind(day=SO2_max1, SO2=SO2_max$`dataMonth$SO2`))
  CO_max2 <- data.frame(cbind(day=CO_max1, CO=CO_max$`dataMonth$CO`))
  O3_max2 <- data.frame(cbind(day=O3_max1, O3=O3_max$`dataMonth$O3`))
  NO2_max2 <- data.frame(cbind(day=NO2_max1, NO2=NO2_max$`dataMonth$NO2`))
  PM10_max2 <- data.frame(cbind(day=PM10_max1, PM10=PM10_max$`dataMonth$PM10`))
  
  data_2003_max_bind1 <- merge(SO2_max2, CO_max2, by='day', all = TRUE)
  data_2003_max_bind2 <- merge(data_2003_max_bind1, O3_max2, by='day', all = TRUE)
  data_2003_max_bind3 <- merge(data_2003_max_bind2, NO2_max2, by='day', all = TRUE)
  data_2003_max_bind4 <- merge(data_2003_max_bind3, PM10_max2, by='day', all = TRUE)
  head(data_2003_max_bind4,5)
  str(data_2003_max_bind4)
  
  #월별 측정소의 대기오염 농도 최소 조인
  SO2_min1 <- paste(SO2_min$`dataMonth$month`,SO2_min$`dataMonth$area`)
  CO_min1 <- paste(CO_min$`dataMonth$month`,CO_min$`dataMonth$area`)
  O3_min1 <- paste(O3_min$`dataMonth$month`,O3_min$`dataMonth$area`)
  NO2_min1 <- paste(NO2_min$`dataMonth$month`,NO2_min$`dataMonth$area`)
  PM10_min1 <- paste(PM10_min$`dataMonth$month`,PM10_min$`dataMonth$area`)
  
  SO2_min2 <- data.frame(cbind(day=SO2_min1, SO2=SO2_min$`dataMonth$SO2`))
  CO_min2 <- data.frame(cbind(day=CO_min1, CO=CO_min$`dataMonth$CO`))
  O3_min2 <- data.frame(cbind(day=O3_min1, O3=O3_min$`dataMonth$O3`))
  NO2_min2 <- data.frame(cbind(day=NO2_min1, NO2=NO2_min$`dataMonth$NO2`))
  PM10_min2 <- data.frame(cbind(day=PM10_min1, PM10=PM10_min$`dataMonth$PM10`))
  
  data_2003_min_bind1 <- merge(SO2_min2, CO_min2, by='day', all = TRUE)
  data_2003_min_bind2 <- merge(data_2003_min_bind1, O3_min2, by='day', all = TRUE)
  data_2003_min_bind3 <- merge(data_2003_min_bind2, NO2_min2, by='day', all = TRUE)
  data_2003_min_bind4 <- merge(data_2003_min_bind3, PM10_min2, by='day', all = TRUE)
  head(data_2003_min_bind4,5)
  str(data_2003_min_bind4)
  
  # output
  data_2003_final <- cbind(year, data_2003_mean_bind4, data_2003_median_bind4, data_2003_max_bind4,data_2003_min_bind4)
  head(data_2003_final,5)
  
  dataResult = rbind(dataResult, data_2003_final)
  
}


write.csv(dataResult, "D:\\01 Study\\08 pm\\processing\\preprocessing\\result\\monthResult_75.csv")

##mean of sigungu

#remove variable
rm(list=ls())

#work directory
setwd("D:\\01 Study\\08 pm\\processing\\input")

#input
dataMonth <- read.csv("pm10_sig_matching_75_mean.csv", header=T, sep=",")
head(dataMonth, 6)
length(dataMonth[dataMonth == -999])
dataMonth[dataMonth == -999] <- NA

#function definition
narmMean <- function(values){
  if(length(values[values!=NA])>=1){
    mean(values, na.rm=TRUE)
  } else {
    print(length(values[values!=NA]))
    NA
  }
}

narmMedian <- function(values){
  if(length(values[values!=NA])>=1){
    median(values, na.rm=TRUE)
  } else {
    NA
  }
}

narmMax <- function(values){
  if(length(values[values!=NA])>=1){
    max(values, na.rm=TRUE)
  } else {
    NA
  }
}

narmMin <- function(values){
  if(length(values[values!=NA])>=1){
    min(values,na.rm=TRUE)
  } else {
    NA
  }
}

#year, month, sig_cd
SO2_mean <- aggregate(dataMonth$SO2~dataMonth$year+dataMonth$month+dataMonth$sig_cd, dataMonth, narmMean)
CO_mean <- aggregate(dataMonth$CO~dataMonth$year+dataMonth$month+dataMonth$sig_cd, dataMonth, narmMean)
O3_mean <- aggregate(dataMonth$O3~dataMonth$year+dataMonth$month+dataMonth$sig_cd, dataMonth, narmMean)
NO2_mean <- aggregate(dataMonth$NO2~dataMonth$year+dataMonth$month+dataMonth$sig_cd, dataMonth, narmMean)
PM10_mean <- aggregate(dataMonth$PM10~dataMonth$year+dataMonth$month+dataMonth$sig_cd, dataMonth, narmMean)

#월별 측정소의 대기오염 농도 평균 조인
SO2_mean1 <- paste(SO2_mean$`dataMonth$year`,SO2_mean$`dataMonth$month`,SO2_mean$`dataMonth$sig_cd`)
CO_mean1 <- paste(CO_mean$`dataMonth$year`,CO_mean$`dataMonth$month`,CO_mean$`dataMonth$sig_cd`)
O3_mean1 <- paste(O3_mean$`dataMonth$year`,O3_mean$`dataMonth$month`,O3_mean$`dataMonth$sig_cd`)
NO2_mean1 <- paste(NO2_mean$`dataMonth$year`,NO2_mean$`dataMonth$month`,NO2_mean$`dataMonth$sig_cd`)
PM10_mean1 <- paste(PM10_mean$`dataMonth$year`,PM10_mean$`dataMonth$month`,PM10_mean$`dataMonth$sig_cd`)

SO2_mean2 <- data.frame(cbind(day=SO2_mean1, SO2=SO2_mean$`dataMonth$SO2`))
CO_mean2 <- data.frame(cbind(day=CO_mean1, CO=CO_mean$`dataMonth$CO`))
O3_mean2 <- data.frame(cbind(day=O3_mean1, O3=O3_mean$`dataMonth$O3`))
NO2_mean2 <- data.frame(cbind(day=NO2_mean1, NO2=NO2_mean$`dataMonth$NO2`))
PM10_mean2 <- data.frame(cbind(day=PM10_mean1, PM10=PM10_mean$`dataMonth$PM10`))

data_2003_mean_bind1 <- merge(SO2_mean2, CO_mean2, by='day', all = TRUE)
data_2003_mean_bind2 <- merge(data_2003_mean_bind1, O3_mean2, by='day', all = TRUE)
data_2003_mean_bind3 <- merge(data_2003_mean_bind2, NO2_mean2, by='day', all = TRUE)
data_2003_mean_bind4 <- merge(data_2003_mean_bind3, PM10_mean2, by='day', all = TRUE)

#output
write.csv(data_2003_mean_bind4, "D:\\01 Study\\08 pm\\processing\\input\\result\\pm10_sig_matching_75_mean_agg.csv")
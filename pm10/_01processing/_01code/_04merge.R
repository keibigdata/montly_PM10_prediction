##merge air pollution & weather

#remove variable
rm(list=ls())

##weather data 
weatherMonth <- read.csv("D:/my-backup/project/pm10/_01processing/_00input/_04merge/monthResult_weather.csv", header=T, sep=",", stringsAsFactors = FALSE)
##pollutantMonth
pollutantMonth <- read.csv("D:/my-backup/project/pm10/_01processing/_02output/_03meanOfAirPollutionOfSigungu/pm10_sig_matching_75_mean_agg.csv", header=T, sep=",", stringsAsFactors = FALSE)
##china
bAggr <- read.csv("D:/my-backup/project/pm10/_01processing/_02output/_00-8preprocessingChina/Beijing_aggr.csv", header=T, sep=",", stringsAsFactors = FALSE)
sAggr <- read.csv("D:/my-backup/project/pm10/_01processing/_02output/_00-8preprocessingChina/Shanghai_aggr.csv", header=T, sep=",", stringsAsFactors = FALSE)
bDist <- read.csv("D:/my-backup/project/pm10/_01processing/_02output/_00-7distanceToChina/distance_beijing.csv", header=T, sep=",", stringsAsFactors = FALSE)
sDist <- read.csv("D:/my-backup/project/pm10/_01processing/_02output/_00-7distanceToChina/distance_shanghai.csv", header=T, sep=",", stringsAsFactors = FALSE)
##emission
emData <- read.csv("D:/01 Study/08 pm/processing/input/emission_joined.csv", header=T, sep=",", stringsAsFactors = FALSE)



colnames(weatherMonth)[1] <- "weather_cd"
merged <- merge(x=pollutantMonth, y=weatherMonth, by=c("year", "month", "weather_cd"), all.x=TRUE)
merged <- merge(x=merged, y=bAggr, by=c("year", "month"), all.x=TRUE)
merged <- merge(x=merged, y=sAggr, by=c("year", "month"), all.x=TRUE)
merged <- merge(x=merged, y=bDist, by=c("sig_cd"), all.x=TRUE)
merged <- merge(x=merged, y=sDist, by=c("sig_cd"), all.x=TRUE)
merged <- merge(x=merged, y=emData, by=c("sig_cd", "year"), all.x=TRUE)



write.csv(merged, "D:\\01 Study\\08 pm\\processing\\input\\result\\pm10_sig_matching_75_mean_agg_weather_merged2.csv")

str(merged)




yearMonthWeather <- paste(weatherMonth$year, weatherMonth$month, weatherMonth$ÁöÁ¡)
yearMonthWeatherInPollutant <- paste(pollutantMonth$year, pollutantMonth$month, pollutantMonth$weather_cd)

weatherMonth <- cbind(weatherMonth, day=yearMonthWeather)
pollutantMonth <- cbind(pollutantMonth, day=yearMonthWeatherInPollutant)


merged <- merge(x=pollutantMonth, y=weatherMonth, by='day', all.x=TRUE)
write.csv(merged, "D:\\01 Study\\08 pm\\processing\\input\\result\\pm10_sig_matching_75_mean_agg_weather_merged.csv")





merged.edt.na <- read.csv("D:\\01 Study\\08 pm\\processing\\input\\result\\pm10_sig_matching_75_mean_agg_weather_merged_edt.csv", header=T, sep=",", stringsAsFactors = FALSE)
merged.edt.na[is.na(merged.edt.na)] <- ""
write.csv(merged.edt.na, "D:\\01 Study\\08 pm\\processing\\input\\result\\pm10_sig_matching_75_mean_agg_weather_merged_edt_narm.csv")

##merge air pollution & weather

#remove variable
rm(list=ls())

##weather data 
weatherMonth <- read.csv("D:/my-backup/project/pm10/_01processing/_00input/_04merge/monthResult_weather.csv", header=T, sep=",")
pollutantMonth <- read.csv("D:/my-backup/project/pm10/_01processing/_02output/_03meanOfAirPollutionOfSigungu/pm10_sig_matching_75_mean_agg.csv", header=T, sep=",")

colnames(weatherMonth)[1] <- "weather_cd"
merged <- merge(x=pollutantMonth, y=weatherMonth, by=c("year", "month", "weather_cd"), all.x=TRUE)






yearMonthWeather <- paste(weatherMonth$year, weatherMonth$month, weatherMonth$ÁöÁ¡)
yearMonthWeatherInPollutant <- paste(pollutantMonth$year, pollutantMonth$month, pollutantMonth$weather_cd)

weatherMonth <- cbind(weatherMonth, day=yearMonthWeather)
pollutantMonth <- cbind(pollutantMonth, day=yearMonthWeatherInPollutant)


merged <- merge(x=pollutantMonth, y=weatherMonth, by='day', all.x=TRUE)
write.csv(merged, "D:\\01 Study\\08 pm\\processing\\input\\result\\pm10_sig_matching_75_mean_agg_weather_merged.csv")
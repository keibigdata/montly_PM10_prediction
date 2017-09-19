##merge air pollution & weather

#remove variable
rm(list=ls())

##weatherData
weatherMonth <- read.csv("D:/my-backup/project/pm10/_01processing/_00input/_04merge/monthResult_weather.csv", header=T, sep=",", stringsAsFactors = FALSE)
weatherData <- weatherMonth[,c(1,4:10,13:16,19:21,24:26,28:31,35:38,41:45,48:55,58:59,61:70)]
colnames(weatherData) <- c("WEATHER_CD","YEAR","MONTH","MEAN_TEMP","MEAN_MAX_TEMP",
                  "MEAN_MIN_TEMP","MAX_TEMP","MIN_TEMP","MEAN_PRES","MEAN_SEA_PRES",
                  "MAX_SEA_PRES","MIN_SEA_PRES","MEAN_WATER_PRES","MAX_WATER_PRES","MIN_WATER_PRES",
                  "MEAN_DEW_TEMP","MEAN_HUM","MIN_HUM","SUM_PRECI","DAY_MAX_PRECI",
                  "HOUR_MAX_PRECI","TEN_MINU_MAX_PRECI","SMA_EVAPO","SMA_MAX_EVAPO","BIG_EVAPO",
                  "BIG_MAX_EVAPO","MEAN_WIND_SPED","MAX_WIND_SPED","MAX_INST_WIND_SPED","MAX_WIND_DIR",
                  "MAX_INST_WIND_DIR","MEAN_CLOUD","MEAN_LOWMID_CLOUD","SUM_SUN","PERC_SUN",
                  "SUM_GLO_RAD","MAX_SNOW_DEP","MAX_NEW_SNOW_DEP","SUM_SNOW","MEAN_MIN_GRA_TEMP",
                  "MIN_GRA_TEMP","MEAN_SURF_TEMP","MEAN_SOIL_TEMP_0.05","MEAN_SOIL_TEMP_0.1","MEAN_SOIL_TEMP_0.2",
                  "MEAN_SOIL_TEMP_0.3","MEAN_SOIL_TEMP_0.5","MEAN_SOIL_TEMP_1.0","MEAN_SOIL_TEMP_1.5","MEAN_SOIL_TEMP_3.0",
                  "MEAN_SOIL_TEMP_5.0")

##pollutantData
pollutantMonth <- read.csv("D:/my-backup/project/pm10/_01processing/_02output/_03meanOfAirPollutionOfSigungu/pm10_sig_matching_75_mean_agg.csv", header=T, sep=",", stringsAsFactors = FALSE)
pollutantData <- pollutantMonth
colnames(pollutantData) <- c("YEAR","MONTH","SIG_CD","WEATHER_CD","SO2","CO","O3","NO2","PM10")

##chinaData
bAggr <- read.csv("D:/my-backup/project/pm10/_01processing/_02output/_00-8preprocessingChina/Beijing_aggr.csv", header=T, sep=",", stringsAsFactors = FALSE)
sAggr <- read.csv("D:/my-backup/project/pm10/_01processing/_02output/_00-8preprocessingChina/Shanghai_aggr.csv", header=T, sep=",", stringsAsFactors = FALSE)
bDist <- read.csv("D:/my-backup/project/pm10/_01processing/_02output/_00-7distanceToChina/distance_beijing.csv", header=T, sep=",", stringsAsFactors = FALSE)
sDist <- read.csv("D:/my-backup/project/pm10/_01processing/_02output/_00-7distanceToChina/distance_shanghai.csv", header=T, sep=",", stringsAsFactors = FALSE)
bDist <- bDist[,c(1,3)]
sDist <- sDist[,c(1,3)]
colnames(bAggr) <- c("YEAR","MONTH","BEIJING_PM2.5")
colnames(sAggr) <- c("YEAR","MONTH","SHANGHAI_PM2.5")
colnames(bDist) <- c("SIG_CD","BEIJING_DIST")
colnames(sDist) <- c("SIG_CD","SHANGHAI_DIST")

##emission
emData <- read.csv("D:/my-backup/project/pm10/_01processing/_02output/_00-6preprocessingEmissionData/emission_joined.csv", header=T, sep=",", stringsAsFactors = FALSE)
emissionData <- emData[,c(1:2,7:14,19:26,31:38,43:50,55:62,67:74,79:86,91:98,103:110)]
colnames(emissionData) <- c("SIG_CD","YEAR",
                            "EM1_CO","EM1_NOX","EM1_SOX","EM1_TSP","EM1_PM10","EM1_VOC","EM1_NH3","EM1_PM2.5",
                            "EM2_CO","EM2_NOX","EM2_SOX","EM2_TSP","EM2_PM10","EM2_VOC","EM2_NH3","EM2_PM2.5",
                            "EM3_CO","EM3_NOX","EM3_SOX","EM3_TSP","EM3_PM10","EM3_VOC","EM3_NH3","EM3_PM2.5",
                            "EM4_CO","EM4_NOX","EM4_SOX","EM4_TSP","EM4_PM10","EM4_VOC","EM4_NH3","EM4_PM2.5",
                            "EM5_CO","EM5_NOX","EM5_SOX","EM5_TSP","EM5_PM10","EM5_VOC","EM5_NH3","EM5_PM2.5",
                            "EM6_CO","EM6_NOX","EM6_SOX","EM6_TSP","EM6_PM10","EM6_VOC","EM6_NH3","EM6_PM2.5",
                            "EM7_CO","EM7_NOX","EM7_SOX","EM7_TSP","EM7_PM10","EM7_VOC","EM7_NH3","EM7_PM2.5",
                            "EM8_CO","EM8_NOX","EM8_SOX","EM8_TSP","EM8_PM10","EM8_VOC","EM8_NH3","EM8_PM2.5",
                            "EM9_CO","EM9_NOX","EM9_SOX","EM9_TSP","EM9_PM10","EM9_VOC","EM9_NH3","EM9_PM2.5")


##population density
pdData <- read.csv("D:/my-backup/project/pm10/_01processing/_02output/_00-10mergePopulationDensity/_00-10mergePopulationDensity.csv", header=T, sep=",", stringsAsFactors = FALSE)
colnames(pdData)
populationData <- pdData[,4:6]

##yellowDust 
ydData <- read.csv("D:/my-backup/project/pm10/_01processing/_02output/_00-12mergeYellowDust/_00-12mergeYellowDust.csv", header=T, sep=",", stringsAsFactors = FALSE)
ydData1 <- read.csv("D:/my-backup/project/pm10/_01processing/_02output/_00-12mergeYellowDust/_00-12mergeYellowDust1.csv", header=T, sep=",", stringsAsFactors = FALSE)

yellowdustData <- ydData[,c(2:5)]
yellowdustData1 <- ydData1[,c(1,3:5)]
colnames(yellowdustData) <- c("WEATHER_CD","YEAR","YD_FREQ","MONTH")
colnames(yellowdustData1) <- c("WEATHER_CD","MONTH","YD_FREQ1","YEAR")

##coordinate
cdData <- read.csv("D:/my-backup/project/pm10/_01processing/_02output/_00-13coordinate/SIG_CENTROID_WGS84.csv", header=T, sep=",", stringsAsFactors = FALSE)
colnames(cdData)
coordinateData <- cdData[,c(2,5,6)]
colnames(coordinateData)


##china aqi data
beijing_aqi <- read.csv("D:/my-backup/project/pm10/_01processing/_02output/_00-8preprocessingChina/Beijing_AQI_aggr.csv", header=T, sep=",", stringsAsFactors = FALSE)
tianjin_aqi <- read.csv("D:/my-backup/project/pm10/_01processing/_02output/_00-8preprocessingChina/Tianjin_AQI_aggr.csv", header=T, sep=",", stringsAsFactors = FALSE)

colnames(beijing_aqi) <- c("YEAR","MONTH","BEIJING_AQI")
colnames(tianjin_aqi) <- c("YEAR","MONTH","TIANJIN_AQI")


########################################################################################################################################################################################
##MERGE
##1 pollutantData <- weatherData
merged <- merge(x=pollutantData, y=weatherData, by=c("YEAR", "MONTH", "WEATHER_CD"), all.x=TRUE)
colnames(merged)

##2 merged <- china data
merged <- merge(x=merged, y=bAggr, by=c("YEAR", "MONTH"), all.x=TRUE)
merged <- merge(x=merged, y=sAggr, by=c("YEAR", "MONTH"), all.x=TRUE)
merged <- merge(x=merged, y=bDist, by=c("SIG_CD"), all.x=TRUE)
merged <- merge(x=merged, y=sDist, by=c("SIG_CD"), all.x=TRUE)

merged[,"BEIJIING_PM2,5_STD"] <- merged[,"BEIJING_PM2.5"] * merged[,"BEIJING_DIST"] 
merged[,"SHANGHAI_PM2,5_STD"] <- merged[,"SHANGHAI_PM2.5"] * merged[,"SHANGHAI_DIST"] 

##3 merged <- emissionData
merged <- merge(x=merged, y=emissionData, by=c("SIG_CD", "YEAR"), all.x=TRUE)

##4 merged <- population density
merged <- merge(x=merged, y=populationData, by=c("SIG_CD", "YEAR"), all.x=TRUE)

##5 merged <- yellowdustData
merged <- merge(x=merged, y=yellowdustData, by=c("WEATHER_CD", "YEAR", "MONTH"), all.x=TRUE)
#merged <- merge(x=merged, y=yellowdustData1, by=c("WEATHER_CD", "YEAR", "MONTH"), all.x=TRUE)

##6 merged <- coordinateData
merged <- merge(x=merged, y=coordinateData, by=c("SIG_CD"), all.x=TRUE)

##7 merged <- china aqi data
merged <- merge(x=merged, y=beijing_aqi, by=c("YEAR","MONTH"), all.x=TRUE)
merged <- merge(x=merged, y=tianjin_aqi, by=c("YEAR","MONTH"), all.x=TRUE)

########################################################################################################################################################################################
##save!!
# all data
merged.all <- merged[,c(-3)]
merged.all[is.na(merged.all)] <- ""
write.csv(merged.all, "D:/my-backup/project/pm10/_01processing/_02output/_04merge/merged_All.csv", row.names=FALSE)

merged.ex1 <- merged.all[merged.all[,"YEAR"]<2007,]
merged.ex2 <- merged.all[merged.all[,"YEAR"]>2007,]
merged.ex3 <- merged.all[merged.all[,"YEAR"]>=2011 & merged.all[,"YEAR"]<=2013,]
merged.ex4 <- merged.all[merged.all[,"YEAR"]>2013,]

write.csv(merged.ex1, "D:/my-backup/project/pm10/_01processing/_02output/_04merge/merged.ex1.csv", row.names=FALSE)
write.csv(merged.ex2, "D:/my-backup/project/pm10/_01processing/_02output/_04merge/merged.ex2.csv", row.names=FALSE)
write.csv(merged.ex3, "D:/my-backup/project/pm10/_01processing/_02output/_04merge/merged.ex3.csv", row.names=FALSE)
write.csv(merged.ex4, "D:/my-backup/project/pm10/_01processing/_02output/_04merge/merged.ex4.csv", row.names=FALSE)

########################################################################################################################################################################################
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

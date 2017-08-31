##mean of Beijing

#remove variable
rm(list=ls())

#define function
narmMean <- function(values){
  if(length(values[values!=NA])>=540){
    mean(values, na.rm=TRUE)
  } else {
    NA
  }
}

#load data
b_08 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Beijing_2008.csv")
b_09 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Beijing_2009.csv")
b_10 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Beijing_2010.csv")
b_11 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Beijing_2011.csv")
b_12 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Beijing_2012.csv")
b_13 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Beijing_2013.csv")
b_14 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Beijing_2014.csv")
b_15 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Beijing_2015.csv")
b_16 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Beijing_2016.csv")

#merge
bData = rbind(b_08, b_09, b_10, b_11, b_12, b_13, b_14, b_15, b_16)

#missing data
bData[bData$Value == -999, "Value"] <- NA

#aggregate
bData <- bData[,c("Year","Month","Value")]
bData.aggr <- aggregate(x=bData[c("Value")],
                             by=bData[c("Year","Month")],
                             FUN=narmMean) #,
                             #na.rm=TRUE)

write.csv(bData.aggr, "D:/my-backup/project/pm10/_01processing/_02output/_00-8preprocessingChina/Beijing_aggr.csv", row.names = FALSE)


##mean of Shanghai
#load data
s_11 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Shanghai_2011.csv")
s_12 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Shanghai_2012.csv")
s_13 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Shanghai_2013.csv")
s_14 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Shanghai_2014.csv")
s_15 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Shanghai_2015.csv")
s_16 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Shanghai_2016.csv")

#merge
sData = rbind(s_11, s_12, s_13, s_14, s_15, s_16)

#missing data
sData[sData$Value == -999, "Value"] <- NA

#aggregate
sData <- sData[,c("Year","Month","Value")]
sData.aggr <- aggregate(x=sData[c("Value")],
                        by=sData[c("Year","Month")],
                        FUN=narmMean)

write.csv(sData.aggr, "D:/my-backup/project/pm10/_01processing/_02output/_00-8preprocessingChina/Shanghai_aggr.csv", row.names = FALSE)

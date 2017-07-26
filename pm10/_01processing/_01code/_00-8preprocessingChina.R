##mean of Beijing

#remove variable
rm(list=ls())

b_08 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Beijing_2008.csv")
b_09 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Beijing_2009.csv")
b_10 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Beijing_2010.csv")
b_11 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Beijing_2011.csv")
b_12 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Beijing_2012.csv")
b_13 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Beijing_2013.csv")
b_14 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Beijing_2014.csv")
b_15 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Beijing_2015.csv")
b_16 = read.csv("D:/my-backup/project/pm10/_01processing/_00input/_00-8preprocessingChina/Beijing_2016.csv")

bData = rbind(b_08, b_09, b_10, b_11, b_12, b_13, b_14, b_15, b_16)
bData[bData$Value == -999, "value"] <- NA

bData.aggr <- aggregate(x=bData[c("Value")],
                             by=aggregate[c("YEAR","SIDO","SIGUNGU","SIGUNGU2","SIG_CD","LARGECATE")],
                             FUN=sum,
                             na.rm=TRUE)






SO2_mean <- aggregate(dataMonth$SO2~dataMonth$month+dataMonth$area, dataMonth, narmMean)
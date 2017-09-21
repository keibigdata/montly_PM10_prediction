##preprocess yellow dust data

#######################################################################################################

#yellow dust data
yd = read.csv("D:/my-backup/project/pm10/_01processing/_02output/_00-11yellowDust/_00-11yellowDust.csv", header = T)

#column -> row
yd01 = yd[,1:4]
yd02 = yd[,c(1:3,5)]
yd03 = yd[,c(1:3,6)]
yd04 = yd[,c(1:3,7)]
yd05 = yd[,c(1:3,8)]
yd06 = yd[,c(1:3,9)]
yd07 = yd[,c(1:3,10)]
yd08 = yd[,c(1:3,11)]
yd09 = yd[,c(1:3,12)]
yd10 = yd[,c(1:3,13)]
yd11 = yd[,c(1:3,14)]
yd12 = yd[,c(1:3,15)]

yd01[,"MONTH"] = 1 
yd02[,"MONTH"] = 2 
yd03[,"MONTH"] = 3 
yd04[,"MONTH"] = 4 
yd05[,"MONTH"] = 5 
yd06[,"MONTH"] = 6 
yd07[,"MONTH"] = 7 
yd08[,"MONTH"] = 8 
yd09[,"MONTH"] = 9 
yd10[,"MONTH"] = 10 
yd11[,"MONTH"] = 11 
yd12[,"MONTH"] = 12 

colnames(yd01) <- c("NAME", "CODE", "YEAR", "YD", "MONTH")
colnames(yd02) <- c("NAME", "CODE", "YEAR", "YD", "MONTH")
colnames(yd03) <- c("NAME", "CODE", "YEAR", "YD", "MONTH")
colnames(yd04) <- c("NAME", "CODE", "YEAR", "YD", "MONTH")
colnames(yd05) <- c("NAME", "CODE", "YEAR", "YD", "MONTH")
colnames(yd06) <- c("NAME", "CODE", "YEAR", "YD", "MONTH")
colnames(yd07) <- c("NAME", "CODE", "YEAR", "YD", "MONTH")
colnames(yd08) <- c("NAME", "CODE", "YEAR", "YD", "MONTH")
colnames(yd09) <- c("NAME", "CODE", "YEAR", "YD", "MONTH")
colnames(yd10) <- c("NAME", "CODE", "YEAR", "YD", "MONTH")
colnames(yd11) <- c("NAME", "CODE", "YEAR", "YD", "MONTH")
colnames(yd12) <- c("NAME", "CODE", "YEAR", "YD", "MONTH")

yd.merged <- rbind(yd01,yd02,yd03,yd04,yd05,yd06,yd07,yd08,yd09,yd10,yd11,yd12)
write.csv(yd.merged, "D:/my-backup/project/pm10/_01processing/_02output/_00-12mergeYellowDust/_00-12mergeYellowDust.csv", row.names = FALSE)


#yellow dust1 data
yd1 = read.csv("D:/my-backup/project/pm10/_01processing/_02output/_00-11yellowDust/_00-11yellowDust1.csv", header = T)

#column -> row
yd1.01 = yd1[,c(1:3,4)]
yd1.02 = yd1[,c(1:3,5)]
yd1.03 = yd1[,c(1:3,6)]
yd1.04 = yd1[,c(1:3,7)]
yd1.05 = yd1[,c(1:3,8)]
yd1.06 = yd1[,c(1:3,9)]
yd1.07 = yd1[,c(1:3,10)]
yd1.08 = yd1[,c(1:3,11)]
yd1.09 = yd1[,c(1:3,12)]
yd1.10 = yd1[,c(1:3,13)]
yd1.11 = yd1[,c(1:3,14)]
yd1.12 = yd1[,c(1:3,15)]
yd1.13 = yd1[,c(1:3,16)]
yd1.14 = yd1[,c(1:3,17)]
yd1.15 = yd1[,c(1:3,18)]
yd1.16 = yd1[,c(1:3,19)]

yd1.01[,"YEAR"] = 2001 
yd1.02[,"YEAR"] = 2002 
yd1.03[,"YEAR"] = 2003 
yd1.04[,"YEAR"] = 2004 
yd1.05[,"YEAR"] = 2005 
yd1.06[,"YEAR"] = 2006 
yd1.07[,"YEAR"] = 2007 
yd1.08[,"YEAR"] = 2008 
yd1.09[,"YEAR"] = 2009 
yd1.10[,"YEAR"] = 2010 
yd1.11[,"YEAR"] = 2011 
yd1.12[,"YEAR"] = 2012 
yd1.13[,"YEAR"] = 2013 
yd1.14[,"YEAR"] = 2014 
yd1.15[,"YEAR"] = 2015 
yd1.16[,"YEAR"] = 2016 

colnames(yd1.01) <- c("CODE", "NAME", "MONTH", "YD", "YEAR")
colnames(yd1.02) <- c("CODE", "NAME", "MONTH", "YD", "YEAR")
colnames(yd1.03) <- c("CODE", "NAME", "MONTH", "YD", "YEAR")
colnames(yd1.04) <- c("CODE", "NAME", "MONTH", "YD", "YEAR")
colnames(yd1.05) <- c("CODE", "NAME", "MONTH", "YD", "YEAR")
colnames(yd1.06) <- c("CODE", "NAME", "MONTH", "YD", "YEAR")
colnames(yd1.07) <- c("CODE", "NAME", "MONTH", "YD", "YEAR")
colnames(yd1.08) <- c("CODE", "NAME", "MONTH", "YD", "YEAR")
colnames(yd1.09) <- c("CODE", "NAME", "MONTH", "YD", "YEAR")
colnames(yd1.10) <- c("CODE", "NAME", "MONTH", "YD", "YEAR")
colnames(yd1.11) <- c("CODE", "NAME", "MONTH", "YD", "YEAR")
colnames(yd1.12) <- c("CODE", "NAME", "MONTH", "YD", "YEAR")
colnames(yd1.13) <- c("CODE", "NAME", "MONTH", "YD", "YEAR")
colnames(yd1.14) <- c("CODE", "NAME", "MONTH", "YD", "YEAR")
colnames(yd1.15) <- c("CODE", "NAME", "MONTH", "YD", "YEAR")
colnames(yd1.16) <- c("CODE", "NAME", "MONTH", "YD", "YEAR")

yd1.merged <- rbind(yd1.01,yd1.02,yd1.03,yd1.04,yd1.05,yd1.06,yd1.07,yd1.08,yd1.09,yd1.10,yd1.11,yd1.12,yd1.13,yd1.14,yd1.15,yd1.16)
write.csv(yd1.merged, "D:/my-backup/project/pm10/_01processing/_02output/_00-12mergeYellowDust/_00-12mergeYellowDust1.csv", row.names = FALSE)



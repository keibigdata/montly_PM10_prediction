##preprocess population density data

#population density data
pd = read.csv("D:/my-backup/project/pm10/_01processing/_02output/_00-9populationDensity/_00-9populationDensity.csv", header = T,)

#column -> row
pd00 = pd[,1:5]
pd01 = pd[,c(1:4,6)]
pd02 = pd[,c(1:4,7)]
pd03 = pd[,c(1:4,8)]
pd04 = pd[,c(1:4,9)]
pd05 = pd[,c(1:4,10)]
pd06 = pd[,c(1:4,11)]
pd07 = pd[,c(1:4,12)]
pd08 = pd[,c(1:4,13)]
pd09 = pd[,c(1:4,14)]
pd10 = pd[,c(1:4,15)]
pd11 = pd[,c(1:4,16)]
pd12 = pd[,c(1:4,17)]
pd13 = pd[,c(1:4,18)]
pd14 = pd[,c(1:4,19)]
pd15 = pd[,c(1:4,20)]
pd16 = pd[,c(1:4,21)]

pd00[,"YEAR"] = 2000 
pd01[,"YEAR"] = 2001 
pd02[,"YEAR"] = 2002 
pd03[,"YEAR"] = 2003 
pd04[,"YEAR"] = 2004 
pd05[,"YEAR"] = 2005 
pd06[,"YEAR"] = 2006 
pd07[,"YEAR"] = 2007 
pd08[,"YEAR"] = 2008 
pd09[,"YEAR"] = 2009 
pd10[,"YEAR"] = 2010 
pd11[,"YEAR"] = 2011 
pd12[,"YEAR"] = 2012 
pd13[,"YEAR"] = 2013 
pd14[,"YEAR"] = 2014 
pd15[,"YEAR"] = 2015 
pd16[,"YEAR"] = 2016 

colnames(pd00) <- c("SIDO", "SIGUNGU", "SIGUNGU2", "SIG_CD", "POP_DEN", "YEAR")
colnames(pd01) <- c("SIDO", "SIGUNGU", "SIGUNGU2", "SIG_CD", "POP_DEN", "YEAR")
colnames(pd02) <- c("SIDO", "SIGUNGU", "SIGUNGU2", "SIG_CD", "POP_DEN", "YEAR")
colnames(pd03) <- c("SIDO", "SIGUNGU", "SIGUNGU2", "SIG_CD", "POP_DEN", "YEAR")
colnames(pd04) <- c("SIDO", "SIGUNGU", "SIGUNGU2", "SIG_CD", "POP_DEN", "YEAR")
colnames(pd05) <- c("SIDO", "SIGUNGU", "SIGUNGU2", "SIG_CD", "POP_DEN", "YEAR")
colnames(pd06) <- c("SIDO", "SIGUNGU", "SIGUNGU2", "SIG_CD", "POP_DEN", "YEAR")
colnames(pd07) <- c("SIDO", "SIGUNGU", "SIGUNGU2", "SIG_CD", "POP_DEN", "YEAR")
colnames(pd08) <- c("SIDO", "SIGUNGU", "SIGUNGU2", "SIG_CD", "POP_DEN", "YEAR")
colnames(pd09) <- c("SIDO", "SIGUNGU", "SIGUNGU2", "SIG_CD", "POP_DEN", "YEAR")
colnames(pd10) <- c("SIDO", "SIGUNGU", "SIGUNGU2", "SIG_CD", "POP_DEN", "YEAR")
colnames(pd11) <- c("SIDO", "SIGUNGU", "SIGUNGU2", "SIG_CD", "POP_DEN", "YEAR")
colnames(pd12) <- c("SIDO", "SIGUNGU", "SIGUNGU2", "SIG_CD", "POP_DEN", "YEAR")
colnames(pd13) <- c("SIDO", "SIGUNGU", "SIGUNGU2", "SIG_CD", "POP_DEN", "YEAR")
colnames(pd14) <- c("SIDO", "SIGUNGU", "SIGUNGU2", "SIG_CD", "POP_DEN", "YEAR")
colnames(pd15) <- c("SIDO", "SIGUNGU", "SIGUNGU2", "SIG_CD", "POP_DEN", "YEAR")
colnames(pd16) <- c("SIDO", "SIGUNGU", "SIGUNGU2", "SIG_CD", "POP_DEN", "YEAR")

pd.merged <- rbind(pd00,pd01,pd02,pd03,pd04,pd05,pd06,pd07,pd08,pd09,pd10,pd11,pd12,pd13,pd14,pd15,pd16)

write.csv(pd.merged, "D:/my-backup/project/pm10/_01processing/_02output/_00-10mergePopulationDensity/_00-10mergePopulationDensity.csv")




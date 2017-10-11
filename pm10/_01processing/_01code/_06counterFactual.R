## counterfactual analysis

varVec = c("SO2","CO","O3","NO2","MEAN_PRES","MEAN_SEA_PRES", "MAX_SEA_PRES", "MIN_SEA_PRES","MAX_WATER_PRES","SUM_PRECI","MEAN_WIND_SPED","SUM_SUN","BEIJING_PM2.5", "BEIJIING_PM2.5_STD","POP_DEN","YD_FREQ")
varVec2 = c("SHANGHAI_PM2.5", "SHANGHAI_PM2.5_STD")
varVec3 = c("BEIJING_AQI", "TIANJIN_AQI")

#####################################
df.diff <- data.frame()
for (i in 1:length(varVec)){
  
  cf.newdata.ex3 <- merged.ex3[ind.ex3==2 & !is.na(merged.ex3[,"PM10"]),]
  cf.newdata.ex3[,varVec[i]] <- cf.newdata.ex3[,varVec[i]] * 0.7
  
  cf.newdata.ex4 <- merged.ex4[ind.ex4==2 & !is.na(merged.ex4[,"PM10"]),]
  cf.newdata.ex4[,varVec[i]] <- cf.newdata.ex4[,varVec[i]] * 0.7
  
  cf.newdata.ex5 <- merged.ex5[ind.ex5==2 & !is.na(merged.ex5[,"PM10"]),]
  cf.newdata.ex5[,varVec[i]] <- cf.newdata.ex5[,varVec[i]] * 0.7
  
  cf.newdata.ex6 <- merged.ex6[ind.ex6==2 & !is.na(merged.ex6[,"PM10"]),]
  cf.newdata.ex6[,varVec[i]] <- cf.newdata.ex6[,varVec[i]] * 0.7
  
  gbm1.cf.ex3 = predict(gbm1.ex3, newdata=cf.newdata.ex3, n.trees=5000, type="response", predict.all = TRUE)
  gbm1.cf.ex4 = predict(gbm1.ex4, newdata=cf.newdata.ex4, n.trees=5000, type="response", predict.all = TRUE)
  gbm1.cf.ex5 = predict(gbm1.ex5, newdata=cf.newdata.ex5, n.trees=5000, type="response", predict.all = TRUE)
  gbm1.cf.ex6 = predict(gbm1.ex6, newdata=cf.newdata.ex6, n.trees=5000, type="response", predict.all = TRUE)
  
  rf.cf.ex3 = predict(rf.ex3, newdata=cf.newdata.ex3, type="response", predict.all = TRUE)
  rf.cf.ex4 = predict(rf.ex4, newdata=cf.newdata.ex4, type="response", predict.all = TRUE)
  rf.cf.ex5 = predict(rf.ex5, newdata=cf.newdata.ex5, type="response", predict.all = TRUE)
  rf.cf.ex6 = predict(rf.ex6, newdata=cf.newdata.ex6, type="response", predict.all = TRUE)
  
  ind.na.ex3 = !is.na(gbm1.cf.ex3)
  diff.gbm.ex3 = mean(gbm1.cf.ex3[ind.na.ex3]-rf.test.ex3[ind.na.ex3]) / mean(rf.test.ex3[ind.na.ex3]) * 100
  
  ind.na.ex4 = !is.na(gbm1.cf.ex4)
  diff.gbm.ex4 = mean(gbm1.cf.ex4[ind.na.ex4]-rf.test.ex4[ind.na.ex4]) / mean(rf.test.ex4[ind.na.ex4]) * 100
  
  ind.na.ex5 = !is.na(gbm1.cf.ex5)
  diff.gbm.ex5 = mean(gbm1.cf.ex5[ind.na.ex5]-rf.test.ex5[ind.na.ex5]) / mean(rf.test.ex5[ind.na.ex5]) * 100

  ind.na.ex6 = !is.na(gbm1.cf.ex6)
  diff.gbm.ex6 = mean(gbm1.cf.ex6[ind.na.ex6]-rf.test.ex6[ind.na.ex6]) / mean(rf.test.ex6[ind.na.ex6]) * 100

  ind.na.ex3 = !is.na(rf.cf.ex3$aggregate)
  diff.rf.ex3 = mean(rf.cf.ex3$aggregate[ind.na.ex3]-rf.test.ex3[ind.na.ex3]) / mean(rf.test.ex3[ind.na.ex3]) * 100
  
  ind.na.ex4 = !is.na(rf.cf.ex4$aggregate)
  diff.rf.ex4 = mean(rf.cf.ex4$aggregate[ind.na.ex4]-rf.test.ex4[ind.na.ex4]) / mean(rf.test.ex4[ind.na.ex4]) * 100
  
  ind.na.ex5 = !is.na(rf.cf.ex5$aggregate)
  diff.rf.ex5 = mean(rf.cf.ex5$aggregate[ind.na.ex5]-rf.test.ex5[ind.na.ex5]) / mean(rf.test.ex5[ind.na.ex5]) * 100
  
  ind.na.ex6 = !is.na(rf.cf.ex6$aggregate)
  diff.rf.ex6 = mean(rf.cf.ex6$aggregate[ind.na.ex6]-rf.test.ex6[ind.na.ex6]) / mean(rf.test.ex6[ind.na.ex6]) * 100
  
  row <- data.frame(varVec[i], diff.gbm.ex3, diff.gbm.ex4, diff.gbm.ex5, diff.gbm.ex6, diff.rf.ex3, diff.rf.ex4, diff.rf.ex5, diff.rf.ex6)
  df.diff <- rbind(df.diff, row)
}



#####################################
df.diff2 <- data.frame()
for (i in 1:length(varVec2)){
  cf.newdata.ex4 <- merged.ex4[ind.ex4==2 & !is.na(merged.ex4[,"PM10"]),]
  cf.newdata.ex4[,varVec2[i]] <- cf.newdata.ex4[,varVec2[i]] * 0.7
  
  cf.newdata.ex5 <- merged.ex5[ind.ex5==2 & !is.na(merged.ex5[,"PM10"]),]
  cf.newdata.ex5[,varVec2[i]] <- cf.newdata.ex5[,varVec2[i]] * 0.7
  
  cf.newdata.ex6 <- merged.ex6[ind.ex6==2 & !is.na(merged.ex6[,"PM10"]),]
  cf.newdata.ex6[,varVec2[i]] <- cf.newdata.ex6[,varVec2[i]] * 0.7
  
  gbm1.cf.ex4 = predict(gbm1.ex4, newdata=cf.newdata.ex4, n.trees=5000, type="response", predict.all = TRUE)
  gbm1.cf.ex5 = predict(gbm1.ex5, newdata=cf.newdata.ex5, n.trees=5000, type="response", predict.all = TRUE)
  gbm1.cf.ex6 = predict(gbm1.ex6, newdata=cf.newdata.ex6, n.trees=5000, type="response", predict.all = TRUE)
  
  rf.cf.ex4 = predict(rf.ex4, newdata=cf.newdata.ex4, type="response", predict.all = TRUE)
  rf.cf.ex5 = predict(rf.ex5, newdata=cf.newdata.ex5, type="response", predict.all = TRUE)
  rf.cf.ex6 = predict(rf.ex6, newdata=cf.newdata.ex6, type="response", predict.all = TRUE)
  
  ind.na.ex4 = !is.na(gbm1.cf.ex4)
  diff.gbm.ex4 = mean(gbm1.cf.ex4[ind.na.ex4]-rf.test.ex4[ind.na.ex4]) / mean(rf.test.ex4[ind.na.ex4]) * 100
  
  ind.na.ex5 = !is.na(gbm1.cf.ex5)
  diff.gbm.ex5 = mean(gbm1.cf.ex5[ind.na.ex5]-rf.test.ex5[ind.na.ex5]) / mean(rf.test.ex5[ind.na.ex5]) * 100
  
  ind.na.ex6 = !is.na(gbm1.cf.ex6)
  diff.gbm.ex6 = mean(gbm1.cf.ex6[ind.na.ex6]-rf.test.ex6[ind.na.ex6]) / mean(rf.test.ex6[ind.na.ex6]) * 100
  
  ind.na.ex4 = !is.na(rf.cf.ex4$aggregate)
  diff.rf.ex4 = mean(rf.cf.ex4$aggregate[ind.na.ex4]-rf.test.ex4[ind.na.ex4]) / mean(rf.test.ex4[ind.na.ex4]) * 100 
  
  ind.na.ex5 = !is.na(rf.cf.ex5$aggregate)
  diff.rf.ex5 = mean(rf.cf.ex5$aggregate[ind.na.ex5]-rf.test.ex5[ind.na.ex5]) / mean(rf.test.ex5[ind.na.ex5]) * 100
  
  ind.na.ex6 = !is.na(rf.cf.ex6$aggregate)
  diff.rf.ex6 = mean(rf.cf.ex6$aggregate[ind.na.ex6]-rf.test.ex6[ind.na.ex6]) / mean(rf.test.ex6[ind.na.ex6]) * 100
  
  row <- data.frame(varVec2[i], diff.gbm.ex4, diff.gbm.ex5, diff.gbm.ex6, diff.rf.ex4, diff.rf.ex5, diff.rf.ex6)
  df.diff2 <- rbind(df.diff2, row)
}


#####################################
df.diff3 <- data.frame()
for (i in 1:length(varVec3)){
  
  cf.newdata.ex5 <- merged.ex5[ind.ex5==2 & !is.na(merged.ex5[,"PM10"]),]
  cf.newdata.ex5[,varVec3[i]] <- cf.newdata.ex5[,varVec3[i]] * 0.7

  gbm1.cf.ex5 = predict(gbm1.ex5, newdata=cf.newdata.ex5, n.trees=5000, type="response", predict.all = TRUE)
  
  rf.cf.ex5 = predict(rf.ex5, newdata=cf.newdata.ex5, type="response", predict.all = TRUE)
  
  ind.na.ex5 = !is.na(gbm1.cf.ex5)
  diff.gbm.ex5 = mean(gbm1.cf.ex5[ind.na.ex5]-rf.test.ex5[ind.na.ex5]) / mean(rf.test.ex5[ind.na.ex5]) * 100
  
  ind.na.ex5 = !is.na(rf.cf.ex5$aggregate)
  diff.rf.ex5 = mean(rf.cf.ex5$aggregate[ind.na.ex5]-rf.test.ex5[ind.na.ex5]) / mean(rf.test.ex5[ind.na.ex5]) * 100
  
  row <- data.frame(varVec3[i], diff.gbm.ex5,  diff.rf.ex5)
  df.diff3 <- rbind(df.diff3, row)
}

write.csv(df.diff, "/Users/kei/Documents/_temp_170921/_05variableImportance/cf.3/df_diff_dec30.csv")
write.csv(df.diff2, "/Users/kei/Documents/_temp_170921/_05variableImportance/cf.3/df_diff2_dec30.csv")
write.csv(df.diff3, "/Users/kei/Documents/_temp_170921/_05variableImportance/cf.3/df_diff3_dec30.csv")

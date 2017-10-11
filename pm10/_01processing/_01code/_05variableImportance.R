
##Data load
merged.ex1 = read.csv("D:/my-backup/project/pm10/_01processing/_02output/_04merge/merged.ex1.csv", header=T, sep=",", stringsAsFactors = FALSE)
merged.ex2 = read.csv("D:/my-backup/project/pm10/_01processing/_02output/_04merge/merged.ex2.csv", header=T, sep=",", stringsAsFactors = FALSE)
merged.ex3 = read.csv("D:/my-backup/project/pm10/_01processing/_02output/_04merge/merged.ex3.csv", header=T, sep=",", stringsAsFactors = FALSE)
merged.ex4 = read.csv("D:/my-backup/project/pm10/_01processing/_02output/_04merge/merged.ex4.csv", header=T, sep=",", stringsAsFactors = FALSE)
merged.ex5 = read.csv("D:/my-backup/project/pm10/_01processing/_02output/_04merge/merged.ex5.csv", header=T, sep=",", stringsAsFactors = FALSE)
merged.ex6 = read.csv("D:/my-backup/project/pm10/_01processing/_02output/_04merge/merged.ex6.csv", header=T, sep=",", stringsAsFactors = FALSE)

colnames(merged.ex1)
colnames(merged.ex2)
colnames(merged.ex3)
colnames(merged.ex4)
colnames(merged.ex5)
colnames(merged.ex6)

merged.colname <- cbind(colnames(merged.ex1), colnames(merged.ex2), colnames(merged.ex3), colnames(merged.ex4), colnames(merged.ex5), colnames(merged.ex6) )
write.csv(merged.colname, "/Users/kei/Documents/_temp_170921/_05variableImportance/merged.colname.csv")
##################################################################################################################################################
##################################################################################################################################################
##randomForest pk experiment
require(randomForest)
set.seed(1)

####################################### merged.ex1 #######################################
str(merged.ex1, list.len=ncol(merged.ex1))

#training 80, test 20
ind.ex1 = sample(2, nrow(merged.ex1), replace=TRUE, prob=c(0.8,0.2))
rf.ex1 = randomForest(formula=PM10~., data=merged.ex1[ind.ex1==1,], na.action=na.roughfix, importance=TRUE)

#prediction and MSE
rf.test.ex1 = merged.ex1[ind.ex1==2 & !is.na(merged.ex1[,"PM10"]),"PM10"]
rf.pred.ex1 = predict(rf.ex1, newdata=merged.ex1[ind.ex1==2 & !is.na(merged.ex1[,"PM10"]),], type="response", predict.all = TRUE)
ind.na.ex1 = !is.na(rf.pred.ex1$aggregate)
MSE.rf.ex1 = mean((rf.pred.ex1$aggregate[ind.na.ex1]-rf.test.ex1[ind.na.ex1])^2)

#importance
vi.rf.ex1=importance(rf.ex1, type=1, scale = TRUE)
varImpPlot(rf.ex1, type=1, scale = TRUE)
write.csv(vi.rf.ex1, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/incMse.ex1.csv")

#linear regression
lm.ex1 = lm(PM10~., data=merged.ex1[ind.ex1==1,])
summary(lm.ex1)
lm.pred.ex1 = predict(lm.ex1, newdata=merged.ex1[ind.ex1==2 & !is.na(merged.ex1[,"PM10"]),])
MSE.lm.ex1 = mean((lm.pred.ex1[ind.na.ex1]-rf.test.ex1[ind.na.ex1])^2)

####################################### merged.ex2 ####################################### 
str(merged.ex2, list.len=ncol(merged.ex2))

#training 80, test 20
ind.ex2 = sample(2, nrow(merged.ex2), replace=TRUE, prob=c(0.8,0.2))
rf.ex2 = randomForest(formula=PM10~., data=merged.ex2[ind.ex2==1,], na.action=na.roughfix, importance=TRUE)

#prediction and MSE
rf.test.ex2 = merged.ex2[ind.ex2==2 & !is.na(merged.ex2[,"PM10"]),"PM10"]
rf.pred.ex2 = predict(rf.ex2, newdata=merged.ex2[ind.ex2==2 & !is.na(merged.ex2[,"PM10"]),], type="response", predict.all = TRUE)
ind.na.ex2 = !is.na(rf.pred.ex2$aggregate)
MSE.rf.ex2 = mean((rf.pred.ex2$aggregate[ind.na.ex2]-rf.test.ex2[ind.na.ex2])^2)

#importance
vi.rf.ex2=importance(rf.ex2, type=1, scale = TRUE)
varImpPlot(rf.ex2, type=1, scale = TRUE)
write.csv(vi.rf.ex2, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/incMse.ex2.csv")

#linear regression
lm.ex2 = lm(PM10~., data=merged.ex2[ind.ex2==1,])
summary(lm.ex2)
lm.pred.ex2 = predict(lm.ex2, newdata=merged.ex2[ind.ex2==2 & !is.na(merged.ex2[,"PM10"]),])
MSE.lm.ex2 = mean((lm.pred.ex2[ind.na.ex2]-rf.test.ex2[ind.na.ex2])^2)

####################################### merged.ex3 ####################################### 
str(merged.ex3, list.len=ncol(merged.ex3))

#training 80, test 20
ind.ex3 = sample(2, nrow(merged.ex3), replace=TRUE, prob=c(0.8,0.2))
rf.ex3 = randomForest(formula=PM10~., data=merged.ex3[ind.ex3==1,], na.action=na.roughfix, importance=TRUE)

#prediction and MSE
rf.test.ex3 = merged.ex3[ind.ex3==2 & !is.na(merged.ex3[,"PM10"]),"PM10"]
rf.pred.ex3 = predict(rf.ex3, newdata=merged.ex3[ind.ex3==2 & !is.na(merged.ex3[,"PM10"]),], type="response", predict.all = TRUE)
ind.na.ex3 = !is.na(rf.pred.ex3$aggregate)
MSE.rf.ex3 = mean((rf.pred.ex3$aggregate[ind.na.ex3]-rf.test.ex3[ind.na.ex3])^2)

#importance
vi.rf.ex3=importance(rf.ex3, type=1, scale = TRUE)
varImpPlot(rf.ex3, type=1, scale = TRUE)
write.csv(vi.rf.ex3, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/incMse.ex3.csv")

#linear regression
lm.ex3 = lm(PM10~., data=merged.ex3[ind.ex3==1,])
summary(lm.ex3)
lm.pred.ex3 = predict(lm.ex3, newdata=merged.ex3[ind.ex3==2 & !is.na(merged.ex3[,"PM10"]),])
MSE.lm.ex3 = mean((lm.pred.ex3[ind.na.ex3]-rf.test.ex3[ind.na.ex3])^2)

####################################### merged.ex4 ####################################### 
str(merged.ex4, list.len=ncol(merged.ex4))

#training 80, test 20
ind.ex4 = sample(2, nrow(merged.ex4), replace=TRUE, prob=c(0.8,0.2))
rf.ex4 = randomForest(formula=PM10~., data=merged.ex4[ind.ex4==1,], na.action=na.roughfix, importance=TRUE)

#prediction and MSE
rf.test.ex4 = merged.ex4[ind.ex4==2 & !is.na(merged.ex4[,"PM10"]),"PM10"]
rf.pred.ex4 = predict(rf.ex4, newdata=merged.ex4[ind.ex4==2 & !is.na(merged.ex4[,"PM10"]),], type="response", predict.all = TRUE)
ind.na.ex4 = !is.na(rf.pred.ex4$aggregate)
MSE.rf.ex4 = mean((rf.pred.ex4$aggregate[ind.na.ex4]-rf.test.ex4[ind.na.ex4])^2)

#importance
vi.rf.ex4=importance(rf.ex4, type=1, scale = TRUE)
varImpPlot(rf.ex4, type=1, scale = TRUE)
write.csv(vi.rf.ex4, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/incMse.ex4.csv")

#linear regression
lm.ex4 = lm(PM10~., data=merged.ex4[ind.ex4==1,])
summary(lm.ex4)
lm.pred.ex4 = predict(lm.ex4, newdata=merged.ex4[ind.ex4==2 & !is.na(merged.ex4[,"PM10"]),])
MSE.lm.ex4 = mean((lm.pred.ex4[ind.na.ex4]-rf.test.ex4[ind.na.ex4])^2)

####################################### merged.ex5 ####################################### 
str(merged.ex5, list.len=ncol(merged.ex5))

#training 80, test 20
ind.ex5 = sample(2, nrow(merged.ex5), replace=TRUE, prob=c(0.8,0.2))
rf.ex5 = randomForest(formula=PM10~., data=merged.ex5[ind.ex5==1,], na.action=na.roughfix, importance=TRUE)

#prediction and MSE
rf.test.ex5 = merged.ex5[ind.ex5==2 & !is.na(merged.ex5[,"PM10"]),"PM10"]
rf.pred.ex5 = predict(rf.ex5, newdata=merged.ex5[ind.ex5==2 & !is.na(merged.ex5[,"PM10"]),], type="response", predict.all = TRUE)
ind.na.ex5 = !is.na(rf.pred.ex5$aggregate)
MSE.rf.ex5 = mean((rf.pred.ex5$aggregate[ind.na.ex5]-rf.test.ex5[ind.na.ex5])^2)

#importance
vi.rf.ex5=importance(rf.ex5, type=1, scale = TRUE)
varImpPlot(rf.ex5, type=1, scale = TRUE)
write.csv(vi.rf.ex5, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/incMse.ex5.csv")

#linear regression
lm.ex5 = lm(PM10~., data=merged.ex5[ind.ex5==1,])
summary(lm.ex5)
lm.pred.ex5 = predict(lm.ex5, newdata=merged.ex5[ind.ex5==2 & !is.na(merged.ex5[,"PM10"]),])
MSE.lm.ex5 = mean((lm.pred.ex5[ind.na.ex5]-rf.test.ex5[ind.na.ex5])^2)

####################################### merged.ex6 ####################################### 
str(merged.ex6, list.len=ncol(merged.ex6))

#training 80, test 20
ind.ex6 = sample(2, nrow(merged.ex6), replace=TRUE, prob=c(0.8,0.2))
rf.ex6 = randomForest(formula=PM10~., data=merged.ex6[ind.ex6==1,], na.action=na.roughfix, importance=TRUE)

#prediction and MSE
rf.test.ex6 = merged.ex6[ind.ex6==2 & !is.na(merged.ex6[,"PM10"]),"PM10"]
rf.pred.ex6 = predict(rf.ex6, newdata=merged.ex6[ind.ex6==2 & !is.na(merged.ex6[,"PM10"]),], type="response", predict.all = TRUE)
ind.na.ex6 = !is.na(rf.pred.ex6$aggregate)
MSE.rf.ex6 = mean((rf.pred.ex6$aggregate[ind.na.ex6]-rf.test.ex6[ind.na.ex6])^2)

#importance
vi.rf.ex6=importance(rf.ex6, type=1, scale = TRUE)
varImpPlot(rf.ex6, type=1, scale = TRUE)
write.csv(vi.rf.ex6, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/incMse.ex6.csv")

#linear regression
lm.ex6 = lm(PM10~., data=merged.ex6[ind.ex6==1,])
summary(lm.ex6)
lm.pred.ex6 = predict(lm.ex6, newdata=merged.ex6[ind.ex6==2 & !is.na(merged.ex6[,"PM10"]),])
MSE.lm.ex6 = mean((lm.pred.ex6[ind.na.ex6]-rf.test.ex6[ind.na.ex6])^2)


##################################################################################################################################################
##################################################################################################################################################
##rpart pk experiment
library(rpart)
dt.control=rpart.control(minsplit=3000, minbucket=1000,maxdepth=30)

####################################### merged.ex1 #######################################
rpart.ex1=rpart(formula=PM10~., data=merged.ex1[ind.ex1==1,], na.action=na.roughfix)
plot(rpart.ex1)
text(rpart.ex1)
sum.ex1=summary(rpart.ex1)
write.csv(sum.ex1["variable.importance"], "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/sum.ex1.csv")

####################################### merged.ex2 #######################################
rpart.ex2=rpart(formula=PM10~., data=merged.ex2[ind.ex2==1,], na.action=na.roughfix)
plot(rpart.ex2)
text(rpart.ex2)
sum.ex2=summary(rpart.ex2)
write.csv(sum.ex2["variable.importance"], "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/sum.ex2.csv")

####################################### merged.ex3 #######################################
rpart.ex3=rpart(formula=PM10~., data=merged.ex3[ind.ex3==1,], na.action=na.roughfix)
plot(rpart.ex3)
text(rpart.ex3)
sum.ex3=summary(rpart.ex3)
write.csv(sum.ex3["variable.importance"], "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/sum.ex3.csv")

####################################### merged.ex4 #######################################
rpart.ex4=rpart(formula=PM10~., data=merged.ex4[ind.ex4==1,], na.action=na.roughfix)
plot(rpart.ex4)
text(rpart.ex4)
sum.ex4=summary(rpart.ex4)
write.csv(sum.ex4["variable.importance"], "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/sum.ex4.csv")

####################################### merged.ex5 #######################################
rpart.ex5=rpart(formula=PM10~., data=merged.ex5[ind.ex5==1,], na.action=na.roughfix)
plot(rpart.ex1)
text(rpart.ex1)
sum.ex5=summary(rpart.ex5)
write.csv(sum.ex5["variable.importance"], "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/sum.ex5.csv")

####################################### merged.ex6 #######################################
rpart.ex6=rpart(formula=PM10~., data=merged.ex6[ind.ex6==1,], na.action=na.roughfix)
plot(rpart.ex6)
text(rpart.ex6)
sum.ex6=summary(rpart.ex6)
write.csv(sum.ex6["variable.importance"], "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/sum.ex6.csv")

################################### Predict ########################################
rpart.pred.ex1 = predict(rpart.ex1, newdata=merged.ex1[ind.ex1==2 & !is.na(merged.ex1[,"PM10"]),])
ind.rpart.na.ex1 = !is.na(rpart.pred.ex1)
MSE.rpart.ex1 = mean((rpart.pred.ex1[ind.rpart.na.ex1]-rf.test.ex1[ind.rpart.na.ex1])^2)

rpart.pred.ex2 = predict(rpart.ex2, newdata=merged.ex2[ind.ex2==2 & !is.na(merged.ex2[,"PM10"]),])
ind.rpart.na.ex2 = !is.na(rpart.pred.ex2)
MSE.rpart.ex2 = mean((rpart.pred.ex2[ind.rpart.na.ex2]-rf.test.ex2[ind.rpart.na.ex2])^2)

rpart.pred.ex3 = predict(rpart.ex3, newdata=merged.ex3[ind.ex3==2 & !is.na(merged.ex3[,"PM10"]),])
ind.rpart.na.ex3 = !is.na(rpart.pred.ex3)
MSE.rpart.ex3 = mean((rpart.pred.ex3[ind.rpart.na.ex3]-rf.test.ex3[ind.rpart.na.ex3])^2)

rpart.pred.ex4 = predict(rpart.ex4, newdata=merged.ex4[ind.ex4==2 & !is.na(merged.ex4[,"PM10"]),])
ind.rpart.na.ex4 = !is.na(rpart.pred.ex4)
MSE.rpart.ex4 = mean((rpart.pred.ex4[ind.rpart.na.ex4]-rf.test.ex4[ind.rpart.na.ex4])^2)

rpart.pred.ex5 = predict(rpart.ex5, newdata=merged.ex5[ind.ex5==2 & !is.na(merged.ex5[,"PM10"]),])
ind.rpart.na.ex5 = !is.na(rpart.pred.ex5)
MSE.rpart.ex5 = mean((rpart.pred.ex5[ind.rpart.na.ex5]-rf.test.ex5[ind.rpart.na.ex5])^2)

rpart.pred.ex6 = predict(rpart.ex6, newdata=merged.ex6[ind.ex6==2 & !is.na(merged.ex6[,"PM10"]),])
ind.rpart.na.ex6 = !is.na(rpart.pred.ex6)
MSE.rpart.ex6 = mean((rpart.pred.ex6[ind.rpart.na.ex6]-rf.test.ex6[ind.rpart.na.ex6])^2)


##################################################################################################################################################
##################################################################################################################################################
##gbm pk experiment
library(gbm)

gbm.ex1=gbm(PM10~.,data=merged.ex1[ind.ex1==1 & !is.na(merged.ex1[,"PM10"]),], distribution="gaussian",n.trees=5000,interaction.depth=4)

gbm.ex2=gbm(PM10~.,data=merged.ex2[ind.ex2==1 & !is.na(merged.ex2[,"PM10"]),], distribution="gaussian",n.trees=5000,interaction.depth=4)

gbm.ex3=gbm(PM10~.,data=merged.ex3[ind.ex3==1 & !is.na(merged.ex3[,"PM10"]),], distribution="gaussian",n.trees=5000,interaction.depth=4)

gbm.ex4=gbm(PM10~.,data=merged.ex4[ind.ex4==1 & !is.na(merged.ex4[,"PM10"]),], distribution="gaussian",n.trees=5000,interaction.depth=4)

gbm.ex5=gbm(PM10~.,data=merged.ex5[ind.ex5==1 & !is.na(merged.ex5[,"PM10"]),], distribution="gaussian",n.trees=5000,interaction.depth=4)

gbm.ex6=gbm(PM10~.,data=merged.ex6[ind.ex6==1 & !is.na(merged.ex6[,"PM10"]),], distribution="gaussian",n.trees=5000,interaction.depth=4)

#prediction and MSE
gbm.pred.ex1 = predict(gbm.ex1, newdata=merged.ex1[ind.ex1==2 & !is.na(merged.ex1[,"PM10"]),], n.trees=5000, type="response", predict.all = TRUE)
ind.na.ex1 = !is.na(gbm.pred.ex1)
MSE.gbm.ex1 = mean((gbm.pred.ex1[ind.na.ex1]-rf.test.ex1[ind.na.ex1])^2)

gbm.pred.ex2 = predict(gbm.ex2, newdata=merged.ex2[ind.ex2==2 & !is.na(merged.ex2[,"PM10"]),], n.trees=5000, type="response", predict.all = TRUE)
ind.na.ex2 = !is.na(gbm.pred.ex2)
MSE.gbm.ex2 = mean((gbm.pred.ex2[ind.na.ex2]-rf.test.ex2[ind.na.ex2])^2)

gbm.pred.ex3 = predict(gbm.ex3, newdata=merged.ex3[ind.ex3==2 & !is.na(merged.ex3[,"PM10"]),], n.trees=5000, type="response", predict.all = TRUE)
ind.na.ex3 = !is.na(gbm.pred.ex3)
MSE.gbm.ex3 = mean((gbm.pred.ex3[ind.na.ex3]-rf.test.ex3[ind.na.ex3])^2)

gbm.pred.ex4 = predict(gbm.ex4, newdata=merged.ex4[ind.ex4==2 & !is.na(merged.ex4[,"PM10"]),], n.trees=5000, type="response", predict.all = TRUE)
ind.na.ex4 = !is.na(gbm.pred.ex4)
MSE.gbm.ex4 = mean((gbm.pred.ex4[ind.na.ex4]-rf.test.ex4[ind.na.ex4])^2)

gbm.pred.ex5 = predict(gbm.ex5, newdata=merged.ex5[ind.ex5==2 & !is.na(merged.ex5[,"PM10"]),], n.trees=5000, type="response", predict.all = TRUE)
ind.na.ex5 = !is.na(gbm.pred.ex5)
MSE.gbm.ex5 = mean((gbm.pred.ex5[ind.na.ex5]-rf.test.ex5[ind.na.ex5])^2)

gbm.pred.ex6 = predict(gbm.ex6, newdata=merged.ex6[ind.ex6==2 & !is.na(merged.ex6[,"PM10"]),], n.trees=5000, type="response", predict.all = TRUE)
ind.na.ex6 = !is.na(gbm.pred.ex6)
MSE.gbm.ex6 = mean((gbm.pred.ex6[ind.na.ex6]-rf.test.ex6[ind.na.ex6])^2)


gbm1.ex1=gbm(PM10~.,data=merged.ex1[ind.ex1==1 & !is.na(merged.ex1[,"PM10"]),], distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.01,verbose=FALSE)

gbm1.ex2=gbm(PM10~.,data=merged.ex2[ind.ex2==1 & !is.na(merged.ex2[,"PM10"]),], distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.01,verbose=FALSE)

gbm1.ex3=gbm(PM10~.,data=merged.ex3[ind.ex3==1 & !is.na(merged.ex3[,"PM10"]),], distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.01,verbose=FALSE)

gbm1.ex4=gbm(PM10~.,data=merged.ex4[ind.ex4==1 & !is.na(merged.ex4[,"PM10"]),], distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.01,verbose=FALSE)

gbm1.ex5=gbm(PM10~.,data=merged.ex5[ind.ex5==1 & !is.na(merged.ex5[,"PM10"]),], distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.01,verbose=FALSE)

gbm1.ex6=gbm(PM10~.,data=merged.ex6[ind.ex6==1 & !is.na(merged.ex6[,"PM10"]),], distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.01,verbose=FALSE)


gbm1.pred.ex1 = predict(gbm1.ex1, newdata=merged.ex1[ind.ex1==2 & !is.na(merged.ex1[,"PM10"]),], n.trees=5000, type="response", predict.all = TRUE)
ind.na.ex1 = !is.na(gbm1.pred.ex1)
MSE.gbm1.ex1 = mean((gbm1.pred.ex1[ind.na.ex1]-rf.test.ex1[ind.na.ex1])^2)

gbm1.pred.ex2 = predict(gbm1.ex2, newdata=merged.ex2[ind.ex2==2 & !is.na(merged.ex2[,"PM10"]),], n.trees=5000, type="response", predict.all = TRUE)
ind.na.ex2 = !is.na(gbm1.pred.ex2)
MSE.gbm1.ex2 = mean((gbm1.pred.ex2[ind.na.ex2]-rf.test.ex2[ind.na.ex2])^2)

gbm1.pred.ex3 = predict(gbm1.ex3, newdata=merged.ex3[ind.ex3==2 & !is.na(merged.ex3[,"PM10"]),], n.trees=5000, type="response", predict.all = TRUE)
ind.na.ex3 = !is.na(gbm1.pred.ex3)
MSE.gbm1.ex3 = mean((gbm1.pred.ex3[ind.na.ex3]-rf.test.ex3[ind.na.ex3])^2)

gbm1.pred.ex4 = predict(gbm1.ex4, newdata=merged.ex4[ind.ex4==2 & !is.na(merged.ex4[,"PM10"]),], n.trees=5000, type="response", predict.all = TRUE)
ind.na.ex4 = !is.na(gbm1.pred.ex4)
MSE.gbm1.ex4 = mean((gbm1.pred.ex4[ind.na.ex4]-rf.test.ex4[ind.na.ex4])^2)

gbm1.pred.ex5 = predict(gbm1.ex5, newdata=merged.ex5[ind.ex5==2 & !is.na(merged.ex5[,"PM10"]),], n.trees=5000, type="response", predict.all = TRUE)
ind.na.ex5 = !is.na(gbm1.pred.ex5)
MSE.gbm1.ex5 = mean((gbm1.pred.ex5[ind.na.ex5]-rf.test.ex5[ind.na.ex5])^2)

gbm1.pred.ex6 = predict(gbm1.ex6, newdata=merged.ex6[ind.ex6==2 & !is.na(merged.ex6[,"PM10"]),], n.trees=5000, type="response", predict.all = TRUE)
ind.na.ex6 = !is.na(gbm1.pred.ex6)
MSE.gbm1.ex6 = mean((gbm1.pred.ex6[ind.na.ex6]-rf.test.ex6[ind.na.ex6])^2)





sum.gbm.ex1 = summary(gbm.ex1)
sum.gbm.ex2 = summary(gbm.ex2)
sum.gbm.ex3 = summary(gbm.ex3)
sum.gbm.ex4 = summary(gbm.ex4)
sum.gbm.ex5 = summary(gbm.ex5)
sum.gbm.ex6 = summary(gbm.ex6)
write.csv(sum.gbm.ex1, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/sum.gbm.ex1.csv")
write.csv(sum.gbm.ex2, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/sum.gbm.ex2.csv")
write.csv(sum.gbm.ex3, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/sum.gbm.ex3.csv")
write.csv(sum.gbm.ex4, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/sum.gbm.ex4.csv")
write.csv(sum.gbm.ex5, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/sum.gbm.ex5.csv")
write.csv(sum.gbm.ex6, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/sum.gbm.ex6.csv")


sum.gbm1.ex1 = summary(gbm1.ex1)
sum.gbm1.ex2 = summary(gbm1.ex2)
sum.gbm1.ex3 = summary(gbm1.ex3)
sum.gbm1.ex4 = summary(gbm1.ex4)
sum.gbm1.ex5 = summary(gbm1.ex5)
sum.gbm1.ex6 = summary(gbm1.ex6)

write.csv(sum.gbm1.ex1, "/Users/kei/Documents/_temp_170921/_05variableImportance/sum.gbm1.ex1.csv")
write.csv(sum.gbm1.ex2, "/Users/kei/Documents/_temp_170921/_05variableImportance/sum.gbm1.ex2.csv")
write.csv(sum.gbm1.ex3, "/Users/kei/Documents/_temp_170921/_05variableImportance/sum.gbm1.ex3.csv")
write.csv(sum.gbm1.ex4, "/Users/kei/Documents/_temp_170921/_05variableImportance/sum.gbm1.ex4.csv")
write.csv(sum.gbm1.ex5, "/Users/kei/Documents/_temp_170921/_05variableImportance/sum.gbm1.ex5.csv")
write.csv(sum.gbm1.ex6, "/Users/kei/Documents/_temp_170921/_05variableImportance/sum.gbm1.ex6.csv")

##########################################################################################
####################################### bagging #######################################

bag.ex1 = randomForest(formula=PM10~., data=merged.ex1[ind.ex1==1,], na.action=na.roughfix, importance=TRUE, mtry=ncol(merged.ex1)-1)
bag.ex2 = randomForest(formula=PM10~., data=merged.ex2[ind.ex2==1,], na.action=na.roughfix, importance=TRUE, mtry=ncol(merged.ex2)-1)
bag.ex3 = randomForest(formula=PM10~., data=merged.ex3[ind.ex3==1,], na.action=na.roughfix, importance=TRUE, mtry=ncol(merged.ex3)-1)
bag.ex4 = randomForest(formula=PM10~., data=merged.ex4[ind.ex4==1,], na.action=na.roughfix, importance=TRUE, mtry=ncol(merged.ex4)-1)
bag.ex5 = randomForest(formula=PM10~., data=merged.ex5[ind.ex5==1,], na.action=na.roughfix, importance=TRUE, mtry=ncol(merged.ex5)-1)
bag.ex6 = randomForest(formula=PM10~., data=merged.ex6[ind.ex6==1,], na.action=na.roughfix, importance=TRUE, mtry=ncol(merged.ex6)-1)


#prediction and MSE
bag.pred.ex1 = predict(bag.ex1, newdata=merged.ex1[ind.ex1==2 & !is.na(merged.ex1[,"PM10"]),], type="response", predict.all = TRUE)
ind.na.ex1 = !is.na(bag.pred.ex1$aggregate)
MSE.bag.ex1 = mean((bag.pred.ex1$aggregate[ind.na.ex1]-rf.test.ex1[ind.na.ex1])^2)

#importance
vi.bag.ex1=importance(bag.ex1, type=1, scale = TRUE)
varImpPlot(bag.ex1, type=1, scale = TRUE)
write.csv(vi.bag.ex1, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/incMse.bag.ex1.csv")

#prediction and MSE
bag.pred.ex2 = predict(bag.ex2, newdata=merged.ex2[ind.ex2==2 & !is.na(merged.ex2[,"PM10"]),], type="response", predict.all = TRUE)
ind.na.ex2 = !is.na(bag.pred.ex2$aggregate)
MSE.bag.ex2 = mean((bag.pred.ex2$aggregate[ind.na.ex2]-rf.test.ex2[ind.na.ex2])^2)

#importance
vi.bag.ex2=importance(bag.ex2, type=1, scale = TRUE)
varImpPlot(bag.ex2, type=1, scale = TRUE)
write.csv(vi.bag.ex2, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/incMse.bag.ex2.csv")

#prediction and MSE
bag.pred.ex3 = predict(bag.ex3, newdata=merged.ex3[ind.ex3==2 & !is.na(merged.ex3[,"PM10"]),], type="response", predict.all = TRUE)
ind.na.ex3 = !is.na(bag.pred.ex3$aggregate)
MSE.bag.ex3 = mean((bag.pred.ex3$aggregate[ind.na.ex3]-rf.test.ex3[ind.na.ex3])^2)

#importance
vi.bag.ex3=importance(bag.ex3, type=1, scale = TRUE)
varImpPlot(bag.ex3, type=1, scale = TRUE)
write.csv(vi.bag.ex3, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/incMse.bag.ex3.csv")

#prediction and MSE
bag.pred.ex4 = predict(bag.ex4, newdata=merged.ex4[ind.ex4==2 & !is.na(merged.ex4[,"PM10"]),], type="response", predict.all = TRUE)
ind.na.ex4 = !is.na(bag.pred.ex4$aggregate)
MSE.bag.ex4 = mean((bag.pred.ex4$aggregate[ind.na.ex4]-rf.test.ex4[ind.na.ex4])^2)

#importance
vi.bag.ex4=importance(bag.ex4, type=1, scale = TRUE)
varImpPlot(bag.ex4, type=1, scale = TRUE)
write.csv(vi.bag.ex4, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/incMse.bag.ex4.csv")

#prediction and MSE
bag.pred.ex5 = predict(bag.ex5, newdata=merged.ex5[ind.ex5==2 & !is.na(merged.ex5[,"PM10"]),], type="response", predict.all = TRUE)
ind.na.ex5 = !is.na(bag.pred.ex5$aggregate)
MSE.bag.ex5 = mean((bag.pred.ex5$aggregate[ind.na.ex5]-rf.test.ex5[ind.na.ex5])^2)

#importance
vi.bag.ex5=importance(bag.ex5, type=1, scale = TRUE)
varImpPlot(bag.ex5, type=1, scale = TRUE)
write.csv(vi.bag.ex5, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/incMse.bag.ex5.csv")


#prediction and MSE
bag.pred.ex6 = predict(bag.ex6, newdata=merged.ex6[ind.ex6==2 & !is.na(merged.ex6[,"PM10"]),], type="response", predict.all = TRUE)
ind.na.ex6 = !is.na(bag.pred.ex6$aggregate)
MSE.bag.ex6 = mean((bag.pred.ex6$aggregate[ind.na.ex6]-rf.test.ex6[ind.na.ex6])^2)

#importance
vi.bag.ex6=importance(bag.ex6, type=1, scale = TRUE)
varImpPlot(bag.ex6, type=1, scale = TRUE)
write.csv(vi.bag.ex6, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/incMse.bag.ex6.csv")


##################################################################################################################################################
##################################################################################################################################################
## MSE
MSE.ex1 <- c(MSE.lm.ex1,MSE.rpart.ex1,MSE.rf.ex1,MSE.bagging.ex1,MSE.gbm.ex1,MSE.gbm1.ex1)
MSE.ex2 <- c(MSE.lm.ex2,MSE.rpart.ex2,MSE.rf.ex2,MSE.bagging.ex2,MSE.gbm.ex2,MSE.gbm1.ex2)
MSE.ex3 <- c(MSE.lm.ex3,MSE.rpart.ex3,MSE.rf.ex3,MSE.bagging.ex3,MSE.gbm.ex3,MSE.gbm1.ex3)
MSE.ex4 <- c(MSE.lm.ex4,MSE.rpart.ex4,MSE.rf.ex4,MSE.bagging.ex4,MSE.gbm.ex4,MSE.gbm1.ex4)
MSE.ex5 <- c(MSE.lm.ex5,MSE.rpart.ex5,MSE.rf.ex5,MSE.bagging.ex5,MSE.gbm.ex5,MSE.gbm1.ex5)
MSE.ex6 <- c(MSE.lm.ex6,MSE.rpart.ex6,MSE.rf.ex6,MSE.bagging.ex6,MSE.gbm.ex6,MSE.gbm1.ex6)

MSE.all <- rbind(MSE.ex1,MSE.ex2,MSE.ex3,MSE.ex4,MSE.ex5,MSE.ex6)
colnames(MSE.all) <- c("lm","dt","rf","bag","gbm","gbm1")
MSE.all <- MSE.all[,-5]

MSE.vec <- sort(c(MSE.all))
plot(MSE.all)

df.lm <- data.frame(row.names(MSE.all),"lm",MSE.all[,"lm"])
df.dt <- data.frame(row.names(MSE.all),"dt",MSE.all[,"dt"])
df.rf <- data.frame(row.names(MSE.all),"rf",MSE.all[,"rf"])
df.bag <- data.frame(row.names(MSE.all),"bag",MSE.all[,"bag"])
df.gbm <- data.frame(row.names(MSE.all),"gbm",MSE.all[,"gbm1"])

colnames(df.lm)=c("exp","method","MSE")
colnames(df.dt)=c("exp","method","MSE")
colnames(df.rf)=c("exp","method","MSE")
colnames(df.bag)=c("exp","method","MSE")
colnames(df.gbm)=c("exp","method","MSE")

df.all <- rbind(df.lm,df.dt,df.rf,df.bag,df.gbm)

df.all.order <- df.all[order(df.all[,3]),]
ggplot(data = df.all.order, aes(x = exp, y = MSE)) +
  geom_point( aes(colour=method)) +
  scale_y_continuous(breaks=seq(0, 300, 50))
  theme_bw() +
  theme(panel.grid.major.x = element_line(color = "grey60", linetype = "dashed"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey60", linetype = "dashed"))
  
##################################################################################################################################################
##################################################################################################################################################
library(adabag)
library(ipred)
library(plyr)
library(dplyr)
library(e1071)
library(caret)
bagging.ex1 <- train(form=PM10~., method="treebag", data=merged.ex1[ind.ex1==1,], importance=TRUE, na.action=na.omit)
bagging.ex2 <- train(form=PM10~., method="treebag", data=merged.ex2[ind.ex2==1,], importance=TRUE, na.action=na.omit)
bagging.ex3 <- train(form=PM10~., method="treebag", data=merged.ex3[ind.ex3==1,], importance=TRUE, na.action=na.omit)
bagging.ex4 <- train(form=PM10~., method="treebag", data=merged.ex4[ind.ex4==1,], importance=TRUE, na.action=na.omit)
bagging.ex5 <- train(form=PM10~., method="treebag", data=merged.ex5[ind.ex5==1,], importance=TRUE, na.action=na.omit)
bagging.ex6 <- train(form=PM10~., method="treebag", data=merged.ex6[ind.ex6==1,], importance=TRUE, na.action=na.omit)

vi.bagging.ex1= varImp(bagging.ex1)
vi.bagging.ex2= varImp(bagging.ex2)
vi.bagging.ex3= varImp(bagging.ex3)
vi.bagging.ex4= varImp(bagging.ex4)
vi.bagging.ex5= varImp(bagging.ex5)
vi.bagging.ex6= varImp(bagging.ex6)

write.csv(vi.bagging.ex1$importance, "/Users/kei/Documents/_temp_170921/_05variableImportance/incMse.bagging.ex1.csv")
write.csv(vi.bagging.ex2$importance, "/Users/kei/Documents/_temp_170921/_05variableImportance/incMse.bagging.ex2.csv")
write.csv(vi.bagging.ex3$importance, "/Users/kei/Documents/_temp_170921/_05variableImportance/incMse.bagging.ex3.csv")
write.csv(vi.bagging.ex4$importance, "/Users/kei/Documents/_temp_170921/_05variableImportance/incMse.bagging.ex4.csv")
write.csv(vi.bagging.ex5$importance, "/Users/kei/Documents/_temp_170921/_05variableImportance/incMse.bagging.ex5.csv")
write.csv(vi.bagging.ex6$importance, "/Users/kei/Documents/_temp_170921/_05variableImportance/incMse.bagging.ex6.csv")


bagging.pred.ex1 = predict(bagging.ex1, newdata=merged.ex1[ind.ex1==2 & !is.na(merged.ex1[,"PM10"]),], na.action = na.pass)
ind.na.ex1 = !is.na(bagging.pred.ex1)
MSE.bagging.ex1 = mean((bagging.pred.ex1[ind.na.ex1]-rf.test.ex1[ind.na.ex1])^2)

bagging.pred.ex2 = predict(bagging.ex2, newdata=merged.ex2[ind.ex2==2 & !is.na(merged.ex2[,"PM10"]),], na.action = na.pass)
ind.na.ex2 = !is.na(bagging.pred.ex2)
MSE.bagging.ex2 = mean((bagging.pred.ex2[ind.na.ex2]-rf.test.ex2[ind.na.ex2])^2)

bagging.pred.ex3 = predict(bagging.ex3, newdata=merged.ex3[ind.ex3==2 & !is.na(merged.ex3[,"PM10"]),], na.action = na.pass)
ind.na.ex3 = !is.na(bagging.pred.ex3)
MSE.bagging.ex3 = mean((bagging.pred.ex3[ind.na.ex3]-rf.test.ex3[ind.na.ex3])^2)

bagging.pred.ex4 = predict(bagging.ex4, newdata=merged.ex4[ind.ex4==2 & !is.na(merged.ex4[,"PM10"]),], na.action = na.pass)
ind.na.ex4 = !is.na(bagging.pred.ex4)
MSE.bagging.ex4 = mean((bagging.pred.ex4[ind.na.ex4]-rf.test.ex4[ind.na.ex4])^2)

bagging.pred.ex5 = predict(bagging.ex5, newdata=merged.ex5[ind.ex5==2 & !is.na(merged.ex5[,"PM10"]),], na.action = na.pass)
ind.na.ex5 = !is.na(bagging.pred.ex5)
MSE.bagging.ex5 = mean((bagging.pred.ex5[ind.na.ex5]-rf.test.ex5[ind.na.ex5])^2)

bagging.pred.ex6 = predict(bagging.ex6, newdata=merged.ex6[ind.ex6==2 & !is.na(merged.ex6[,"PM10"]),], na.action = na.pass)
ind.na.ex6 = !is.na(bagging.pred.ex6)
MSE.bagging.ex6 = mean((bagging.pred.ex6[ind.na.ex6]-rf.test.ex6[ind.na.ex6])^2)

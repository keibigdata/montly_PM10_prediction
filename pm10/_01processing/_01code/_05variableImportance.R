
##Data load
merged.ex1 = read.csv("D:/my-backup/project/pm10/_01processing/_02output/_04merge/merged.ex1.csv", header=T, sep=",", stringsAsFactors = FALSE)
merged.ex2 = read.csv("D:/my-backup/project/pm10/_01processing/_02output/_04merge/merged.ex2.csv", header=T, sep=",", stringsAsFactors = FALSE)
merged.ex3 = read.csv("D:/my-backup/project/pm10/_01processing/_02output/_04merge/merged.ex3.csv", header=T, sep=",", stringsAsFactors = FALSE)
merged.ex4 = read.csv("D:/my-backup/project/pm10/_01processing/_02output/_04merge/merged.ex4.csv", header=T, sep=",", stringsAsFactors = FALSE)
merged.ex5 = read.csv("D:/my-backup/project/pm10/_01processing/_02output/_04merge/merged.ex5.csv", header=T, sep=",", stringsAsFactors = FALSE)
merged.ex6 = read.csv("D:/my-backup/project/pm10/_01processing/_02output/_04merge/merged.ex6.csv", header=T, sep=",", stringsAsFactors = FALSE)

#########################################################################
##randomForest pk experiment
require(randomForest)
set.seed(1)

####################################### merged.ex1 #######################################
str(merged.ex1, list.len=ncol(merged.ex1))

#training 80, test 20
ind.ex1 = sample(2, nrow(merged.ex1), replace=TRUE, prob=c(0.8,0.2))
rf.ex1 = randomForest(formula=PM10~., data=merged.ex1[ind.ex1==1,], na.action=na.roughfix, importance=TRUE)

#prediction and MSE
rf.test.ex1 = merged.ex1[ind.ex1==2,"PM10"]
rf.pred.ex1 = predict(rf.ex1, newdata=merged.ex1[ind.ex1==2,], type="response", predict.all = TRUE)
MSE.rf.ex1 = mean((rf.pred.ex1-rf.test.ex1)^2)

#importance
vi.rf.ex1=importance(rf.ex1, type=1, scale = TRUE)
varImpPlot(rf.ex1, type=1, scale = TRUE)
write.csv(vi.rf.ex1, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/incMse.ex1.csv")


####################################### merged.ex2 ####################################### 
str(merged.ex2, list.len=ncol(merged.ex2))

#training 80, test 20
ind.ex2 = sample(2, nrow(merged.ex2), replace=TRUE, prob=c(0.8,0.2))
rf.ex2 = randomForest(formula=PM10~., data=merged.ex2[ind.ex2==1,], na.action=na.roughfix, importance=TRUE)

#prediction and MSE
rf.test.ex2 = merged.ex2[ind.ex2==2,"PM10"]
rf.pred.ex2 = predict(rf.ex2, newdata=merged.ex2[ind.ex2==2,-7])
MSE.rf.ex2 = mean((rf.pred.ex2-rf.test.ex2)^2)

#importance
vi.rf.ex2=importance(rf.ex2, type=1, scale = TRUE)
varImpPlot(rf.ex2, type=1, scale = TRUE)
write.csv(vi.rf.ex2, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/incMse.ex2.csv")


####################################### merged.ex3 ####################################### 
str(merged.ex3, list.len=ncol(merged.ex3))

#training 80, test 20
ind.ex3 = sample(2, nrow(merged.ex3), replace=TRUE, prob=c(0.8,0.2))
rf.ex3 = randomForest(formula=PM10~., data=merged.ex3[ind.ex3==1,], na.action=na.roughfix, importance=TRUE)

#prediction and MSE
rf.test.ex3 = merged.ex3[ind.ex3==2,"PM10"]
rf.pred.ex3 = predict(rf.ex3, newdata=merged.ex3[ind.ex3==2,-7])
MSE.rf.ex3 = mean((rf.pred.ex3-rf.test.ex3)^2)

#importance
vi.rf.ex3=importance(rf.ex3, type=1, scale = TRUE)
varImpPlot(rf.ex3, type=1, scale = TRUE)
write.csv(vi.rf.ex3, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/incMse.ex3.csv")

####################################### merged.ex4 ####################################### 
str(merged.ex4, list.len=ncol(merged.ex4))

#training 80, test 20
ind.ex4 = sample(2, nrow(merged.ex4), replace=TRUE, prob=c(0.8,0.2))
rf.ex4 = randomForest(formula=PM10~., data=merged.ex4[ind.ex4==1,], na.action=na.roughfix, importance=TRUE)

#prediction and MSE
rf.test.ex4 = merged.ex4[ind.ex4==2,"PM10"]
rf.pred.ex4 = predict(rf.ex4, newdata=merged.ex4[ind.ex4==2,-7])
MSE.rf.ex4 = mean((rf.pred.ex4-rf.test.ex4)^2)

#importance
vi.rf.ex4=importance(rf.ex4, type=1, scale = TRUE)
varImpPlot(rf.ex4, type=1, scale = TRUE)
write.csv(vi.rf.ex4, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/incMse.ex4.csv")

####################################### merged.ex5 ####################################### 
str(merged.ex5, list.len=ncol(merged.ex5))

#training 80, test 20
ind.ex5 = sample(2, nrow(merged.ex5), replace=TRUE, prob=c(0.8,0.2))
rf.ex5 = randomForest(formula=PM10~., data=merged.ex5[ind.ex5==1,], na.action=na.roughfix, importance=TRUE)

#prediction and MSE
rf.test.ex5 = merged.ex5[ind.ex5==2,"PM10"]
rf.pred.ex5 = predict(rf.ex5, newdata=merged.ex5[ind.ex5==2,-7])
MSE.rf.ex5 = mean((rf.pred.ex5-rf.test.ex5)^2)

#importance
vi.rf.ex5=importance(rf.ex5, type=1, scale = TRUE)
varImpPlot(rf.ex5, type=1, scale = TRUE)
write.csv(vi.rf.ex5, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/incMse.ex5.csv")

####################################### merged.ex6 ####################################### 
str(merged.ex6, list.len=ncol(merged.ex6))

#training 80, test 20
ind.ex6 = sample(2, nrow(merged.ex6), replace=TRUE, prob=c(0.8,0.2))
rf.ex6 = randomForest(formula=PM10~., data=merged.ex5[ind.ex6==1,], na.action=na.roughfix, importance=TRUE)

#prediction and MSE
rf.test.ex6 = merged.ex6[ind.ex6==2,"PM10"]
rf.pred.ex6 = predict(rf.ex6, newdata=merged.ex6[ind.ex6==2,-7])
MSE.rf.ex6 = mean((rf.pred.ex6-rf.test.ex6)^2)

#importance
vi.rf.ex6=importance(rf.ex6, type=1, scale = TRUE)
varImpPlot(rf.ex6, type=1, scale = TRUE)
write.csv(vi.rf.ex6, "D:/my-backup/project/pm10/_01processing/_02output/_05variableImportance/incMse.ex6.csv")


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





tree.ex1=tree(PM10~.,merged.ex1, na.action=na.roughfix)
summary(tree.ex1)
plot(tree.ex1)
text(tree.ex1,pretty=0)

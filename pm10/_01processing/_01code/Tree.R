library(tidyverse)
library(tree)
library(randomForest)
library(gbm)
#library(leaps)

load("D:/my-backup/project/pm10/_01processing/_01code/M.data.kk.Rdata")
#prepapare data only fine
M.data.kk_tree=M.data.kk[,-c(1,3:7,9:10)]

# s_gov_fund top 6 seems outlier. 
M.data.kk_tree %>% select(S_gov_fund) %>% arrange(S_gov_fund)  %>% mutate(L.fund=lag(S_gov_fund,1)) %>% mutate(gap=round(abs(L.fund-S_gov_fund),digits=3)) %>% tail(n=40)
M.data.kk_tree=M.data.kk_tree[M.data.kk_tree$S_gov_fund<25,]

## Tree
set.seed(1)
train=sample(1:nrow(M.data.kk_tree),nrow(M.data.kk_tree)/2)
tree.M=tree(S_gov_fund~.,M.data.kk_tree,subset=train)
summary(tree.M)
plot(tree.M)
text(tree.M,pretty=0)

cv.M=cv.tree(tree.M)
plot(cv.M$size,cv.M$dev,type='b')

#Pruned tree = Most complicated Tree. 
prune.M=prune.tree(tree.M,best=8)
plot(prune.M)
text(prune.M,pretty=0)

yhat=predict(tree.M,newdata=M.data.kk_tree[-train,])
M.test=M.data.kk_tree[-train,"S_gov_fund"]
plot(yhat,M.test)
abline(0,1)
MSE.tree=mean((yhat-M.test)^2)
print("MSE with unpruned tree")
print(MSE.tree)
# Almost identical with Lasso or Ridge

## Bagging

set.seed(1)
bag.M=randomForest(S_gov_fund~.,data=M.data.kk_tree,subset=train,mtry=87,importance=TRUE)

yhat.bag=predict(bag.M,newdata=M.data.kk_tree[-train,])
plot(yhat.bag, M.test)
abline(0,1)
MSE.bag=mean((yhat.bag - M.test)^2)
print("MSE with  bagging")
print(MSE.bag)

bag.M.25=randomForest(S_gov_fund~.,data=M.data.kk_tree,subset=train,mtry=87,ntree=25)
yhat.bag.25=predict(bag.M.25,newdata=M.data.kk_tree[-train,])
plot(yhat.bag.25, M.test)
abline(0,1)
MSE.bag.25=mean((yhat.bag.25-M.test)^2)
print("MSE with  bagging.ntree=25")
print(MSE.bag.25)

## Random Forest

set.seed(1)
# for m. we use default value p/3. But since Bagging used 88. We use 88 as p, instead of 95
rf.M=randomForest(S_gov_fund~.,data=M.data.kk_tree,subset=train,mtry=round(87/3),importance=TRUE)
yhat.rf=predict(rf.M,newdata=M.data.kk_tree[-train,])
MSE.rf=mean((yhat.rf-M.test)^2)
plot(yhat.rf, M.test)
abline(0,1)
print("MSE with random forest")
print(MSE.rf)
importance(rf.M)
varImpPlot(rf.M)

## Boosting
set.seed(1)
boost.M=gbm(S_gov_fund~.,data=M.data.kk_tree[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.M)
par(mfrow=c(2,2))
plot(boost.M,i="S_dsum2010")
plot(boost.M,i="S_kk")
plot(boost.M,i="S_da2010")
plot(boost.M,i="S_sci_total")
yhat.boost=predict(boost.M,newdata=M.data.kk_tree[-train,],n.trees=5000)
MSE.boost=mean((yhat.boost-M.test)^2)
print("MSE with boosting.lambda=0.001")
print(MSE.boost)

#with faster learning
set.seed(1)
boost.M.fast=gbm(S_gov_fund~.,data=M.data.kk_tree[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.01,verbose=FALSE)
yhat.boost.fast=predict(boost.M.fast,newdata=M.data.kk_tree[-train,],n.trees=5000)
MSE.boost.fast=mean((yhat.boost.fast-M.test)^2)
print("MSE with boosting. lambda=0.01")
print(MSE.boost.fast) # better choice. fast learning reduces mse a bit

set.seed(1)
boost.M.fast=gbm(S_gov_fund~.,data=M.data.kk_tree[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.05,verbose=FALSE)
yhat.boost.fast=predict(boost.M.fast,newdata=M.data.kk_tree[-train,],n.trees=5000)
MSE.boost.fast=mean((yhat.boost.fast-M.test)^2)
print("MSE with boosting. lambda=0.05")
print(MSE.boost.fast) # bad choice. Too fast learning increases mse a bit


set.seed(1)
boost.M.fast=gbm(S_gov_fund~.,data=M.data.kk_tree[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.1,verbose=FALSE)
yhat.boost.fast=predict(boost.M.fast,newdata=M.data.kk_tree[-train,],n.trees=5000)
MSE.boost.fast=mean((yhat.boost.fast-M.test)^2)
print("MSE with boosting. lambda=0.1")
print(MSE.boost.fast) # bad choice. Too fast learning increases mse a bit

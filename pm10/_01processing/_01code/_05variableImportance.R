
##Data load
merged.edt <- read.csv("D:/01 Study/08 pm/processing/input/result/pm10_sig_matching_75_mean_agg_weather_merged_edt.csv")

colnames(merged.edt)
merged.edt$year <- as.factor(merged.edt$year)
merged.edt$month <- as.factor(merged.edt$month)
str(merged.edt$year)
str(merged.edt$month)


##Data load
merged.ex1 <- read.csv("D:/my-backup/project/pm10/_01processing/_02output/_04merge/merged.ex1.csv", header=T, sep=",", stringsAsFactors = FALSE)
merged.ex2 <- read.csv("D:/my-backup/project/pm10/_01processing/_02output/_04merge/merged.ex2.csv", header=T, sep=",", stringsAsFactors = FALSE)
merged.ex3 <- read.csv("D:/my-backup/project/pm10/_01processing/_02output/_04merge/merged.ex3.csv", header=T, sep=",", stringsAsFactors = FALSE)
merged.ex4 <- read.csv("D:/my-backup/project/pm10/_01processing/_02output/_04merge/merged.ex4.csv", header=T, sep=",", stringsAsFactors = FALSE)

#########################################################################
##randomForest pk test
require(randomForest)
merged.ex1[merged.ex1==""] <- NA
fit=randomForest(formula=PM10~., data=merged.ex1, na.action=na.roughfix)
vi_fit=importance(fit, type=1, scale = TRUE)
varImpPlot(fit, type=1)

importanceOrder=order(-fit$importance)
names=rownames(fit$importance)[importanceOrder][1:15]
par(mfrow=c(5,3), xpd=NA)
for (name in names) {
  partialPlot(fit, merged.edt, eval(name), main=name, xlab=name, ylim=c(-2,9))
}

require(caret)
varImp(fit)

##rpart pk test
library(rpart)
fit.rpart=rpart(formula=PM10~., data=merged.edt, na.action=na.roughfix)
plot(fit.rpart)
text(fit.rpart)
summary(fit.rpart)

source("http://www.maths.tcd.ie/~louis/DataMining/importance.R")
vi_fit.rpart = importance(fit.rpart)

tmp=rownames(fit.rpart$splits)
allVars=colnames(attributes(fit.rpart$terms)$factors)  
rownames(fit.rpart$splits)=1:nrow(fit.rpart$splits)
splits=data.frame(fit.rpart$splits)
splits$var=tmp


> splits$type=""
> frame=as.data.frame(fit$frame)
> index=0
> for(i in 1:nrow(frame)){
  +   if(frame$var[i] != ""){
    +   index=index + 1
    +   splits$type[index]="primary"
    +   if(frame$ncompete[i] > 0){
      +   for(j in 1:frame$ncompete[i]){
        +   index=index + 1
        +   splits$type[index]="competing"}}
    +   if(frame$nsurrogate[i] > 0){
      +   for(j in 1:frame$nsurrogate[i]){
        +      index=index + 1
        +      splits$type[index]="surrogate"}}}}
> splits$var=factor(as.character(splits$var))
> splits=subset(splits, type != "surrogate")
> out=aggregate(splits$improve,
                +     list(Variable = splits$var),
                +     sum, na.rm = TRUE)
> allVars=colnames(attributes(fit$terms)$factors)
>  if(!all(allVars %in% out$Variable)){
  +   missingVars=allVars[!(allVars %in% out$Variable)]
  +   zeros=data.frame(x = rep(0, length(missingVars)), Variable = missingVars)
  +   out=rbind(out, zeros)}
> out2=data.frame(Overall = out$x)
> rownames(out2)=out$Variable
> out2
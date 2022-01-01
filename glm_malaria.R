rm(list = ls())
library(MASS)
library(readxl)


model<- read_excel("glm-R0.xlsx")
t1=900
t=t1*0.7
train=model[(1:t),]
test=model[((t+1):t1),]
#train 11 model
glm1=glm.nb((inci)~log(popu),data=train)
glm2=glm.nb((inci)~log(popu)+log(GDP)+mediratio+cropland+grassland+water+forest,data=train)
glm3=glm.nb((inci)~log(popu)+log(GDP)+mediratio,data=train)
glm4=glm.nb((inci)~log(popu)+cropland+grassland+water+forest,data=train)
glm5=glm.nb((inci)~(log(popu))+R0,data=train)
glm6=glm.nb((inci)~R0,data=train)
glm7=glm.nb((inci)~R0+log(GDP)+mediratio+cropland+grassland+water+forest+(log(popu)),data=train)
glm8=glm.nb((inci)~R0+log(GDP)+mediratio+log(popu),data=train)
glm9=glm.nb((inci)~R0+log(GDP)+log(popu),data=train)
glm10=glm.nb((inci)~R0+mediratio+log(popu),data=train)
glm11=glm.nb((inci)~R0+cropland+grassland+water+forest+log(popu),data=train)


glm2=glm.nb((case)~offset(log(popu))+log(GDP)+mediratio+cropland+grassland+water+forest,data=train)
glm3=glm.nb((case)~offset(log(popu))+log(GDP)+mediratio,data=train)
glm4=glm.nb((case)~offset(log(popu))+cropland+grassland+water+forest,data=train)
glm5=glm.nb((case)~offset(log(popu))+R0,data=train)
glm6=glm.nb((case)~R0,data=train)
glm7=glm.nb((case)~R0+log(GDP)+mediratio+cropland+grassland+water+forest+offset(log(popu)),data=train)
glm8=glm.nb((case)~R0+log(GDP)+mediratio+offset(log(popu)),data=train)
glm9=glm.nb((case)~R0+log(GDP)+offset(log(popu)),data=train)
glm10=glm.nb((case)~R0+mediratio+offset(log(popu)),data=train)
glm11=glm.nb((case)~R0+cropland+grassland+water+forest+offset(log(popu)),data=train)

summary(glm1)
summary(glm2)
summary(glm3)
summary(glm4)
summary(glm5)
summary(glm6)
summary(glm7)
summary(glm8)
summary(glm9)
summary(glm10)
summary(glm11)


####glm1
#test
pred <- predict.glm(glm1,test)
plot(model$inci[1:t1],col="black",type = "l",lwd=1.5,ylab="inci")
lines((glm1$fit),col="red",type="l",lwd=1.5)
p = ts(pred,start=c(t+1))
lines((exp(p)),col="blue",type="l",lwd=1.5)
legend("topright",legend=c("observe","train","test"),col=c("black","red","blue"),lwd=1.8)
#计算R squre
yi_tr<-(train$inci)
yii_tr<-(glm1$fit)
ssres_tr=sum((yi_tr-yii_tr)^2)
sstot_tr=sum((yi_tr-mean(yi_tr))^2)
r2_tr=1-ssres_tr/sstot_tr

yi_te<-(test$inci)
yii_te<-exp((pred))
ssres_te=sum((yi_te-yii_te)^2)
sstot_te=sum((yi_te-mean(yi_te))^2)
r2_te=1-ssres_te/sstot_te



####glm2
#test
pred <- predict.glm(glm2,test)
plot(model$inci[1:t1],col="black",type = "l",lwd=1.5,ylab="inci")
lines((glm2$fit),col="red",type="l",lwd=1.5)
p = ts(pred,start=c(t+1))
lines((exp(p)),col="blue",type="l",lwd=1.5)
legend("topright",legend=c("observe","train","test"),col=c("black","red","blue"),lwd=1.8)
#计算R-square
yi_tr<-(train$inci)
yii_tr<-(glm2$fit)
ssres_tr=sum((yi_tr-yii_tr)^2)
sstot_tr=sum((yi_tr-mean(yi_tr))^2)
r2_tr=1-ssres_tr/sstot_tr

yi_te<-(test$inci)
yii_te<-exp((pred))
ssres_te=sum((yi_te-yii_te)^2)
sstot_te=sum((yi_te-mean(yi_te))^2)
r2_te=1-ssres_te/sstot_te


####glm3
#test
pred <- predict.glm(glm3,test)
plot(model$inci[1:t1],col="black",type = "l",lwd=1.5,ylab="inci")
lines((glm3$fit),col="red",type="l",lwd=1.5)
p = ts(pred,start=c(t+1))
lines((exp(p)),col="blue",type="l",lwd=1.5)
legend("topright",legend=c("observe","train","test"),col=c("black","red","blue"),lwd=1.8)
#计算R-square
yi_tr<-(train$inci)
yii_tr<-(glm3$fit)
ssres_tr=sum((yi_tr-yii_tr)^2)
sstot_tr=sum((yi_tr-mean(yi_tr))^2)
r2_tr=1-ssres_tr/sstot_tr

yi_te<-(test$inci)
yii_te<-exp((pred))
ssres_te=sum((yi_te-yii_te)^2)
sstot_te=sum((yi_te-mean(yi_te))^2)
r2_te=1-ssres_te/sstot_te



####glm4
#test
pred <- predict.glm(glm4,test)
plot(model$inci[1:t1],col="black",type = "l",lwd=1.5,ylab="inci")
lines((glm4$fit),col="red",type="l",lwd=1.5)
p = ts(pred,start=c(t+1))
lines((exp(p)),col="blue",type="l",lwd=1.5)
legend("topright",legend=c("observe","train","test"),col=c("black","red","blue"),lwd=1.8)
#计算R-square
yi_tr<-(train$inci)
yii_tr<-(glm4$fit)
ssres_tr=sum((yi_tr-yii_tr)^2)
sstot_tr=sum((yi_tr-mean(yi_tr))^2)
r2_tr=1-ssres_tr/sstot_tr

yi_te<-(test$inci)
yii_te<-exp((pred))
ssres_te=sum((yi_te-yii_te)^2)
sstot_te=sum((yi_te-mean(yi_te))^2)
r2_te=1-ssres_te/sstot_te




####glm5
#test
pred <- predict.glm(glm5,test)
plot(model$inci[1:t1],col="black",type = "l",lwd=1.5,ylab="inci")
lines((glm5$fit),col="red",type="l",lwd=1.5)
p = ts(pred,start=c(t+1))
lines((exp(p)),col="blue",type="l",lwd=1.5)
legend("topright",legend=c("observe","train","test"),col=c("black","red","blue"),lwd=1.8)
#计算R-square
yi_tr<-(train$inci)
yii_tr<-(glm5$fit)
ssres_tr=sum((yi_tr-yii_tr)^2)
sstot_tr=sum((yi_tr-mean(yi_tr))^2)
r2_tr=1-ssres_tr/sstot_tr

yi_te<-(test$inci)
yii_te<-exp((pred))
ssres_te=sum((yi_te-yii_te)^2)
sstot_te=sum((yi_te-mean(yi_te))^2)
r2_te=1-ssres_te/sstot_te


####glm6
#test
pred <- predict.glm(glm6,test)
plot(model$inci[1:t1],col="black",type = "l",lwd=1.5,ylab="inci")
lines((glm6$fit),col="red",type="l",lwd=1.5)
p = ts(pred,start=c(t+1))
lines((exp(p)),col="blue",type="l",lwd=1.5)
legend("topright",legend=c("observe","train","test"),col=c("black","red","blue"),lwd=1.8)
#计算R-square
yi_tr<-(train$inci)
yii_tr<-(glm6$fit)
ssres_tr=sum((yi_tr-yii_tr)^2)
sstot_tr=sum((yi_tr-mean(yi_tr))^2)
r2_tr=1-ssres_tr/sstot_tr

yi_te<-(test$inci)
yii_te<-exp((pred))
ssres_te=sum((yi_te-yii_te)^2)
sstot_te=sum((yi_te-mean(yi_te))^2)
r2_te=1-ssres_te/sstot_te



####glm7
#test
pred <- predict.glm(glm7,test)
plot(model$inci[1:t1],col="black",type = "l",lwd=1.5,ylab="inci")
lines((glm7$fit),col="red",type="l",lwd=1.5)
p = ts(pred,start=c(t+1))
lines((exp(p)),col="blue",type="l",lwd=1.5)
legend("topright",legend=c("observe","train","test"),col=c("black","red","blue"),lwd=1.8)
#计算R-square
yi_tr<-(train$inci)
yii_tr<-(glm7$fit)
ssres_tr=sum((yi_tr-yii_tr)^2)
sstot_tr=sum((yi_tr-mean(yi_tr))^2)
r2_tr=1-ssres_tr/sstot_tr

yi_te<-(test$inci)
yii_te<-exp((pred))
ssres_te=sum((yi_te-yii_te)^2)
sstot_te=sum((yi_te-mean(yi_te))^2)
r2_te=1-ssres_te/sstot_te



####glm8
#test
pred <- predict.glm(glm8,test)
plot(model$inci[1:t1],col="black",type = "l",lwd=1.5,ylab="inci")
lines((glm8$fit),col="red",type="l",lwd=1.5)
p = ts(pred,start=c(t+1))
lines((exp(p)),col="blue",type="l",lwd=1.5)
legend("topright",legend=c("observe","train","test"),col=c("black","red","blue"),lwd=1.8)
#计算R-square
yi_tr<-(train$inci)
yii_tr<-(glm8$fit)
ssres_tr=sum((yi_tr-yii_tr)^2)
sstot_tr=sum((yi_tr-mean(yi_tr))^2)
r2_tr=1-ssres_tr/sstot_tr

yi_te<-(test$inci)
yii_te<-exp((pred))
ssres_te=sum((yi_te-yii_te)^2)
sstot_te=sum((yi_te-mean(yi_te))^2)
r2_te=1-ssres_te/sstot_te



####glm9
#test
pred <- predict.glm(glm9,test)
plot(model$inci[1:t1],col="black",type = "l",lwd=1.5,ylab="inci")
lines((glm9$fit),col="red",type="l",lwd=1.5)
p = ts(pred,start=c(t+1))
lines((exp(p)),col="blue",type="l",lwd=1.5)
legend("topright",legend=c("observe","train","test"),col=c("black","red","blue"),lwd=1.8)
#计算R-square
yi_tr<-(train$inci)
yii_tr<-(glm9$fit)
ssres_tr=sum((yi_tr-yii_tr)^2)
sstot_tr=sum((yi_tr-mean(yi_tr))^2)
r2_tr=1-ssres_tr/sstot_tr

yi_te<-(test$inci)
yii_te<-exp((pred))
ssres_te=sum((yi_te-yii_te)^2)
sstot_te=sum((yi_te-mean(yi_te))^2)
r2_te=1-ssres_te/sstot_te




####glm10
#test
pred <- predict.glm(glm10,test)
plot(model$inci[1:t1],col="black",type = "l",lwd=1.5,ylab="inci")
lines((glm10$fit),col="red",type="l",lwd=1.5)
p = ts(pred,start=c(t+1))
lines((exp(p)),col="blue",type="l",lwd=1.5)
legend("topright",legend=c("observe","train","test"),col=c("black","red","blue"),lwd=1.8)
#计算R-square
yi_tr<-(train$inci)
yii_tr<-(glm10$fit)
ssres_tr=sum((yi_tr-yii_tr)^2)
sstot_tr=sum((yi_tr-mean(yi_tr))^2)
r2_tr=1-ssres_tr/sstot_tr

yi_te<-(test$inci)
yii_te<-exp((pred))
ssres_te=sum((yi_te-yii_te)^2)
sstot_te=sum((yi_te-mean(yi_te))^2)
r2_te=1-ssres_te/sstot_te




####glm11
#test
pred <- predict.glm(glm11,test)
plot(model$inci[1:t1],col="black",type = "l",lwd=1.5,ylab="inci")
lines((glm11$fit),col="red",type="l",lwd=1.5)
p = ts(pred,start=c(t+1))
lines((exp(p)),col="blue",type="l",lwd=1.5)
legend("topright",legend=c("observe","train","test"),col=c("black","red","blue"),lwd=1.8)
#计算R-square
yi_tr<-(train$inci)
yii_tr<-(glm11$fit)
ssres_tr=sum((yi_tr-yii_tr)^2)
sstot_tr=sum((yi_tr-mean(yi_tr))^2)
r2_tr=1-ssres_tr/sstot_tr

yi_te<-(test$inci)
yii_te<-exp((pred))
ssres_te=sum((yi_te-yii_te)^2)
sstot_te=sum((yi_te-mean(yi_te))^2)
r2_te=1-ssres_te/sstot_te
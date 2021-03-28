library(MASS)
datadir = "http://www.uio.no/studier/emner/matnat/math/STK2100/data/" 
nuclear = read.table(paste(datadir,"nuclear.dat",sep=""),header=T)
n = nrow( nuclear )

attach(nuclear)
nuclear["cost"] = log(cost)
names(nuclear)[names(nuclear) == "cost"] <- "logcost"
attach(nuclear)
head(nuclear)

nuclear.plot = subset(nuclear,select = -c(pr,ne,ct,bw,pt))
pairs(nuclear.plot)

boxplot(logcost ~ pr)
boxplot(logcost ~ ne)
boxplot(logcost ~ ct)
boxplot(logcost ~ bw)
boxplot(logcost ~ pt)

fit1 = lm(logcost ~.,data=nuclear)
summary(fit1)

fit2 = lm(logcost ~.-t1,data=nuclear)
summary(fit2)

fit3 = lm(logcost ~.-t1 -bw -pr-t2 -cum.n -ct,data=nuclear)
summary(fit3)
plot(fit3)
yHat = predict(fit3,newdata = nuclear)
yi = logcost
MSE = 1/n * sum((yi - yHat)^2)


fullModel = lm(logcost ~.,data = nuclear)
AICModel = stepAIC(fullModel,direction = "backward", k =2)
BICModel = stepAIC(fullModel,direction = "backward", k =log(n))
summary(AICModel)
summary(BICModel)

d.new = data.frame(date=70.0,t1=13,t2=50,cap=800,pr=1,ne=0,ct=0,bw=1,cum.n=8,pt=1)
thetaManually = predict(fit3,d.new)
thetaAIC = predict(AICModel,d.new)
thetaBIC = predict(BICModel,d.new)

sigmaManually = summary(fit3)$sigma
sigmaAIC = summary(AICModel)$sigma
sigmaBIC = summary(BICModel)$sigma

etaManually = exp(thetaManually + (1/2) * sigmaManually^2 )
etaAIC = exp(thetaAIC + (1/2) * sigmaAIC^2 )
etaBIC = exp(thetaBIC + (1/2) * sigmaBIC^2 )

datadir = "http://www.uio.no/studier/emner/matnat/math/STK2100/data/" 
nuclear = read.table(paste(datadir,"nuclear.dat",sep=""),header=T)
attach(nuclear)

n = nrow(nuclear)
ind = sample(1:n,n/2,replace=FALSE)
RMSE.test1 = rep(NA,11) 
RMSE.test2 = rep(NA,11) 
model_narrow = lm(log(cost) ~ 1, data = nuclear)
model_wide = lm(log(cost) ~ ., data = nuclear)
for(i in 0:10)
{
  
  fit = stepAIC(model_narrow, direction="forward", steps=i,data=nuclear[ind,],
                scope=list(lower=model_narrow, upper=model_wide), k = 0)
  pred = predict(fit,nuclear[-ind,])
  RMSE.test1[i+1] = sqrt(mean((log(nuclear$cost)-pred)^2))
  RMSE.test2[i+1] = sqrt(mean((nuclear$cost-exp(pred))^2))
  
}
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(0:10,RMSE.test1,xlab="Complexity",ylab="RMSE1",type="l") # first plot
par(new = TRUE)
plot(0:10,RMSE.test2, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "",col=2)
axis(side=4, at = pretty(range(RMSE.test2)))
mtext("RMSE2", side=4, line=3)

library(lmvar)
RMSE.cv1 = rep(0,10)
RMSE.cv2 = rep(0,10)
for(i in 0:10)
{
  fit = stepAIC(model_narrow, direction="forward", steps=i,k=0,
                scope=list(lower=model_narrow, upper=model_wide),trace=0)
  fit = lm(formula(fit),data=nuclear,x=TRUE,y=TRUE)
  #Note: the k in the command below has a different meaning than k above!!!
  RMSE.cv1[i+1] = cv.lm(fit,k=10)$MSE$mean
  RMSE.cv2[i+1] = cv.lm(fit,k=10,log=TRUE)$MSE$mean
}
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(0:10,RMSE.cv1,xlab="Complexity",ylab="RMSE1",type="l") # first plot
par(new = TRUE)
plot(0:10,RMSE.cv2, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "",col=2)
axis(side=4, at = pretty(range(RMSE.cv2)))
mtext("RMSE2", side=4, line=3)

datadir = "http://www.uio.no/studier/emner/matnat/math/STK2100/data/"
Fe <- read.table(paste(datadir,"fe.txt",sep=""),
                 header=T,sep=",")
fit <- lm(Fe~form,data=Fe)
summary(fit)
Fe$form <- as.factor(Fe$form)
fit1 <- lm(Fe~form-1,data=Fe)
summary(fit1)
options()$contrasts
options(contrasts=c("contr.treatment","contr.treatment"))
fit2 <- lm(Fe~form,data=Fe)
summary(fit2)
options(contrasts=c("contr.sum","contr.sum"))
options()$contrasts
fit3 <- lm(Fe~form,data=Fe)
summary(fit3)
-sum(fit3$coefficients[-1])

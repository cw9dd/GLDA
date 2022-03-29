#dat=read.csv('rev.csv')
dat=read.csv('revii.csv')
View(dat)

summary(lm(avgbrier~gad,dat))

summary(lm(avgbrier~sex+age+hama+hamd,dat))

summary(lm(avgbrier~anger+gad,dat))
summary(lm(avgbrier~anger,dat))

#summary(lm(avgauc~anger,dat))
#summary(lm(bestauc~anger,dat))
#summary(lm(lowbrier~anger,dat))
#summary(lm(acutebrier~anger,dat))

hist(dat$avgbrier)
hist(dat$avgauc)

library(glmnet)
library(psych)
options(max.print=1000000)
describe(dat)

mat1=data.matrix(dat[,c(25:51,53,54)])
mat2=data.matrix(dat[,c(25:31,55:74,76,77)])
mat3=data.matrix(dat[,c(25:51,53,54,78:97,99,100)])
mat4=data.matrix(dat[,c(25:31,55:74,76,77,101:120,122,123)])

set.seed(1234)
m1=cv.glmnet(mat1,dat$avgbrier,family='gaussian',standardize=T,alpha=.5)
c1=coef(m1, s = "lambda.min")
c1

set.seed(1234)
m2=cv.glmnet(mat2,dat$avgbrier,family='gaussian',standardize=T,alpha=.5)
c2=coef(m2, s = "lambda.min")
c2

set.seed(1234)
m3=cv.glmnet(mat3,dat$avgbrier,family='gaussian',standardize=T,alpha=.5)
c3=coef(m3, s = "lambda.min")
c3

set.seed(1234)
m4=cv.glmnet(mat4,dat$avgauc,family='gaussian',standardize=T,alpha=.5)
c4=coef(m4, s = "lambda.min")
c4
predL1=predict.cv.glmnet(regL1, newx=matL1.2, s = "lambda.min", type = "link")
testL0$regpred1=as.numeric(predL1)

set.seed(1234)
m1=cv.glmnet(mat1,dat$avgauc,family='gaussian',standardize=T,alpha=.5)
c1=coef(m1, s = "lambda.min")
c1

set.seed(1234)
m2=cv.glmnet(mat2,dat$avgauc,family='gaussian',standardize=T,alpha=.5)
c2=coef(m2, s = "lambda.min")
c2

set.seed(1234)
m3=cv.glmnet(mat3,dat$avgauc,family='gaussian',standardize=T,alpha=.5)
c3=coef(m3, s = "lambda.min")
c3

set.seed(1234)
m4=cv.glmnet(mat4,dat$avgauc,family='gaussian',standardize=T,alpha=.5)
c4=coef(m4, s = "lambda.min")
c4

predL1=predict.cv.glmnet(regL1, newx=matL1.2, s = "lambda.min", type = "link")
testL0$regpred1=as.numeric(predL1)


######################################
#############  Figures  ##############
######################################

#Average accuracy

#AUC
hist(dat$avgauc,xlab='Area Under the Curve',ylab='Frequency',main='')
mean(dat$avgauc,na.rm=T) #.77
sd(dat$avgauc,na.rm=T) #.10

pdf('avgauc.pdf')
par(mar=c(5.1,4.6,4.1,3))
hist(dat$avgauc, col=rgb(1,0,0,0.5),xlim=c(.5,1), ylim=c(0,10), main='',
     xlab='Area Under the Curve',ylab='Frequency',breaks=10,cex.axis=1,cex.lab=1.5)
box()
abline(v = mean(dat$avgauc,na.rm=TRUE), col = rgb(1,0,0,1), lwd = 2)
legend(x = "topright",c("Mean = .77", "SD = .10"),cex=.85)
dev.off()

#Brier
hist(dat$avgbrier,xlab='Brier Score',ylab='Frequency',main='')
mean(dat$avgbrier,na.rm=T) #.172
sd(dat$avgbrier,na.rm=T) #.036

pdf('avgbrier.pdf')
par(mar=c(5.1,4.6,4.1,3))
hist(dat$avgbrier, col=rgb(1,0,0,0.5),xlim=c(.095,.255), ylim=c(0,9), main='',
     xlab='Brier Score',ylab='Frequency',breaks=10,cex.axis=1,cex.lab=1.5)
box()
abline(v = mean(dat$avgbrier,na.rm=TRUE), col = rgb(1,0,0,1), lwd = 2)
legend(x = "topright",c("Mean = .172", "SD = .036"),cex=.85)
dev.off()

#Active Distress States

#AUC
hist(dat$acuteauc,xlab='Area Under the Curve',ylab='Frequency',main='')
mean(dat$acuteauc,na.rm=T) #.82
sd(dat$acuteauc,na.rm=T) #.11

pdf('acuteauc.pdf')
par(mar=c(5.1,4.6,4.1,3))
hist(dat$acuteauc, col=rgb(1,0,0,0.5),xlim=c(.5,1), ylim=c(0,12), main='',
     xlab='Area Under the Curve',ylab='Frequency',breaks=10,cex.axis=1,cex.lab=1.5)
box()
abline(v = mean(dat$acuteauc,na.rm=TRUE), col = rgb(1,0,0,1), lwd = 2)
legend(x = "topright",c("Mean = .82", "SD = .11"),cex=.85)
dev.off()

#Brier
hist(dat$acutebrier,xlab='Brier Score',ylab='Frequency',main='')
mean(dat$acutebrier,na.rm=T) #.150
sd(dat$acutebrier,na.rm=T) #.055

pdf('acutebrier.pdf')
par(mar=c(5.1,4.6,4.1,3))
hist(dat$acutebrier, col=rgb(1,0,0,0.5),xlim=c(.04,.3), ylim=c(0,10), main='',
     xlab='Brier Score',ylab='Frequency',breaks=10,cex.axis=1,cex.lab=1.5)
box()
abline(v = mean(dat$acutebrier,na.rm=TRUE), col = rgb(1,0,0,1), lwd = 2)
legend(x = "topright",c("Mean = .150", "SD = .055"),cex=.85)
dev.off()

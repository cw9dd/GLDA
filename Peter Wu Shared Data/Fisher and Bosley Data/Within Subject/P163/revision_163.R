

#############################################
##########     Fisher & Bosley     ##########
##########        Revision         ##########
#############################################

#################################################################
##################   LIBRARIES AND FUNCTIONS ####################
#################################################################
rm(list=ls())
lagpad <- function(x, k) {
  c(rep(NA, k), x)[1 : length(x)] 
}

library(psych)
library(glmnet)
library(Hmisc)
library(fmsb)
library(DataCombine)
library(tidyLPA)
library(tidyverse)
library(dplyr)
library(mclust)
library(mgm)
library(qgraph)
library(pROC)
library(reshape2)
source('beepday2consec.R', encoding = 'UTF-8')
source('lagData.R', encoding = 'UTF-8')

#See external .R files for 'beepday2consec' and 'lagData' functions
#Taken from mgm mvar internal code

options(width=90)

#################################################################
##################  DATA SETUP AND CLEANING #####################
#################################################################

#Read in data
data=read.csv('dat.csv',as.is=TRUE)
data = data[,2:29]

#Rename
colnames(data) <- c("start","finish","energetic","enthusiastic","content","irritable","restless","worried","guilty","afraid","anhedonia","angry","hopeless","down","positive","fatigue","tension","concentrate","accepted","threatened","ruminate","avoid_act","reassure","procrast","hours","difficult","unsatisfy","avoid_people")

#Duplicate and lag time
data$lag=lagpad(data$start,1)

#Calculate time differences
data$tdif=as.numeric(difftime(strptime(data$start,"%m/%d/%Y %H:%M"),strptime(data$lag,"%m/%d/%Y %H:%M")))

#Replace NA
data$tdif[is.na(data$tdif)]<- 0

#Calculate cumulative sum of numeric elapsed time
data$cumsumT=cumsum(data$tdif)

# how many obs, and how many are complete
nrow(data)
length(which(complete.cases(data[,3])))

#Trim time series to even 4obs/day
#first visualize and determine where to trim.
options(width=90)
data$start
dati=data[,]
length(which(complete.cases(dati[,3])))

#Use for internal missing rows: add a row where it's missing
new <- rep(NA, length(dati))
dati <- InsertRow(dati, NewRow=new, RowNum = 1)
dati <- InsertRow(dati, NewRow=new, RowNum = 2)
dati <- InsertRow(dati, NewRow=new, RowNum = 45)
dati <- InsertRow(dati, NewRow=new, RowNum = 114)
dati <- InsertRow(dati, NewRow=new, RowNum = 143)
dati$start

#Code days of the week
dati$day <- rep(1:7, each=4, length.out=nrow(dati))
# 1 = Friday
dati$mon=ifelse(dati$day==4,1,0)
dati$tues=ifelse(dati$day==5,1,0)
dati$wed=ifelse(dati$day==6,1,0)
dati$thur=ifelse(dati$day==7,1,0)
dati$fri=ifelse(dati$day==1,1,0)
dati$sat=ifelse(dati$day==2,1,0)
dati$sun=ifelse(dati$day==3,1,0)

#Code pings
dati$ping=seq(0,3,1)
dati$morning=ifelse(dati$ping==0,1,0)
dati$midday=ifelse(dati$ping==1,1,0)
dati$eve=ifelse(dati$ping==2,1,0)
dati$night=ifelse(dati$ping==3,1,0)
datx=dati

#Temporal variables
datx$linear=scale(datx$cumsumT)
datx$quad=datx$linear^2
datx$cub=datx$linear^3
datx$cosT=cos(((2*pi)/24)*datx$cumsumT)
datx$sinT=sin(((2*pi)/24)*datx$cumsumT)
datx$cos2T=cos(((2*pi)/12)*datx$cumsumT)
datx$sin2T=sin(((2*pi)/12)*datx$cumsumT)
datx$cosW=cos(((2*pi)/168)*datx$cumsumT)
datx$sinW=sin(((2*pi)/168)*datx$cumsumT)

#Index consecutive measurements by ping and day
datx$dayvar=rep(1:(nrow(datx)/4),each=4)
datx$beepvar=datx$ping+1

#Create filter to mark cases with missing data
datx$filter=ifelse(!complete.cases(datx[,c(3:24)]),1,0)

## Remove NAs, suppress row names
daty=subset(datx,filter==0)
row.names(daty)<- NULL
View(daty)

########################################################################
##################  Gaussian Finite Mixture Model  #####################
##################    (Latent Profile Analysis)    #####################
########################################################################

#Subset data for analysis
clustdat <- as.data.frame(scale(daty[, c(6,12,10,8,21,14,13,11,22,28)]))
colnames(clustdat)<- c("Irritable", "Angry", "Afraid", "Worried", "Ruminating", "Down", "Hopeless", "Anhedonic", "Avoid_Act", "Avoid_People")

#Run mclust GFMM's with equal & varying variances, zero covariance
summary(Mclust(clustdat, modelName = c("EEI", "VII", "EEI", "VEI", "EVI", "VVI"))) 
#summary(Mclust(clustdat, G=1:4, modelName = c("EEI", "VII", "EEI", "VEI", "EVI", "VVI")))
#summary(Mclust(clustdat, G=1:5, modelName = c("EEI", "VII", "EEI", "VEI", "EVI", "VVI")))
#summary(Mclust(clustdat, G=1:4, modelName = c("EEI", "VII", "EEI", "VEI", "EVI", "VVI")))
#summary(Mclust(clustdat, G=1:3, modelName = c("EEI", "VII", "EEI", "VEI", "EVI", "VVI")))

#Verify model selection via BIC, ICL, and BLRT
BIC <- mclustBIC(clustdat,G=1:4, modelName = c("EEI", "VII", "EEI", "VEI", "EVI", "VVI"))
summary(BIC)
ICL <- mclustICL(clustdat,G=1:4, modelName = c("EEI", "VII", "EEI", "VEI", "EVI", "VVI"))
summary(ICL)
blrt=mclustBootstrapLRT(clustdat, maxG=3, modelName = "VVI")
blrt

#Save best model as an object
mod <- Mclust(clustdat, G = 3, modelName = "VVI") 
summary(mod, parameters=T)

#Save classification data
x=as.data.frame(cbind(mod$z,mod$classification,mod$uncertainty))
colnames(x) <- c("prob1", "prob2", "prob3", "class", "uncertain")
View(x)

#Clustering table:
#  1  2  3
# 50 34 39 

#Mixing probabilities:
#        1         2         3
#0.4057372 0.2859001 0.3083627 

#Extract means by class for symptoms/behaviors
means <- data.frame(mod$parameters$mean, stringsAsFactors = FALSE) %>%
  rownames_to_column() %>%
  rename(Variable = rowname) %>%
  melt(id.vars = "Variable", variable.name = "Class", value.name = "Mean") 

means$Class=recode(means$Class, X1 = "1", X2 = "2", X3 = "3")

#Plot profiles
pdf('p163_3class.pdf')
means%>%
  ggplot(aes(Variable, Mean, group = Class, color = Class)) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  scale_x_discrete(limits = c("Irritable", "Angry", "Afraid", "Worried", "Ruminating", "Down", "Hopeless", "Anhedonic", "Avoid_Act", "Avoid_People")) +
  labs(x = NULL, y = "Z-score") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")
dev.off()

################################################################################################
##################      Mixed Vector Autoregressive Model (mVAR)            ####################
##################   via elastic-net regularized Generalized Linear Model   ####################
################################################################################################

#Consecutive measurements
beepvar=daty$ping+1
dayvar=daty$dayvar

#Create dichotomous class variables
x$class1=ifelse(x$class==1,1,0)
x$class2=ifelse(x$class==2,1,0)
x$class3=ifelse(x$class==3,1,0)
#x$class4=ifelse(x$class==4,1,0)
#x$class5=ifelse(x$class==5,1,0)

psych::describe(x)

#Run mVAR
set.seed(1234)
var3=mvar(x[,6:8], type=c("c", "c", "c"), level=c(2, 2, 2), lamdaSel="EBIC",
          lags=1, beepvar=beepvar, dayvar=dayvar, alphaSeq=1, binarySign = TRUE, overparameterize = TRUE)

qgraph(t(var3$wadj[, , 1]),edge.color = t(var3$edgecolor[, , 1]))

pdf('p163_mVAR.pdf')
qgraph(t(var3$wadj[, , 1]),edge.color = t(var3$edgecolor[, , 1]))
dev.off()

########################################################################
################### Elastic Net Regression Models  ######################
#########################################################################

#Add classes to data frame
daty$uncertain=x$uncertain
daty$class=x$class
daty$class1=x$class1
daty$class2=x$class2
daty$class3=x$class3
#daty$class4=x$class4
#daty$class5=x$class5

#Visualize class frequencies
even=seq(0,nrow(daty)-1,1)
#plot(daty$class1~even,type='o')
#plot(daty$class2~even,type='o')
#plot(daty$class3~even,type='o')
#plot(daty$class4~even,type='o')

#Now we create separate lag0 (contemporaneous) and lag1 (lagged) data structures
#Functions 'beepday2consec' and 'lagData' taken from mgm mvar internal code

#Uses beepvar and dayvar info to index consecutive measurements
daty$consec <- beepday2consec(beepvar = beepvar, dayvar = dayvar)

#Object with original and lagged data
data_lagged <- lagData(data = daty, 
                       lags = 1, 
                       consec = daty$consec)

#Original & lagged data
data_response <- data_lagged$data_response
l_data_lags <- data_lagged$l_data_lags
#n_design <- nrow(data_response) #used in other mvar calculations

# delete rows that cannot be predicted
data_response <- data_response[data_lagged$included, ]
l_data_lags <- lapply(l_data_lags, function(x) x[data_lagged$included, ])

#Original dat, with appropriate rows for lagged analysis (L0 = lag zero)
datyL0=as.data.frame(data_response)

#Lagged data (L1 = lag one)
datyL1=as.data.frame(l_data_lags[[1]])

#Change column names of lagged data
colnames(datyL1)<-colnames(daty)

#Recode variables as numeric (via character)
datyL0[,c(3:24,28,30:31,45:53,57)] <- sapply(datyL0[,c(3:24,28,30:31,45:53,57)], as.character)
datyL0[,c(3:24,28,30:31,45:53,57)] <- sapply(datyL0[,c(3:24,28,30:31,45:53,57)], as.numeric)
datyL1[,c(3:24,28,30:31,45:53,57)] <- sapply(datyL1[,c(3:24,28,30:31,45:53,57)], as.character)
datyL1[,c(3:24,28,30:31,45:53,57)] <- sapply(datyL1[,c(3:24,28,30:31,45:53,57)], as.numeric)

#Inspect data
#View(datyL0)
#View(datyL1)

## Training/Testing ##
## 50% of the sample size
smp_size <- floor(0.5 * nrow(datyL1))

#Create random sets of 50% each
##set the seed to make your partition reproducible
set.seed(1234)
train_ind <- sample(seq_len(nrow(datyL1)), size = smp_size)

#Training and testing sets
trainL0 <- datyL0[train_ind, ]
testL0 <- datyL0[-train_ind, ]
trainL1 <- datyL1[train_ind, ]
testL1 <- datyL1[-train_ind, ]

#Prediction variables for regression models (training/testing)
#Prediction matrices (feature space)
#Vars from datyL1 are lagged, vars from datyL0 are contemporaneous with DV
#Psychosocial variables + time variables
matL1.1 = data.matrix(cbind(trainL1[,c(3:24,28)],trainL0[,c(33:39,42:53)]))
matL1.2 = data.matrix(cbind(testL1[,c(3:24,28)],testL0[,c(33:39,42:53)]))
#Psychosocial variables + time variables + uncertainty
matL2.1 = data.matrix(cbind(trainL1[,c(3:24,28)],trainL0[,c(33:39,42:53,57)]))
matL2.2 = data.matrix(cbind(testL1[,c(3:24,28)],testL0[,c(33:39,42:53,57)]))

##############################
########  Class 1  ###########
##############################

set.seed(1234)
regL1=cv.glmnet(matL1.1,trainL0$class1,family='binomial',standardize=T,alpha=.1)
coefL1=coef(regL1, s = "lambda.min")
coefL1

predL1=predict.cv.glmnet(regL1, newx=matL1.2, s = "lambda.min", type = "link")
testL0$regpred1=as.numeric(predL1)
pROC::auc(pROC::roc(testL0$class1~testL0$regpred1))
#0.71
pROC::coords((pROC::roc(testL0$class1~testL0$regpred1)),"best", ret=c("threshold", "specificity", "sensitivity"), transpose=FALSE)
#     threshold specificity sensitivity
#best -0.4888636         0.5           1
pred.prob1 <- predict.cv.glmnet(regL1, newx=matL1.2, s = "lambda.min", type = "response")
brierScore1 <- mean((pred.prob1-as.numeric(as.character(testL0$class1)))^2)
brierScore1
#0.208

##############################
########  Class 2  ###########
##############################

set.seed(1234)
regL2=cv.glmnet(matL1.1,trainL0$class2,family='binomial',standardize=T,alpha=.7)
coefL2=coef(regL2, s = "lambda.min")
coefL2

predL2=predict.cv.glmnet(regL2, newx=matL1.2, s = "lambda.min", type = "link")
testL0$regpred2=as.numeric(predL2)
pROC::auc(pROC::roc(testL0$class2~testL0$regpred2))
#0.86
pROC::coords((pROC::roc(testL0$class2~testL0$regpred2)),"best", ret=c("threshold", "specificity", "sensitivity"), transpose=FALSE)
#  threshold specificity sensitivity
# -1.597584   0.8965517        0.75
pred.prob2 <- predict.cv.glmnet(regL2, newx=matL1.2, s = "lambda.min", type = "response")
brierScore2 <- mean((pred.prob2-as.numeric(as.character(testL0$class2)))^2)
brierScore2
#0.146

##############################
########  Class 3  ###########
##############################

set.seed(1234)
regL3=cv.glmnet(matL1.1,trainL0$class3,family='binomial',standardize=T,alpha=.01)
coefL3=coef(regL3, s = "lambda.min")
coefL3

predL3=predict.cv.glmnet(regL3, newx=matL1.2, s = "lambda.min", type = "link")
testL0$regpred3=as.numeric(predL3)
pROC::auc(pROC::roc(testL0$class3~testL0$regpred3))
#0.68
pROC::coords((pROC::roc(testL0$class3~testL0$regpred3)),"best", ret=c("threshold", "specificity", "sensitivity"), transpose=FALSE)
# threshold specificity sensitivity 
# -0.1748854        0.96      0.4375
pred.prob3 <- predict.cv.glmnet(regL3, newx=matL1.2, s = "lambda.min", type = "response")
brierScore3 <- mean((pred.prob3-as.numeric(as.character(testL0$class3)))^2)
brierScore3
#0.219

##############################
########  Class 4  ###########
##############################

set.seed(1234)
regL4=cv.glmnet(matL1.1,trainL0$class4,family='binomial',standardize=T,alpha=.5)
coefL4=coef(regL4, s = "lambda.min")
coefL4

predL4=predict.cv.glmnet(regL4, newx=matL1.2, s = "lambda.min", type = "link")
testL0$regpred4=as.numeric(predL4)
pROC::auc(pROC::roc(testL0$class4~testL0$regpred4))
#0.64
pROC::coords((pROC::roc(testL0$class4~testL0$regpred4)),"best", ret=c("threshold", "specificity", "sensitivity"), transpose=FALSE)
#  threshold specificity sensitivity 
# -2.36743   0.6571429   0.7142857
pred.prob4 <- predict.cv.glmnet(regL4, newx=matL1.2, s = "lambda.min", type = "response")
brierScore4 <- mean((pred.prob4-as.numeric(as.character(testL0$class4)))^2)
brierScore4
#0.137


















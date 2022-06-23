


#Prepping data

natest = lapply(testlist, function(x) na.omit(x[,c(2, 11, 17, 5, 6)]))
trainz = nalist
testz = natest


for (i in 1:length(trainz)){
  trainz[[i]][,1:5] = scale(trainz[[i]][,1:5])
}

for (i in 1:length(testz)){
  testz[[i]][,1:5] = scale(testz[[i]][,1:5])
}

for (i in 1:length(testz)){
  testz[[i]]$time = seq(0, nrow(testz[[i]])-1, 1)
}


###########
### GLM ###
###########

###########################
###   c047 for Class 6  ###
###########################

#isolate c047 and remove from list
c047 = trainz$c047
trainz$c047 <- NULL

train_glm = glm(bClass6 ~ hr + rmssd, c047, family = binomial)

#Test training model in holdout data
test_glm = lapply(testz, function(x) predict(train_glm, x, type = "response"))
test_glmbin = lapply(test_glm, function(x) ifelse(x > 0.50, 1, 0))
glm_results = data.frame(id = names(test_glmbin))
for (i in 1:nrow(glm_results)) {
  glm_results$predhr[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], testz[[paste(glm_results$id[i])]]$hr)$estimate)^2  
  glm_results$predrsa[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], testz[[paste(glm_results$id[i])]]$rmssd)$estimate)^2  
  glm_results$predslope[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], testz[[paste(glm_results$id[i])]]$time)$estimate)  
  glm_results$hrslope[i] = (cor.test(testz[[paste(glm_results$id[i])]]$hr, testz[[paste(glm_results$id[i])]]$time)$estimate)  
  glm_results$rsaslope[i] = (cor.test(testz[[paste(glm_results$id[i])]]$rmssd, testz[[paste(glm_results$id[i])]]$time)$estimate)  
} 

mean(glm_results$predhr, na.rm = T) # 0.44
sd(glm_results$predhr, na.rm = T) # SD = 0.11
mean(glm_results$predrsa, na.rm = T) # 0.35
sd(glm_results$predrsa, na.rm = T) # SD = 0.14

length(which(is.na(glm_results$predhr) == F)) #56

summary(lm(hrslope ~ predslope, glm_results)) #R2 = 0.91
summary(lm(rsaslope ~ predslope, glm_results)) #R2 = 0.65

hist(glm_results$predhr)
hist(glm_results$predrsa)


########################################
#####     Splitting Train List     #####
########################################

##########################
##  Those with class 6  ##
##########################

groupdata$id[groupdata$cl6 == 1]

cl6list = trainz[c('p008', 'p010', 'p012', 'p023', 'p025', 'p027', 'p030', 'p035', 'p042', 'p044', 'p052',
                   'p061', 'p065', 'p074', 'p075', 'p076', 'p078', 'p082', 'p084', 'p086', 'p100', 'p113',
                   'p127', 'p129', 'p130', 'p137', 'p138', 'p139', 'p142', 'p143', 'p150', 'p154', 'p160',
                   'p162', 'p167', 'p172', 'p176', 'p186', 'p198', 'p200', 'p205', 'p206', 'p224', 'p230',
                   'p236', 'p237', 'p239', 'p241', 'p242', 'p244', 'p069', 'p134', 'p153', 'p164', 'c001',
                   'c002', 'c009', 'c015', 'c032', 'c033', 'c034', 'c039', 'c041', 'c048', 'c050', 'c052',
                   'c063', 'c065', 'c071', 'c072')]

train_glm = glm(bClass6 ~ hr + rmssd, c047, family = binomial)

#Test training model in holdout data
test_glm = lapply(cl6list, function(x) predict(train_glm, x, type = "response"))
test_glmbin = lapply(test_glm, function(x) ifelse(x > 0.50, 1, 0))
glm_results = data.frame(id = names(test_glmbin))
for (i in 1:nrow(glm_results)) {
  glm_results$predhr[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], cl6list[[paste(glm_results$id[i])]]$hr)$estimate)^2  
  glm_results$predrsa[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], cl6list[[paste(glm_results$id[i])]]$rmssd)$estimate)^2  
  glm_results$predslope[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], cl6list[[paste(glm_results$id[i])]]$time)$estimate)  
  glm_results$hrslope[i] = (cor.test(cl6list[[paste(glm_results$id[i])]]$hr, cl6list[[paste(glm_results$id[i])]]$time)$estimate)  
  glm_results$rsaslope[i] = (cor.test(cl6list[[paste(glm_results$id[i])]]$rmssd, cl6list[[paste(glm_results$id[i])]]$time)$estimate)  
} 

mean(glm_results$predhr, na.rm = T) # 0.52
mean(glm_results$predrsa, na.rm = T) # 0.44

length(which(is.na(glm_results$predhr) == F)) #70

summary(lm(hrslope ~ predslope, glm_results)) #R2 = 0.90
summary(lm(rsaslope ~ predslope, glm_results)) #R2 = 0.76

hist(glm_results$predhr)
hist(glm_results$predrsa)

#############################
##  Those without class 6  ##
#############################

groupdata$id[groupdata$cl6 == 0]

nocl6list = trainz[c('p003', 'p007', 'p009', 'p014', 'p019', 'p022', 'p028', 'p032', 'p039', 'p040', 'p041',
                     'p046', 'p048', 'p049', 'p051', 'p058', 'p062', 'p068', 'p072', 'p079', 'p083', 'p085',
                     'p088', 'p090', 'p092', 'p094', 'p095', 'p096', 'p098', 'p101', 'p103', 'p108', 'p110',
                     'p111', 'p114', 'p115', 'p117', 'p123', 'p125', 'p128', 'p132', 'p140', 'p146', 'p147',
                     'p155', 'p156', 'p159', 'p161', 'p175', 'p185', 'p191', 'p193', 'p196', 'p202', 'p203',
                     'p208', 'p212', 'p217', 'p218', 'p219', 'p220', 'p222', 'p229', 'p235', 'p240', 'p124',
                     'p126', 'p174', 'p177', 'p181', 'p182', 'p184', 'p188', 'p194', 'p197', 'c004', 'c005',
                     'c010', 'c012', 'c014', 'c019', 'c023', 'c024', 'c025', 'c026', 'c027', 'c028', 'c029',
                     'c031', 'c045', 'c046', 'c053', 'c057', 'c067', 'c068')]


train_glm = glm(bClass6 ~ hr + rmssd, c047, family = binomial)

#Test training model in holdout data
test_glm = lapply(nocl6list, function(x) predict(train_glm, x, type = "response"))
test_glmbin = lapply(test_glm, function(x) ifelse(x > 0.50, 1, 0))
glm_results = data.frame(id = names(test_glmbin))
for (i in 1:nrow(glm_results)) {
  glm_results$predhr[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], nocl6list[[paste(glm_results$id[i])]]$hr)$estimate)^2  
  glm_results$predrsa[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], nocl6list[[paste(glm_results$id[i])]]$rmssd)$estimate)^2  
  glm_results$predslope[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], nocl6list[[paste(glm_results$id[i])]]$time)$estimate)  
  glm_results$hrslope[i] = (cor.test(nocl6list[[paste(glm_results$id[i])]]$hr, nocl6list[[paste(glm_results$id[i])]]$time)$estimate)  
  glm_results$rsaslope[i] = (cor.test(nocl6list[[paste(glm_results$id[i])]]$rmssd, nocl6list[[paste(glm_results$id[i])]]$time)$estimate)  
} 

mean(glm_results$predhr, na.rm = T) # 0.40
mean(glm_results$predrsa, na.rm = T) # 0.33

length(which(is.na(glm_results$predhr) == F)) #104

summary(lm(hrslope ~ predslope, glm_results)) #R2 = 0.80
summary(lm(rsaslope ~ predslope, glm_results)) #R2 = 0.59

hist(glm_results$predhr)
hist(glm_results$predrsa)


###################################################
#####  Complete Training Set, Minus Exemplar  #####
###################################################

#isolate c047 and remove from list
trainz$c047 <- NULL

train_glm = glm(bClass6 ~ hr + rmssd, c047, family = binomial)

#Test training model in holdout data
test_glm = lapply(trainz, function(x) predict(train_glm, x, type = "response"))
test_glmbin = lapply(test_glm, function(x) ifelse(x > 0.50, 1, 0))
glm_results = data.frame(id = names(test_glmbin))
for (i in 1:nrow(glm_results)) {
  glm_results$predhr[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], trainz[[paste(glm_results$id[i])]]$hr)$estimate)^2  
  glm_results$predrsa[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], trainz[[paste(glm_results$id[i])]]$rmssd)$estimate)^2  
  glm_results$predslope[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], trainz[[paste(glm_results$id[i])]]$time)$estimate)  
  glm_results$hrslope[i] = (cor.test(trainz[[paste(glm_results$id[i])]]$hr, trainz[[paste(glm_results$id[i])]]$time)$estimate)  
  glm_results$rsaslope[i] = (cor.test(trainz[[paste(glm_results$id[i])]]$rmssd, trainz[[paste(glm_results$id[i])]]$time)$estimate)  
} 

mean(glm_results$predhr, na.rm = T) # R2 = 0.45
sd(glm_results$predhr, na.rm = T) # SD = 0.13
mean(glm_results$predrsa, na.rm = T) # R2 = 0.38
sd(glm_results$predrsa, na.rm = T) # SD = 0.15

length(which(is.na(glm_results$predhr) == F)) #165

summary(lm(hrslope ~ predslope, glm_results)) #R2 = 0.84
summary(lm(rsaslope ~ predslope, glm_results)) #R2 = 0.67

hist(glm_results$predhr)
hist(glm_results$predrsa)



























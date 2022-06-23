


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
###   p242 for Class 4  ###
###########################

#isolate P242 and remove from list
p242 = trainz$p242
trainz$p242 <- NULL

train_glm = glm(bClass4 ~ hr + rmssd, p242, family = binomial)

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

mean(glm_results$predhr, na.rm = T) # R2 = 0.48
sd(glm_results$predhr, na.rm = T) # SD = 0.11
mean(glm_results$predrsa, na.rm = T) # R2 = 0.32
sd(glm_results$predrsa, na.rm = T) # SD = 0.14

length(which(is.na(glm_results$predhr) == F)) #56

summary(lm(hrslope ~ predslope, glm_results)) #R2 = 0.93
summary(lm(rsaslope ~ predslope, glm_results)) #R2 = 0.61

hist(glm_results$predhr)
hist(glm_results$predrsa)

glm_results$predhr[glm_results$id == 'c036'] # 0.66
glm_results$predrsa[glm_results$id == 'c036'] # 0.26


########################################
#####     Splitting Train List     #####
########################################

##########################
##  Those with class 4  ##
##########################

groupdata$id[groupdata$cl4 == 1]

cl4list = trainz[c('p007', 'p008', 'p009', 'p025', 'p027', 'p028', 'p030', 'p042', 'p044', 'p049', 'p052',
                   'p061', 'p062', 'p065', 'p074', 'p076', 'p078', 'p079', 'p082', 'p086', 'p095', 'p098',
                   'p100', 'p127', 'p129', 'p130', 'p139', 'p142', 'p143', 'p146', 'p154', 'p162', 'p167',
                   'p185', 'p186', 'p198', 'p206', 'p219', 'p224', 'p236', 'p237', 'p242', 'p134', 'p153',
                   'p164', 'c001', 'c002', 'c015', 'c024', 'c028', 'c032', 'c033', 'c034', 'c039', 'c041',
                   'c047', 'c052', 'c057', 'c063', 'c065', 'c071', 'c072')]

cl4list$p242 <- NULL
train_glm = glm(bClass4 ~ hr + rmssd, p242, family = binomial)

#Test training model in holdout data
test_glm = lapply(cl4list, function(x) predict(train_glm, x, type = "response"))
test_glmbin = lapply(test_glm, function(x) ifelse(x > 0.50, 1, 0))
glm_results = data.frame(id = names(test_glmbin))
for (i in 1:nrow(glm_results)) {
  glm_results$predhr[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], cl4list[[paste(glm_results$id[i])]]$hr)$estimate)^2  
  glm_results$predrsa[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], cl4list[[paste(glm_results$id[i])]]$rmssd)$estimate)^2  
  glm_results$predslope[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], cl4list[[paste(glm_results$id[i])]]$time)$estimate)  
  glm_results$hrslope[i] = (cor.test(cl4list[[paste(glm_results$id[i])]]$hr, cl4list[[paste(glm_results$id[i])]]$time)$estimate)  
  glm_results$rsaslope[i] = (cor.test(cl4list[[paste(glm_results$id[i])]]$rmssd, cl4list[[paste(glm_results$id[i])]]$time)$estimate)  
} 

mean(glm_results$predhr, na.rm = T) # 0.54
mean(glm_results$predrsa, na.rm = T) # 0.44

length(which(is.na(glm_results$predhr) == F)) #61

summary(lm(hrslope ~ predslope, glm_results)) #R2 = 0.92
summary(lm(rsaslope ~ predslope, glm_results)) #R2 = 0.76

hist(glm_results$predhr)
hist(glm_results$predrsa)

#############################
##  Those without class 4  ##
#############################

groupdata$id[groupdata$cl4 == 0]

nocl4list = trainz[c('p003', 'p010', 'p012', 'p014', 'p019', 'p022', 'p023', 'p032', 'p035', 'p039', 'p040',
                     'p041', 'p046', 'p048', 'p051', 'p058', 'p068', 'p072', 'p075', 'p083', 'p084', 'p085',
                     'p088', 'p090', 'p092', 'p094', 'p096', 'p101', 'p103', 'p108', 'p110', 'p111', 'p113',
                     'p114', 'p115', 'p117', 'p123', 'p125', 'p128', 'p132', 'p137', 'p138', 'p140', 'p147',
                     'p150', 'p155', 'p156', 'p159', 'p160', 'p161', 'p172', 'p175', 'p176', 'p191', 'p193',
                     'p196', 'p200', 'p202', 'p203', 'p205', 'p208', 'p212', 'p217', 'p218', 'p220', 'p222',
                     'p229', 'p230', 'p235', 'p239', 'p240', 'p241', 'p244', 'p069', 'p124', 'p126', 'p174',
                     'p177', 'p181', 'p182', 'p184', 'p188', 'p194', 'p197', 'c004', 'c005', 'c009', 'c010',
                     'c012', 'c014', 'c019', 'c023', 'c025', 'c026', 'c027', 'c029', 'c031', 'c045', 'c046',
                     'c048', 'c050', 'c053', 'c067', 'c068')]


train_glm = glm(bClass4 ~ hr + rmssd, p242, family = binomial)

#Test training model in holdout data
test_glm = lapply(nocl4list, function(x) predict(train_glm, x, type = "response"))
test_glmbin = lapply(test_glm, function(x) ifelse(x > 0.50, 1, 0))
glm_results = data.frame(id = names(test_glmbin))
for (i in 1:nrow(glm_results)) {
  glm_results$predhr[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], nocl4list[[paste(glm_results$id[i])]]$hr)$estimate)^2  
  glm_results$predrsa[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], nocl4list[[paste(glm_results$id[i])]]$rmssd)$estimate)^2  
  glm_results$predslope[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], nocl4list[[paste(glm_results$id[i])]]$time)$estimate)  
  glm_results$hrslope[i] = (cor.test(nocl4list[[paste(glm_results$id[i])]]$hr, nocl4list[[paste(glm_results$id[i])]]$time)$estimate)  
  glm_results$rsaslope[i] = (cor.test(nocl4list[[paste(glm_results$id[i])]]$rmssd, nocl4list[[paste(glm_results$id[i])]]$time)$estimate)  
} 

mean(glm_results$predhr, na.rm = T) # 0.45
mean(glm_results$predrsa, na.rm = T) # 0.29

length(which(is.na(glm_results$predhr) == F)) #104

summary(lm(hrslope ~ predslope, glm_results)) #R2 = 0.82
summary(lm(rsaslope ~ predslope, glm_results)) #R2 = 0.53

hist(glm_results$predhr)
hist(glm_results$predrsa)


###################################################
#####  Complete Training Set, Minus Exemplar  #####
###################################################

#isolate P242 and remove from list
trainz$p242 <- NULL

train_glm = glm(bClass4 ~ hr + rmssd, p242, family = binomial)

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

mean(glm_results$predhr, na.rm = T) # R2 = 0.48
sd(glm_results$predhr, na.rm = T) # SD = 0.11
mean(glm_results$predrsa, na.rm = T) # R2 = 0.35
sd(glm_results$predrsa, na.rm = T) # SD = 0.15

length(which(is.na(glm_results$predhr) == F)) #165

summary(lm(hrslope ~ predslope, glm_results)) #R2 = 0.86
summary(lm(rsaslope ~ predslope, glm_results)) #R2 = 0.63

hist(glm_results$predhr)
hist(glm_results$predrsa)



























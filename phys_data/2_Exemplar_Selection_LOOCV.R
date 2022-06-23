
#Prepping data
#Important: Data should be standardized
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


#Generate between-subject data frame for storing within-person correlations
listdat = data.frame(id = names(trainz))

#Note: Some of these variables are redundant to those produced for 'groupdata' (reproduced in 'dat')
for (i in 1:nrow(listdat)){
  listdat$cl4hr[i] = (cor.test(trainz[[i]]$bClass4, trainz[[i]]$hr)$estimate)
  listdat$cl4rmssd[i] = (cor.test(trainz[[i]]$bClass4, trainz[[i]]$rmssd)$estimate)
  listdat$cl4pep[i] = (cor.test(trainz[[i]]$bClass4, trainz[[i]]$pep)$estimate)
  listdat$cl6hr[i] = (cor.test(trainz[[i]]$bClass6, trainz[[i]]$hr)$estimate)
  listdat$cl6rmssd[i] = (cor.test(trainz[[i]]$bClass6, trainz[[i]]$rmssd)$estimate)
  listdat$cl6pep[i] = (cor.test(trainz[[i]]$bClass6, trainz[[i]]$pep)$estimate)
}  

write.csv(listdat, 'listdat_N166.csv')


###################
###   Class 4   ###
###################

#Between-subject means for reference
mean(listdat$cl4hr, na.rm = T) # 0.59
mean(listdat$cl4rmssd, na.rm = T) # -0.51
mean(listdat$cl4pep, na.rm = T) # -0.12

listdat$cl4hr_R2 = listdat$cl4hr^2
listdat$cl4rmssd_R2 = listdat$cl4rmssd^2
listdat$cl4pep_R2 = listdat$cl4pep^2

#Between-subject R2 for comparison
mean(listdat$cl4hr_R2, na.rm = T) # 0.37
mean(listdat$cl4rmssd_R2, na.rm = T) # 0.28
mean(listdat$cl4pep_R2, na.rm = T) # 0.15


#Brute Force Examination of Potential Exemplars in Training Data -- Class 4

cl4_results_list<-list()

for (k in seq_along(trainz)){
  train_glm = glm(bClass4 ~ hr + rmssd, trainz[[k]], family = binomial)
  test_glm = lapply(trainz, function(x) predict(train_glm, x, type = "response"))
  test_glmbin = lapply(test_glm, function(x) ifelse(x > 0.50, 1, 0))
  glm_results = data.frame(id = names(test_glmbin))
  
  for (i in 1:nrow(glm_results)) {
    glm_results$predpep[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], trainz[[paste(glm_results$id[i])]]$pep)$estimate)^2  
    glm_results$predrmssd[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], trainz[[paste(glm_results$id[i])]]$rmssd)$estimate)^2  
    glm_results$predhr[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], trainz[[paste(glm_results$id[i])]]$hr)$estimate)^2  
} 
  cl4_results_list[[k]]<-glm_results
}
names(cl4_results_list) <- names(trainz)

cl4_results_list2<-cl4_results_list

for (j in seq_along(cl4_results_list2)){
  cl4_results_list2[[j]]<-cl4_results_list2[[j]][!(cl4_results_list2[[j]]$id==names(cl4_results_list2[j])),]
}

cl4_final_r2 = data.frame(ID= names(cl4_results_list2))
for (i in seq_along(cl4_results_list2)){
  cl4_final_r2$Mean_predpep[i] = mean(cl4_results_list2[[i]]$predpep, na.rm=TRUE)
  cl4_final_r2$Mean_predrmssd[i] = mean(cl4_results_list2[[i]]$predrmssd, na.rm=TRUE)
  cl4_final_r2$Mean_predhr[i] = mean(cl4_results_list2[[i]]$predhr, na.rm=TRUE)
}

cl4_final_r2$sum_r2<-cl4_final_r2$Mean_predrmssd + cl4_final_r2$Mean_predhr

write.table(cl4_final_r2,file="clipboard-1500",sep="\t",col.name=NA)
write.csv(cl4_final_r2, 'Class4_r2.csv')

#Review Results, select best exemplar
#Sort CSV by sum of HR R2 and RSA R2
#Exemplar P242
#HR R2 = 0.48
#RSA R2 = 0.35
#Number of observations = 223

##  Prediction in Holdout Sample  ##

train_glm = glm(bClass4 ~ hr + rmssd, trainz$p242, family = binomial)

#Test training model in holdout data
test_glm = lapply(testz, function(x) predict(train_glm, x, type = "response"))
test_glmbin = lapply(test_glm, function(x) ifelse(x > 0.50, 1, 0))
glm_results = data.frame(id = names(test_glmbin))
for (i in 1:nrow(glm_results)) {
  glm_results$predpep[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], testz[[paste(glm_results$id[i])]]$pep)$estimate)^2  
  glm_results$predrmssd[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], testz[[paste(glm_results$id[i])]]$rmssd)$estimate)^2  
  glm_results$predhr[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], testz[[paste(glm_results$id[i])]]$hr)$estimate)^2  
} 

mean(glm_results$predhr, na.rm = T) # R2 = 0.48
mean(glm_results$predrmssd, na.rm = T) # R2 = 0.32

sd(glm_results$predhr, na.rm = T) # SD = 0.11
sd(glm_results$predrmssd, na.rm = T) # SD = 0.14


###################
###   Class 6   ###
###################

listdat$cl6pep_R2 = listdat$cl6pep^2
listdat$cl6rmssd_R2 = listdat$cl6rmssd^2
listdat$cl6hr_R2 = listdat$cl6hr^2

#Between-subject R2 for comparison
mean(listdat$cl6hr_R2, na.rm = T) # 0.34
mean(listdat$cl6rmssd_R2, na.rm = T) # 0.27
mean(listdat$cl6pep_R2, na.rm = T) # 0.15

#Brute Force Examination of Potential Exemplars in Training Data -- Class 7

cl6_results_list<-list()

for (k in seq_along(trainz)){
  train_glm = glm(bClass6 ~ hr + rmssd, trainz[[k]], family = binomial)
  test_glm = lapply(trainz, function(x) predict(train_glm, x, type = "response"))
  test_glmbin = lapply(test_glm, function(x) ifelse(x > 0.50, 1, 0))
  glm_results = data.frame(id = names(test_glmbin))
  
  for (i in 1:nrow(glm_results)) {
    glm_results$predpep[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], trainz[[paste(glm_results$id[i])]]$pep)$estimate)^2  
    glm_results$predrmssd[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], trainz[[paste(glm_results$id[i])]]$rmssd)$estimate)^2  
    glm_results$predhr[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], trainz[[paste(glm_results$id[i])]]$hr)$estimate)^2  
} 
  cl6_results_list[[k]]<-glm_results
}
names(cl6_results_list) <- names(trainz)

cl6_results_list2<-cl6_results_list

for (j in seq_along(cl6_results_list2)){
  cl6_results_list2[[j]]<-cl6_results_list2[[j]][!(cl6_results_list2[[j]]$id==names(cl6_results_list2[j])),]
}

cl6_final_r2 = data.frame(ID= names(cl6_results_list2))
for (i in seq_along(cl6_results_list2)){
  cl6_final_r2$Mean_predpep[i] = mean(cl6_results_list2[[i]]$predpep, na.rm=TRUE)
  cl6_final_r2$Mean_predrmssd[i] = mean(cl6_results_list2[[i]]$predrmssd, na.rm=TRUE)
  cl6_final_r2$Mean_predhr[i] = mean(cl6_results_list2[[i]]$predhr, na.rm=TRUE)
}

cl6_final_r2$sum_r2 <- cl6_final_r2$Mean_predhr + cl6_final_r2$Mean_predrmssd

write.table(cl6_final_r2,file="clipboard-1500",sep="\t",col.name=NA)
write.csv(cl6_final_r2, 'Class6_r2.csv')

#Review Results, select best exemplar
#Sort CSV by sum of HR R2 and RSA R2
#Exemplar C047
#HR R2 = 0.45
#RSA R2 = 0.38
#Number of observations = 269

##  Prediction in Holdout Sample  ##

train_glm = glm(bClass6 ~ hr + rmssd, trainz$c047, family = binomial)

#Test training model in holdout data
test_glm = lapply(testz, function(x) predict(train_glm, x, type = "response"))
test_glmbin = lapply(test_glm, function(x) ifelse(x > 0.50, 1, 0))
glm_results = data.frame(id = names(test_glmbin))
for (i in 1:nrow(glm_results)) {
  glm_results$predpep[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], testz[[paste(glm_results$id[i])]]$pep)$estimate)^2  
  glm_results$predrmssd[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], testz[[paste(glm_results$id[i])]]$rmssd)$estimate)^2  
  glm_results$predhr[i] = (cor.test(test_glmbin[[paste(glm_results$id[i])]], testz[[paste(glm_results$id[i])]]$hr)$estimate)^2  
} 

mean(glm_results$predhr, na.rm = T) # R2 = 0.44
mean(glm_results$predrmssd, na.rm = T) # R2 = 0.33

sd(glm_results$predhr, na.rm = T) # SD = 0.11
sd(glm_results$predrmssd, na.rm = T) # SD = 0.14











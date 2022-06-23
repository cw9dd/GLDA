#################################################################
##################   LIBRARIES AND FUNCTIONS ####################
#################################################################

lagpad <- function(x, k) {
  c(rep(NA, k), x)[1 : length(x)] 
}

library(psych)
library(DataCombine)
library(tidyLPA)
library(tidyverse)
library(dplyr)
library(car)
library(rsq)
library(tidyr)
library(lubridate)
library(data.table)
library(readxl)
library(mclust)

#############################################################################################
##################################### Analyses ##############################################
#############################################################################################

#############################################################################################
#############################################################################################

#Create List for Analysis

dfList <- list(p001[,c(10, 12, 11, 15, 4, 5)], p003[,c(10, 12, 11, 15, 4, 5)], p004[,c(10, 12, 11, 15, 4, 5)], p006[,c(10, 12, 11, 15, 4, 5)],
               p007[,c(10, 12, 11, 15, 4, 5)], p008[,c(10, 12, 11, 15, 4, 5)], p009[,c(10, 12, 11, 15, 4, 5)], p010[,c(10, 12, 11, 15, 4, 5)],
               p012[,c(10, 12, 11, 15, 4, 5)], p013[,c(10, 12, 11, 15, 4, 5)], p014[,c(10, 12, 11, 15, 4, 5)], p019[,c(10, 12, 11, 15, 4, 5)],
               p021[,c(10, 12, 11, 15, 4, 5)], p023[,c(10, 12, 11, 15, 4, 5)], p025[,c(10, 12, 11, 15, 4, 5)], p033[,c(10, 12, 11, 15, 4, 5)],
               p037[,c(10, 12, 11, 15, 4, 5)], p040[,c(10, 12, 11, 15, 4, 5)], p048[,c(10, 12, 11, 15, 4, 5)], p063[,c(10, 12, 11, 15, 4, 5)],
               p065[,c(10, 12, 11, 15, 4, 5)], p067[,c(10, 12, 11, 15, 4, 5)], p068[,c(10, 12, 11, 15, 4, 5)], p069[,c(10, 12, 11, 15, 4, 5)],
               p071[,c(10, 12, 11, 15, 4, 5)], p072[,c(10, 12, 11, 15, 4, 5)], p074[,c(10, 12, 11, 15, 4, 5)], p075[,c(10, 12, 11, 15, 4, 5)],
               p078[,c(10, 12, 11, 15, 4, 5)], p082[,c(10, 12, 11, 15, 4, 5)], p087[,c(10, 12, 11, 15, 4, 5)], p092[,c(10, 12, 11, 15, 4, 5)],
               p094[,c(10, 12, 11, 15, 4, 5)], p096[,c(10, 12, 11, 15, 4, 5)], p100[,c(10, 12, 11, 15, 4, 5)], p102[,c(10, 12, 11, 15, 4, 5)],
               p107[,c(10, 12, 11, 15, 4, 5)], p109[,c(10, 12, 11, 15, 4, 5)], p111[,c(10, 12, 11, 15, 4, 5)], p113[,c(10, 12, 11, 15, 4, 5)],
               p115[,c(10, 12, 11, 15, 4, 5)], p116[,c(10, 12, 11, 15, 4, 5)], p117[,c(10, 12, 11, 15, 4, 5)], p120[,c(10, 12, 11, 15, 4, 5)],
               p123[,c(10, 12, 11, 15, 4, 5)], p126[,c(10, 12, 11, 15, 4, 5)], p127[,c(10, 12, 11, 15, 4, 5)], p128[,c(10, 12, 11, 15, 4, 5)],
               p129[,c(10, 12, 11, 15, 4, 5)], p134[,c(10, 12, 11, 15, 4, 5)], p137[,c(10, 12, 11, 15, 4, 5)], p139[,c(10, 12, 11, 15, 4, 5)],
               p140[,c(10, 12, 11, 15, 4, 5)], p143[,c(10, 12, 11, 15, 4, 5)], p145[,c(10, 12, 11, 15, 4, 5)], p160[,c(10, 12, 11, 15, 4, 5)],
               p163[,c(10, 12, 11, 15, 4, 5)], p164[,c(10, 12, 11, 15, 4, 5)], p169[,c(10, 12, 11, 15, 4, 5)], p170[,c(10, 12, 11, 15, 4, 5)],
               p174[,c(10, 12, 11, 15, 4, 5)], p177[,c(10, 12, 11, 15, 4, 5)], p178[,c(10, 12, 11, 15, 4, 5)], p181[,c(10, 12, 11, 15, 4, 5)],
               p189[,c(10, 12, 11, 15, 4, 5)], p190[,c(10, 12, 11, 15, 4, 5)], p192[,c(10, 12, 11, 15, 4, 5)], p194[,c(10, 12, 11, 15, 4, 5)],
               p195[,c(10, 12, 11, 15, 4, 5)], p196[,c(10, 12, 11, 15, 4, 5)], p202[,c(10, 12, 11, 15, 4, 5)], p203[,c(10, 12, 11, 15, 4, 5)],
               p204[,c(10, 12, 11, 15, 4, 5)], p206[,c(10, 12, 11, 15, 4, 5)], p215[,c(10, 12, 11, 15, 4, 5)], p217[,c(10, 12, 11, 15, 4, 5)],
               p219[,c(10, 12, 11, 15, 4, 5)], p220[,c(10, 12, 11, 15, 4, 5)], p223[,c(10, 12, 11, 15, 4, 5)], p244[,c(10, 12, 11, 15, 4, 5)],
               c002[,c(6, 16, 27, 34, 3, 36)], c005[,c(6, 16, 27, 34, 3, 36)], c011[,c(6, 16, 27, 34, 3, 36)], c013[,c(6, 16, 27, 34, 3, 36)],
               c014[,c(6, 16, 27, 34, 3, 36)], c021[,c(6, 16, 27, 34, 3, 36)], c029[,c(6, 16, 27, 34, 3, 36)], c031[,c(6, 16, 27, 34, 3, 36)],
               c032[,c(6, 16, 27, 34, 3, 36)], c033[,c(6, 16, 27, 34, 3, 36)], c036[,c(6, 16, 27, 34, 3, 36)], c039[,c(6, 16, 27, 34, 3, 36)],
               c043[,c(6, 16, 27, 34, 3, 36)], c046[,c(6, 16, 27, 34, 3, 36)], c051[,c(6, 16, 27, 34, 3, 36)], c054[,c(6, 16, 27, 34, 3, 36)],
               c061[,c(6, 16, 27, 34, 3, 36)], c067[,c(6, 16, 27, 34, 3, 36)], c068[,c(6, 16, 27, 34, 3, 36)], c069[,c(6, 16, 27, 34, 3, 36)],
               c072[,c(6, 16, 27, 34, 3, 36)]) #101

names(dfList) <- c('p001', 'p003', 'p004', 'p006', 'p007', 'p008', 'p009', 'p010', 'p012', 'p013',
                   'p014', 'p019', 'p021', 'p023', 'p025', 'p033', 'p037', 'p040', 'p048', 'p063',
                   'p065', 'p067', 'p068', 'p069', 'p071', 'p072', 'p074', 'p075', 'p078', 'p082',
                   'p087', 'p092', 'p094', 'p096', 'p100', 'p102', 'p107', 'p109', 'p111', 'p113',
                   'p115', 'p116', 'p117', 'p120', 'p123', 'p126', 'p127', 'p128', 'p129', 'p134',
                   'p137', 'p139', 'p140', 'p143', 'p145', 'p160', 'p163', 'p164', 'p169', 'p170',
                   'p174', 'p177', 'p178', 'p181', 'p189', 'p190', 'p192', 'p194', 'p195', 'p196',
                   'p202', 'p203', 'p204', 'p206', 'p215', 'p217', 'p219', 'p220', 'p223', 'p244',
                   'c002', 'c005', 'c011', 'c013', 'c014', 'c021', 'c029', 'c031', 'c032', 'c033',
                   'c036', 'c039', 'c043', 'c046', 'c051', 'c054', 'c061', 'c067', 'c068', 'c069',
                   'c072')

#Save as .RData to reload and exclude original files
save(dfList, file = "dfList.RData")

#print(dfList)

psych::describe(dfList$p002)

eachLCA = lapply(dfList, function(x) Mclust(scale(na.omit(x), center = TRUE, scale = FALSE), G = 1:15, modelName = c('EII', 'VII', 'EEI', 'VEI', 'EVI', 'VVI'))) 


#create a data frame with summary stats on each mclust model
#where I put 101, you should enter your own N (each row of the resulting data frame is a person)
lcaSummary=data.frame(id=character(101),g=double(101), stringsAsFactors = FALSE) ##making an empty data frame with two columns, specifying the type of variable for each column (double=numeric)
for (i in 1:length(eachLCA)){ #for each data frame 
  lcaSummary$id[i] = as.character(names(eachLCA[i]))
  lcaSummary$g[i] = eachLCA[[i]]$G
  lcaSummary$n[i] = eachLCA[[i]]$n
  lcaSummary$uncertainty[i] = eachLCA[[i]]$uncertainty
  lcaSummary$bic[i] = eachLCA[[i]]$bic
}

#summary stats on yr classes (could add #mode here?)
median(lcaSummary$g) #5
sd(lcaSummary$g) #3.02
range(lcaSummary$g) # (2, 15)

#this creates the ID and class variables as vectors, according to how many within-classes each person had
idXg=character(0)
classes = character(0)
for (i in 1:nrow(lcaSummary)){
  idXg = append(idXg, rep(lcaSummary$id[i], lcaSummary$g[i]))
  classes = append(classes, seq(1, lcaSummary$g[i], 1))
}

#then add those vectors to the class output from the individual LCAs to create the data frame for btwn-subjects LCA
mc=data.frame(id=idXg, class = classes, afraid=double(sum(lcaSummary$g)), 
              angry=double(sum(lcaSummary$g)), anhedonia=double(sum(lcaSummary$g)),
              positive=double(sum(lcaSummary$g)), enthusiastic=double(sum(lcaSummary$g)), 
              content=double(sum(lcaSummary$g)))


#populate this data frame by referencing the list of individual LCA outputs ("eachLCA")
for (i in 1:nrow(mc)){
  mc[i,3:8] = eachLCA[[paste(mc$id[i])]]$parameters$mean[,as.numeric(paste(mc$class[i]))]
}

#run between-subjects LCA
#  change G to whatever is appropriate for your data, make sure you also index same columns
mod = Mclust(mc[,c(3:8)], modelName = c('EII', 'VII', 'EEI', 'VEI', 'EVI', 'VVI'))
summary(mod)

#use BIC, ICL plots, BLRT, other metrics to select best model (other code for that elsewhere)

#Verify model selection via BIC, ICL, and BLRT
BIC <- mclustBIC(mc[,c(3:8)], modelName = c("EII", "VII", "EEI", "VEI", "EVI", "VVI"))
summary(BIC) 
plot(BIC)

ICL <- mclustICL(mc[,c(3:8)], modelName = c("EII", "VII", "EEI", "VEI", "EVI", "VVI"))
summary(ICL) 
plot(ICL)

#Entropy plot
output <- clustCombi(data = mc[,3:8], G = 1:9, modelNames = "VVI") 
entPlot(output$MclustOutput$z, output$combiM, abc = c('standard', 'normalized'), reg = c(2,3))
entPlot(output$MclustOutput$z, output$combiM, abc = c('standard', 'normalized'), reg = c(2))
entPlot(output$MclustOutput$z, output$combiM, abc = c('standard', 'normalized'), reg = c(3))

#Sequential Likelihood Ratio Test for adding successive classes
blrt=mclustBootstrapLRT(mc[,c(3:8)], maxG=10, modelName = "VVI") 
blrt 

#save best model as an object
mod = Mclust(mc[,c(3:8)], G=8, modelName = c('VVI'))
summary(mod)

#save classification data btwn subjects
#x=as.data.frame(cbind(mod$z,mod$classification,mod$uncertainty))
x=as.data.frame(cbind(mod$classification,mod$uncertainty))

#set the number of column names equivalent to the number of nomothetic classes. here i had 7 classes.
colnames(x) <- c("bclass", "uncertain")

#recode each individual file by appending dummy-coded between-subjects class info 
classcodes = data.frame(id = mc$id, wclass = mc$class, bclass = x$bclass )

#Extract means by class for symptoms/behaviors
means <- data.frame(mod$parameters$mean, stringsAsFactors = FALSE) %>%
  rownames_to_column() %>%
  rename(Variable = rowname) %>%
  reshape2::melt(id.vars = "Variable", variable.name = "Class", value.name = "Mean") 

means$Class=dplyr::recode(means$Class, X1 = "1", X2 = "2", X3 = "3", X4 = "4", X5 = "5", X6 = "6", X7 = "7", X8 = "8")

#Plot profiles
pdf('C:/Users/Aaron/Dropbox/Lab/Combined Sample/ML Mixture Model/Centered/8_Class_Centered.pdf')
means%>%
  ggplot(aes(Variable, Mean, group = Class, color = Class)) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  scale_x_discrete(limits = c("afraid", "angry", "anhedonia", "positive", "enthusiastic", "content")) +
  labs(x = NULL, y = "Person-Mean-Centered from 100-pt Scale") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

dev.off()

#Individual participant files will differ in length from dfList due to na.omit in mclust function
#New list (nalist) contains na.omit data for dfList
nalist = lapply(dfList, function(x) na.omit(x))

#first add within-subject classification info to each individual data file
for (i in 1:length(nalist)){
  nalist[[i]]$wClass = eachLCA[[paste(names(nalist)[i])]]$classification
  nalist[[i]]$bClass = numeric(nrow(nalist[[i]])) #set up bClass row for later
}


#now recode each datafile with between-class info
for (i in 1:length(nalist)){
  #identify the bclass-wclass correspondences for each person
  codes = subset(classcodes, id == paste(names(nalist)[i]))
  #for each person, sequence along their unique bclass-wclass correspondences and for each wclass, identify the rows
  #where it exists and make the bclass for those rows the appropriate one based on the recodes.
  for (j in 1:nrow(codes)){
    nalist[[i]]$bClass[nalist[[i]]$wClass == codes$wclass[j]] = codes$bclass[j]
  }
}

#dummy code each person's bclasses
for (i in 1:length(nalist)){
  for (j in 1:nrow(nalist[[i]])){
    nalist[[i]]$bClass1[j] = ifelse(nalist[[i]]$bClass[j] == 1, 1, 0)
    nalist[[i]]$bClass2[j] = ifelse(nalist[[i]]$bClass[j] == 2, 1, 0)
    nalist[[i]]$bClass3[j] = ifelse(nalist[[i]]$bClass[j] == 3, 1, 0)
    nalist[[i]]$bClass4[j] = ifelse(nalist[[i]]$bClass[j] == 4, 1, 0)
    nalist[[i]]$bClass5[j] = ifelse(nalist[[i]]$bClass[j] == 5, 1, 0)
    nalist[[i]]$bClass6[j] = ifelse(nalist[[i]]$bClass[j] == 6, 1, 0)
    nalist[[i]]$bClass7[j] = ifelse(nalist[[i]]$bClass[j] == 7, 1, 0)
    nalist[[i]]$bClass8[j] = ifelse(nalist[[i]]$bClass[j] == 8, 1, 0)
  }
}

#Add linear time
for (i in 1:length(nalist)){
  nalist[[i]]$time = seq(0, nrow(nalist[[i]])-1, 1)
}

#Between-subject
#Class presence/absence and rate
groupdata = data.frame(id = names(nalist))
for (i in 1:nrow(groupdata)){
  groupdata$cl1[i] = ifelse(sum(nalist[[paste(groupdata$id[i])]]$bClass1) > 0, 1, 0)
  groupdata$cl1rate[i] = sum(nalist[[paste(groupdata$id[i])]]$bClass1)/nrow(nalist[[paste(groupdata$id[i])]])
  
  groupdata$cl2[i] = ifelse(sum(nalist[[paste(groupdata$id[i])]]$bClass2) > 0, 1, 0)
  groupdata$cl2rate[i] = sum(nalist[[paste(groupdata$id[i])]]$bClass2)/nrow(nalist[[paste(groupdata$id[i])]])
  
  groupdata$cl3[i] = ifelse(sum(nalist[[paste(groupdata$id[i])]]$bClass3) > 0, 1, 0)
  groupdata$cl3rate[i] = sum(nalist[[paste(groupdata$id[i])]]$bClass3)/nrow(nalist[[paste(groupdata$id[i])]])
  
  groupdata$cl4[i] = ifelse(sum(nalist[[paste(groupdata$id[i])]]$bClass4) > 0, 1, 0)
  groupdata$cl4rate[i] = sum(nalist[[paste(groupdata$id[i])]]$bClass4)/nrow(nalist[[paste(groupdata$id[i])]])
  
  groupdata$cl5[i] = ifelse(sum(nalist[[paste(groupdata$id[i])]]$bClass5) > 0, 1, 0)
  groupdata$cl5rate[i] = sum(nalist[[paste(groupdata$id[i])]]$bClass5)/nrow(nalist[[paste(groupdata$id[i])]])
  
  groupdata$cl6[i] = ifelse(sum(nalist[[paste(groupdata$id[i])]]$bClass6) > 0, 1, 0)
  groupdata$cl6rate[i] = sum(nalist[[paste(groupdata$id[i])]]$bClass6)/nrow(nalist[[paste(groupdata$id[i])]])
  
  groupdata$cl7[i] = ifelse(sum(nalist[[paste(groupdata$id[i])]]$bClass7) > 0, 1, 0)
  groupdata$cl7rate[i] = sum(nalist[[paste(groupdata$id[i])]]$bClass7)/nrow(nalist[[paste(groupdata$id[i])]])
  
  groupdata$cl8[i] = ifelse(sum(nalist[[paste(groupdata$id[i])]]$bClass8) > 0, 1, 0)
  groupdata$cl8rate[i] = sum(nalist[[paste(groupdata$id[i])]]$bClass8)/nrow(nalist[[paste(groupdata$id[i])]])
  
  groupdata$nbclasses[i] =sum(groupdata$cl1[i] + groupdata$cl2[i] + groupdata$cl3[i] + groupdata$cl4[i] +
                                groupdata$cl5[i] + groupdata$cl6[i] + groupdata$cl7[i] + groupdata$cl8[i])
}

#Slopes for Class expression during measurement period
for (i in 1:nrow(groupdata)){
groupdata$cl1slope[i] = ifelse(groupdata$cl1[i] == 1 & groupdata$nbclasses[i] > 1,
                               lm(scale(nalist[[paste(groupdata$id[i])]]$bClass1) ~ scale(nalist[[paste(groupdata$id[i])]]$time))$coefficients[2], NA)

groupdata$cl2slope[i] = ifelse(groupdata$cl2[i] == 1 & groupdata$nbclasses[i] > 1,
                               lm(scale(nalist[[paste(groupdata$id[i])]]$bClass2) ~ scale(nalist[[paste(groupdata$id[i])]]$time))$coefficients[2], NA)

groupdata$cl3slope[i] = ifelse(groupdata$cl3[i] == 1 & groupdata$nbclasses[i] > 1,
                               lm(scale(nalist[[paste(groupdata$id[i])]]$bClass3) ~ scale(nalist[[paste(groupdata$id[i])]]$time))$coefficients[2], NA)

groupdata$cl4slope[i] = ifelse(groupdata$cl4[i] == 1 & groupdata$nbclasses[i] > 1,
                               lm(scale(nalist[[paste(groupdata$id[i])]]$bClass4) ~ scale(nalist[[paste(groupdata$id[i])]]$time))$coefficients[2], NA)

groupdata$cl5slope[i] = ifelse(groupdata$cl5[i] == 1 & groupdata$nbclasses[i] > 1,
                               lm(scale(nalist[[paste(groupdata$id[i])]]$bClass5) ~ scale(nalist[[paste(groupdata$id[i])]]$time))$coefficients[2], NA)

groupdata$cl6slope[i] = ifelse(groupdata$cl6[i] == 1 & groupdata$nbclasses[i] > 1,
                               lm(scale(nalist[[paste(groupdata$id[i])]]$bClass6) ~ scale(nalist[[paste(groupdata$id[i])]]$time))$coefficients[2], NA)

groupdata$cl7slope[i] = ifelse(groupdata$cl7[i] == 1 & groupdata$nbclasses[i] > 1,
                               lm(scale(nalist[[paste(groupdata$id[i])]]$bClass7) ~ scale(nalist[[paste(groupdata$id[i])]]$time))$coefficients[2], NA)

groupdata$cl8slope[i] = ifelse(groupdata$cl8[i] == 1 & groupdata$nbclasses[i] > 1,
                               lm(scale(nalist[[paste(groupdata$id[i])]]$bClass8) ~ scale(nalist[[paste(groupdata$id[i])]]$time))$coefficients[2], NA)
}

View(groupdata)

#Add Correlations
#Orient to data structure
cor(nalist$p001)
cor(nalist$p001)[1:6,17]
as.vector(cor(nalist$p001)[1:6,17])

#Add correlations and means to groupdata
for (i in 1:nrow(groupdata)){
  groupdata[i, 27:32] = as.vector(cor(nalist[[paste(groupdata$id[i])]])[1:6,17])
  groupdata[i, 33] = mean(nalist[[paste(groupdata$id[i])]]$afraid, na.rm = TRUE)
  groupdata[i, 34] = mean(nalist[[paste(groupdata$id[i])]]$angry, na.rm = TRUE)
  groupdata[i, 35] = mean(nalist[[paste(groupdata$id[i])]]$anhedonia, na.rm = TRUE)
  groupdata[i, 36] = mean(nalist[[paste(groupdata$id[i])]]$positive, na.rm = TRUE)
  groupdata[i, 37] = mean(nalist[[paste(groupdata$id[i])]]$enthusiastic, na.rm = TRUE)
  groupdata[i, 38] = mean(nalist[[paste(groupdata$id[i])]]$content, na.rm = TRUE)
  groupdata[i, 39] = mssd(nalist[[paste(groupdata$id[i])]]$afraid)
  groupdata[i, 40] = mssd(nalist[[paste(groupdata$id[i])]]$angry)
  groupdata[i, 41] = mssd(nalist[[paste(groupdata$id[i])]]$anhedonia)
  groupdata[i, 42] = mssd(nalist[[paste(groupdata$id[i])]]$positive)
  groupdata[i, 43] = mssd(nalist[[paste(groupdata$id[i])]]$enthusiastic)
  groupdata[i, 44] = mssd(nalist[[paste(groupdata$id[i])]]$content)
  groupdata[i, 45] = ifelse(sd(nalist[[paste(groupdata$id[i])]]$afraid, na.rm = TRUE)>0, ar(nalist[[paste(groupdata$id[i])]]$afraid, max.order = 1)$ar, NA)
  groupdata[i, 46] = ifelse(sd(nalist[[paste(groupdata$id[i])]]$angry, na.rm = TRUE)>0, ar(nalist[[paste(groupdata$id[i])]]$angry, max.order = 1)$ar, NA)
  groupdata[i, 47] = ifelse(sd(nalist[[paste(groupdata$id[i])]]$anhedonia, na.rm = TRUE)>0, ar(nalist[[paste(groupdata$id[i])]]$anhedonia, max.order = 1)$ar, NA)
  groupdata[i, 48] = ifelse(sd(nalist[[paste(groupdata$id[i])]]$positive, na.rm = TRUE)>0, ar(nalist[[paste(groupdata$id[i])]]$positive, max.order = 1)$ar, NA)
  groupdata[i, 49] = ifelse(sd(nalist[[paste(groupdata$id[i])]]$enthusiastic, na.rm = TRUE)>0, ar(nalist[[paste(groupdata$id[i])]]$enthusiastic, max.order = 1)$ar, NA)
  groupdata[i, 50] = ifelse(sd(nalist[[paste(groupdata$id[i])]]$content, na.rm = TRUE)>0, ar(nalist[[paste(groupdata$id[i])]]$content, max.order = 1)$ar, NA)
  groupdata[i, 51] = sd(nalist[[paste(groupdata$id[i])]]$afraid, na.rm = TRUE)
  groupdata[i, 52] = sd(nalist[[paste(groupdata$id[i])]]$angry, na.rm = TRUE)
  groupdata[i, 53] = sd(nalist[[paste(groupdata$id[i])]]$anhedonia, na.rm = TRUE)
  groupdata[i, 54] = sd(nalist[[paste(groupdata$id[i])]]$positive, na.rm = TRUE)
  groupdata[i, 55] = sd(nalist[[paste(groupdata$id[i])]]$enthusiastic, na.rm = TRUE)
  groupdata[i, 56] = sd(nalist[[paste(groupdata$id[i])]]$content, na.rm = TRUE)
  }

colnames(groupdata)[27:56] = c('afrslope', 'angslope', 'anhslope', 'posslope', 'enthslope', 'contslope',
                               'afravg', 'angavg', 'anhavg', 'posavg', 'enthavg', 'contavg',
                               'afrmssd', 'angmssd', 'anhmssd', 'posmssd', 'enthmssd', 'contmssd',
                               'afrar', 'angar', 'anhar', 'posar', 'enthar', 'contar',
                               'afrsd', 'angsd', 'anhsd', 'possd', 'enthsd', 'contsd')

#Merge with clinical data
dati = read.csv('dat.csv')
datx = merge(dati, groupdata, by = 'id')
describe(datx) #several extraneous vars & mismatches between studies
dat = datx[, c(1:4, 6:19, 21:24, 29:31, 35,36, 45:49, 77:131)]
View(dat)

#Additional Variables
dat$nodx = ifelse(dat$dx1 == 'na', 1, 0)
dat$clinical = ifelse(dat$dx1 == 'na', 0, 1)

dat$dep = ifelse(dat$dx1 == 'mdd' | dat$dx2 == 'mdd'| dat$dx3 == 'mdd'| dat$dx4 == 'mdd'| dat$dx5 == 'mdd' |
                   dat$dx1 == 'pdd' | dat$dx2 == 'pdd'| dat$dx3 == 'pdd'| dat$dx4 == 'pdd'| dat$dx5 == 'pdd', 1, 0)
dat$panic = ifelse(dat$dx1 == 'panic' | dat$dx2 == 'panic'| dat$dx3 == 'panic'| dat$dx4 == 'panic'| dat$dx5 == 'panic', 1, 0)
dat$phob = ifelse(dat$dx1 == 'phob' | dat$dx2 == 'phob'| dat$dx3 == 'phob'| dat$dx4 == 'phob'| dat$dx5 == 'phob', 1, 0)
dat$pdd = ifelse(dat$dx1 == 'pdd' | dat$dx2 == 'pdd'| dat$dx3 == 'pdd'| dat$dx4 == 'pdd'| dat$dx5 == 'pdd', 1, 0)

dat$gadprime = ifelse(dat$dx1 == 'gad', 1, 0)
dat$mddprime = ifelse(dat$dx1 == 'mdd', 1, 0)
dat$sadprime = ifelse(dat$dx1 == 'sad', 1, 0)
dat$ptsdprime = ifelse(dat$dx1 == 'ptsd', 1, 0)
dat$panicprime = ifelse(dat$dx1 == 'panic', 1, 0)
dat$pddprime = ifelse(dat$dx1 == 'pdd', 1, 0)
dat$depprime = ifelse(dat$dx1 == 'pdd' | dat$dx1 == 'mdd', 1, 0)

dat$openptsd = ifelse(dat$ptsd == 1 & dat$study == 'open', 1, 0)
dat$ptsdptsd = ifelse(dat$ptsd == 1 & dat$study == 'ptsd', 1, 0)




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
library(ggplot2)

#############################################################################################
##################################### Analyses ##############################################
#############################################################################################

#############################################################################################
#############################################################################################

#Create List for Analysis

dfList <- list(p002, p003, p004, p006, p007, p008, p009, p010, p012, p014, #10
               p017, p019, p022, p023, p025, p027, p028, p030, p032, p033, #20
               p035, p036, p039, p040, p041, p042, p044, p045, p046, p048, #30
               p049, p051, p052, p058, p060, p061, p062, p063, p064, p065, #40
               p068, p070, p071, p072, p073, p074, p075, p076, p078, p079, #50
               p080, p081, p082, p083, p084, p085, p086, p088, p089, p090, #60
               p092, p094, p095, p096, p098, p100, p101, p103, p104, p108, #70
               p109, p110, p111, p113, p114, p115, p117, p123, p125, p127, #80
               p128, p129, p130, p132, p135, p136, p137, p138, p139, p140, #90
               p141, p142, p143, p146, p147, p150, p151, p152, p154, p155, #100
               p156, p157, p158, p159, p160, p161, p162, p165, p167, p172, #110
               p175, p176, p185, p186, p189, p191, p193, p196, p198, p200, #120
               p201, p202, p203, p204, p205, p206, p208, p209, p212, p217, #130
               p218, p219, p220, p222, p224, p225, p226, p228, p229, p230, #140
               p234, p235, p236, p237, p238, p239, p240, p241, p242, p244, #150
               p067, p069, p102, p118, p119, p120, p124, p126, p134, p153, #160
               p164, p174, p177, p178, p181, p182, p184, p188, p192, p194, #170
               p197, c001, c002, c004, c005, c009, c010, c011, c012, c013, #180
               c014, c015, c018, c019, c020, c021, c023, c024, c025, c026, #190
               c027, c028, c029, c031, c032, c033, c034, c036, c038, c039, #200
               c041, c044, c045, c046, c047, c048, c050, c052, c053, c054, #210
               c055, c057, c058, c061, c063, c065, c066, c067, c068, c070, #220
               c071, c072) #222

names(dfList) <- c('p002', 'p003', 'p004', 'p006', 'p007', 'p008', 'p009', 'p010', 'p012', 'p014',
                   'p017', 'p019', 'p022', 'p023', 'p025', 'p027', 'p028', 'p030', 'p032', 'p033',
                   'p035', 'p036', 'p039', 'p040', 'p041', 'p042', 'p044', 'p045', 'p046', 'p048',
                   'p049', 'p051', 'p052', 'p058', 'p060', 'p061', 'p062', 'p063', 'p064', 'p065',
                   'p068', 'p070', 'p071', 'p072', 'p073', 'p074', 'p075', 'p076', 'p078', 'p079',
                   'p080', 'p081', 'p082', 'p083', 'p084', 'p085', 'p086', 'p088', 'p089', 'p090',
                   'p092', 'p094', 'p095', 'p096', 'p098', 'p100', 'p101', 'p103', 'p104', 'p108',
                   'p109', 'p110', 'p111', 'p113', 'p114', 'p115', 'p117', 'p123', 'p125', 'p127',
                   'p128', 'p129', 'p130', 'p132', 'p135', 'p136', 'p137', 'p138', 'p139', 'p140',
                   'p141', 'p142', 'p143', 'p146', 'p147', 'p150', 'p151', 'p152', 'p154', 'p155',
                   'p156', 'p157', 'p158', 'p159', 'p160', 'p161', 'p162', 'p165', 'p167', 'p172',
                   'p175', 'p176', 'p185', 'p186', 'p189', 'p191', 'p193', 'p196', 'p198', 'p200',
                   'p201', 'p202', 'p203', 'p204', 'p205', 'p206', 'p208', 'p209', 'p212', 'p217',
                   'p218', 'p219', 'p220', 'p222', 'p224', 'p225', 'p226', 'p228', 'p229', 'p230',
                   'p234', 'p235', 'p236', 'p237', 'p238', 'p239', 'p240', 'p241', 'p242', 'p244',
                   'p067', 'p069', 'p102', 'p118', 'p119', 'p120', 'p124', 'p126', 'p134', 'p153',
                   'p164', 'p174', 'p177', 'p178', 'p181', 'p182', 'p184', 'p188', 'p192', 'p194',
                   'p197', 'c001', 'c002', 'c004', 'c005', 'c009', 'c010', 'c011', 'c012', 'c013',
                   'c014', 'c015', 'c018', 'c019', 'c020', 'c021', 'c023', 'c024', 'c025', 'c026',
                   'c027', 'c028', 'c029', 'c031', 'c032', 'c033', 'c034', 'c036', 'c038', 'c039',
                   'c041', 'c044', 'c045', 'c046', 'c047', 'c048', 'c050', 'c052', 'c053', 'c054',
                   'c055', 'c057', 'c058', 'c061', 'c063', 'c065', 'c066', 'c067', 'c068', 'c070',
                   'c071', 'c072')

#Save as .RData to reload and exclude original files
save(dfList, file = "dfList.RData")

#Create random Training and Testing sets
names = data.frame(names(dfList), sample(1:length(dfList), length(dfList), replace = FALSE), seq(1, length(dfList), 1))
colnames(names) <- c('dats', 'random', 'order')
View(names)

#Take the first N=166 for training
names$order[names$random < 167]

trainlist = dfList[c(2, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18, 19, 21, 23, 24, 25, 26, 27, 29, 30, 31, 32, 33, 34, 36, 37,
                     40, 41, 44, 46, 47, 48, 49, 50, 53, 54, 55, 56, 57, 58, 60, 61, 62, 63, 64, 65, 66, 67, 68, 70, 72, 73, 74,
                     75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 87, 88, 89, 90, 92, 93, 94, 95, 96, 99, 100, 101, 104, 105, 106, 107,
                     109, 110, 111, 112, 113, 114, 116, 117, 118, 119, 120, 122, 123, 125, 126, 127, 129, 130, 131, 132, 133, 134,
                     135, 139, 140, 142, 143, 144, 146, 147, 148, 149, 150, 152, 157, 158, 159, 160, 161, 162, 163, 165, 166, 167,
                     168, 170, 171, 172, 173, 174, 175, 176, 177, 179, 181, 182, 184, 187, 188, 189, 190, 191, 192, 193, 194, 195,
                     196, 197, 200, 201, 203, 204, 205, 206, 207, 208, 209, 212, 215, 216, 218, 219, 221, 222)]

#Take the last N=56 for testing
names$order[names$random > 166]

testlist = dfList[c(1, 3, 4, 11, 20, 22, 28, 35, 38, 39, 42, 43, 45, 51, 52, 59, 69, 71, 85, 86, 91, 97, 98, 102, 103, 108, 115,
                    121, 124, 128, 136, 137, 138, 141, 145, 151, 153, 154, 155, 156, 164, 169, 178, 180, 183, 185, 186, 198, 199,
                    202, 210, 211, 213, 214, 217, 220)]


#Discovery phase
#Apply FMM to N=166 training data
eachLCA = lapply(trainlist, function(x) {
  tryCatch({
    Mclust(scale(na.omit(x[,c(2, 11, 17, 5, 6)])), modelName = c('EII', 'VII', 'EEI', 'VEI', 'EVI', 'VVI'))    
  }, error = function(e) { })
})

#create a dataframe with summary stats on each mclust model
#where I put 115, you should enter your own N (each row of the resulting data frame is a person)
lcaSummary=data.frame(id=character(166),g=double(166), stringsAsFactors = FALSE) ##making an empty data frame with two columns, specifying the type of variable for each column (double=numeric)
for (i in 1:length(eachLCA)){ #for each data frame 
  tryCatch({
    lcaSummary$id[i] = as.character(names(eachLCA[i]))
    lcaSummary$g[i] = eachLCA[[i]]$G
    lcaSummary$n[i] = eachLCA[[i]]$n
    lcaSummary$uncertainty[i] = eachLCA[[i]]$uncertainty
    lcaSummary$bic[i] = eachLCA[[i]]$bic    
  }, error = function(e) { })
}

#summary stats on yr classes (could add #mode here?)
median(lcaSummary$g) #3
sd(lcaSummary$g) #1.48
range(lcaSummary$g) # (1, 9)

#this creates the ID and class variables as vectors, according to how many within-classes each person had
idXg=character(0)
classes = character(0)
for (i in 1:nrow(lcaSummary)){
  idXg = append(idXg, rep(lcaSummary$id[i], lcaSummary$g[i]))
  classes = append(classes, seq(1, lcaSummary$g[i], 1))
}

#then add those vectors to the class output from the individual LCAs to create the data frame for btwn-subjects LCA
mc=data.frame(id=idXg, class = classes, hr=double(sum(lcaSummary$g)), 
              rsa=double(sum(lcaSummary$g)), pep=double(sum(lcaSummary$g)),
              rate=double(sum(lcaSummary$g)), amp=double(sum(lcaSummary$g)))


#populate this data frame by referencing the list of individual LCA outputs ("eachLCA")
for (i in 1:nrow(mc)){
  mc[i,3:7] = eachLCA[[paste(mc$id[i])]]$parameters$mean[,as.numeric(paste(mc$class[i]))]
}

#run between-subjects LCA
#  change G to whatever is appropriate for your data, make sure you also index same columns
mod = Mclust(mc[,c(3:7)], modelName = c('EII', 'VII', 'EEI', 'VEI', 'EVI', 'VVI'))
mod = Mclust(mc[,c(3:7)], G = 1:20, modelName = c('EII', 'VII', 'EEI', 'VEI', 'EVI', 'VVI'))
summary(mod)

#use BIC, ICL plots, BLRT, other metrics to select best model (other code for that elsewhere)

#Verify model selection via BIC, ICL, and BLRT
BIC <- mclustBIC(mc[,c(3:7)], G = 1:20, modelName = c("EII", "VII", "EEI", "VEI", "EVI", "VVI"))
summary(BIC) 
plot(BIC)

ICL <- mclustICL(mc[,c(3:7)], G = 1:20, modelName = c("EII", "VII", "EEI", "VEI", "EVI", "VVI"))
summary(ICL) 
plot(ICL)

#Entropy plot
output <- clustCombi(data = mc[,3:7], G = 9, modelNames = "VVI") 
entPlot(output$MclustOutput$z, output$combiM, abc = c('standard', 'normalized'), reg = c(2,3))
entPlot(output$MclustOutput$z, output$combiM, abc = c('standard', 'normalized'), reg = c(2))
entPlot(output$MclustOutput$z, output$combiM, abc = c('standard', 'normalized'), reg = c(3))

blrt=mclustBootstrapLRT(mc[,c(3:7)], maxG=2, modelName = "EVI") 
blrt 

#save best model as an object
mod = Mclust(mc[,c(3:7)], G=6, modelName = c('VVI'))
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

means$Class=dplyr::recode(means$Class, X1 = "1", X2 = "2", X3 = "3", X4 = "4", X5 = "5", X6 = "6")

#Plot profiles
pdf('Train_FMM_6.pdf')
means%>%
  ggplot(aes(Variable, Mean, group = Class, color = Class)) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  scale_x_discrete(limits = c("hr", "rsa", "pep", "rate", "amp")) +
  labs(x = NULL, y = "Z-score") +
  theme_bw(base_size = 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), legend.position = "top")
dev.off()

#Individual participant files will differ in length from dfList due to na.omit in mclust function
#New list (nalist) contains na.omit data for dfList
nalist = lapply(trainlist, function(x) na.omit(x[,c(2, 11, 17, 5, 6)]))

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
  
  groupdata$nbclasses[i] =sum(groupdata$cl1[i]+groupdata$cl2[i]+groupdata$cl3[i]+groupdata$cl4[i]+
                                groupdata$cl5[i]+groupdata$cl6[i])
}

#Slopes for Class expression during interview
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

}

View(groupdata)

for (i in 1:nrow(groupdata)) {
  groupdata$cl4hr[i] = (cor.test(nalist[[paste(groupdata$id[i])]]$bClass4, nalist[[paste(groupdata$id[i])]]$hr)$estimate)^2  
  groupdata$cl4rsa[i] = (cor.test(nalist[[paste(groupdata$id[i])]]$bClass4, nalist[[paste(groupdata$id[i])]]$rmssd)$estimate)^2  
  groupdata$cl6hr[i] = (cor.test(nalist[[paste(groupdata$id[i])]]$bClass6, nalist[[paste(groupdata$id[i])]]$hr)$estimate)^2  
  groupdata$cl6rsa[i] = (cor.test(nalist[[paste(groupdata$id[i])]]$bClass6, nalist[[paste(groupdata$id[i])]]$rmssd)$estimate)^2  
  groupdata$hrslope[i] = (cor.test(nalist[[paste(groupdata$id[i])]]$hr, nalist[[paste(groupdata$id[i])]]$time)$estimate)  
  groupdata$rsaslope[i] = (cor.test(nalist[[paste(groupdata$id[i])]]$rmssd, nalist[[paste(groupdata$id[i])]]$time)$estimate)  
} 

mean(groupdata$cl4hr, na.rm = T) # R2 = 0.37
sd(groupdata$cl4hr, na.rm = T) # SD = 0.14
mean(groupdata$cl4rsa, na.rm = T) # R2 = 0.28
sd(groupdata$cl4rsa, na.rm = T) # SD = 0.14
summary(lm(hrslope ~ cl4slope, groupdata)) # R2 = 0.83
summary(lm(rsaslope ~ cl4slope, groupdata)) # R2 = 0.67

mean(groupdata$cl6hr, na.rm = T) # R2 = 0.34
sd(groupdata$cl6hr, na.rm = T) # SD = 0.16
mean(groupdata$cl6rsa, na.rm = T) # R2 = 0.27
sd(groupdata$cl6rsa, na.rm = T) # SD = 0.19
summary(lm(hrslope ~ cl6slope, groupdata)) # R2 = 0.73
summary(lm(rsaslope ~ cl6slope, groupdata)) # R2 = 0.60

groupdata$cl4hr[groupdata$id == 'p242'] # 0.63
groupdata$cl4rsa[groupdata$id == 'p242'] # 0.61
groupdata$cl4hr[groupdata$id == 'p027'] # 0.39
groupdata$cl4rsa[groupdata$id == 'p027'] # 0.46

groupdata$cl6hr[groupdata$id == 'p242'] # 0.36
groupdata$cl6rsa[groupdata$id == 'p242'] # 0.34
groupdata$cl6hr[groupdata$id == 'p027'] # 0.39
groupdata$cl6rsa[groupdata$id == 'p027'] # 0.46

length(which(groupdata$cl4 == 1)) # N = 62 (37%)
length(which(groupdata$cl6 == 1)) # N = 71 (43%)


#Merge with clinical data
dati = read.csv('dat.csv')
datx = merge(dati, groupdata, by = 'id')
describe(datx) #several extraneous vars & mismatches between studies
dat = datx[, c(1:4, 6:19, 21:24, 29:31, 35,36, 45:49, 77:123)]
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




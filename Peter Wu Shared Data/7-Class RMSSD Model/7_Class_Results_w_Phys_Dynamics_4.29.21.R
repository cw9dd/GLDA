
length(which(dat$cl1 == 1)) #180 (81%)
length(which(dat$cl2 == 1)) #182 (82%)
length(which(dat$cl3 == 1)) #182 (82%)
length(which(dat$cl4 == 1)) #189 (85%)
length(which(dat$cl5 == 1)) #81 (36%)
length(which(dat$cl6 == 1)) #65 (29%)
length(which(dat$cl7 == 1)) #96 (43%)

depdat = subset(dat, dep == 1)
mdddat = subset(dat, mdd == 1)
clindat = subset(dat, clinical == 1)
nodxdat = subset(dat, clinical == 0)

############################################
#####   Class presence (vs. absence)   #####
############################################

#Reference Group = No Diagnosis (healthy controls + nodx from PTSD study)
summary(glm(cl1 ~ clinical + study + sex, dat, family = binomial)) # clinical [t = 4.84, 6.17 odds]
summary(glm(cl2 ~ clinical + study + sex, dat, family = binomial)) # clinical [t = 4.63, 6.00 odds]
summary(glm(cl3 ~ clinical + study + sex, dat, family = binomial)) # clinical [t = 4.01, 4.53 odds]
summary(glm(cl4 ~ clinical + study + sex, dat, family = binomial)) # clinical [t = 2.86, 3.10 odds]
summary(glm(cl5 ~ clinical + study + sex, dat, family = binomial)) #
summary(glm(cl6 ~ clinical + study + sex, dat, family = binomial)) #
summary(glm(cl7 ~ clinical + study + sex, dat, family = binomial)) # clinical [t = 3.03, 2.64 odds]

#Reference Group = No Diagnosis (healthy controls + nodx from PTSD study)
summary(glm(cl1 ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat, family = binomial)) # DEP [t = 3.02, 9.78 odds]
summary(glm(cl2 ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat, family = binomial)) # 
summary(glm(cl3 ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat, family = binomial)) # SAD [t = 2.08,  2.83 odds]
summary(glm(cl4 ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat, family = binomial)) # SAD [t = 3.05, 10.59 odds]
summary(glm(cl5 ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat, family = binomial)) # SAD [t = 2.41,  2.18 odds]
summary(glm(cl6 ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat, family = binomial)) #
summary(glm(cl7 ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat, family = binomial)) #

sad_cl5 = dat$id[dat$cl5 == 1 & dat$sad == 1]

#Continuous Predictors (Neuroticism & Hamilton Depression)
summary(glm(cl1 ~ scale(neon) + study + sex, dat, family = binomial)) # t = 3.86, odds = 1.95 for each SD
summary(glm(cl2 ~ scale(neon) + study + sex, dat, family = binomial)) # t = 4.56, odds = 2.32 for each SD
summary(glm(cl3 ~ scale(neon) + study + sex, dat, family = binomial)) # t = 4.56, odds = 2.29 for each SD
summary(glm(cl4 ~ scale(neon) + study + sex, dat, family = binomial)) # t = 2.19, odds = 1.50 for each SD
summary(glm(cl5 ~ scale(neon) + study + sex, dat, family = binomial)) # t = 1.96, odds = 1.34 for each SD
summary(glm(cl6 ~ scale(neon) + study + sex, dat, family = binomial)) #
summary(glm(cl7 ~ scale(neon) + study + sex, dat, family = binomial)) # t = 2.37, odds = 1.40 for each SD

summary(glm(cl1 ~ scale(hrsd) + study + sex, dat, family = binomial)) # t = 4.36, odds = 2.86 for each SD
summary(glm(cl2 ~ scale(hrsd) + study + sex, dat, family = binomial)) # t = 3.67, odds = 2.29 for each SD
summary(glm(cl3 ~ scale(hrsd) + study + sex, dat, family = binomial)) # t = 3.63, odds = 2.29 for each SD
summary(glm(cl4 ~ scale(hrsd) + study + sex, dat, family = binomial)) # t = 2.47, odds = 1.73 for each SD
summary(glm(cl5 ~ scale(hrsd) + study + sex, dat, family = binomial)) #
summary(glm(cl6 ~ scale(hrsd) + study + sex, dat, family = binomial)) #
summary(glm(cl7 ~ scale(hrsd) + study + sex, dat, family = binomial)) # t = 2.23, odds = 1.40

summary(glm(cl1 ~ scale(hama) + study + sex, dat, family = binomial)) # t = 4.32, odds = 3.10 for each SD
summary(glm(cl2 ~ scale(hama) + study + sex, dat, family = binomial)) # t = 4.00, odds = 2.80 for each SD
summary(glm(cl3 ~ scale(hama) + study + sex, dat, family = binomial)) # t = 3.95, odds = 2.80 for each SD
summary(glm(cl4 ~ scale(hama) + study + sex, dat, family = binomial)) # t = 2.50, odds = 1.80 for each SD
summary(glm(cl5 ~ scale(hama) + study + sex, dat, family = binomial)) #
summary(glm(cl6 ~ scale(hama) + study + sex, dat, family = binomial)) # t = -2.05, odds = 0.70
summary(glm(cl7 ~ scale(hama) + study + sex, dat, family = binomial)) # t = 2.48, odds = 1.46


#############################################################
#####   proportions (rates of occurrence) for classes   #####
#############################################################

#No-Dx exhibited higher rates of Class 1 (+7%), Class 4 (+6%), and Class 5 (+17%)
summary(lm(cl1rate ~ clinical + study + sex, dat, cl1rate > 0.0)) # clinical [t = -2.14], R2 = 0.05
summary(lm(cl2rate ~ clinical + study + sex, dat, cl2rate > 0.0)) # clinical [t = -2.91], R2 = 0.06
summary(lm(cl3rate ~ clinical + study + sex, dat, cl3rate > 0.0)) # clinical [t = -2.89], R2 = 0.05
summary(lm(cl4rate ~ clinical + study + sex, dat, cl4rate > 0.0)) # clinical [t = -6.03], R2 = 0.19
summary(lm(cl5rate ~ clinical + study + sex, dat, cl5rate > 0.0)) #
summary(lm(cl6rate ~ clinical + study + sex, dat, cl6rate > 0.0)) # clinical [t = -2.46], R2 = 0.15
summary(lm(cl7rate ~ clinical + study + sex, dat, cl7rate > 0.0)) # clinical [t = -4.73], R2 = 0.23

#Class 4 (SAD) and Class 5 (Depression, SAD, Panic) differences are diagnosis-specific
summary(lm(cl1rate ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat, cl1rate > 0.0)) #
summary(lm(cl2rate ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat, cl2rate > 0.0)) #
summary(lm(cl3rate ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat, cl3rate > 0.0)) # SAD [t = -2.07], R2 = 0.07
summary(lm(cl4rate ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat, cl4rate > 0.0)) # GAD [t = -2.41], SAD [t = -2.02], R2 = 0.13
summary(lm(cl5rate ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat, cl5rate > 0.0)) #
summary(lm(cl6rate ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat, cl5rate > 0.0)) #
summary(lm(cl7rate ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat, cl5rate > 0.0)) # GAD [t = -2.02], R2 = 0.12

#Continuous Predictors (Neuroticism & Hamilton Depression)
summary(lm(cl1rate ~ neon + study + sex, dat, cl1rate > 0.0)) # N = 166, neon [t = -3.10], study [t = -3.10]; R2 = 0.11
summary(lm(cl2rate ~ neon + study + sex, dat, cl2rate > 0.0)) #
summary(lm(cl3rate ~ neon + study + sex, dat, cl3rate > 0.0)) #
summary(lm(cl4rate ~ neon + study + sex, dat, cl4rate > 0.0)) # N = 188, neon [t = -3.15], R2 = 0.06
summary(lm(cl5rate ~ neon + study + sex, dat, cl5rate > 0.0)) # N = 190, neon [t = -4.95], R2 = 0.14

summary(lm(cl1rate ~ hrsd + study + sex, dat, cl1rate > 0.0)) # N = 156, t = -2.88, R2 = 0.10
summary(lm(cl2rate ~ hrsd + study + sex, dat, cl2rate > 0.0)) #
summary(lm(cl3rate ~ hrsd + study + sex, dat, cl3rate > 0.0)) # N = 115, t = -2.17, R2 = 0.06
summary(lm(cl4rate ~ hrsd + study + sex, dat, cl4rate > 0.0)) # N = 177, t = -2.99, R2 = 0.05
summary(lm(cl5rate ~ hrsd + study + sex, dat, cl5rate > 0.0)) # N = 182, hrsd [t = -5.02], R2 = 0.15
summary(lm(cl6rate ~ hrsd + study + sex, dat, cl5rate > 0.0)) # N = 182, hrsd [t = -5.02], R2 = 0.15
summary(lm(cl7rate ~ hrsd + study + sex, dat, cl5rate > 0.0)) # N = 182, hrsd [t = -5.02], R2 = 0.15

summary(lm(cl1rate ~ hama + study + sex, dat, cl1rate > 0.0)) # N = 156, t = -2.79, R2 = 0.10
summary(lm(cl2rate ~ hama + study + sex, dat, cl2rate > 0.0)) #
summary(lm(cl3rate ~ hama + study + sex, dat, cl3rate > 0.0)) # N = 115, t = -2.16, R2 = 0.06
summary(lm(cl4rate ~ hama + study + sex, dat, cl4rate > 0.0)) # N = 177, t = -2.40, R2 = 0.03
summary(lm(cl5rate ~ hama + study + sex, dat, cl5rate > 0.0)) # N = 182, hrsd [t = -3.84], R2 = 0.10
summary(lm(cl6rate ~ hama + study + sex, dat, cl5rate > 0.0)) # N = 182, hrsd [t = -3.84], R2 = 0.10
summary(lm(cl7rate ~ hama + study + sex, dat, cl5rate > 0.0)) # N = 182, hrsd [t = -3.84], R2 = 0.10

#No signal from average phys levels
#summary(lm(cl1rate ~ hr + rsa + pep + study + sex, dat, cl1rate > 0.0)) # study, R2 = 0.07
#summary(lm(cl2rate ~ hr + rsa + pep + study + sex, dat, cl2rate > 0.0)) # 
#summary(lm(cl3rate ~ hr + rsa + pep + study + sex, dat, cl3rate > 0.0)) # 
#summary(lm(cl4rate ~ hr + rsa + pep + study + sex, dat, cl4rate > 0.0)) # 
#summary(lm(cl5rate ~ hr + rsa + pep + study + sex, dat, cl5rate > 0.0)) #


################################################
#####   Change During Interview (Slopes)   #####
################################################

#Change in HR During Interview
#Evidence for habituation (significant intercept + additional degree of decline in clinical sample)
summary(lm(hrslope ~ clinical + study + sex, dat))# clinical [t = -3.44], study [t = 3.57], R2 = 0.09
summary(lm(hrslope ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat))# DEP [t = -2.36], R2 = 0.09
summary(lm(hrslope ~ cl1 + cl2 + cl3 + cl4 + cl5 + cl6 + cl7 + clinical + study + sex, dat))# clinical [t = -2.24], study [t = 4.12], class3  [t = 2.53] R2 = 0.15
summary(lm(hrslope ~ cl1rate + cl2rate + cl3rate + cl5rate + cl6rate + cl7rate + clinical + study + sex, dat)) #R2 = 0.15
summary(lm(hrslope ~ cl6rate + clinical + study + sex, dat)) #R2 = 0.11
# Class 1 [t = -2.37], Class 3  [t = 2.47] 
# Clinical [t = -3.36], Study (PTSD) [t = 3.77]

summary(lm(hrslope ~ clinical + study + sex, dat))# clinical [t = -3.45], R2 = 0.10
summary(lm(hrslope ~ cl3slope + clinical + study + sex, dat)) # t = -2.06, R2 = 0.11
summary(lm(hrslope ~ cl5slope + clinical + study + sex, dat)) # t = 15.19, R2 = 0.77
summary(lm(hrslope ~ cl7slope + clinical + study + sex, dat)) # t = -5.55, R2 = 0.37
summary(lm(hrslope ~ cl3slope + cl5slope + cl7slope + clinical + study + sex, dat))# R2 = 0.88
summary(lm(hrslope ~ cl5slope + cl7slope + clinical + study + sex, dat))# R2 = 0.90
# Class 5 slope fully mediates relationship between HR change and Clinical Status in the sub-sample that has class 5

dat$cl5slope0 = dat$cl5slope
dat$cl5slope0[is.na(dat$cl5slope0)] <- 0
dat$cl7slope0 = dat$cl7slope
dat$cl7slope0[is.na(dat$cl7slope0)] <- 0
summary(lm(hrslope ~ cl5slope0 + clinical + study + sex, dat))# R2 = 0.39
summary(lm(hrslope ~ cl5slope0 + cl7slope0 + clinical + study + sex, dat))# R2 = 0.41
summary(lm(hrslope ~ cl5slope0 + cl7slope0 + clinical + study + sex, dat, cl5 == 1 | cl7 == 1))# R2 = 0.55
summary(lm(hrslope ~ cl5slope0 + cl7slope0 + rsaslope + clinical + study + sex, dat, cl5 == 1 | cl7 == 1))# R2 = 0.55

summary(lm(hrslope ~ rsaslope + pepslope + clinical + study + sex, dat))# R2 = 0.47
summary(lm(hrslope ~ rsaslope + pepslope + clinical + study + sex, dat, cl5 == 1))# R2 = 0.68
summary(lm(hrslope ~ rsaslope + pepslope + clinical + study + sex, dat, cl5 == 0))# R2 = 0.36
summary(lm(hrslope ~ rsaslope + pepslope + hr, dat, cl5 == 1))# R2 = 0.70
summary(lm(hrslope ~ rsaslope + pepslope + hr, dat, cl5 == 0))# R2 = 0.28
summary(lm(hrslope ~ rsaslope + pepslope, dat, cl5 == 1))# R2 = 0.66
summary(lm(hrslope ~ rsaslope + pepslope, dat, cl5 == 0))# R2 = 0.28

summary(glm(cl5 ~ scale(hr) + scale(rsa) + scale(pep) + clinical, dat, family = binomial))
summary(glm(cl5 ~ scale(hr) + scale(rsa) + scale(pep) + gad + sad + dep + ptsd + panic, dat, family = binomial))
summary(glm(cl5 ~ scale(hr) + scale(rsa) + scale(pep) + sad, dat, family = binomial))

summary(glm(cl5 ~ scale(sdhr) + scale(rsa) + scale(pep) + clinical, dat, family = binomial))
summary(glm(cl5 ~ scale(sdhr) + scale(rsa) + scale(pep) + gad + sad + dep + ptsd + panic, dat, family = binomial))
summary(glm(cl5 ~ scale(sdhr) + scale(rsa) + scale(pep) + sad, dat, family = binomial))

summary(glm(cl5 ~ scale(sqrt(hrmssd)) + scale(rsa) + scale(pep) + clinical, dat, family = binomial))
summary(glm(cl5 ~ scale(sqrt(hrmssd)) + scale(rsa) + scale(pep) + gad + sad + dep + ptsd + panic, dat, family = binomial))
summary(glm(cl5 ~ scale(sqrt(hrmssd)) + hr + scale(rsa) + scale(pep) + sad, dat, family = binomial))

summary(glm(cl5 ~ scale(sqrt(hrrange)) + scale(sqrt(hrmssd)) + scale(hrslope) + sad, dat, family = binomial))
summary(glm(cl5 ~ scale(hrslope) + hr + sad, dat, family = binomial)) # NO effect in isolation

summary(glm(cl5 ~ hr + sad, dat, family = binomial)) # HR significant, SAD significant
summary(glm(cl5 ~ hr + sqrt(hrmssd) + sad, dat, family = binomial)) # MSSD 'mediates' HR, SAD still significant
summary(glm(cl5 ~ hr + sdhr + sad, dat, family = binomial)) # SD more strongly mediates HR, now mediates SAD
summary(glm(cl5 ~ hr + hrrange + sad, dat, family = binomial)) # HR range also mediates HR, mediates SAD
summary(glm(cl5 ~ hr + hrrange + sdhr, dat, family = binomial)) # range significant, average and SD not
summary(glm(cl5 ~ hrrange + sqrt(hrmssd) + hrslope, dat, family = binomial)) # HR slope ONLY significant w/ range AND MSSD
summary(glm(cl5 ~ hrrange + sqrt(hrmssd) + hrslope, dat, family = binomial)) # HR slope ONLY significant w/ range AND MSSD

summary(glm(cl5 ~ hrrange + sqrt(hrmssd) + hrslope + rsa + pep, dat, family = binomial)) # HR slope ONLY significant w/ range AND MSSD

summary(lm(hrslope ~ cl5slope + rsaslope + pepslope + clinical + study + sex, dat))# R2 = 0.82

summary(lm(hr ~ cl7 + clinical + study + sex, dat))# R2 = 0.39
summary(lm(hr ~ cl7rate + clinical + study + sex, dat))# R2 = 0.41

#Discretizing autonomic cardiac control. Locating it in time.
#Class 5 was minority of segments, yet accounted for majority of HR variance. These are the moments that matter.

summary(glm(cl7 ~ hr + clinical, dat, family = binomial)) # HR significant, Clinical significant
summary(glm(cl7 ~ hr + sqrt(hrmssd) + clinical, dat, family = binomial)) # MSSD = non-significant
summary(glm(cl7 ~ hr + sdhr + clinical, dat, family = binomial)) # SD = non-significant
summary(glm(cl7 ~ hr + hrrange + clinical, dat, family = binomial)) # HR range = non-significant
summary(glm(cl7 ~ hr + hrrange + sdhr, dat, family = binomial)) #
summary(glm(cl7 ~ hr + clinical, dat, family = binomial)) # HR significant, Clinical significant


#Change in RSA
#Both Clinical and non-Clinical had statistically flat slopes
summary(lm(rsaslope ~ clinical + study + sex, dat))# clinical [t = 2.94], R2 = 0.04
summary(lm(rsaslope ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat))# DEP [t = 2.19], R2 = 0.04
summary(lm(rsaslope ~ cl1rate + cl2rate + cl3rate + cl5rate + cl6rate + cl7rate + clinical + study + sex, dat))# class5 [t = 2.06], clinical [t = 2.06], R2 = 0.07

summary(lm(rsaslope ~ cl5slope + clinical + study + sex, dat))# class5 [t = -10.52], R2 = 0.60
summary(lm(rsaslope ~ cl7slope + clinical + study + sex, dat))# class5 [t = 5.65], R2 = 0.30
summary(lm(rsaslope ~ cl5slope0 + clinical + study + sex, dat))# class5 [t = -9.52], R2 = 0.33
summary(lm(rsaslope ~ cl7slope0 + clinical + study + sex, dat))# class5 [t = 5.48], R2 = 0.16
summary(lm(rsaslope ~ cl5slope0 + cl7slope0 + clinical + study + sex, dat))# class5 [t = 5.48], R2 = 0.35

#Change in PEP
#No significant changes (or contrasts by group/class presence)
summary(lm(pepslope ~ clinical + study + sex, dat))#
summary(lm(pepslope ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat))#
summary(lm(pepslope ~ cl1 + cl2 + cl3 + cl4 + cl5 + cl6 + cl7 + clinical + study + sex, dat))#
summary(lm(pepslope ~ cl1rate + cl2rate + cl3rate + cl5rate + cl6rate + cl7rate + clinical + study + sex, dat))#
summary(lm(pepslope ~ cl2slope + clinical + study + sex, dat))# t = -3.17, R2 = 0.07
summary(lm(pepslope ~ cl3slope + clinical + study + sex, dat))# t = 2.87, R2 = 0.05
summary(lm(pepslope ~ cl7slope + clinical + study + sex, dat))# t = 4.49, R2 = 0.22

dat$cl2slope0 = dat$cl2slope
dat$cl2slope0[is.na(dat$cl2slope0)] <- 0
dat$cl3slope0 = dat$cl3slope
dat$cl3slope0[is.na(dat$cl3slope0)] <- 0

summary(lm(pepslope ~ cl2slope0 + cl3slope0 + cl7slope0 + clinical + study + sex, dat))# R2 = 0.13
summary(lm(pepslope ~ cl3slope0 + cl7slope0 + clinical + study + sex, dat))# R2 = 0.13

#Relationships between Classes 2 and 4 and PEP change
#Class 2 slope negatively correlates with PEP slope, Class 4 slope positively correlates with PEP slope
#Increase in Class 2 is associated with shortening of PEP (+ inotropy)
#Increase in Class 4 is associated with lengthening of PEP (- inotropy)
summary(lm(pepslope ~ cl2slope + clinical + study + sex, dat))# R2 = 0.07
summary(lm(pepslope ~ cl3slope + clinical + study + sex, dat))# R2 = 0.05
# Class 2 and Class 4 relationships with PEP are amplified in depressed individuals
summary(lm(pepslope ~ gad + sad + ptsd + panic + phob + study + sex + scale(cl2slope)*scale(dep), dat))# R2 = 0.16
summary(lm(pepslope ~ gad + sad + ptsd + panic + phob + study + sex + scale(cl3slope)*scale(dep), dat))# Borderline Effect, R2 = 0.10
summary(lm(pepslope ~ clinical + study + sex + scale(cl2slope)*scale(dep), dat))# t = -3.76, R2 = 0.14
summary(lm(pepslope ~ clinical + study + sex + scale(cl3slope)*scale(dep), dat))# t = 2.18, R2 = 0.07
#Increase in Class 2 is associated with *greater* shortening of PEP (+ inotropy)
#Increase in Class 4 is associated with *greater* lengthening of PEP (- inotropy)

#For clinical vs. non-Clinical simple slopes
clindat = subset(dat, clinical == 1)
nodxdat = subset(dat, clinical == 0)

#Change in Class Frequency During Interview
#Class 1 change
#No change in Class 1 frequency for non-Clinical sample (t = -0.22)
#Significant decrease in Class 1 frequency for Clinical sample (t = -3.11)
summary(lm(cl1slope ~ clinical + study + sex, dat))# clinical [t = -3.51], R2 = 0.08
summary(lm(cl1slope ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat))# DEP [t = -2.25], R2 = 0.10
summary(lm(cl1slope ~ hrslope + rsaslope + pepslope + clinical + study + sex, dat))# R2 = 0.10

#Class 2 change
#No significant change in Class 2 frequency during interview
summary(lm(cl2slope ~ clinical + study + sex, dat))# 
summary(lm(cl2slope ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat))# 
#However, slopes ranged from, -0.76 to 0.63
#Thus, some individuals exhibited strong positive and strong negative slopes during the interview
hist(dat$cl2slope)
range(dat$cl2slope, na.rm = TRUE) # [-0.76, 0.63]
#Change in PEP negatively correlated with change in Class 2 frequency
summary(lm(cl2slope ~ hrslope + rsaslope + pepslope + clinical + study + sex, dat))# PEP Slope [t = -3.00] R2 = 0.07

#Negative correlation is amplified in individuals with MDD (p = 0.05)
summary(lm(cl2slope ~ hrslope + rsaslope + scale(pepslope)*scale(mdd) + clinical + study + sex, dat))# PEP Slope (-), MDD (-), Interaction (-), R2 = 0.12

#Class 3 change
#No significant change in Class 3 frequency during interview
summary(lm(cl3slope ~ clinical + study + sex, dat))# 
summary(lm(cl3slope ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat))# 
summary(lm(cl3slope ~ hrslope + rsaslope + pepslope + clinical + study + sex, dat))# 
#Slopes were fairly clustered around zero, nevertheless, there was a range from -0.46 to 0.70
hist(dat$cl3slope)
range(dat$cl3slope, na.rm = TRUE) # [-0.46, 0.70]

#Class 4 change
#Overall average was non-significant
#Borderline positive slope for non-Clinical sample (p = 0.05)
#Significant positive slope for depressed participants (t = 2.92)
#Positive correlation with PEP slope
summary(lm(cl4slope ~ clinical + study + sex, dat))#
summary(lm(cl4slope ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat))# DEP [t = 2.27], R2 = 0.05
summary(lm(cl4slope ~ hrslope + rsaslope + pepslope + clinical + study + sex, dat))# PEP Slope [t = 4.17] R2 = 0.11

#Positive correlation is amplified in individuals with MDD (p = 0.04)
summary(lm(cl4slope ~ hrslope + rsaslope + scale(pepslope)*scale(mdd) + clinical + study + sex, dat))# PEP Slope (+), MDD (+), Interaction (+), R2 = 0.16

#Class 5 change
summary(lm(cl5slope ~ clinical + study + sex, dat))#
summary(lm(cl5slope ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat))#
summary(lm(cl5slope ~ hrslope + rsaslope + pepslope + clinical + study + sex, dat))#

#############################################
#####   Class 2 and PEP in Depression   #####
#############################################

#Increase in Class 2 is associated with shortening of PEP
#Increase in Class 3 is associated with lengthening of PEP
cor.test(dat$pepslope, dat$cl2slope) # r = -0.22
cor.test(dat$pepslope, dat$cl3slope) # r = 0.21

cor.test(dat$pepslope, dat$svslope) # r = -0.18
cor.test(dat$pepslope, dat$coslope) # r = -0.29

#In depressed patients, correlations between Classes 2 & 4 and PEP are greater
depdat = subset(dat, dep == 1)
cor.test(depdat$pepslope, depdat$cl2slope) # r = -0.50
cor.test(depdat$pepslope, depdat$cl3slope) # r = 0.40

cor.test(depdat$pepslope, depdat$svslope) # r = -0.36
cor.test(depdat$pepslope, depdat$coslope) # r = -0.49

#Meanwhile, the correlations for the remaining sample are much lower
depdat = subset(dat, dep == 1)
cor.test(dat$pepslope[dat$dep == 0], dat$cl2slope[dat$dep == 0]) # r = -0.04
cor.test(dat$pepslope[dat$dep == 0], dat$cl3slope[dat$dep == 0]) # r = 0.09

cor.test(dat$pepslope[dat$dep == 0], dat$svslope[dat$dep == 0]) # r = -0.09
cor.test(dat$pepslope[dat$dep == 0], dat$coslope[dat$dep == 0]) # r = -0.17

#Effect was more pronounced in MDD
#MDD
mdddat = subset(dat, mdd == 1)
cor.test(mdddat$pepslope, mdddat$cl2slope) # r = -0.52
cor.test(mdddat$pepslope, mdddat$cl3slope) # r = 0.43

cor.test(mdddat$pepslope, mdddat$svslope) # r = -0.45
cor.test(mdddat$pepslope, mdddat$coslope) # r = -0.54

summary(lm(pepslope ~ scale(coslope)*dep, dat)) #R2 = 0.11
summary(lm(pepslope ~ scale(coslope)*mdd, dat)) #R2 = 0.11

summary(lm(coslope ~ scale(pepslope)*dep + hrslope + scale(co)*dep + hr + sv, dat)) #R2 = 0.11

summary(lm(hrpep ~ scale(pepslope) + coslope*dep + hrslope + dep, dat)) #R2 = 0.11

summary(lm(coslope ~ scale(pepslope)*dep + dep*scale(rsa) + hrslope + dep, dat)) #R2 = 0.11
summary(lm(pepslope ~ rsa + rsaslope + scale(dep) * scale(coslope) + clinical + hr + hrslope + sex, dat)) # R2 = 0.18
summary(lm(pepslope ~ rsa + rsaslope + scale(dep) * scale(coslope) + clinical + hr + hrslope + sex, dat)) # R2 = 0.18


#Importantly these effects are largely inotropic, rather than chronotropic
cor.test(dat$hrslope, dat$cl2slope) # r = 0.06
cor.test(dat$hrslope, dat$cl3slope) # r = -0.13

cor.test(depdat$hrslope, depdat$cl2slope) # r = 0.23
cor.test(depdat$hrslope, depdat$cl3slope) # r = -0.19
cor.test(mdddat$hrslope, mdddat$cl2slope) # r = 0.14
cor.test(mdddat$hrslope, mdddat$cl3slope) # r = -0.16

length(which(dat$cl2 == 1)) #182
length(which(dat$cl3 == 1)) #182
length(which(dat$cl2 == 1 & dat$dep == 1)) #65 (90% of DEP; 36% of class2)
length(which(dat$cl3 == 1 & dat$dep == 1)) #65 (90% of DEP; 36% of class3)
length(which(dat$cl2 == 1 & dat$mdd == 1)) #48 (94% of MDD)
length(which(dat$cl3 == 1 & dat$mdd == 1)) #47 (92% of MDD)
length(which(dat$dep == 1)) #72
length(which(dat$mdd == 1)) #51

#Class 2 and class 3 are positively correlated (occurred in the same people)
#However, slopes for Class 2 and class 3 are strongly negatively correlated
cor.test(dat$cl2, dat$cl3) # r = 0.42
cor.test(depdat$cl2, depdat$cl3) # r = 0.37
cor.test(mdddat$cl2, mdddat$cl3) # r = 0.24
cor.test(dat$cl2slope, dat$cl3slope) # r = -0.75
cor.test(depdat$cl2slope, depdat$cl3slope) # r = -0.77
cor.test(mdddat$cl2slope, mdddat$cl3slope) # r = -0.77


############################################
#####   Phys Variable Average Levels   #####
############################################


#HR Effects: No differences in HR by Dx or class
summary(lm(hr ~ clinical + study + sex, dat)) #
summary(lm(hr ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat)) #
summary(lm(hr ~ cl1 + cl2 + cl3 + cl4 + cl5 + cl6 + cl7 + study + sex, dat)) #Class 7 [t = 2.44]
summary(lm(hr ~ cl1rate + cl2rate + cl3rate + cl5rate + cl6rate + cl7rate + study + sex, dat)) #

#RSA Effects: No difference in RSA by Dx, small diff in Class 4
summary(lm(rsa ~ clinical + study + sex, dat)) #
summary(lm(rsa ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat)) #
summary(lm(rsa ~ cl1 + cl2 + cl3 + cl4 + cl5 + study + sex, dat)) # class 5 [t = -2.15]
summary(lm(rsa ~ cl2rate + cl3rate + cl4rate + cl5rate + study + sex, dat)) #

#PEP effects
#Individuals with PTSD exhibit significantly shorter PEP (+ SNS, + inotropy)
#Greater frequency of Class 3 predicts shorter PEP
#No effects on PEP level from Class 2 or Class 4 (presence or rate)
summary(lm(pep ~ clinical + study + sex, dat)) # t = -1.90 (p = 0.06)
summary(lm(pep ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat)) # PTSD [t = -3.38], female [t = 3.23], R2 = 0.11
summary(lm(pep ~ cl1 + cl2 + cl3 + cl4 + cl5 + study + sex, dat)) # No main effect for class 2 or class 4 (though class 4 has a t = 1.94), R2 = 0.08
summary(lm(pep ~ cl3rate + gad + dep + sad + ptsd + panic + phob + study + sex, dat)) # Class 3 rate [t = -2.19], PTSD [t = -3.60], R2 = 0.13

#Stroke Volume effects: No difference by class presence
summary(lm(sv ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat)) # Panic [t = -2.08]
summary(lm(sv ~ cl1 + cl2 + cl3 + cl4 + cl5 + study + sex, dat)) # 
summary(lm(sv ~ cl1rate + cl2rate + cl3rate + cl4rate + study + sex, dat)) # 

#Cardiac Output effects: No differences by Dx or Class
summary(lm(co ~ gad + dep + sad + ptsd + panic + phob + study + sex, dat)) #
summary(lm(co ~ cl1 + cl2 + cl3 + cl4 + cl5 + study + sex, dat)) # 
summary(lm(co ~ cl1rate + cl3rate + cl2rate + cl5rate + study + sex, dat)) # 


#################################################
#####   Phys Variable Change and Dynamics   #####
#################################################


#Change in HR is predicted by all vars except study and sex
#As PEP lengthens, HR goes down
#As RSA increases, HR goes down
#Increased rates of Class 6 mitigate HR reduction
#Increased MSSD in HR mitigates HR reduction
summary(lm(hrslope ~ cl6rate + rsaslope + pepslope + hrrange + sqrt(hrmssd) + clinical + ptsd + study + sex, dat)) #R2 = 0.53

#As PEP lengthens, MSSD increases (likely adaptive PNS influence)
#This effect is attenuated in depression
summary(lm(sqrt(dat$hrmssd) ~ scale(pepslope)*scale(dep) + cl3rate + panic + hr + hrslope + study + sex, dat)) # R2 = 0.12
summary(lm(sqrt(dat$hrmssd) ~ scale(pepslope)*scale(dep) + scale(rsaslope)*scale(dep) + cl3rate + panic + hr + hrslope + study + sex, dat)) # R2 = 0.14

#Meanwhile, lengthening of PEP is predicted by increases in Class 3 frequency, which is increased in depression
summary(lm(pepslope ~ rsa + rsaslope + scale(dep) * scale(cl3slope) + clinical + hr + hrslope + study + sex, dat)) # R2 = 0.13
#Removing HR-Slope decreases R2, but does not affect other findings from model
summary(lm(pepslope ~ rsa + rsaslope + scale(dep) * scale(cl3slope) + clinical + hr + study + sex, dat)) # R2 = 0.11

summary(lm(pepslope ~ rsa + rsaslope + scale(dep) * scale(coslope) + clinical + hr + study + sex, dat)) # R2 = 0.11

#HR-MSSD effects do not hold for average levels of PEP/RSA (only slopes)
summary(lm(sqrt(dat$hrmssd) ~ scale(pep)*scale(dep) + cl3rate + panic + hr + hrslope + study + sex, dat)) # R2 = 0.10
summary(lm(sqrt(dat$hrmssd) ~ scale(pep)*scale(dep) + scale(rsa)*scale(dep) + cl3rate + panic + hr + hrslope + study + sex, dat)) # R2 = 0.27

#Change in HR is predicted by change in PEP, RSA, clinical status, and rates of Classes 3 & 5
summary(lm(hrslope ~ rsaslope + pepslope + cl3rate + cl5rate + clinical + hr + study + sex, dat)) # R2 = 0.38

#SD in HR is predicted by Classes 2 and 5
summary(lm(sdhr ~ rsa + pep + cl2rate + panic + hr + hrslope + study + sex, dat)) # R2 = 0.27
summary(lm(sdhr ~ rsa + pep + cl5rate + panic + hr + hrslope + study + sex, dat)) # R2 = 0.30
summary(lm(sdhr ~ rsa + pep + cl2rate + cl5rate + panic + hr + hrslope + study + sex, dat)) # R2 = 0.31

#Effect is maintained with RSA and PEP slopes, but weaker by nearly half
summary(lm(sdhr ~ rsaslope + pepslope + cl3rate + panic + hr + hrslope + study + sex, dat)) # R2 = 0.18
summary(lm(sdhr ~ rsaslope + pepslope + scale(cl3rate) * scale(panic) + hr + hrslope + study + sex, dat)) # R2 = 0.19 Amplifying effect of Panic + Class 3

#Rates for Classes 3 & 6 predict decreases in RSA
#However, the degree to which 2 & 4 were positively correlated predicts increases in RSA
summary(lm(rsaslope ~ cl3rate + hrslope + hr + pepslope + clinical + study + sex, dat)) #R2 = 0.44
summary(lm(rsaslope ~ cl6rate + hrslope + hr + pepslope + clinical + study + sex, dat)) #R2 = 0.44
summary(lm(rsaslope ~ cl3rate + cl6rate + hrslope + hr + pepslope + clinical + study + sex, dat)) #R2 = 0.46


#HR Autoregression is predicted by greater Class 5 frequency and lower RSA
#RSA effects are mitigated in the presence of elevated PEP
summary(lm(hrar ~ cl5rate + scale(rsa) + scale(pep) + hr + clinical + study + sex, dat)) #R2 = 0.14
summary(lm(hrar ~ cl5rate + scale(rsa) * scale(pep) + hr + clinical + study + sex, dat)) #R2 = 0.18


###################################################################
#####   Reappraisal and Variation in HR (Open Trial Sample)   #####
###################################################################

#Reappraisal decreases variation in HR
summary(lm(sdhr ~ rsa + scale(erqrtotal) * scale(hr) + cl3rate + clinical + hrslope + sex, dat)) # R2 = 0.37
summary(lm(sqrt(dat$hrmssd) ~ rsa + scale(erqrtotal) * scale(hr) + cl3rate + clinical + hrslope + sex, dat)) # R2 = 0.37
#Effect does not require controlling for RSA
summary(lm(sdhr ~ scale(erqrtotal) * scale(hr) + cl3rate + clinical + hrslope + sex, dat)) # R2 = 0.22
summary(lm(sqrt(dat$hrmssd) ~ scale(erqrtotal) * scale(hr) + cl3rate + clinical + hrslope + sex, dat)) # R2 = 0.11


summary(lm(sdhr ~ scale(erqrtotal) * scale(hr) + scale(neon) + cl3rate + clinical + hrslope + sex, dat)) # R2 = 0.22
summary(lm(sqrt(dat$hrmssd) ~ scale(erqrtotal) * scale(hr) + scale(hr) * scale(neon) + cl3rate + clinical + hrslope + sex, dat)) # R2 = 0.11

summary(lm(sdhr ~ rsa + scale(erqrtotal) * scale(hr) + scale(erqrtotal) * scale(neon) + cl3rate + clinical + hrslope + sex, dat)) # R2 = 0.22
summary(lm(sqrt(dat$hrmssd) ~ rsa + scale(erqrtotal) * scale(hr) + scale(erqrtotal) * scale(neon) + cl3rate + clinical + hrslope + sex, dat)) # R2 = 0.11

summary(lm(cl5rate ~ scale(erqrtotal) * scale(neon) + clinical + sex, dat, cl5rate > 0)) #R2 = 0.20
summary(lm(cl5rate ~ scale(erqrtotal) * scale(neon) + gad + dep + sad + ptsd + panic + phob + sex, dat, cl5rate > 0)) #R2 = 0.25
summary(lm(cl5rate ~ scale(erqrtotal) * scale(hrsd) + gad + dep + sad + ptsd + panic + phob + sex, dat, cl5rate > 0)) #R2 = 0.22




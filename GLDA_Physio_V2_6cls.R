library(dplyr)
library(RCurl)
library(RJSONIO)
library(ggplot2)
library(lubridate)
library(tidyr)
library(reshape2)
library(tm)
library(topicmodels)
library(lda)
library(rstan)
library(slam)
library(shinystan)
library(bayesplot) # not all of these packages are used 

setwd("/Users/cw29265-admin/Documents/LDA_EMA/") # change to your own file path
load("phys_data/project_data_4.15.22.RData") # shared by Aaron

# the following train/test split is inherited from 1_setup.R shared by Aaron
trainlist = dfList[c(2, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18, 19, 21, 23, 24, 25, 26, 27, 29, 30, 31, 32, 33, 34, 36, 37,
                     40, 41, 44, 46, 47, 48, 49, 50, 53, 54, 55, 56, 57, 58, 60, 61, 62, 63, 64, 65, 66, 67, 68, 70, 72, 73, 74,
                     75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 87, 88, 89, 90, 92, 93, 94, 95, 96, 99, 100, 101, 104, 105, 106, 107,
                     109, 110, 111, 112, 113, 114, 116, 117, 118, 119, 120, 122, 123, 125, 126, 127, 129, 130, 131, 132, 133, 134,
                     135, 139, 140, 142, 143, 144, 146, 147, 148, 149, 150, 152, 157, 158, 159, 160, 161, 162, 163, 165, 166, 167,
                     168, 170, 171, 172, 173, 174, 175, 176, 177, 179, 181, 182, 184, 187, 188, 189, 190, 191, 192, 193, 194, 195,
                     196, 197, 200, 201, 203, 204, 205, 206, 207, 208, 209, 212, 215, 216, 218, 219, 221, 222)]
testlist = dfList[c(1, 3, 4, 11, 20, 22, 28, 35, 38, 39, 42, 43, 45, 51, 52, 59, 69, 71, 85, 86, 91, 97, 98, 102, 103, 108, 115,
                    121, 124, 128, 136, 137, 138, 141, 145, 151, 153, 154, 155, 156, 164, 169, 178, 180, 183, 185, 186, 198, 199,
                    202, 210, 211, 213, 214, 217, 220)]

# prepare raw training data 
data <- bind_rows(
  lapply(1:length(trainlist), function(i) {
    tryCatch({
      trainlist[[i]] %>% 
        select(-12) %>% # note, for all the "c" pids the hr column is repeated; here removing the second hr column
        mutate(pid = names(trainlist)[i]) %>% 
        select(pid, segment, hr, rmssd, pep, resp_rate, resp_amp) %>% 
        filter_all(all_vars(!is.na(.)))
    }, error = function(e){})
  })) 

# standardize within each participant  
data.scaled <- data %>% group_by(pid) %>% mutate_at(vars(-pid, -segment), ~ scale(.)) %>% as.data.frame()

# set up input data for GLDA
K = 6 # based on 1_setup.R
V = 5
stan_data <- list(
  K = K,
  V = V,
  M = length(unique(data.scaled$pid)),
  N = nrow(data.scaled),
  w = data.scaled[,3:7],
  pid = as.integer(factor(data.scaled$pid)), 
  alpha = rep(1, K),
  nu = V+2, # minimum is V (number of predictors), the smaller the less restrictive around psi; see https://math.stackexchange.com/questions/2803164/degrees-of-freedom-in-a-wishart-distribution 
  psi = diag(V),
  mu0 = rep(0, V),
  kappa = 1
)  

# run STAN program for GLDA and save result in an rds
if (FALSE) {
  fit_rstan <- stan(
    file = "ema.stan",
    data = stan_data,
    chains = 1,      
    iter = 1000,    
    warmup = 200
    )
  saveRDS(fit_rstan, "fit_rstan_physio_v2_6cls.rds")
}

# read in GLDA result
fit_rstan <- readRDS("fit_rstan_physio_v2_6cls.rds")

# check dimensions
dim(rstan::extract(fit_rstan)$theta) # number of post-warmup draws * number of participants (M) * number of clusters (K)
dim(rstan::extract(fit_rstan)$mu) # number of post-warmup draws * number of clusters (K) * number of features (V)

# check convergence, should converge after warmup period
rstan::traceplot(fit_rstan, pars = paste('mu[', 1:6, ',1]', sep = ''), inc_warmup = T)

# check autocorrelations, should decay exponentially 
stan_ac(fit_rstan, pars = 'mu', lags = 40)

# extract results from the fit_rstan draws
theta <- apply(extract(fit_rstan)$theta[1:800, ,], c(2,3), mean) # M=166 by K=6 
mu <- apply(extract(fit_rstan)$mu[1:800, ,], c(2,3), mean) # K=6 by V=5
sigma <- apply(extract(fit_rstan)$sigma[1:800, , ,], c(2,3,4), mean) # 6 by 5 by 5

# plot cluster means over variables
ggplot(data.frame(index = rep(1:5,3), variable = c(rep("K1",5), rep("K2", 5), rep("K3",5), rep("K4",5), rep("K5",5), rep("K6",5))) %>% 
  mutate(mu = c(mu[1,], mu[2,], mu[3,], mu[4,], mu[5,], mu[6,])), aes(x = as.numeric(index), color = as.factor(variable))) + theme_bw() +
  geom_line(aes(y = mu), size = 1)  +
  geom_point(aes(y = mu), size = 2) +
  scale_x_continuous(breaks = 1:5, labels = names(data.scaled)[3:7]) + # , name = "physio measure") +
  scale_y_continuous(breaks = seq(-1,2,1), name = "Z-score") +
  scale_color_discrete(name = "cluster", limits = c("K1", "K2", "K3", "K4", "K5", "K6")) +
  theme(panel.grid.minor.x = element_blank(), axis.text=element_text(size=12),
        axis.title.x=element_blank(), axis.title.y=element_text(size=14))

# compute cluster membership (based on posterior probabilities, hence var names "post.X") for each observation of each participant
library(mvtnorm)
data.scaled.post <- data.scaled %>% mutate(m = as.integer(factor(data.scaled$pid))) %>% 
  mutate(post.1 = dmvnorm(data.scaled[, 3:7], mean = mu[1, ], sigma = sigma[1, ,], log = FALSE) * theta[m, 1]) %>% 
  mutate(post.2 = dmvnorm(data.scaled[, 3:7], mean = mu[2, ], sigma = sigma[2, ,], log = FALSE) * theta[m, 2]) %>%
  mutate(post.3 = dmvnorm(data.scaled[, 3:7], mean = mu[3, ], sigma = sigma[3, ,], log = FALSE) * theta[m, 3]) %>%
  mutate(post.4 = dmvnorm(data.scaled[, 3:7], mean = mu[4, ], sigma = sigma[4, ,], log = FALSE) * theta[m, 4]) %>%
  mutate(post.5 = dmvnorm(data.scaled[, 3:7], mean = mu[5, ], sigma = sigma[5, ,], log = FALSE) * theta[m, 5]) %>%
  mutate(post.6 = dmvnorm(data.scaled[, 3:7], mean = mu[6, ], sigma = sigma[6, ,], log = FALSE) * theta[m, 6]) %>%
  mutate(post.sum = post.1 + post.2 + post.3 + post.4 + post.5 + post.6) %>% 
  mutate(post.1 = post.1/post.sum, post.2 = post.2/post.sum, post.3 = post.3/post.sum, 
         post.4 = post.4/post.sum, post.5 = post.5/post.sum, post.6 = post.6/post.sum)

data.scaled.post$glda.cl <- apply(data.scaled.post[, c("post.1", "post.2", "post.3", "post.4", "post.5", "post.6")], 1, 
                                  function(x)  factor(c("K1", "K2", "K3", "K4", "K5", "K6"))[which.max(x)])
data.scaled.post

# calculate BIC of the 6-class GLDA model (manually)
K <- 6
V <- 5
M <- stan_data$M
logli <- sum(log(data.scaled.post$post.sum))
d <- K*V + K*V*(V+1)/2 + (K-1)*M
N <- nrow(data.scaled.post)
bic.6cls <- d*log(N) - 2*logli
bic.6cls

# explore correlations between physio variables and class posteriors over all observations: the strongest correlation is between resp_amp and class 2 probability 
cor(data.scaled.post[,c("hr", "rmssd", "pep", "resp_rate", "resp_amp")], 
    data.scaled.post[, c("post.1", "post.2", "post.3", "post.4", "post.5", "post.6")])

# try plotting an example participants' momentary cluster membership transition
pid.example <- data.scaled.post %>% select(pid, segment, post.1:post.6, glda.cl) %>% filter(pid == "c001")  
ggplot(pid.example, aes(x = segment, y = as.integer(glda.cl))) + theme_bw() +
  geom_point(size = 0.5) +
  geom_path() +
  scale_x_continuous(breaks = seq(0, nrow(pid.example), 25)) +
  scale_y_continuous(name = "GLDA-inferred discrete state", breaks = 1:6, labels = c("K1", "K2", "K3", "K4", "K5", "K6")) +
  theme(axis.text = element_text(size = 12), 
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

# create a dataframe modeled after "groupdata" in 1_setup.R
groupdata.glda <- data.scaled.post %>% mutate(time = segment-1) %>% group_by(pid) %>% 
  summarize(cl1rate= length(which(glda.cl == "K1"))/n(),
            cl2rate= length(which(glda.cl == "K2"))/n(),
            cl3rate= length(which(glda.cl == "K3"))/n(),
            cl4rate= length(which(glda.cl == "K4"))/n(),
            cl5rate= length(which(glda.cl == "K5"))/n(),
            cl6rate= length(which(glda.cl == "K6"))/n(),
            nbclasses = length(unique(glda.cl)), 
            cl1 = ifelse(cl1rate >0, 1, 0),
            cl2 = ifelse(cl2rate >0, 1, 0),
            cl3 = ifelse(cl3rate >0, 1, 0),
            cl4 = ifelse(cl4rate >0, 1, 0),
            cl5 = ifelse(cl5rate >0, 1, 0),
            cl6 = ifelse(cl6rate >0, 1, 0),
            post1slope = lm(scale(post.1) ~ scale(time))$coefficients[2],
            post2slope = lm(scale(post.2) ~ scale(time))$coefficients[2],
            post3slope = lm(scale(post.3) ~ scale(time))$coefficients[2],
            post4slope = lm(scale(post.4) ~ scale(time))$coefficients[2],
            post5slope = lm(scale(post.5) ~ scale(time))$coefficients[2],
            post6slope = lm(scale(post.6) ~ scale(time))$coefficients[2]
            ) %>% 
  cbind(data.frame(theta1 = theta[,1], theta2 = theta[,2], theta3 = theta[,3],
            theta4 = theta[,4], theta5 = theta[,5], theta6 = theta[,6])) # the participant order of theta matches that of groupdata.glda after the summarize

groupdata.glda

# check correlations between physio variables and class posteriors for each participant; and then compute element-wise mean and sd
groupdata.glda.cor <- lapply(unique(data.scaled.post$pid), function(p) {
  cor(data.scaled.post[which(data.scaled.post$pid == p), c("hr", "rmssd", "pep", "resp_rate", "resp_amp")], 
      data.scaled.post[which(data.scaled.post$pid == p), c("post.1", "post.2", "post.3", "post.4", "post.5", "post.6")])
})

apply(array(unlist(groupdata.glda.cor), c(5, 6, 166)), c(1,2), mean) 
apply(array(unlist(groupdata.glda.cor), c(5, 6, 166)), c(1,2), sd) 

# check correlations between personalized class weights (theta) and realized class frequencies; notice the diagonal is all >0.9
cor(groupdata.glda[,c("cl1rate", "cl2rate", "cl3rate", "cl4rate", "cl5rate", "cl6rate")], 
    groupdata.glda[,c("theta1", "theta2", "theta3", "theta4", "theta5", "theta6")])

# *********************************************************
# compare with Gaussian Mixture Model over all observations
library(mclust)
X <- data.scaled %>% select(-pid, -segment)
gmm <- Mclust(X, G = 6, modelName = c('EII', 'VII', 'EEI', 'VEI', 'EVI', 'VVI'))

mu.gmm <- gmm$parameters$mean %>% as.data.frame() %>%
  `colnames<-`(factor(c("K1", "K2", "K3", "K4", "K5", "K6"))) %>%
  mutate(index = factor(1:5)) %>% melt()
mu.gmm

ggplot(mu.gmm, aes(x = as.numeric(index), y = value, color = variable)) + theme_bw() + 
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:5, labels = names(data.scaled)[3:7], name = "physio measure") +
  scale_y_continuous(breaks = seq(-1,2,1), name = "Z-score") +
  scale_color_discrete(name = "cluster", limits = c("K1", "K2", "K3", "K4", "K5", "K6")) +
  theme(panel.grid.minor.x = element_blank(), axis.text=element_text(size=12), axis.title.x=element_blank(), axis.title.y=element_text(size=14))

theta.gmm <- data.scaled %>% mutate(cluster = gmm$classification) %>% group_by(pid) %>%
  summarize(theta1 = length(which(cluster == 1))/n(), theta2 = length(which(cluster == 2))/n(),  theta3 = length(which(cluster == 3))/n(),
            theta4 = length(which(cluster == 4))/n(), theta5 = length(which(cluster == 5))/n(),  theta6 = length(which(cluster == 6))/n()) %>% 
  as.data.frame()

theta.gmm # gmm class proportions per individual
theta # glda class weights per individual; note that the class numbers are not meaningful (gmm's class 1 does not necessarily correspond to glda's class 1)

# **************************************************
# explore correlations with mental disorder diagnoses
truth <- dat %>% filter(id %in% unique(data.scaled$pid)) %>% mutate(pid = id) %>% select(-id) # %>% select(pid = id, sex, age, hrsd, hama, dasss)
hist(truth$hrsd, breaks = 10)
hist(truth$hama, breaks = 10)
hist(truth$dasss, breaks = 10)

# mental disorder ~ GLDA class weights 
training.glda <- data.frame(pid = groupdata.glda$pid, theta1 = theta[,1], theta2 = theta[,2], theta3 = theta[,3],
                       theta4 = theta[,4], theta5 = theta[,5], theta6 = theta[,6]) %>% inner_join(truth, by = "pid")

summary(lm(hrsd ~ theta1, training.glda))
summary(lm(hrsd ~ theta2, training.glda)) # p=0.002 
summary(lm(hrsd ~ theta3, training.glda))
summary(lm(hrsd ~ theta4, training.glda)) # p=0.000
summary(lm(hrsd ~ theta5, training.glda)) 
summary(lm(hrsd ~ theta6, training.glda)) 

summary(lm(hama ~ theta1, training.glda))
summary(lm(hama ~ theta2, training.glda)) 
summary(lm(hama ~ theta3, training.glda))
summary(lm(hama ~ theta4, training.glda)) # p=0.001
summary(lm(hama ~ theta5, training.glda))
summary(lm(hama ~ theta6, training.glda))

summary(lm(dasss ~ theta1, training.glda)) 
summary(lm(dasss ~ theta2, training.glda)) # p=0.000
summary(lm(dasss ~ theta3, training.glda)) 
summary(lm(dasss ~ theta4, training.glda)) # p=0.000
summary(lm(dasss ~ theta5, training.glda)) 
summary(lm(dasss ~ theta6, training.glda))

# Mental disorder ~ GMM class frequencies 
training.gmm <- data.frame(theta.gmm) %>% inner_join(truth, by = "pid")
training.gmm

summary(lm(hrsd ~ theta1, training.gmm))
summary(lm(hrsd ~ theta2, training.gmm)) 
summary(lm(hrsd ~ theta3, training.gmm))
summary(lm(hrsd ~ theta4, training.gmm))  
summary(lm(hrsd ~ theta5, training.gmm)) 
summary(lm(hrsd ~ theta6, training.gmm))

summary(lm(hama ~ theta1, training.gmm))
summary(lm(hama ~ theta2, training.gmm)) 
summary(lm(hama ~ theta3, training.gmm))
summary(lm(hama ~ theta4, training.gmm))
summary(lm(hama ~ theta5, training.gmm)) 
summary(lm(hama ~ theta6, training.gmm))

summary(lm(dasss ~ theta1, training.gmm))
summary(lm(dasss ~ theta2, training.gmm)) 
summary(lm(dasss ~ theta3, training.gmm)) 
summary(lm(dasss ~ theta4, training.gmm))
summary(lm(dasss ~ theta5, training.gmm)) 
summary(lm(dasss ~ theta6, training.gmm))

# save.image("GLDA_Physio_V2_6cls.RData")

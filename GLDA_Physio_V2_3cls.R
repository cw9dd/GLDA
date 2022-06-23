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
library(bayesplot)

file.path <- "/Users/cw29265-admin/Documents/LDA_EMA/"
setwd(file.path)

load("phys_data/project_data_4.15.22.RData")

pids <- names(dfList)
length(pids) # 222

# the following train/test split is inherited from 1_setup.R
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
pids.train <- names(trainlist)
pids.test <- names(testlist)

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
K = 3
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

# run STAN program for GLDA
fit_rstan <- stan(
  file = "ema.stan",
  data = stan_data,
  chains = 1,      
  iter = 1000,    
  warmup = 200
)

# saveRDS(fit_rstan, "fit_rstan_physio_v2_3cls.rds")
fit_rstan <- readRDS("fit_rstan_physio_v2_3cls.rds")

# check dimensions
dim(rstan::extract(fit_rstan)$theta) # number of post-warmup draws * number of participants (M) * number of clusters (K)
dim(rstan::extract(fit_rstan)$mu) # number of post-warmup draws * number of clusters (K) * number of features (V)

# check convergence, should converge after warmup period
rstan::traceplot(fit_rstan, pars = paste('mu[', 1:3, ',1]', sep = ''), inc_warmup = T)

# check autocorrelations, should decay exponentially 
stan_ac(fit_rstan, pars = 'mu', lags = 20)

# extract results from the fit_rstan draws
theta <- apply(extract(fit_rstan)$theta[1:800, ,], c(2,3), mean) # M=50 by K=3 
mu <- apply(extract(fit_rstan)$mu[1:800, ,], c(2,3), mean) # K=3 by V=7
sigma <- apply(extract(fit_rstan)$sigma[1:800, , ,], c(2,3,4), mean) # 3 by 7 by 7

# plot cluster means over variables
mu.for.plot <- data.frame(index = rep(1:5,3), variable = c(rep("K1",5), rep("K2", 5), rep("K3",5))) %>% 
  mutate(mu = c(mu[1,], mu[2,], mu[3,]))
mu.for.plot

# original visualization code (sent to Aaron)
ggplot(mu.for.plot, aes(x = as.numeric(index), color = as.factor(variable))) + theme_bw() +
  geom_line(aes(y = mu), size = 1)  +
  geom_point(aes(y= mu), size = 2) +
  scale_x_continuous(breaks = 1:5, labels = names(data.scaled)[3:7]) + # , name = "physio measure") +
  scale_y_continuous(breaks = seq(-1,2,1), name = "Z-score") +
  scale_color_discrete(name = "cluster", limits = c("K1", "K2", "K3")) +
  theme(panel.grid.minor.x = element_blank(), axis.text=element_text(size=12),
        axis.title.x=element_blank(), axis.title.y=element_text(size=14))

# plot for the paper
ggplot(mu.for.plot, aes(x = as.numeric(index), linetype = as.factor(variable))) + theme_bw() +
  geom_line(aes(y = mu), size = 1)  +
  #geom_point(aes(y= mu), size = 2) +
  scale_x_continuous(breaks = 1:5, labels = c("hr","rsa","pep","resp_rate","resp_amp")) + # , name = "physio measure") +
  scale_y_continuous(breaks = seq(-1,2,0.1), name = "Z-score") +
  scale_linetype_manual(values = c(1,2,3)) +
  theme(panel.grid.minor.x = element_blank(), axis.text.y=element_text(size =12), axis.text.x=element_text(size=12, vjust = 1, hjust = 1),  legend.position = "none",
        axis.title.x=element_blank(), axis.title.y=element_text(size=14))


# compute cluster membership (based on posterior probabilities) for each observation of each participant
library(mvtnorm)
data.scaled.post <- data.scaled %>% 
  mutate(m = as.integer(factor(data.scaled$pid))) %>% 
  mutate(post.1 = dmvnorm(data.scaled[, 3:7], mean = mu[1, ], sigma = sigma[1, ,], log = FALSE) * theta[m, 1]) %>% 
  mutate(post.2 = dmvnorm(data.scaled[, 3:7], mean = mu[2, ], sigma = sigma[2, ,], log = FALSE) * theta[m, 2]) %>%
  mutate(post.3 = dmvnorm(data.scaled[, 3:7], mean = mu[3, ], sigma = sigma[3, ,], log = FALSE) * theta[m, 3]) %>%
  mutate(post.sum = post.1 + post.2 + post.3) %>% 
  mutate(post.1 = post.1/post.sum, post.2 = post.2/post.sum, post.3 = post.3/post.sum)

data.scaled.post$glda.cl <- apply(data.scaled.post[, c("post.1", "post.2", "post.3")], 1, 
                                      function(x)  factor(c("K1", "K2", "K3"))[which.max(x)])
data.scaled.post

# calculate BIC of the 3-class GLDA model (manually)
K <- 3
V <- 5
M <- stan_data$M
logli <- sum(log(data.scaled.post$post.sum))
d <- K*V + K*V*(V+1)/2 + (K-1)*M
N <- nrow(data.scaled.post)
bic.3cls <- d*log(N) - 2*logli
bic.3cls

# check *realized frequency* of each cluster (note the relation with and difference from *mixture weights*)
data.scaled.post %>% group_by(pid) %>% 
  summarize(K1= length(which(glda.cl == "K1"))/n(),
            K2= length(which(glda.cl == "K2"))/n(),
            K3= length(which(glda.cl == "K3"))/n()) %>% as.data.frame() 

# plot a few example participants' momentary cluster membership transition
pid.example <- data.scaled.post %>% select(pid, segment, post.1:post.3, glda.cl) %>% filter(pid == "c001")  
ggplot(pid.example, aes(x = segment, y = as.integer(glda.cl))) + theme_bw() +
  geom_point(size = 0.5) +
  geom_path() +
  scale_x_continuous(breaks = seq(0, nrow(pid.example), 25)) +
  scale_y_continuous(name = "GLDA-inferred discrete state", breaks = 1:3, labels = c("K1", "K2", "K3")) +
  theme(axis.text = element_text(size = 12), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"))

# try vanilla GMM
# Compare with vanilla Gaussian Mixture Models
library(mclust)
X <- data.scaled %>% select(-pid, -segment)
gmm <- Mclust(X, G = 3, modelName = c('EII', 'VII', 'EEI', 'VEI', 'EVI', 'VVI'))

mu.gmm <- gmm$parameters$mean %>% as.data.frame() %>%
  `colnames<-`(factor(c("K1", "K2", "K3"))) %>%
  mutate(index = factor(1:5)) %>% melt()
mu.gmm
# saveRDS(mu.gmm, "gmm_mu_dataset1.rds")
# mu.gmm <- readRDS("gmm_mu_dataset1.rds")

ggplot(mu.gmm, aes(x = as.numeric(index), y = value, color = variable)) + theme_bw() + 
  geom_line(size = 1) + 
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:5, labels = names(data.scaled)[3:7]) +
  scale_y_continuous(breaks = seq(-1,2,1), name = "Z-score") +
  scale_color_discrete(name = "cluster", limits = c("K1", "K2", "K3")) +
  theme(panel.grid.minor.x = element_blank(), axis.text=element_text(size=12),
        axis.title.x=element_blank(), axis.title.y=element_text(size=14))

ggplot(mu.for.plot, aes(x = as.numeric(index), color = as.factor(variable))) + theme_bw() +
  geom_line(aes(y = mu), size = 1)  +
  geom_point(aes(y= mu), size = 2) +
  scale_x_continuous(breaks = 1:5, labels = names(data.scaled)[3:7]) + # , name = "physio measure") +
  scale_y_continuous(breaks = seq(-1,2,1), name = "Z-score") +
  scale_color_discrete(name = "cluster", limits = c("K1", "K2", "K3")) +
  theme(panel.grid.minor.x = element_blank(), axis.text=element_text(size=12),
        axis.title.x=element_blank(), axis.title.y=element_text(size=14))

# plot for the paper
ggplot(mu.gmm, aes(x = as.numeric(index), y = value, linetype = as.factor(variable))) + theme_bw() + 
  geom_line(size = 1) + 
  scale_x_continuous(breaks = 1:5, labels = c("hr","rsa","pep","resp_rate","resp_amp")) +
  scale_y_continuous(breaks = seq(-1,2,0.1), limits = c(-0.5, 1.75), name = "Z-score") +
  scale_linetype_manual(values = c(1,2,3)) +
  theme(panel.grid.minor.x = element_blank(), axis.text.y=element_text(size =12), 
        axis.text.x=element_text(size=12, angle = 30, vjust = 1, hjust = 1),  legend.position = "none",
        axis.title.x=element_blank(), axis.title.y=element_text(size=14))
ggplot(mu.for.plot, aes(x = as.numeric(index), linetype = as.factor(variable))) + theme_bw() +
  geom_line(aes(y = mu), size = 1)  +
  #geom_point(aes(y= mu), size = 2) +
  scale_x_continuous(breaks = 1:5, labels = c("hr","rsa","pep","resp_rate","resp_amp")) + # , name = "physio measure") +
  scale_y_continuous(breaks = seq(-1,2,0.1),limits = c(-0.5, 1.75), name = "Z-score") +
  scale_linetype_manual(values = c(1,2,3)) +
  theme(panel.grid.minor.x = element_blank(), axis.text.y=element_text(size =12), 
        axis.text.x=element_text(size=12, angle = 30, vjust = 1, hjust = 1),  legend.position = "none",
        axis.title.x=element_blank(), axis.title.y=element_text(size=14))

theta.gmm <- data.scaled %>% mutate(cluster = gmm$classification) %>% group_by(pid) %>%
  summarize(theta1 = length(which(cluster == 1))/n(), theta2 = length(which(cluster == 2))/n(),  theta3 = length(which(cluster == 3))/n()) %>% as.data.frame()
# cbind(theta.gmm, theta) # inspect the differences between the 

# cor(theta.gmm[,2], theta[,1]) 
# cor(theta.gmm[,3], theta[,2]) 
# cor(theta.gmm[,4], theta[,3]) 

# compute correlation with mental disorder diagnoses
truth <- dat %>% filter(id %in% unique(data.scaled$pid)) %>% mutate(pid = id) %>% select(-id) # %>% select(pid = id, sex, age, hrsd, hama, dasss)
hist(truth$hrsd, breaks = 10)
hist(truth$hama, breaks = 10)
hist(truth$dasss, breaks = 10)

# mental disorder diagnoses ~ GLDA cluster frequency 
training <- data.frame(pid = data.scaled.post %>% select(pid, m) %>% unique() %>% arrange(m) %>% select(pid), theta1 = theta[,1], theta2 = theta[,2], theta3 = theta[,3]) %>%
  inner_join(truth, by = "pid")

summary(lm(hrsd ~ theta1, training))
summary(lm(hrsd ~ theta2, training))
summary(lm(hrsd ~ theta3, training)) # p=0.012

summary(lm(hama ~ theta1, training))
summary(lm(hama ~ theta2, training))
summary(lm(hama ~ theta3, training)) # p=0.047

summary(lm(dasss ~ theta1, training))
summary(lm(dasss ~ theta2, training))
summary(lm(dasss ~ theta3, training)) # p=0.001

# mental disorder diagnoses ~ GMM cluster frequency 
training <- data.frame(theta.gmm) %>%
  inner_join(truth, by = "pid")

summary(lm(hrsd ~ theta1, training))
summary(lm(hrsd ~ theta2, training)) 
summary(lm(hrsd ~ theta3, training)) 

summary(lm(hama ~ theta1, training))
summary(lm(hama ~ theta2, training))
summary(lm(hama ~ theta3, training))

summary(lm(dasss ~ theta1, training))
summary(lm(dasss ~ theta2, training))
summary(lm(dasss ~ theta3, training)) # p=0.015


plot(training$theta1, training$theta2)
plot(training[which(training$dx1 == "ptsd"), "theta1"], training[which(training$dx1 == "ptsd"), "theta2"])
plot(training[which(training$dx1 == "na"), "theta1"], training[which(training$dx1 == "na"), "theta2"])
plot(training[which(training$gad == "1"), "theta1"], training[which(training$gad == "1"), "theta2"])


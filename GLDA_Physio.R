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
data.path <- "/Users/cw29265-admin/Documents/LDA_EMA/Physio/"
setwd(file.path)

load("Physio/dflist.RData") # dflist, 150 participants ("p..."); ptlist, 50 participants ("c...")
load("Physio/GIMME_input_data.RData") # dats, only the 4 physio variables for the 150 participants 
load("Physio/ptsd_data.RData") # ground truth (mental disorder diagnoses) for 50 participants
load("phys_data/project_data_4.15.22.RData")

# cor(ptlist$c001 %>% filter(!is.na(pep)) %>% select(mean_ibi, rmssd, resp_rate, resp_amp, lvet, pep, dzdt_max)) 
# decided to use mean_ibi, rmssd, resp_rate, resp_amp, lvet, pep, and dzdt_max (7 variables) to do clustering after checking cor matrix
pids <- names(ptlist)
length(pids)

data <- bind_rows(lapply(1:length(ptlist), function(i) ptlist[[i]] %>% filter(!is.na(pep)) %>% 
         mutate(pid = pids[i]) %>% select(pid, segment, mean_ibi, rmssd, resp_rate, resp_amp, lvet, pep, dzdt_max)))

data.normalized <- data %>% mutate_at(vars(-pid, -segment), ~ scale(.)) %>% as.data.frame() # normalize across all observations
# alternatively:
data.normalized <- data %>% group_by(pid) %>% mutate_at(vars(-pid, -segment), ~ scale(.)) %>% as.data.frame() # normalize within each participant

K = 3
V = 7
stan_data <- list(
  K = K,
  V = V,
  M = length(unique(data.normalized$pid)),
  N = nrow(data.normalized),
  w = data.normalized[,3:9], # can be a data.frame?
  pid = as.integer(factor(data.normalized$pid)),
  alpha = rep(1, K),
  nu = V+3, # previously set it to +5, minimum is V (number of predictors), the smaller the less restrictive around psi
  psi = diag(V),
  mu0 = rep(0, V),
  kappa = 1
)  

fit_rstan <- stan(
  file = "ema.stan",
  data = stan_data,
  chains = 1,     # default is 4
  iter = 1000,    # default is 2000
  warmup = 200    # default is half of iter
)

# saveRDS(fit_rstan, "fit_rstan_physio_centered_by_pid.rds")
# saveRDS(fit_rstan, "fit_rstan_physio.rds")
fit_rstan <- readRDS("fit_rstan_physio_centered_by_pid.rds")

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
mu.for.plot <- data.frame(index = rep(1:7,3), variable = c(rep("K1",7), rep("K2", 7), rep("K3",7))) %>% 
  mutate(mu = c(mu[1,], mu[2,], mu[3,]))
mu.for.plot

ggplot(mu.for.plot, aes(x = as.numeric(index), color = as.factor(variable))) + theme_bw() +
  geom_line(aes(y = mu), size = 1)  +
  scale_x_continuous(breaks = 1:7, labels = names(data.normalized)[3:9], name = "physio measure") +
  scale_y_continuous(breaks = seq(-1,2,0.1), name = "standardized value") +
  scale_color_discrete(name = "cluster", limits = c("K1", "K2", "K3")) +
  theme(panel.grid.minor.x = element_blank(), axis.text=element_text(size=12),
        axis.title.x=element_text(size=14, face="bold"), axis.title.y=element_text(size=14, face="bold"))

# compute cluster membership (posterior probabilities) for each observation of each participant
library(mvtnorm)
data.normalized.post <- data.normalized %>% 
  mutate(m = sapply(1:nrow(data.normalized), function(x) which(pids == data.normalized$pid[x]))) %>% 
  mutate(post.1 = dmvnorm(data.normalized[, 3:9], mean = mu[1, ], sigma = sigma[1, ,], log = FALSE) * theta[m, 1]) %>% 
  mutate(post.2 = dmvnorm(data.normalized[, 3:9], mean = mu[2, ], sigma = sigma[2, ,], log = FALSE) * theta[m, 2]) %>%
  mutate(post.3 = dmvnorm(data.normalized[, 3:9], mean = mu[3, ], sigma = sigma[3, ,], log = FALSE) * theta[m, 3]) %>%
  mutate(post.sum = post.1 + post.2 + post.3) %>% 
  mutate(post.1 = post.1/post.sum, post.2 = post.2/post.sum, post.3 = post.3/post.sum)

data.normalized.post$glda.cl <- apply(data.normalized.post[, c("post.1", "post.2", "post.3")], 1, 
                                      function(x)  factor(c("K1", "K2", "K3"))[which.max(x)])
data.normalized.post

# plot a few example participants' momentary cluster membership transition
data.normalized.post %>% group_by(pid) %>% 
  summarize(K1= length(which(glda.cl == "K1"))/n(),
            K2= length(which(glda.cl == "K2"))/n(),
            K3= length(which(glda.cl == "K3"))/n()) %>% as.data.frame() 

pids.example <- data.normalized.post %>% select(pid, segment, post.1:post.3, glda.cl) %>% filter(pid == "c001")  
ggplot(pids.example, aes(x = segment, y = as.integer(glda.cl))) + theme_bw() +
  geom_point(size = 0.5) +
  geom_path() +
  scale_x_continuous(breaks = seq(0, nrow(pids.example), 25)) +
  scale_y_continuous(name = "GLDA-inferred discrete state", breaks = 1:3, labels = c("K1", "K2", "K3")) +
  theme(axis.text = element_text(size = 12), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"))

# try vanilla GMM
# Compare with vanilla Gaussian Mixture Models
library(mclust)
X <- data.normalized %>% select(-pid, -segment)
gmm <- Mclust(X, G = 3, modelName = c('EII', 'VII', 'EEI', 'VEI', 'EVI', 'VVI'))

mu.gmm <- gmm$parameters$mean %>% as.data.frame() %>%
  `colnames<-`(factor(c("K1", "K2", "K3"))) %>%
  mutate(index = factor(1:7)) %>% melt()
mu.gmm

ggplot(mu.gmm, aes(x = as.numeric(index), y = value, color = variable)) + theme_bw() + geom_line(size = 1) +
  scale_x_continuous(breaks = 1:7, labels = names(data.normalized)[3:9], name = "physio measure") +
  scale_y_continuous(breaks = seq(-1,1,0.1), name = "standardized value") +
  scale_color_discrete(name = "cluster", limits = c("K1", "K2", "K3")) +
  theme(panel.grid.minor.x = element_blank())

theta.gmm <- data.normalized %>% mutate(cluster = gmm$classification) %>% group_by(pid) %>%
  summarize(theta1 = length(which(cluster == 2))/n(), theta2 = length(which(cluster == 1))/n(),  theta3 = length(which(cluster == 3))/n()) %>% as.data.frame()
# cbind(theta.gmm, theta) # inspect the differences between the 

cor(theta.gmm[,2], theta[,1]) # 0.94 / -0.09
cor(theta.gmm[,3], theta[,2]) # 0.88 / 0.8
cor(theta.gmm[,4], theta[,3]) # 0.69 / 0.54

# compute correlation with mental disorder diagnoses
truth <- ptsd %>% filter(id %in% unique(data.normalized$pid)) %>% mutate(pid = id) %>% select(-id) # %>% select(pid = id, sex, age, hrsd, hama, dasss)
hist(truth$hrsd, breaks = 10)
hist(truth$hama, breaks = 10)
hist(truth$dasss, breaks = 10)

# mental disorder diagnoses ~ GLDA cluster frequency 
training <- data.frame(pid = unique(data.normalized$pid), theta1 = theta[,1], theta2 = theta[,2], theta3 = theta[,3]) %>%
  inner_join(truth, by = "pid")

summary(lm(hrsd ~ theta1, training))
summary(lm(hrsd ~ theta2, training))
summary(lm(hrsd ~ theta3, training))

summary(lm(hama ~ theta1, training))
summary(lm(hama ~ theta2, training))
summary(lm(hama ~ theta3, training))

summary(lm(dasss ~ theta1, training))
summary(lm(dasss ~ theta2, training))
summary(lm(dasss ~ theta3, training))

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
summary(lm(dasss ~ theta3, training))

plot(training$theta1, training$theta2)
plot(training[which(training$dx1 == "ptsd"), "theta1"], training[which(training$dx1 == "ptsd"), "theta2"])
plot(training[which(training$dx1 == "na"), "theta1"], training[which(training$dx1 == "na"), "theta2"])
plot(training[which(training$gad == "1"), "theta1"], training[which(training$gad == "1"), "theta2"])


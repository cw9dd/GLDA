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
K = 4 # based on 1_setup.R
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
  saveRDS(fit_rstan, "fit_rstan_physio_v2_4cls.rds")
}

# read in GLDA result
fit_rstan <- readRDS("fit_rstan_physio_v2_4cls.rds")

# check dimensions
dim(rstan::extract(fit_rstan)$theta) # number of post-warmup draws * number of participants (M) * number of clusters (K)
dim(rstan::extract(fit_rstan)$mu) # number of post-warmup draws * number of clusters (K) * number of features (V)

# check convergence, should converge after warmup period
rstan::traceplot(fit_rstan, pars = paste('mu[', 1:4, ',1]', sep = ''), inc_warmup = T)

# check autocorrelations, should decay exponentially 
stan_ac(fit_rstan, pars = 'mu', lags = 40)

# extract results from the fit_rstan draws
theta <- apply(extract(fit_rstan)$theta[1:800, ,], c(2,3), mean) # M=166 by K=6 
mu <- apply(extract(fit_rstan)$mu[1:800, ,], c(2,3), mean) # K=6 by V=5
sigma <- apply(extract(fit_rstan)$sigma[1:800, , ,], c(2,3,4), mean) # 6 by 5 by 5

# plot cluster means over variables
ggplot(data.frame(index = rep(1:5,4), variable = c(rep("K1",5), rep("K2", 5), rep("K3",5), rep("K4",5))) %>% 
         mutate(mu = c(mu[1,], mu[2,], mu[3,], mu[4,])), aes(x = as.numeric(index), color = as.factor(variable))) + theme_bw() +
  geom_line(aes(y = mu), size = 1)  +
  geom_point(aes(y = mu), size = 2) +
  scale_x_continuous(breaks = 1:5, labels = names(data.scaled)[3:7]) + # , name = "physio measure") +
  scale_y_continuous(breaks = seq(-1,2,1), name = "Z-score") +
  scale_color_discrete(name = "cluster", limits = c("K1", "K2", "K3", "K4")) +
  theme(panel.grid.minor.x = element_blank(), axis.text=element_text(size=12),
        axis.title.x=element_blank(), axis.title.y=element_text(size=14))

# compute cluster membership (based on posterior probabilities, hence var names "post.X") for each observation of each participant
library(mvtnorm)
data.scaled.post <- data.scaled %>% mutate(m = as.integer(factor(data.scaled$pid))) %>% 
  mutate(post.1 = dmvnorm(data.scaled[, 3:7], mean = mu[1, ], sigma = sigma[1, ,], log = FALSE) * theta[m, 1]) %>% 
  mutate(post.2 = dmvnorm(data.scaled[, 3:7], mean = mu[2, ], sigma = sigma[2, ,], log = FALSE) * theta[m, 2]) %>%
  mutate(post.3 = dmvnorm(data.scaled[, 3:7], mean = mu[3, ], sigma = sigma[3, ,], log = FALSE) * theta[m, 3]) %>%
  mutate(post.4 = dmvnorm(data.scaled[, 3:7], mean = mu[4, ], sigma = sigma[4, ,], log = FALSE) * theta[m, 4]) %>%
  mutate(post.sum = post.1 + post.2 + post.3 + post.4) %>% 
  mutate(post.1 = post.1/post.sum, post.2 = post.2/post.sum, post.3 = post.3/post.sum, 
         post.4 = post.4/post.sum)

data.scaled.post$glda.cl <- apply(data.scaled.post[, c("post.1", "post.2", "post.3", "post.4")], 1, 
                                  function(x)  factor(c("K1", "K2", "K3", "K4"))[which.max(x)])
data.scaled.post

# calculate BIC of the 4-class GLDA model (manually)
K <- 4
V <- 5
M <- stan_data$M
logli <- sum(log(data.scaled.post$post.sum))
d <- K*V + K*V*(V+1)/2 + (K-1)*stan_data$M
N <- nrow(data.scaled.post)
bic.4cls <- d*log(N) - 2*logli
bic.4cls

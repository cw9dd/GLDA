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
data.path <- "/Users/cw29265-admin/Documents/LDA_EMA/Peter Wu Shared Data/Fisher and Bosley Data/Within Subject/"
setwd(file.path)

pids <- list.files(data.path)

# Prepare data for STAN specification
data <- bind_rows(lapply(list.files(data.path), function(p) {read.csv(paste0(data.path, p, "/dat.csv")) %>%
    dplyr::select(-X, -Survey.Creation.Date) %>% mutate(pid = p)})) %>%
  dplyr::select(-How.many.hours.did.you.sleep.last.night, -Experienced.difficulty.falling.or.staying.asleep, -Experienced.restless.or.unsatisfying.sleep,
         -X.1, -X.2, -X.3, -X.4, -X.5, -X.6, -Felt.tense, -Felt.judged.or.criticized) %>%
  filter(!is.na(Felt.energetic)) %>%
  dplyr::select(pid = pid, time = Survey.Completion.Date, irritable = Felt.irritable, angry = Felt.angry, afraid = Felt.frightened.or.afraid, worried = Felt.worried, ruminating = Dwelled.on.the.past,
         down = Felt.down.or.depressed, hopeless = Felt.hopeless, anhedonic = Experienced.loss.of.interest.or.pleasure, avoidact = Avoided.activities, avoidpeople = Avoided.people)


data.descriptive <- data %>% group_by(pid) %>% summarize_at(vars(irritable:avoidpeople), mean) %>% as.data.frame()
hist(data.descriptive$afraid)

ggplot(data.descriptive) + theme_bw() +
  geom_density(aes(x = hopeless))  +
  geom_rug(aes(x = hopeless, y = 0), position = position_jitter(height = 0)) +
  geom_vline(xintercept = mean(data.descriptive$hopeless), color = "red", size = 1.2) +
  geom_rect(xmin = mean(data.descriptive$hopeless) - sd(data.descriptive$hopeless), xmax = mean(data.descriptive$hopeless) + sd(data.descriptive$hopeless),
            ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.01) +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0,100)) +
  theme(axis.text=element_text(size=12), axis.title.x=element_text(size=14,face="bold"), axis.title.y=element_text(size=14))


# Scale each variable to zero mean unit sd by the participant
data.centered <- data %>% group_by(pid) %>% mutate_at(vars(-pid, -time), ~ scale(.)) %>% as.data.frame()
data.centered

K = 3
V = 10
stan_data <- list(
  K = K,
  V = V,
  M = length(unique(data.centered$pid)),
  N = nrow(data.centered),
  w = data.centered[,3:12], # can be a data.frame?
  pid = as.integer(factor(data.centered$pid)),
  alpha = rep(1, K),
  nu = V, # previously set it to +5, minimum is V (number of predictors), the smaller the less restrictive around psi
  psi = diag(V),
  mu0 = rep(0, V),
  kappa = 1
)

fit_rstan <- stan(
  file = "ema.stan",
  data = stan_data,
  chains = 3,     # default is 4
  iter = 1000,    # default is 2000
  warmup = 200    # default is half of iter
)

fit_rstan <- stan(
  file = "ema.stan",
  data = stan_data,
  chains = 1,     # default is 4
  iter = 2000,    # default is 2000
  warmup = 1000    # default is half of iter
)

fit_rstan <- readRDS("fit_rstan_1chain.rds")
fit_rstan <- readRDS("fit_rstan_glda.rds") # used this for the following analyses! This one was run on 3 mcmc chains, each chain with 1000 iterations with first 200 being warmup iterations (gave warning about too few iterations)
fit_rstan <- readRDS("fit_rstan_new.rds") # this one was run on 4 mcmc chains, each chain with 2000 iterations with first 1000 being warmup iterations (converged like a champ)

dim(rstan::extract(fit_rstan)$theta) # number of post-warmup draws * number of participants (M) * number of clusters (K)
dim(rstan::extract(fit_rstan)$mu) # number of post-warmup draws * number of clusters (K) * number of features (V)
rstan::extract(fit_rstan)$theta[1000,,]
fit_rstan

rstan::extract(fit_rstan)$mu[2400, ,]
plot(as.data.frame(t(rstan::extract(fit_rstan)$mu[2400, ,]))$V2, type = "l")
# print(fit, pars=c("theta", "sigma", "z_init"), probs=c(0.1, 0.5, 0.9), digits = 3)

names(data.centered)[3:12]

fit_rstan %>% mcmc_trace()

library(mvtnorm)
for (m in 1:45) {
  m <- 1
}

theta <- apply(extract(fit_rstan)$theta[1:800, ,], c(2,3), mean) # M=45 by K=3 
mu <- apply(extract(fit_rstan)$mu[1:800, ,], c(2,3), mean) # K=3 by V=10
sigma <- apply(extract(fit_rstan)$sigma[1:800, , ,], c(2,3,4), mean) # 3 by 10 by 10

avg <- as.numeric(as.data.frame(mu) %>% apply(1, sum))
theta.glda <- as.data.frame(theta) %>% `colnames<-`(factor(rank(avg), labels = c("low", "med", "high")))


m <- 1
data.centered.post <- data.centered %>% 
  mutate(m = sapply(1:nrow(data.centered), function(x) which(pids == data.centered$pid[x]))) %>% 
  mutate(post.1 = dmvnorm(data.centered[, 3:12], mean = mu[1, ], sigma = sigma[1, ,], log = FALSE) * theta[m, 1]) %>% 
  mutate(post.2 = dmvnorm(data.centered[, 3:12], mean = mu[2, ], sigma = sigma[2, ,], log = FALSE) * theta[m, 2]) %>%
  mutate(post.3 = dmvnorm(data.centered[, 3:12], mean = mu[3, ], sigma = sigma[3, ,], log = FALSE) * theta[m, 3]) %>%
  mutate(post.sum = post.1 + post.2 + post.3) %>% 
  mutate(post.1 = post.1/post.sum, post.2 = post.2/post.sum, post.3 = post.3/post.sum)

data.centered.post$glda.cl <- apply(data.centered.post[, c("post.1", "post.2", "post.3")], 1, function(x) factor(rank(apply(mu, 1, mean)), labels = c("low", "med", "high"))[which.max(x)])
data.centered.post

# A predominantly High and Low participant
pids.40 <- data.centered.post %>% select(pid, time, post.1:post.3, glda.cl) %>% filter(pid == pids[40]) %>% 
  mutate(time = as.POSIXct(strptime(time, format = "%m/%d/%Y %H:%M"))) # this reformats the original timestamps to POSIXct local time
ggplot(pids.40, aes(x = time, y = as.integer(glda.cl))) + theme_bw() +
  geom_point() +
  geom_path() +
  labs(title = "Participant No.40") + 
  scale_y_continuous(name = "GLDA-inferred Discrete State", breaks = 1:3, labels = c("low", "med", "high")) +
  scale_x_datetime(name = "Time", minor_breaks = NULL, breaks = seq(as.POSIXct("2016-11-29"), as.POSIXct("2016-12-30"), 3600*24)) + 
  theme(axis.text = element_text(size = 12), 
        axis.title.x = element_text(size = 14), #, face = "bold"),
        axis.title.y = element_text(size = 14), 
        axis.text.x = element_text(angle = 90))

# A participant with quite even distribution between High Med and Low
pids.26 <- data.centered.post %>% select(pid, time, post.1:post.3, glda.cl) %>% filter(pid == pids[26]) %>% 
  mutate(time = as.POSIXct(strptime(time, format = "%m/%d/%Y %H:%M"))) # this reformats the original timestamps to POSIXct local time
ggplot(pids.26, aes(x = time, y = as.integer(glda.cl))) + theme_bw() +
  geom_point() +
  geom_path() +
  labs(title = "Participant No.26") + 
  scale_y_continuous(name = "GLDA-inferred Discrete State", breaks = 1:3, labels = c("low", "med", "high")) +
  scale_x_datetime(name = "Time", minor_breaks = NULL, breaks = c(seq(as.POSIXct("2015-10-06"), as.POSIXct("2015-11-01"), 3600*24), seq(as.POSIXct("2015-11-02"), as.POSIXct("2015-12-03"), 3600*24))) + 
  theme(axis.text = element_text(size = 12), 
        axis.title.x = element_text(size = 14), #, face = "bold"),
        axis.title.y = element_text(size = 14), 
        axis.text.x = element_text(angle = 90))

which.max(theta[,1])
which.max(theta[,2])
which.max(theta[,3])
theta[,]

# ************************************************************
# Grount truth data for the 45 participants in Fisher & Bosley
truth <- read.csv(paste0(file.path, "Peter Wu Shared Data/Between_Subject_Data.csv"))
head(truth)
head(truth %>% mutate(pid = toupper(as.character(id))) %>% select(pid, sex, dob, hrsd, hama, dassa, dassd, dasss) %>% filter(pid %in% pids))

# dignoses:
# mdd: major depressive disorder
# gad: general anxiety disorder
# sad: social anxiety disorder
# ptsd: post traumatic stress disorder
# hrsd : hamilton depression rating score

truth <- truth %>% filter(id %in% tolower(unique(data.centered$pid)))
truth$id == tolower(unique(data.centered$pid)) # confirming that the two lists of pids are identical

truth.descriptive <- truth %>% select(id, hrsd, hama, dassa, dassd, dasss) %>% mutate(id = toupper(id)) %>%
  inner_join(data, by = c("id" = "pid")) %>% group_by(id) %>% summarize_at(vars(hrsd:dasss), first) %>% as.data.frame()

ggplot(truth.descriptive) + theme_bw() +
  geom_density(aes(x = hrsd))  +
  geom_rug(aes(x = hrsd, y = 0), position = position_jitter(height = 0)) +
  geom_vline(xintercept = mean(truth.descriptive$hrsd), color = "red", size = 1.2) +
  geom_rect(xmin = mean(truth.descriptive$hrsd) - sd(truth.descriptive$hrsd), xmax = mean(truth.descriptive$hrsd) + sd(truth.descriptive$hrsd),
            ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.01) +
  scale_x_continuous(breaks = seq(0, 52, 5), limits = c(0,52)) +
  theme(axis.text=element_text(size=12), axis.title.x=element_text(size=14,face="bold"), axis.title.y=element_text(size=14))

ggplot(truth.descriptive) + theme_bw() +
  geom_density(aes(x = hama))  +
  geom_rug(aes(x = hama, y = 0), position = position_jitter(height = 0)) +
  geom_vline(xintercept = mean(truth.descriptive$hama), color = "red", size = 1.2) +
  geom_rect(xmin = mean(truth.descriptive$hama) - sd(truth.descriptive$hama), xmax = mean(truth.descriptive$hama) + sd(truth.descriptive$hama),
            ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.01) +
  scale_x_continuous(breaks = seq(0, 56, 5), limits = c(0,56)) +
  theme(axis.text=element_text(size=12), axis.title.x=element_text(size=14,face="bold"), axis.title.y=element_text(size=14))

ggplot(truth.descriptive) + theme_bw() +
  geom_density(aes(x = dasss))  +
  geom_rug(aes(x = dasss, y = 0), position = position_jitter(height = 0)) +
  geom_vline(xintercept = mean(truth.descriptive$dasss), color = "red", size = 1.2) +
  geom_rect(xmin = mean(truth.descriptive$dasss) - sd(truth.descriptive$dasss), xmax = mean(truth.descriptive$dasss) + sd(truth.descriptive$dasss),
            ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.01) +
  scale_x_continuous(breaks = seq(0, 42, 5), limits = c(0,42)) +
  theme(axis.text=element_text(size=12), axis.title.x=element_text(size=14,face="bold"), axis.title.y=element_text(size=14))



# Some diagnostics
list_of_draws <- extract(fit_rstan)
print(names(list_of_draws))

sampler_params <- get_sampler_params(fit_rstan, inc_warmup = FALSE)
sampler_params_chain1 <- sampler_params[[1]]
colnames(sampler_params_chain1)

mean_accept_stat_by_chain <- sapply(sampler_params, function(x) mean(x[, "accept_stat__"]))
print(mean_accept_stat_by_chain)

max_treedepth_by_chain <- sapply(sampler_params, function(x) max(x[, "treedepth__"]))
print(max_treedepth_by_chain)

rstan::traceplot(fit_rstan, pars = paste('mu[', 1:3, ',1]', sep = ''), inc_warmup = T) # check convergence, should converge after warmup ends
stan_ac(fit_rstan, pars = 'mu', lags = 20)  # check autocorrelations, should decay exponentially
library(coda)
coda::gelman.plot(As.mcmc.list(fit_rstan, pars = 'mu'), autoburnin = F)

# rstan::extract(fit_rstan)$mu[4000, ,] # check the clusters -- which is which
# training <- data.frame(low = rstan::extract(fit_rstan)$theta[4000,,2], med = rstan::extract(fit_rstan)$theta[4000,,1], high = rstan::extract(fit_rstan)$theta[4000,,3],
#                       truth %>% select(hrsd, hama, dassd, dassa, dasss, gadqtotal, pswqtotal, neon, neoe, neoo, neoa, neoc))

# directly extract results from the fit_rstan object
n_chains <- length(summary(fit_rstan)$c_summary[1, 1,])
mu.df.all <- data.frame(index = rep(1:10,3), variable = c(rep("high",10), rep("low", 10), rep("med",10)))

for (n in 1:n_chains) {
  fit_summary  <- summary(fit_rstan)$c_summary[, , n] # choose a chain to inspect results from
  mu <- rbind(fit_summary[which(grepl("mu\\[1", rownames(fit_summary))), "mean"],
              fit_summary[which(grepl("mu\\[2", rownames(fit_summary))), "mean"],
              fit_summary[which(grepl("mu\\[3", rownames(fit_summary))), "mean"])
  avg <- as.data.frame(mu) %>% apply(1, sum)
  mu.df <- as.data.frame(t(mu)) %>%
    `colnames<-`(factor(rank(avg), labels = c("low", "med", "high"))) %>%
    mutate(index = factor(1:10)) %>% melt() %>% arrange(as.character(variable))
  mu.df.all <- cbind(mu.df.all, mu.df$value)
  colnames(mu.df.all)[n+2] <- paste0("V", n)
}

mu.df.all <- mu.df.all %>% rowwise() %>%
  mutate(max = max(V1, V2, V3), min = min(V1, V2, V3), mean = sum(V1, V2, V3)/3) %>% as.data.frame()

mu.df.all

# plot(mu[which(avg == max(avg)), ], ylim= c(-1,1), type = "l")
# lines(mu[which(avg == min(avg)), ], ylim= c(-1,1), col = "red")
# lines(mu[which(avg > min(avg) & avg < max(avg)), ], ylim= c(-1,1), col = "blue")

ggplot(mu.df.all, aes(x = as.numeric(index), color = as.factor(variable))) + theme_bw() +
  geom_line(aes(y = mean), size = 1)  +
  geom_ribbon(aes(ymin = min, ymax = max, fill = as.factor(variable)), color = NA,  alpha = 0.2) +
  scale_x_continuous(breaks = 1:10, labels = names(data.centered)[3:12], name = "EMA item") +
  scale_y_continuous(breaks = seq(-1,1,0.1), name = "normalized value") +
  #scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_color_discrete(name = "cluster", limits = c("high", "low", "med")) +
  theme(panel.grid.minor.x = element_blank(), legend.position = "none",
        axis.text=element_text(size=12), axis.title.x=element_text(size=14,face="bold"), axis.title.y=element_text(size=14))

data.frame(truth %>% select(pid = id),
           weight.low = round(as.numeric(fit_summary[which(grepl(paste0("theta\\[\\d*,", which(factor(rank(avg), labels = c("low", "med", "high")) == "low"), "\\]"), rownames(fit_summary))), "mean"]), 3),
           weight.med = round(as.numeric(fit_summary[which(grepl(paste0("theta\\[\\d*,", which(factor(rank(avg), labels = c("low", "med", "high")) == "med"), "\\]"), rownames(fit_summary))), "mean"]), 3),
           weight.high = round(as.numeric(fit_summary[which(grepl(paste0("theta\\[\\d*,", which(factor(rank(avg), labels = c("low", "med", "high")) == "high"), "\\]"), rownames(fit_summary))), "mean"]), 3),
           truth %>% select(hrsd, hama, dasss)
          )

training <- data.frame(low = as.numeric(fit_summary[which(grepl(paste0("theta\\[\\d*,", which(factor(rank(avg), labels = c("low", "med", "high")) == "low"), "\\]"), rownames(fit_summary))), "mean"]),
                       med = as.numeric(fit_summary[which(grepl(paste0("theta\\[\\d*,", which(factor(rank(avg), labels = c("low", "med", "high")) == "med"), "\\]"), rownames(fit_summary))), "mean"]),
                       high = as.numeric(fit_summary[which(grepl(paste0("theta\\[\\d*,", which(factor(rank(avg), labels = c("low", "med", "high")) == "high"), "\\]"), rownames(fit_summary))), "mean"]),
                       truth %>% select(hrsd, hama, dassd, dassa, dasss, gadqtotal, pswqtotal, neon, neoe, neoo, neoa, neoc))
training


# Try making some triangle simplex plots (not going very well)
library(Ternary)
TernaryPlot(point = "right", atip = 'A', btip = 'B', ctip = 'C',
            alab = 'Well', blab = 'Baseline', clab = 'Unwell',
            lab.col = c("#E7B800", "#E7B800", "#FC4E07"),
            grid.minor.lines = 0)
data_points <- training %>% select(low, med, high)
AddToTernary(points, data_points[1,])

# Just plot scatter plot for now (high vs. low)
ggplot(training, aes(x = low, y = high)) + theme_bw() +
  geom_point() +
  geom_abline(intercept = 0.5, slope = -1, color = "red") +
  scale_x_continuous(limits = c(0,1), name = "Weight of the \"beter-than-baseline\" component") +
  scale_y_continuous(limits = c(0,1), name = "Weight of the \"worse-than-baseline\" component") +
  theme(axis.text=element_text(size=12), axis.title.x=element_text(size=14), axis.title.y=element_text(size=14))
training

# depression scores
summary(lm(hrsd ~ high, data = training %>% select(low, med, high, hrsd))) # the best model is the one using only the weight of the high cluster; med is also significant, low is not; hamilton rating scale for depression
summary(lm(dassd ~ high, data = training %>% select(low, med, high, dassd))) # depression anxiety stress scale -depression; med is NOT significant, neither is low; p = 0.011

# anxiety scores
summary(lm(hama ~ high, data = training %>% select(low, med, high, hama))) # best model is high; med is NOT significant, neither is low; hamilton anxiety scale; p = 0.014
summary(lm(dassa ~ high, data = training %>% select(low, med, high, dassa))) # depression anxiety stress scale -anxiety; med is NOT significant, neither is low; p=0.0587
summary(lm(gadqtotal ~ high, data = training %>% select(low, med, high, gadqtotal))) # general anxiety disorder; not significant

# stress
summary(lm(dasss ~ high, data = training %>% select(low, med, high, dasss))) # depression anxiety stress scale -stress;  med is NOT significant, neither is low; p = 0.007!!

# misc
summary(lm(pswqtotal ~ high, data = training %>% select(low, med, high, pswqtotal))) # penn state worry questionnaire; high is barely significant; p = 0.0496
summary(lm(neon ~ high, data = training %>% select(low, med, high, neon)))
summary(lm(neoe ~ high, data = training %>% select(low, med, high, neoe)))
summary(lm(neoo ~ high, data = training %>% select(low, med, high, neoo)))
summary(lm(neoa ~ high, data = training %>% select(low, med, high, neoa)))
summary(lm(neoc ~ high, data = training %>% select(low, med, high, neoc))) # none of these personality shit is significant

# Compare with vanilla Gaussian Mixture Models
library(mclust)
X <- data.centered %>% select(-pid, -time)
gmm <-Mclust(X, G = 3, modelName = c('EII', 'VII', 'EEI', 'VEI', 'EVI', 'VVI'))

mu <- gmm$parameters$mean
avg <- as.numeric(as.data.frame(mu) %>% apply(2, sum))
mu.df <- as.data.frame(mu) %>%
  `colnames<-`(factor(rank(avg), labels = c("low", "med", "high"))) %>%
  mutate(index = factor(1:10)) %>% melt()

mu.df

# plot(mu[which(avg == max(avg)), ], ylim= c(-1,1), type = "l")
# lines(mu[which(avg == min(avg)), ], ylim= c(-1,1), col = "red")
# lines(mu[which(avg > min(avg) & avg < max(avg)), ], ylim= c(-1,1), col = "blue")

ggplot(mu.df, aes(x = as.numeric(index), y = value, color = variable)) + theme_bw() + geom_line(size = 1) +
  scale_x_continuous(breaks = 1:10, labels = names(data.centered)[3:12], name = "EMA item") +
  scale_y_continuous(breaks = seq(-1,1,0.1), name = "normalized value") +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_color_discrete(name = "cluster", limits = c("high", "med", "low")) +
  theme(panel.grid.minor.x = element_blank(), legend.position = "none")

theta.gmm <- data.centered %>% mutate(cluster = gmm$classification) %>% group_by(pid) %>%
  summarize(low = length(which(cluster == 3))/n(), med = length(which(cluster == 1))/n(), high = length(which(cluster == 2))/n()) %>% as.data.frame()


training.gmm <- data.frame(data.centered %>% mutate(cluster = gmm$classification) %>% group_by(pid) %>%
                         summarize(low = length(which(cluster == 3))/n(), med = length(which(cluster == 1))/n(), high = length(which(cluster == 2))/n()) %>% as.data.frame(),
                       truth %>% select(hrsd, hama, dassd, dassa, dasss, gadqtotal, pswqtotal, neon, neoe, neoo, neoa, neoc))
training.gmm

# depression scores
summary(lm(hrsd ~ high, data = training.gmm %>% select(low, med, high, hrsd))) # far from significant
summary(lm(dassd ~ high, data = training.gmm %>% select(low, med, high, dassd))) # far from significant

# anxiety scores
summary(lm(hama ~ high, data = training.gmm %>% select(low, med, high, hama))) # far from significant
summary(lm(dassa ~ high, data = training.gmm %>% select(low, med, high, dassa))) # far from significant
summary(lm(gadqtotal ~ high, data = training.gmm %>% select(low, med, high, gadqtotal))) # far from significant

# stress
summary(lm(dasss ~ high, data = training.gmm %>% select(low, med, high, dasss))) # p =0.087 bordering getting significant

# Compare with Fisher and Bosley's two-step GMM manipulation to get inter-participant comparison

dfList <- lapply(pids, function(x) data.centered %>% filter(pid == x))
names(dfList) <- pids
View(dfList[[1]])

eachLCA <- lapply(dfList, function(x) Mclust(x %>% select(-pid, -time), G = 2:10 , modelName = c('EII', 'VII', 'EEI', 'VEI', 'EVI', 'VVI')))

for( i in 1:length(eachLCA)) {
  print(eachLCA[[i]]$G)
}

X.2step <- bind_rows(lapply(1:length(pids), function(i) data.frame(pid = names(eachLCA)[i], t(eachLCA[[i]]$parameters$mean))))
X.2step

gmm.2step <- Mclust(X.2step %>% select(-pid), G = 3, modelName = c('EII', 'VII', 'EEI', 'VEI', 'EVI', 'VVI'))
gmm.2step$G
gmm.2step$classification
gmm.2step$parameters$mean

training.gmm.2step <- data.frame(X.2step %>% mutate(cluster = gmm.2step$classification) %>% group_by(pid) %>%
                        summarize(low = length(which(cluster == 3))/n(), med = length(which(cluster == 2))/n(), high = length(which(cluster == 1))/n()) %>% as.data.frame(),
                           truth %>% select(hrsd, hama, dassd, dassa, dasss, gadqtotal, pswqtotal, neon, neoe, neoo, neoa, neoc))
training.gmm.2step

# depression scores
summary(lm(hrsd ~ high, data = training.gmm.2step %>% select(low, med, high, hrsd))) # far from significant
summary(lm(dassd ~ high, data = training.gmm.2step %>% select(low, med, high, dassd))) # far from significant

# anxiety scores
summary(lm(hama ~ high, data = training.gmm.2step %>% select(low, med, high, hama))) # far from significant
summary(lm(dassa ~ high, data = training.gmm.2step %>% select(low, med, high, dassa))) # far from significant
summary(lm(gadqtotal ~ high, data = training.gmm.2step %>% select(low, med, high, gadqtotal))) # far from significant

# stress
summary(lm(dasss ~ high, data = training.gmm.2step %>% select(low, med, high, dasss))) # p =0.002!

# *************************************************************
# Kaya's MoviSense and Word Count data from mothers and infants

data.path
data.path.kaya <- "/Users/cw29265-admin/Documents/Kaya_physio/"

wc <- read.csv(paste0(data.path.kaya, "LENA_HOURLY.csv")) 

# To extract: AWC_COUNT, TV_Secs, Noise
moms.wc <- wc %>% filter(Recording_Gender == "F", duration_percenthour > 0.5) %>% # there are only two observations with duration_percenthour <0.1 all else are >=0.72
  mutate(date.time = strptime(substr(as.character(EndTime), 1, 17), format = "%m/%d/%y %H:%M:%S")) %>%
  select(pid = ChildKey, date.time, AWC_COUNT, TV_Secs, Noise)
length(unique(moms.wc$pid)) # 25 moms
pids.wc <- as.character(unique(moms.wc$pid)) 


pids.movie <- substr(list.files(data.path.kaya)[grepl("P.*.1_", list.files(data.path.kaya))], 1,3)
moms.movie <- lapply(list.files(data.path.kaya, full.names = T)[grepl("P.*.1_", list.files(data.path.kaya))], read.csv)

which(sapply(moms.movie, function(x) "ActivityEnergyExpenditure..kcal.d." %in% names(x))) # this seems to be the smallest subset, tied with some other variables
which(sapply(moms.movie, function(x) "InclinationDown..deg." %in% names(x)))
which(sapply(moms.movie, function(x) "InclinationForward..deg." %in% names(x)))
which(sapply(moms.movie, function(x) "InclinationRight..deg." %in% names(x)))
which(sapply(moms.movie, function(x) "MET..." %in% names(x)))
which(sapply(moms.movie, function(x) "MovementAcceleration..g." %in% names(x))) # all have this variable
which(sapply(moms.movie, function(x) "StepCount..steps." %in% names(x))) # all have this variable
which(sapply(moms.movie, function(x) "TempMean..." %in% names(x))) # all have this variable
which(sapply(moms.movie, function(x) "TotalEnergyExpenditure..kcal.d." %in% names(x)))
which(sapply(moms.movie, function(x) "VerticalSpeed..m.s." %in% names(x))) # all have this variable

moms.movie.index.complete <- which(sapply(moms.movie, function(x) "ActivityEnergyExpenditure..kcal.d." %in% names(x)))
pids.movie.2 <- pids.movie[moms.movie.index.complete]

# Get data of selected variables from selected moms 
moms.movie.2 <- lapply(1:length(pids.movie.2), function(i){
  tryCatch({
    output <- moms.movie[[moms.movie.index.complete[i]]]  %>% 
      mutate(pid = pids.movie.2[i]) 
    if (grepl("/", output$Date.abs..yyyy.mm.dd[1])) {
      output <- output %>% 
        mutate(local.time = as.POSIXct(strptime(paste(Date.abs..yyyy.mm.dd., Time.abs..hh.mm.ss.), format = "c"))) # one participant's date format is different; addressing that
    } else{
      output <- output %>% 
        mutate(local.time = as.POSIXct(paste(Date.abs..yyyy.mm.dd., Time.abs..hh.mm.ss.)))
    }
    output <- output %>%
      select(pid,
             local.time,
             act.expd = "ActivityEnergyExpenditure..kcal.d.", 
             incl.down = "InclinationDown..deg.",
             incl.forw = "InclinationForward..deg.", # heavily right skewed
             incl.right = "InclinationRight..deg.",
             met = "MET...",
             accl = "MovementAcceleration..g.", # heavily right skewed
             stp.ct = "StepCount..steps.", # heavily right skewed
             temp.mean = "TempMean...",
             tot.expd = "TotalEnergyExpenditure..kcal.d.", # heavily right skewed
             vert.spd = "VerticalSpeed..m.s.")    
    return(output)
  }, error = function(e) {})
})

sapply(moms.movie.2, nrow) # check to make sure every pid has data

moms.movie.2[[1]]
moms.movie.df <- bind_rows(moms.movie.2) %>% filter_all(~!is.na(.)) %>% select(-act.expd, -met)
lapply(pids.movie.2, function(x){
  summary(moms.movie.df %>% filter(pid == x))
})

moms.movie.hourly <- moms.movie.df %>% mutate(rounded.time = round_date(local.time, "60 minutes")) %>% 
  group_by(pid, rounded.time) %>% summarize_all(~first(.)) %>% ungroup() %>%
  select(pid, time = rounded.time, incl.down, incl.forw, incl.right, accl, stp.ct, temp.mean, tot.expd, vert.spd) %>% as.data.frame() 

moms.movie.hourly.centered <- moms.movie.df %>% mutate(rounded.time = round_date(local.time, "60 minutes")) %>% 
  group_by(pid, rounded.time) %>% summarize_all(~first(.)) %>% ungroup() %>%
  group_by(pid) %>% mutate_at(vars(-pid, -rounded.time, -local.time), ~ scale(.)) %>%
  select(pid, time = rounded.time, incl.down, incl.forw, incl.right, accl, stp.ct, temp.mean, tot.expd, vert.spd) %>% as.data.frame()

# log transform doesn't quite fix the non-Gaussianity of accl and stp.ct (when log of stp.ct shows normalcy it's because it discarded 0s); did fix tot.expd
moms.movie.hourly.transformed <- moms.movie.df %>% mutate(rounded.time = round_date(local.time, "60 minutes")) %>% 
  group_by(pid, rounded.time) %>% summarize_all(~first(.)) %>% ungroup() %>% 
  mutate(log.accl = log(accl), log.stp.ct = log(stp.ct + 1), log.incl.forw = log(incl.forw), log.tot.expd = log(tot.expd)) %>%
  # group_by(pid) %>% mutate_at(vars(-pid, -rounded.time, -local.time), ~ scale(.)) %>% ungroup() %>%
  select(pid, time = rounded.time, incl.down, log.incl.forw, incl.right, temp.mean, log.tot.expd, vert.spd) %>% as.data.frame()

hist(moms.movie.hourly.transformed$incl.down)
hist(moms.movie.hourly.transformed$log.incl.forw)
hist(moms.movie.hourly.transformed$incl.right)
hist(moms.movie.hourly.transformed$temp.mean)
hist(moms.movie.hourly.transformed$log.tot.expd)
hist(moms.movie.hourly.transformed$vert.spd)

# Do GLDA (on transformed data; first tried on original data, results not right)
K = 3
V = 6 # incl.down, log.incl.forw, incl.right, temp.mean, log.tot.expd, vert.spd 
stan_data <- list(
  K = K,
  V = V,
  M = length(unique(moms.movie.hourly.transformed$pid)),
  N = nrow(moms.movie.hourly.transformed),
  w = moms.movie.hourly.transformed[,3:8], # can be a data.frame?
  pid = as.integer(factor(moms.movie.hourly.transformed$pid)),
  alpha = rep(1, K),
  nu = V + 3,
  psi = diag(V),
  mu0 = rep(0, V),
  kappa = 1
)

# Do GLDA (on centered data; first tried on original data, results not right)
K = 3
V = 8
stan_data <- list(
  K = K,
  V = V,
  M = length(unique(moms.movie.hourly.centered$pid)),
  N = nrow(moms.movie.hourly.centered),
  w = moms.movie.hourly.centered[,3:10], # can be a data.frame?
  pid = as.integer(factor(moms.movie.hourly.centered$pid)),
  alpha = rep(1, K),
  nu = V + 4,
  psi = diag(V),
  mu0 = rep(0, V),
  kappa = 1
)

fit_rstan_moms_movie <- stan(
  file = "ema.stan",
  data = stan_data,
  chains = 1,     # default is 4
  iter = 1000,    # default is 2000
  warmup = 200    # default is half of iter
)

saveRDS(fit_rstan_moms_movie, "fit_rstan_moms_movie_centered.rds") # converges 
# saveRDS(fit_rstan_moms_movie, "fit_rstan_moms_movie_transformed.rds") # does not converge
# saveRDS(fit_rstan_moms_movie, "fit_rstan_moms_movie.rds") # does not converge

fit_rstan <- fit_rstan_moms_movie

rstan::traceplot(fit_rstan, pars = paste('mu[', 1:3, ',2]', sep = ''), inc_warmup = T) # check convergence, should converge after warmup ends

# First run failed (3/28) because mu returned negative values where there should not be; also the theta is predominantly heavy on the normal (i.e. positive) mu
# Issues identified: some variables are too non-Gaussian (right skewed), and need to standardized by the particiant

theta <- apply(extract(fit_rstan)$theta[1:800, ,], c(2,3), mean) 
mu <- apply(extract(fit_rstan)$mu[1:800, ,], c(2,3), mean) 
sigma <- apply(extract(fit_rstan)$sigma[1:800, , ,], c(2,3,4), mean) # 3 by 10 by 10

theta

mu.for.plot <- as.data.frame(t(mu)) %>% `colnames<-`(c("K=1", "K=2", "K=3")) %>% 
  mutate(cluster = 1:8) %>% gather(cluster) %>% mutate(item = rep(1:8, 3))
 
ggplot(mu.for.plot, aes(x = item, y = value, color = cluster)) + theme_bw() +
  geom_line(size = 1) +
  scale_x_continuous(breaks = 1:8, labels = names(moms.movie.hourly.centered)[3:10], name = "mothers' kinesio/physiological measure") +
  scale_y_continuous(breaks = seq(-1,2.5,0.5), name = "normalized value") +
  # scale_color_discrete(name = "cluster", limits = c("high", "low", "med")) +
  theme(panel.grid.minor.x = element_blank(), 
        axis.text=element_text(size=12), axis.title.x=element_text(size=14,face="bold"), axis.title.y=element_text(size=14))

# Compare with vanilla Gaussian Mixture Models

library(mclust)
X.moms <- moms.movie.hourly.centered %>% select(-pid, -time)
gmm.moms <- Mclust(X.moms, G = 3, modelName = c('EII', 'VII', 'EEI', 'VEI', 'EVI', 'VVI'))
mu <- gmm.moms$parameters$mean
# avg <- as.numeric(as.data.frame(mu) %>% apply(2, sum))
mu.df <- as.data.frame(mu) %>%
  `colnames<-`(c("K1", "K2", "K3")) %>%
  mutate(index = factor(1:8)) %>% melt() %>% `colnames<-`(c("index","cluster","value"))

mu.df

ggplot(mu.df, aes(x = as.numeric(index), y = value, color = cluster)) + theme_bw() + geom_line(size = 1) +
  scale_x_continuous(breaks = 1:8, labels = names(moms.movie.hourly.centered)[3:10], name = "mothers' kinesio/physiological measure") +
  # scale_y_continuous(breaks = seq(-1,2,0.1), name = "normalized value") +
  scale_color_discrete(name = "GMM cluster") +
  theme(panel.grid.minor.x = element_blank())

theta.gmm <- moms.movie.hourly.centered %>% mutate(cluster = gmm.moms$classification) %>% group_by(pid) %>%
  summarize(K1 = length(which(cluster == 1))/n(), K2 = length(which(cluster == 2))/n(), K3 = length(which(cluster == 3))/n()) %>% as.data.frame()

theta.glda <- as.data.frame(theta) %>% mutate(K1 = V2, K2 = V3, K3 = V1) %>% select(K1, K2, K3)

cor(theta.glda$K1, theta.gmm$K1) # very high correlation
cor(theta.glda$K2, theta.gmm$K2) # very high correlation 
cor(theta.glda$K3, theta.gmm$K3) # very high correlation

# *******************************************
# Look at word count and movie sense together (maybe not for now)
pids.wc # 25 moms had word count data
pids.movie.2 # 25 moms had "relatively" complete movie sense data
pids.both <- intersect(pids.movie.2, pids.wc) # 10 moms had both word count and movie sense data

moms.wc %>% filter(pid %in% pids.both) %>% mutate(date.time = date.time + 1) %>% filter_all(~!is.na(.))
moms.movie.df %>% filter(pid %in% pids.both) %>% filter_all(~!is.na(.))




# To extract "ActivityEnergyExpenditure..kcal.d.", "InclinationDown..deg.", "InclinationForward..deg.", "InclinationRight..deg.", "MET...",           
# "MovementAcceleration..g.", "StepCount..steps.", "TempMean...", "TotalEnergyExpenditure..kcal.d.", "VerticalSpeed..m.s."  





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

file.path <- "/Users/cw29265-admin/Documents/LDA_EMA/"
data.path <- "/Users/cw29265-admin/Documents/LDA_EMA/Peter Wu Shared Data/Fisher and Bosley Data/Within Subject/"

pids <- list.files(data.path)

data <- bind_rows(lapply(list.files(data.path), function(p) {
    read.csv(paste0(data.path, p, "/dat.csv")) %>% 
      select(-X, -Survey.Creation.Date) %>% mutate(pid = p)  
  })) %>% select(-How.many.hours.did.you.sleep.last.night, -Experienced.difficulty.falling.or.staying.asleep, -Experienced.restless.or.unsatisfying.sleep, 
                 -X.1, -X.2, -X.3, -X.4, -X.5, -X.6, -Felt.tense, -Felt.judged.or.criticized) %>% filter(!is.na(Felt.energetic)) %>% 
    select(pid = pid, time = Survey.Completion.Date, irritable = Felt.irritable, angry = Felt.angry, afraid = Felt.frightened.or.afraid, worried = Felt.worried, ruminating = Dwelled.on.the.past, 
           down = Felt.down.or.depressed, hopeless = Felt.hopeless, anhedonic = Experienced.loss.of.interest.or.pleasure, avoidact = Avoided.activities, avoidpeople = Avoided.people) %>% 
    mutate(word = paste(irritable, angry, afraid, worried, ruminating, down, hopeless, anhedonic, avoidact, avoidpeople, sep = "n"))

length(unique(data$word))
nrow(data)
data

# time-center by the participant
data.centered <- data %>% group_by(pid) %>% mutate_at(vars(-pid, -time, -word), ~ round(scale(.),2)) %>% 
    mutate(word = paste(irritable, angry, afraid, worried, ruminating, down, hopeless, anhedonic, avoidact, avoidpeople, sep = "n")) %>% as.data.frame()

length(unique(data.centered$word))
nrow(data.centered)
data.centered

# write.csv(data, paste0(file.path, "data.csv"))
# write.csv(data.centered, paste0(file.path, "data_centered.csv"))

# After runnnig Gaussian LDA in Python
ema.assignments <- read.csv(paste0(file.path, "data_centered_assignments_4_new.csv")) %>% select(-1) %>% group_by(pid) %>% 
  summarize(S1 = length(which(assignment == 0)), S2 = length(which(assignment == 1)), 
  S3 = length(which(assignment == 2)), S4 = length(which(assignment == 3)), S5 = length(which(assignment == 4))) %>% as.data.frame() %>% 
  mutate(sum = S1 + S2 + S3 + S4 + S5) %>% mutate_at(vars(-pid, -sum), ~ ./sum) %>% select(-sum)

rownames(ema.assignments) <- ema.assignments$pid
clusters <- hclust(dist(ema.assignments[, 2:5]))
plot(clusters)

library(MASS)
truth <- read.csv(paste0(file.path, "Peter Wu Shared Data/Between_Subject_Data.csv")) 
d.ema <- dist(ema.assignments[,2:5])
d.dx <- dist(truth %>% dplyr::filter(id %in% tolower(ema.assignments$pid)) %>% dplyr::select(mdd, gad, sad, ptsd), method = "manhattan")

dim(as.matrix(d.ema))
dim(as.matrix(d.dx))

ema.dx.sim <- data.frame(ema.sim =  rep(NA, 990), dx.sim = rep(NA, 990))
r <- 0
for (i in 2:nrow(as.matrix(d.ema))) {
  for (j in 1:(i-1)) {
    r = r + 1
    ema.dx.sim$ema.sim[r] = as.matrix(d.ema)[i,j]
    ema.dx.sim$dx.sim[r] = as.matrix(d.dx)[i,j]
  }
}

summary(lm(dx.sim ~ ema.sim, data = ema.dx.sim))

mds <- cmdscale(d.ema, k = 2)



x <- mds[,1]
y <- mds[,2]
plot(x,y, main="MDS", color = "grey") #col=rainbow(5)[as.numeric(labels)]
text(x,y, labels=rownames(mds), cex=0.9, font=2)


mds <- mds %>% as.data.frame() %>% dplyr::mutate(id = tolower(as.character(rownames(mds)))) %>% 
  inner_join(truth %>% dplyr::select(id, sex, age, dx1, dx2, hrsd, hama) %>% dplyr::mutate(id = as.character(id)))

ggplot(mds, aes(x = V1, y = V2)) + theme_bw() +
  geom_point(aes(color = as.factor(sex)))




state.means.plot <- read.csv(paste0(file.path, "state_raw_means_5.csv")) %>% mutate(state = c("S1", "S2", "S3", "S4", "S5")) %>% 
  select(-X) %>% melt(factor_key = state)
state.means.plot 
state.means.plot <- as.data.frame(t(allLCA$parameters$mean))  %>% mutate(state = c("S1", "S2", "S3", "S4", "S5")) %>% melt(factor_key = state) 
ggplot(state.means.plot, aes(x = variable, y = value, color = state)) +
  geom_point() + geom_line(aes(group = state)) + theme_bw()
allLCA$parameters$mean


library(mclust)
X <- data.centered %>% select(-pid, -time, -word)

dfList <- lapply(pids, function(x) data.centered %>% filter(pid == x))
names(dfList) <- pids
View(dfList[[1]])

eachLCA <- lapply(dfList, function(x) Mclust(x %>% select(-pid, -time, -word), G = 4, modelName = c('EII', 'VII', 'EEI', 'VEI', 'EVI', 'VVI'))) 
allLCA <- Mclust(bind_rows(dfList) %>% select(-pid, -time, -word), G=  5, modelNames =  c('EII', 'VII', 'EEI', 'VEI', 'EVI', 'VVI'))
allLCA$G
allLCA$classification
allLCA$parameters$mean


allLCA$parameters$mean

data.centered %>% mutate(class = allLCA$classification) #%>% group_by(pid) %>% summarize(ncl= n_distinct(class)) %>% as.data.frame()
# data.centered %>% mutate(class = allLCA$classification) %>% filter(pid == names(eachLCA[1]))

data.centered %>% mutate(class = allLCA$classification) %>% group_by(class) %>% summarize_at(vars(irritable:avoidpeople), ~ var(.)) %>% 
  mutate(sumsq = select(., irritable:avoidpeople) %>% rowSums())

read.csv(paste0(file.path, "data_centered_assignments_5.csv")) %>% group_by(assignment) %>% summarize_at(vars(irritable:avoidpeople), ~ var(.)) %>% 
  mutate(sumsq = select(., irritable:avoidpeople) %>% rowSums())
                     
  #   mutate(classGLDA = read.csv(paste0(file.path, "data_centered_assignments_5.csv"))[, "assignment"] + 1)


  
lcaSummary=data.frame(id=character(length(pids)),g=double(length(pids)), stringsAsFactors = FALSE) 
for (i in 1:length(eachLCA)){ #for each data frame 
  lcaSummary$id[i] = as.character(names(eachLCA[i]))
  lcaSummary$g[i] = eachLCA[[i]]$G
  lcaSummary$n[i] = eachLCA[[i]]$n
  lcaSummary$uncertainty[i] = eachLCA[[i]]$uncertainty
  lcaSummary$bic[i] = eachLCA[[i]]$bic
}
eachLCA[[1]]$parameters$mean
eachLCA[[1]]$classification

#summary stats on yr classes (could add #mode here?)
median(lcaSummary$g) #4
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(lcaSummary$g) #3
sd(lcaSummary$g) #2.21
range(lcaSummary$g) # (1, 11)

#this creates the ID and class variables as vectors, according to how many within-classes each person had
idXg=character(0)
classes = character(0)
for (i in 1:nrow(lcaSummary)){
  idXg = append(idXg, rep(lcaSummary$id[i], lcaSummary$g[i]))
  classes = append(classes, seq(1, lcaSummary$g[i], 1))
}

#then add those vectors to the class output from the individual LCAs to create the data frame for btwn-subjects LCA
mc=data.frame(id=idXg, class = classes, irritable=double(sum(lcaSummary$g)),  angry=double(sum(lcaSummary$g)), 
              afraid=double(sum(lcaSummary$g)), worried = double(sum(lcaSummary$g)), ruminating = double(sum(lcaSummary$g)),
              down = double(sum(lcaSummary$g)), hopeless = double(sum(lcaSummary$g)), anhedonic = double(sum(lcaSummary$g)),
              avoidact =double(sum(lcaSummary$g)), avoidpeople = double(sum(lcaSummary$g)))

#populate this data frame by referencing the list of individual LCA outputs ("eachLCA")
for (i in 1:nrow(mc)){
  mc[i,3:12] = eachLCA[[paste(mc$id[i])]]$parameters$mean[,as.numeric(paste(mc$class[i]))]
}

#run between-subjects LCA
#  change G to whatever is appropriate for your data, make sure you also index same columns
mod = Mclust(mc[,c(3:12)], modelName = c('EII', 'VII', 'EEI', 'VEI', 'EVI', 'VVI'), G =1:15)
summary(mod)
mod$classification

length(mod$classification)
lookup <- mc %>% mutate(class2 = mod$classification) %>% select(id, class, class2) %>% mutate(class = as.numeric(as.character(class)))
lookup

twoStepGMM <- bind_rows(lapply(1:length(eachLCA), function(i) {
  data.frame(id = names(eachLCA[i]), eachLCA[[i]]$data, class = eachLCA[[i]]$classification, stringsAsFactors = F) %>% left_join(lookup, by = c("id","class"))  
}))

table(twoStepGMM$class2)





# discretize_ten_ema <- function(numbers) {
#   sapply(numbers, function(num) {
#     if (num < 10) {
#       num <- "one"
#     } else if (num < 20) {
#       num <- "teen"
#     } else if (num < 30) {
#       num <- "twenty"
#     } else if (num < 40) {
#       num <- "thirty"
#     } else if (num < 50) {
#       num <- "fourty"
#     } else if (num < 60) {
#       num <- "fifty"
#     } else if (num < 70) {
#       num <- "sixty"
#     } else if (num < 80) {
#       num <- "seventy"
#     } else if (num < 90) {
#       num <- "eighty"
#     } else {
#       num <- "ninety"
#     }
#     return(num)
#   })
# }
# 
# discretize_five_ema <- function(numbers) {
#   sapply(numbers, function(num) {
#     if (num < 20) {
#       num <- "none"
#     } else if (num < 40) {
#       num <- "little"
#     } else if (num < 60) {
#       num <- "moderate"
#     } else if (num < 80) {
#       num <- "much"
#     } else {
#       num <- "extreme"
#     }
#     return(num)
#   })
# }

# data <- data %>% mutate_at(.vars = vars(-pid, -time), .funs = discretize_five_ema) 
# for (col in 3:ncol(data)) {
#   data[,col] <- paste0(names(data)[col], data[, col])
# }


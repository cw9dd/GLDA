# *************************************************************
# Kaya's MoviSense and Word Count data from mothers and infants 
# (the code that tries GLDA on this data is in GLDA_EMA_STAN.R)

file.path <- "/Users/cw29265-admin/Documents/LDA_EMA/"
data.path <- "/Users/cw29265-admin/Documents/Kaya_physio/"

# Word count, to extract: AWC_COUNT, TV_Secs, Noise
wc <- read.csv(paste0(data.path, "LENA_HOURLY.csv")) 
moms.wc <- wc %>% filter(Recording_Gender == "F", duration_percenthour > 0.5) %>% # there are only two observations with duration_percenthour <0.1 all else are >=0.72
  mutate(date.time = strptime(substr(as.character(EndTime), 1, 17), format = "%m/%d/%y %H:%M:%S")) %>%
  select(pid = ChildKey, date.time, AWC_COUNT, TV_Secs, Noise)
length(unique(moms.wc$pid)) # 25 moms
pids.wc <- as.character(unique(moms.wc$pid)) 

# MoviSense
pids.movie <- substr(list.files(data.path)[grepl("P.*.1_", list.files(data.path))], 1,3)
# pids <- substr(list.files(data.path)[grepl("P.*.2_", list.files(data.path))], 1,3) # the same as checking the moms' pid

moms.movie <- lapply(list.files(data.path, full.names = T)[grepl("P.*.1_", list.files(data.path))], read.csv)
infs.movie <- lapply(list.files(data.path, full.names = T)[grepl("P.*.2_", list.files(data.path))], read.csv)

which(sapply(moms.movie, function(x) "Hr..1.min." %in% names(x))) # all have heart rate
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
moms.pid.2 <- pids.movie[moms.movie.index.complete]
moms.pid.2

which(sapply(infs.movie, function(x) "Hr..1.min." %in% names(x))) # all have heart rate
which(sapply(infs.movie, function(x) "ActivityEnergyExpenditure..kcal.d." %in% names(x))) # this seems to be the smallest subset, tied with some other variables
which(sapply(infs.movie, function(x) "InclinationDown..deg." %in% names(x)))
which(sapply(infs.movie, function(x) "InclinationForward..deg." %in% names(x)))
which(sapply(infs.movie, function(x) "InclinationRight..deg." %in% names(x)))
which(sapply(infs.movie, function(x) "MET..." %in% names(x)))
which(sapply(infs.movie, function(x) "MovementAcceleration..g." %in% names(x))) # all have this variable
which(sapply(infs.movie, function(x) "StepCount..steps." %in% names(x))) # all have this variable
which(sapply(infs.movie, function(x) "TempMean..." %in% names(x))) # all have this variable
which(sapply(infs.movie, function(x) "TotalEnergyExpenditure..kcal.d." %in% names(x)))
which(sapply(infs.movie, function(x) "VerticalSpeed..m.s." %in% names(x))) # all have this variable

infs.movie.index.complete <- which(sapply(infs.movie, function(x) "ActivityEnergyExpenditure..kcal.d." %in% names(x)))
infs.pid.2 <- pids.movie[infs.movie.index.complete]
infs.pid.2

pids.movie.2 <- intersect(moms.pid.2, infs.pid.2)
pids.movie.index.complete <- intersect(moms.movie.index.complete, infs.movie.index.complete)

# Get data of selected variables from selected moms and infants 
moms.movie.2 <- lapply(1:length(pids.movie.2), function(i){
  tryCatch({
    output <- moms.movie[[pids.movie.index.complete[i]]]  %>% 
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
             mom.hr = "Hr..1.min.",
             mom.act.expd = "ActivityEnergyExpenditure..kcal.d.", 
             mom.incl.down = "InclinationDown..deg.",
             mom.incl.forw = "InclinationForward..deg.", # heavily right skewed
             mom.incl.right = "InclinationRight..deg.",
             mom.met = "MET...",
             mom.accl = "MovementAcceleration..g.", # heavily right skewed
             mom.stp.ct = "StepCount..steps.", # heavily right skewed
             mom.temp.mean = "TempMean...",
             mom.tot.expd = "TotalEnergyExpenditure..kcal.d.", # heavily right skewed
             mom.vert.spd = "VerticalSpeed..m.s.")    
    return(output)
  }, error = function(e) {})
})

sapply(moms.movie.2, nrow) # check to make sure every pid has data

moms.movie.2[[1]]
moms.movie.df <- bind_rows(moms.movie.2) %>% filter_all(~!is.na(.)) %>% select(-mom.act.expd, -mom.met)
lapply(pids.movie.2, function(x){
  summary(moms.movie.df %>% filter(pid == x))
})

infs.movie.2 <- lapply(1:length(pids.movie.2), function(i){
  tryCatch({
    output <- infs.movie[[pids.movie.index.complete[i]]]  %>% 
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
             inf.hr = "Hr..1.min.",
             inf.act.expd = "ActivityEnergyExpenditure..kcal.d.", 
             inf.incl.down = "InclinationDown..deg.",
             inf.incl.forw = "InclinationForward..deg.", # heavily right skewed
             inf.incl.right = "InclinationRight..deg.",
             inf.met = "MET...",
             inf.accl = "MovementAcceleration..g.", # heavily right skewed
             inf.stp.ct = "StepCount..steps.", # heavily right skewed
             inf.temp.mean = "TempMean...",
             inf.tot.expd = "TotalEnergyExpenditure..kcal.d.", # heavily right skewed
             inf.vert.spd = "VerticalSpeed..m.s.")    
    return(output)
  }, error = function(e) {})
})

sapply(infs.movie.2, nrow) # check to make sure every pid has data
sapply(moms.movie.2, nrow)

infs.movie.2[[1]]
moms.movie.2[[1]]
infs.movie.df <- bind_rows(infs.movie.2) %>% filter_all(~!is.na(.)) %>% select(-inf.act.expd, -inf.met)

pair.movie.df <- moms.movie.df %>% inner_join(infs.movie.df, by = c("pid" = "pid", "local.time" = "local.time"))
pair.movie.df
# ---------------------------- stopped here 4/14/2022

# started here 4/15/2022
moms.movie.hourly <- pair.movie.df %>% select(pid, local.time, mom.hr, mom.incl.down, mom.incl.forw, mom.incl.right, mom.accl, mom.stp.ct, mom.temp.mean, mom.tot.expd, mom.vert.spd) %>%
  mutate(rounded.time = round_date(local.time, "60 minutes")) %>% 
  group_by(pid, rounded.time) %>% summarize_all(~first(.)) %>% ungroup() %>%
  select(pid, time = local.time, mom.hr, mom.incl.down, mom.incl.forw, mom.incl.right, mom.accl, mom.stp.ct, mom.temp.mean, mom.tot.expd, mom.vert.spd) %>% as.data.frame() 

moms.movie.hourly.centered <- pair.movie.df %>% select(pid, local.time, mom.hr, mom.incl.down, mom.incl.forw, mom.incl.right, mom.accl, mom.stp.ct, mom.temp.mean, mom.tot.expd, mom.vert.spd) %>%
  mutate(rounded.time = round_date(local.time, "60 minutes")) %>% 
  group_by(pid, rounded.time) %>% summarize_all(~first(.)) %>% ungroup() %>%
  group_by(pid) %>% mutate_at(vars(-pid, -rounded.time, -local.time), ~ scale(.)) %>%
  select(pid, time = local.time, mom.hr, mom.incl.down, mom.incl.forw, mom.incl.right, mom.accl, mom.temp.mean, mom.tot.expd, mom.vert.spd) %>% as.data.frame() # removed step count to keep with infs data frame

# log transform doesn't quite fix the non-Gaussianity of accl and stp.ct (when log of stp.ct shows normalcy it's because it discarded 0s); did fix tot.expd
# moms.movie.hourly.transformed <- moms.movie.df %>% mutate(rounded.time = round_date(local.time, "60 minutes")) %>% 
#   group_by(pid, rounded.time) %>% summarize_all(~first(.)) %>% ungroup() %>% 
#   mutate(log.accl = log(accl), log.stp.ct = log(stp.ct + 1), log.incl.forw = log(incl.forw), log.tot.expd = log(tot.expd)) %>%
#   # group_by(pid) %>% mutate_at(vars(-pid, -rounded.time, -local.time), ~ scale(.)) %>% ungroup() %>%
#   select(pid, time = rounded.time, incl.down, log.incl.forw, incl.right, temp.mean, log.tot.expd, vert.spd) %>% as.data.frame()

infs.movie.hourly <- pair.movie.df %>% select(pid, local.time, inf.hr, inf.incl.down, inf.incl.forw, inf.incl.right, inf.accl, inf.stp.ct, inf.temp.mean, inf.tot.expd, inf.vert.spd) %>%
  mutate(rounded.time = round_date(local.time, "60 minutes")) %>% 
  group_by(pid, rounded.time) %>% summarize_all(~first(.)) %>% ungroup() %>%
  select(pid, time = local.time, inf.hr, inf.incl.down, inf.incl.forw, inf.incl.right, inf.accl, inf.stp.ct, inf.temp.mean, inf.tot.expd, inf.vert.spd) %>% as.data.frame() 

infs.movie.hourly.centered <- pair.movie.df %>% select(pid, local.time, inf.hr, inf.incl.down, inf.incl.forw, inf.incl.right, inf.accl, inf.stp.ct, inf.temp.mean, inf.tot.expd, inf.vert.spd) %>%
  mutate(rounded.time = round_date(local.time, "60 minutes")) %>% 
  group_by(pid, rounded.time) %>% summarize_all(~first(.)) %>% ungroup() %>%
  group_by(pid) %>% mutate_at(vars(-pid, -rounded.time, -local.time), ~ scale(.)) %>%
  select(pid, time = local.time, inf.hr, inf.incl.down, inf.incl.forw, inf.incl.right, inf.accl, inf.temp.mean, inf.tot.expd, inf.vert.spd) %>% as.data.frame() # removed step count because P27 baby's every moment had 0 step count

pair.movie.hourly <- pair.movie.df %>%
  mutate(rounded.time = round_date(local.time, "60 minutes")) %>% 
  group_by(pid, rounded.time) %>% summarize_all(~first(.)) %>% ungroup() %>%
  select(-rounded.time) %>% as.data.frame() 

pair.movie.hourly.centered <- pair.movie.df %>% 
  mutate(rounded.time = round_date(local.time, "60 minutes")) %>% 
  group_by(pid, rounded.time) %>% summarize_all(~first(.)) %>% ungroup() %>%
  group_by(pid) %>% mutate_at(vars(-pid, -rounded.time, -local.time), v) %>%
  select(-rounded.time, -mom.stp.ct, -inf.stp.ct) %>% as.data.frame() # removed step count because P27 baby's every moment had 0 step count

library(mclust)
# GMM for mothers physio
X.moms <- moms.movie.hourly.centered %>% select(-pid, -time)
gmm.moms <- Mclust(X.moms, G= 3, modelName = c('EII', 'VII', 'EEI', 'VEI', 'EVI', 'VVI'))
mu.moms <- gmm.moms$parameters$mean

mu.df <- as.data.frame(mu.moms) %>%
  `colnames<-`(c("K1", "K2", "K3")) %>%
  mutate(index = factor(1:8)) %>% melt() %>% `colnames<-`(c("index","cluster","value"))

ggplot(mu.df, aes(x = as.numeric(index), y = value, color = cluster)) + theme_bw() + geom_line(size = 1) +
  scale_x_continuous(breaks = 1:8, labels = names(moms.movie.hourly.centered)[3:10], name = "mothers' kinesio/physiological measure") +
  # scale_y_continuous(breaks = seq(-1,2,0.1), name = "normalized value") +
  scale_color_discrete(name = "GMM cluster") +
  theme(panel.grid.minor.x = element_blank())

# GMM for infants physio
X.infs <- infs.movie.hourly.centered %>% select(-pid, -time)
gmm.infs <- Mclust(X.infs, G= 3, modelName = c('EII', 'VII', 'EEI', 'VEI', 'EVI', 'VVI'))
mu.infs <- gmm.infs$parameters$mean

mu.df <- as.data.frame(mu.infs) %>%
  `colnames<-`(c("K1", "K2", "K3")) %>%
  mutate(index = factor(1:8)) %>% melt() %>% `colnames<-`(c("index","cluster","value"))

ggplot(mu.df, aes(x = as.numeric(index), y = value, color = cluster)) + theme_bw() + geom_line(size = 1) +
  scale_x_continuous(breaks = 1:8, labels = names(infs.movie.hourly.centered)[3:10], name = "infants' kinesio/physiological measure") +
  # scale_y_continuous(breaks = seq(-1,2,0.1), name = "normalized value") +
  scale_color_discrete(name = "GMM cluster") +
  theme(panel.grid.minor.x = element_blank())

# GMM for pairs physio
X.pairs <- pair.movie.hourly %>% filter(pid != "P31") %>% select(-pid, -local.time) %>% mutate_all( ~ scale(.))
gmm.pairs <- Mclust(X.pairs, G= 3, modelName = c('EII', 'VII', 'EEI', 'VEI', 'EVI', 'VVI'))
mu.pairs <- gmm.pairs$parameters$mean

mu.df <- as.data.frame(mu.pairs) %>%
  `colnames<-`(c("K1", "K2", "K3")) %>%
  mutate(index = factor(1:18)) %>% melt() %>% `colnames<-`(c("index","cluster","value"))

ggplot(mu.df, aes(x = as.numeric(index), y = value, color = cluster)) + theme_bw() + geom_line(size = 1) +
  scale_x_continuous(breaks = 1:18, labels = names(pair.movie.hourly)[3:20], name = "mother-infant kinesio/physiological measure") +
  # scale_y_continuous(breaks = seq(-1,2,0.1), name = "normalized value") +
  scale_color_discrete(name = "GMM cluster") +
  theme(panel.grid.minor.x = element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust= 1, size =  10), 
        axis.title.x = element_blank())

# ******************************************* stopped here 4/15

# started here 4/18: map membership to moments

# Build list of data frames that contain activity class and body position for both moms and infs
moms.movie.3 <- lapply(1:length(pids.movie.2), function(i){
  tryCatch({
    output <- moms.movie[[pids.movie.index.complete[i]]]  %>% 
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
             mom.act.class = "ActivityClass...",
             mom.bod.pos = "BodyPosition...")    
    return(output)
  }, error = function(e) {})
})

infs.movie.3 <- lapply(1:length(pids.movie.2), function(i){
  tryCatch({
    output <- infs.movie[[pids.movie.index.complete[i]]]  %>% 
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
             inf.act.class = "ActivityClass...",
             inf.bod.pos = "BodyPosition...")    
    return(output)
  }, error = function(e) {})
})

moms.movie.3.df <- bind_rows(moms.movie.3) %>% filter_all(~!is.na(.))
infs.movie.3.df <- bind_rows(infs.movie.3) %>% filter_all(~!is.na(.))
pair.movie.3.df <- moms.movie.3.df %>% inner_join(infs.movie.3.df, by = c("pid" = "pid", "local.time" = "local.time"))
pair.movie.3.df # data frame with momentary activity class and body position of boths moms and infs

# pair.movie.hourly.3 <- pair.movie.hourly %>% mutate(k = gmm.pairs$classification) %>% inner_join(pair.movie.3.df, by = c("pid" = "pid", "local.time" = "local.time"))
pair.movie.hourly.3 <- pair.movie.hourly %>% filter(pid != "P31") %>% mutate(k = gmm.pairs$classification) %>% inner_join(pair.movie.3.df, by = c("pid" = "pid", "local.time" = "local.time"))

pair.movie.hourly.3$local.time

table(pair.movie.hourly.3$mom.act.class[which(pair.movie.hourly.3$k == 1)])
table(pair.movie.hourly.3$mom.act.class[which(pair.movie.hourly.3$k == 2)])
table(pair.movie.hourly.3$mom.act.class[which(pair.movie.hourly.3$k == 3)])

table(pair.movie.hourly.3$mom.bod.pos[which(pair.movie.hourly.3$k == 1)])
table(pair.movie.hourly.3$mom.bod.pos[which(pair.movie.hourly.3$k == 2)])
table(pair.movie.hourly.3$mom.bod.pos[which(pair.movie.hourly.3$k == 3)])

table(pair.movie.hourly.3$inf.act.class[which(pair.movie.hourly.3$k == 1)])
table(pair.movie.hourly.3$inf.act.class[which(pair.movie.hourly.3$k == 2)])
table(pair.movie.hourly.3$inf.act.class[which(pair.movie.hourly.3$k == 3)])

table(pair.movie.hourly.3$inf.bod.pos[which(pair.movie.hourly.3$k == 1)])
table(pair.movie.hourly.3$inf.bod.pos[which(pair.movie.hourly.3$k == 2)])
table(pair.movie.hourly.3$inf.bod.pos[which(pair.movie.hourly.3$k == 3)])

table(paste0(pair.movie.hourly.3$mom.act.class, pair.movie.hourly.3$inf.act.class)[which(pair.movie.hourly.3$k == 1)])
table(paste0(pair.movie.hourly.3$mom.act.class, pair.movie.hourly.3$inf.act.class)[which(pair.movie.hourly.3$k == 2)])
table(paste0(pair.movie.hourly.3$mom.act.class, pair.movie.hourly.3$inf.act.class)[which(pair.movie.hourly.3$k == 3)])

k.by.hour <- data.frame(table(substr(pair.movie.hourly.3$local.time, 12, 13)[which(pair.movie.hourly.3$k == 2)])) %>% 
  full_join(data.frame(table(substr(pair.movie.hourly.3$local.time, 12, 13)[which(pair.movie.hourly.3$k == 3)])), by = "Var1") %>% 
  full_join(data.frame(table(substr(pair.movie.hourly.3$local.time, 12, 13)[which(pair.movie.hourly.3$k == 1)])), by = "Var1") %>%
  full_join(data.frame(table(substr(pair.movie.hourly.3$local.time, 12, 13))), by = "Var1") %>% 
  mutate(hour = 1:24, Freq = ifelse(is.na(Freq.x.x), 0, Freq.x.x), Freq.x = ifelse(is.na(Freq.x), 0, Freq.x)) %>%
  mutate(K1 = Freq/sum(Freq), K2 = Freq.x/sum(Freq.x), K3 = Freq.y/sum(Freq.y), Prior = Freq.y.y/sum(Freq.y.y)) %>%
  select(hour, K1, K2, K3, Prior) %>%
  melt(id.vars = "hour") 

k.by.hour

ggplot(k.by.hour, aes(x = hour, y = value, color = factor(variable))) + theme_bw() +
  geom_line(size = 1) + 
  scale_x_continuous(breaks = 1:24, name = "hour of day") +
  scale_y_continuous(breaks = seq(0,1,0.02), limits =c(0,0.18), name = "proportion") +
  scale_color_discrete(name = "Class") +
  theme(panel.grid.minor.x = element_blank(), axis.text.x = element_text(size =  10))
  
View(pair.movie.hourly.3 %>% filter(pid == "P31"))
View(pair.movie.hourly.3 %>% filter(k == 3))

# *******************************************
# Look at word count and movie sense together (maybe not for now)
pids.wc # 25 moms had word count data
pids.movie.2 # 25 moms had "relatively" complete movie sense data
pids.both <- intersect(pids.movie.2, pids.wc) # 10 moms had both word count and movie sense data

moms.wc %>% filter(pid %in% pids.both) %>% mutate(date.time = date.time + 1) %>% filter_all(~!is.na(.))
moms.movie.df %>% filter(pid %in% pids.both) %>% filter_all(~!is.na(.))


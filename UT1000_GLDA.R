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
data.path <- "/Users/cw29265-admin/Documents/UT1000_fall2018/"
data.path <- "/Users/cw29265-admin/Documents/UT1000_spring2019/"

pids <- list.files(data.path)
pids.android <- pids[sapply(list.files(data.path, full.names = T), function(dir) {"bluetooth" %in% list.files(dir)})]

file.path <- "/Users/cw29265-admin/Documents/UT1000/"
source(paste(file.path, "UT1000_getGroundTruth.R", sep= ""))
source(paste(file.path, "UT1000_gps.R", sep = "")) # one line needs tweaking when switching between UTX000 and UT1000
source(paste(file.path, "UT1000_accelerometer.R", sep = ""))
source(paste(file.path, "UT1000_screentime.R", sep =""))
source(paste(file.path, "UT1000_bt.R", sep = ""))
file.path <- "/Users/cw29265-admin/Documents/LDA_EMA/"

ut1000.rt.ema <- lapply(1:10, function(x)  tryCatch({
  get_rt_complete(pids[x]) %>% filter(!is.na(date), !is.na(loc))}, error = function(e){}))


pid.ema <- get_rt_complete(pids[2]) %>% filter(!is.na(date), !is.na(loc))

unique(bind_rows(ut1000.rt.ema)$loc)
unique(pid.ema$loc)


# ut2000.rt.5items <- lapply(1:length(pids), function(x)  tryCatch({
#   get_rt_complete(pids[x]) %>% filter(!is.na(date)) %>% select(date:energy)}, error = function(e){}))
# 
# utx000.rt.5items <- lapply(1:length(pids), function(x)  tryCatch({
#   get_rt_complete(pids[x]) %>% filter(!is.na(date)) %>% select(date:energy)}, error = function(e){}))

# saveRDS(ut1000.rt.5items, paste0(file.path, "ut1000.rt.5items.rds"))
# ut1000.rt.5items <- readRDS(paste0(file.path, "ut1000.rt.5items.rds"))

data <- bind_rows(ut1000.rt.5items) %>% mutate(word = paste0(sad, "sad", stress, "stress", content, "content", lonely, "lonely", energy, "energy"))

data.centered <- bind_rows(ut1000.rt.5items) %>% group_by(pid) %>% mutate_at(vars(sad:energy), ~ round(scale(.))) %>% as.data.frame() %>% 
  mutate(word = paste0(sad, "sad", stress, "stress", content, "content", lonely, "lonely", energy, "energy"))

write.csv(data, paste0(file.path, "ut1000_glda_data.csv"))
write.csv(data.centered, paste0(file.path, "ut1000_glda_data_centered.csv"))



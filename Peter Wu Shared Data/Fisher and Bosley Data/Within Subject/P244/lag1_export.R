
colnames(datyL0)
colnames(datyL1)

library(psych)
describe(datyL1)
colnames(datyL1)[c(3:25)] <- c("energeticL" , "enthusiasticL" , "contentL" , "irritableL" , "restlessL" , "worriedL" , "guiltyL" , "afraidL" ,
                                 "anhedoniaL" , "angryL" , "hopelessL" , "downL" , "positiveL" , "fatigueL" , "tensionL" , "concentrateL" , "acceptedL" ,
                                 "threatenedL" , "ruminateL" , "reassureL" , "procrastL" , "avoid_peopleL", "avoid_actL")
id=as.vector(rep(244, nrow(datyL0)))

merged = cbind(id,datyL1[c(3:21,25,22,23,24)],datyL0)
#View(merged)

write.csv(merged,'id244lag1.csv',row.names = FALSE)


colnames(datyL0)
colnames(datyL1)

library(psych)
describe(datyL1)
colnames(datyL1)[c(3:23,27)] <- c("energeticL" , "enthusiasticL" , "contentL" , "irritableL" , "restlessL" , "worriedL" , "guiltyL" , "afraidL" ,
                                 "anhedoniaL" , "angryL" , "hopelessL" , "downL" , "positiveL" , "fatigueL" , "tensionL" , "concentrateL" , "acceptedL" ,
                                 "threatenedL" , "ruminateL" , "avoid_actL" , "procrastL" , "avoid_peopleL")
id=as.vector(rep(206, nrow(datyL0)))
reassureL=as.vector(rep(NA, nrow(datyL0)))
reassure=as.vector(rep(NA, nrow(datyL0)))

merged = cbind(id,datyL1[c(3:22)],reassureL,datyL1[c(23,27)],datyL0[1:22],reassure,datyL0[23:61])
View(merged)

write.csv(merged,'id206lag1.csv',row.names = FALSE)


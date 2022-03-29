
colnames(datyL0)
colnames(datyL1)

library(psych)
describe(datyL1)
colnames(datyL1)[c(3:24,28)] <- c("energeticL" , "enthusiasticL" , "contentL" , "irritableL" , "restlessL" , "worriedL" , "guiltyL" , "afraidL" ,
                                 "anhedoniaL" , "angryL" , "hopelessL" , "downL" , "positiveL" , "fatigueL" , "tensionL" , "concentrateL" , "acceptedL" ,
                                 "threatenedL" , "ruminateL" , "avoid_actL" , "reassureL" , "procrastL" , "avoid_peopleL")
id=as.vector(rep(037, nrow(datyL0)))

merged = cbind(id,datyL1[c(3:24,28)],datyL0)
#View(merged)

write.csv(merged,'id037lag1.csv',row.names = FALSE)

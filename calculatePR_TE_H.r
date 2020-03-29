#required functions
source("functions/tarEnt.r")
source("functions/searchInf.r")

#pagerank
accPS_PR<-lapply(accPS,page.rank)
accCD_PR<-lapply(accCD,page.rank)
accICS_PR<-lapply(accICS,page.rank)
singlePS_PR<-lapply(weeksPS,page.rank)
singleCD_PR<-lapply(weeksCD,page.rank)
singleICS_PR<-lapply(weeksICS,page.rank)

save.image(file = "PRTEH.RData")
#target entropy
accPS_TE<-lapply(accPS,TargetEntropy)
accCD_TE<-lapply(accCD,TargetEntropy)
accICS_TE<-lapply(accICS,TargetEntropy)
singlePS_TE<-lapply(weeksPS,TargetEntropy)
singleCD_TE<-lapply(weeksCD,TargetEntropy)
singleICS_TE<-lapply(weeksICS,TargetEntropy)
save.image(file = "PRTEH.RData")

#hide
accPS_S<-lapply(accPS,sInfMatrix)
accCD_S<-lapply(accCD,sInfMatrix)
accICS_S<-lapply(accICS,sInfMatrix)
singlePS_S<-lapply(weeksPS,sInfMatrix)
singleCD_S<-lapply(weeksCD,sInfMatrix)
singleICS_S<-lapply(weeksICS,sInfMatrix)

accPS_H<-lapply(accPS_S,colSums,na.rm=T)
accCD_H<-lapply(accCD_S,colSums,na.rm=T)
accICS_H<-lapply(accICS_S,colSums,na.rm=T)
singlePS_H<-lapply(singlePS_S,colSums,na.rm=T)
singleCD_H<-lapply(singleCD_S,colSums,na.rm=T)
singleICS_H<-lapply(singleICS_S,colSums,na.rm=T)
save.image(file = "PRTEH.RData")



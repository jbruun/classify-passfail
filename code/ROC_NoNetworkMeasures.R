## Models without networks measures
###logistic regression
ROC_GCF_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("gender", "cohort", "fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GCF_log[[i]]<-ROC
}

ROC_GC_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("gender",  "cohort"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GC_log[[i]]<-ROC
}

ROC_GF_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("gender",  "fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GF_log[[i]]<-ROC
}

ROC_CF_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("cohort",  "fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GF_log[[i]]<-ROC
}

ROC_C_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("cohort"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_C_log[[i]]<-ROC
}
ROC_F_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_F_log[[i]]<-ROC
}
ROC_G_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("gender"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_G_log[[i]]<-ROC
}
save(ROC_PS_log,ROC_CD_log,ROC_ICS_log, ROC_PS_justpass_log,ROC_CD_justpass_log,ROC_ICS_justpass_log,file="data/ROC_NN_logreg.Rdata")
## Models without networks measures

rm(list = ls())

library(igraph)
library(dplyr)



# Import pass/fail centrality data
#loadvars <- load("data/centPassFail.Rdata")
load("data/centrality_data_frames.Rdata")
load("data/ROC_logreg.Rdata")
source("code/jackknife_functions.R")

## Run jackknife logistic regression 

# Turn long data frame into list of weekly frames
centPS <- dfPS %>% group_split(Week)
centCD <- dfCD %>% group_split(Week)
centICS <- dfICS %>% group_split(Week)

###logistic regression passed
ROC_GCF_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("gender", "cohort", "fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GCF_log[[i]]<-ROC
}
ROC_GCF_TPR_log<-vector()
ROC_GCF_FPR_log<-vector()
ROC_GCF_PPV_log<-vector()
ROC_GCF_SR_log<-vector()
for (i in 1:100){
  ROC_GCF_TPR_log[i]<-ROC_GCF_log[[i]]$TPR[1]
  ROC_GCF_FPR_log[i]<-ROC_GCF_log[[i]]$FPR[1]
  ROC_GCF_PPV_log[i]<-ROC_GCF_log[[i]]$PPV[1]
  ROC_GCF_SR_log[i]<-ROC_GCF_log[[i]]$SR[1]
}

GCF_AUC_log<-simple_auc(ROC_GCF_TPR_log,ROC_GCF_FPR_log)


ROC_GC_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("gender",  "cohort"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GC_log[[i]]<-ROC
}

ROC_GC_TPR_log<-vector()
ROC_GC_FPR_log<-vector()
ROC_GC_PPV_log<-vector()
ROC_GC_SR_log<-vector()
for (i in 1:100){
  ROC_GC_TPR_log[i]<-ROC_GC_log[[i]]$TPR[1]
  ROC_GC_FPR_log[i]<-ROC_GC_log[[i]]$FPR[1]
  ROC_GC_PPV_log[i]<-ROC_GC_log[[i]]$PPV[1]
  ROC_GC_SR_log[i]<-ROC_GC_log[[i]]$SR[1]
}

GC_AUC_log<-simple_auc(ROC_GC_TPR_log,ROC_GC_FPR_log)

ROC_GF_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("gender",  "fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GF_log[[i]]<-ROC
}
ROC_GF_TPR_log<-vector()
ROC_GF_FPR_log<-vector()
ROC_GF_PPV_log<-vector()
ROC_GF_SR_log<-vector()
for (i in 1:100){
  ROC_GF_TPR_log[i]<-ROC_GF_log[[i]]$TPR[1]
  ROC_GF_FPR_log[i]<-ROC_GF_log[[i]]$FPR[1]
  ROC_GF_PPV_log[i]<-ROC_GF_log[[i]]$PPV[1]
  ROC_GF_SR_log[i]<-ROC_GF_log[[i]]$SR[1]
}

GF_AUC_log<-simple_auc(ROC_GF_TPR_log,ROC_GF_FPR_log)

ROC_CF_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("cohort",  "fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GF_log[[i]]<-ROC
}
ROC_CF_TPR_log<-vector()
ROC_CF_FPR_log<-vector()
ROC_CF_PPV_log<-vector()
ROC_CF_SR_log<-vector()
for (i in 1:100){
  ROC_CF_TPR_log[i]<-ROC_CF_log[[i]]$TPR[1]
  ROC_CF_FPR_log[i]<-ROC_CF_log[[i]]$FPR[1]
  ROC_CF_PPV_log[i]<-ROC_CF_log[[i]]$PPV[1]
  ROC_CF_SR_log[i]<-ROC_CF_log[[i]]$SR[1]
}

CF_AUC_log<-simple_auc(ROC_CF_TPR_log,ROC_CF_FPR_log)

ROC_G_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("gender"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_G_log[[i]]<-ROC
}

ROC_G_TPR_log<-vector()
ROC_G_FPR_log<-vector()
ROC_G_PPV_log<-vector()
ROC_G_SR_log<-vector()
for (i in 1:100){
  ROC_G_TPR_log[i]<-ROC_G_log[[i]]$TPR[1]
  ROC_G_FPR_log[i]<-ROC_G_log[[i]]$FPR[1]
  ROC_G_PPV_log[i]<-ROC_G_log[[i]]$PPV[1]
  ROC_G_SR_log[i]<-ROC_G_log[[i]]$SR[1]
}

G_AUC_log<-simple_auc(ROC_G_TPR_log,ROC_G_FPR_log)


ROC_C_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("cohort"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_C_log[[i]]<-ROC
}
ROC_C_TPR_log<-vector()
ROC_C_FPR_log<-vector()
ROC_C_PPV_log<-vector()
ROC_C_SR_log<-vector()
for (i in 1:100){
  ROC_C_TPR_log[i]<-ROC_C_log[[i]]$TPR[1]
  ROC_C_FPR_log[i]<-ROC_C_log[[i]]$FPR[1]
  ROC_C_PPV_log[i]<-ROC_C_log[[i]]$PPV[1]
  ROC_C_SR_log[i]<-ROC_C_log[[i]]$SR[1]
}

C_AUC_log<-simple_auc(ROC_C_TPR_log,ROC_C_FPR_log)

ROC_F_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_F_log[[i]]<-ROC
}

ROC_F_TPR_log<-vector()
ROC_F_FPR_log<-vector()
ROC_F_PPV_log<-vector()
ROC_F_SR_log<-vector()
for (i in 1:100){
  ROC_F_TPR_log[i]<-ROC_F_log[[i]]$TPR[1]
  ROC_F_FPR_log[i]<-ROC_F_log[[i]]$FPR[1]
  ROC_F_PPV_log[i]<-ROC_F_log[[i]]$PPV[1]
  ROC_F_SR_log[i]<-ROC_F_log[[i]]$SR[1]
}

F_AUC_log<-simple_auc(ROC_F_TPR_log,ROC_F_FPR_log)

##Logistic regression Just passed
ROC_GCF_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS, outcome="justpass",predictors = c("gender", "cohort", "fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GCF_log[[i]]<-ROC
}
ROC_GCF_TPR_log<-vector()
ROC_GCF_FPR_log<-vector()
ROC_GCF_PPV_log<-vector()
ROC_GCF_SR_log<-vector()
for (i in 1:100){
  ROC_GCF_TPR_log[i]<-ROC_GCF_log[[i]]$TPR[1]
  ROC_GCF_FPR_log[i]<-ROC_GCF_log[[i]]$FPR[1]
  ROC_GCF_PPV_log[i]<-ROC_GCF_log[[i]]$PPV[1]
  ROC_GCF_SR_log[i]<-ROC_GCF_log[[i]]$SR[1]
}

GCF_AUC_justpass_log<-simple_auc(ROC_GCF_TPR_log,ROC_GCF_FPR_log)


ROC_GC_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS, outcome="justpass",predictors = c("gender",  "cohort"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GC_log[[i]]<-ROC
}

ROC_GC_TPR_log<-vector()
ROC_GC_FPR_log<-vector()
ROC_GC_PPV_log<-vector()
ROC_GC_SR_log<-vector()
for (i in 1:100){
  ROC_GC_TPR_log[i]<-ROC_GC_log[[i]]$TPR[1]
  ROC_GC_FPR_log[i]<-ROC_GC_log[[i]]$FPR[1]
  ROC_GC_PPV_log[i]<-ROC_GC_log[[i]]$PPV[1]
  ROC_GC_SR_log[i]<-ROC_GC_log[[i]]$SR[1]
}

GC_AUC_justpass_log<-simple_auc(ROC_GC_TPR_log,ROC_GC_FPR_log)

ROC_GF_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS, outcome="justpass",predictors = c("gender",  "fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GF_log[[i]]<-ROC
}
ROC_GF_TPR_log<-vector()
ROC_GF_FPR_log<-vector()
ROC_GF_PPV_log<-vector()
ROC_GF_SR_log<-vector()
for (i in 1:100){
  ROC_GF_TPR_log[i]<-ROC_GF_log[[i]]$TPR[1]
  ROC_GF_FPR_log[i]<-ROC_GF_log[[i]]$FPR[1]
  ROC_GF_PPV_log[i]<-ROC_GF_log[[i]]$PPV[1]
  ROC_GF_SR_log[i]<-ROC_GF_log[[i]]$SR[1]
}

GF_AUC_justpass_log<-simple_auc(ROC_GF_TPR_log,ROC_GF_FPR_log)

ROC_CF_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS, outcome="justpass",predictors = c("cohort",  "fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GF_log[[i]]<-ROC
}
ROC_CF_TPR_log<-vector()
ROC_CF_FPR_log<-vector()
ROC_CF_PPV_log<-vector()
ROC_CF_SR_log<-vector()
for (i in 1:100){
  ROC_CF_TPR_log[i]<-ROC_CF_log[[i]]$TPR[1]
  ROC_CF_FPR_log[i]<-ROC_CF_log[[i]]$FPR[1]
  ROC_CF_PPV_log[i]<-ROC_CF_log[[i]]$PPV[1]
  ROC_CF_SR_log[i]<-ROC_CF_log[[i]]$SR[1]
}

CF_AUC_justpass_log<-simple_auc(ROC_CF_TPR_log,ROC_CF_FPR_log)

ROC_G_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS, outcome="justpass",predictors = c("gender"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_G_log[[i]]<-ROC
}

ROC_G_TPR_log<-vector()
ROC_G_FPR_log<-vector()
ROC_G_PPV_log<-vector()
ROC_G_SR_log<-vector()
for (i in 1:100){
  ROC_G_TPR_log[i]<-ROC_G_log[[i]]$TPR[1]
  ROC_G_FPR_log[i]<-ROC_G_log[[i]]$FPR[1]
  ROC_G_PPV_log[i]<-ROC_G_log[[i]]$PPV[1]
  ROC_G_SR_log[i]<-ROC_G_log[[i]]$SR[1]
}

G_AUC_justpass_log<-simple_auc(ROC_G_TPR_log,ROC_G_FPR_log)


ROC_C_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS, outcome="justpass",predictors = c("cohort"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_C_log[[i]]<-ROC
}
ROC_C_TPR_log<-vector()
ROC_C_FPR_log<-vector()
ROC_C_PPV_log<-vector()
ROC_C_SR_log<-vector()
for (i in 1:100){
  ROC_C_TPR_log[i]<-ROC_C_log[[i]]$TPR[1]
  ROC_C_FPR_log[i]<-ROC_C_log[[i]]$FPR[1]
  ROC_C_PPV_log[i]<-ROC_C_log[[i]]$PPV[1]
  ROC_C_SR_log[i]<-ROC_C_log[[i]]$SR[1]
}

C_AUC_justpass_log<-simple_auc(ROC_C_TPR_log,ROC_C_FPR_log)

ROC_F_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS, outcome="justpass",predictors = c("fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_F_log[[i]]<-ROC
}

ROC_F_TPR_log<-vector()
ROC_F_FPR_log<-vector()
ROC_F_PPV_log<-vector()
ROC_F_SR_log<-vector()
for (i in 1:100){
  ROC_F_TPR_log[i]<-ROC_F_log[[i]]$TPR[1]
  ROC_F_FPR_log[i]<-ROC_F_log[[i]]$FPR[1]
  ROC_F_PPV_log[i]<-ROC_F_log[[i]]$PPV[1]
  ROC_F_SR_log[i]<-ROC_F_log[[i]]$SR[1]
}

F_AUC_justpass_log<-simple_auc(ROC_F_TPR_log,ROC_F_FPR_log)

save(ROC_GCF_log,ROC_GC_log,ROC_GF_log, ROC_CF_log,ROC_F_log,ROC_C_log,ROC_F_log,
     ROC_GCF_justpass_log,ROC_GC_justpass_log,ROC_GF_justpass_log, ROC_CF_justpass_log,ROC_F_justpass_log,ROC_C_justpass_log,ROC_F_justpass_log,file="data/ROC_NN_logreg.Rdata")

##########LDA############
##########LDA############
##########LDA############
##########LDA############
##########LDA############
###linear discriminant analysis passed
ROC_GCF_lda<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("gender", "cohort", "fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GCF_lda[[i]]<-ROC
}
ROC_GCF_TPR_lda<-vector()
ROC_GCF_FPR_lda<-vector()
ROC_GCF_PPV_lda<-vector()
ROC_GCF_SR_lda<-vector()
for (i in 1:100){
  ROC_GCF_TPR_lda[i]<-ROC_GCF_lda[[i]]$TPR[1]
  ROC_GCF_FPR_lda[i]<-ROC_GCF_lda[[i]]$FPR[1]
  ROC_GCF_PPV_lda[i]<-ROC_GCF_lda[[i]]$PPV[1]
  ROC_GCF_SR_lda[i]<-ROC_GCF_lda[[i]]$SR[1]
}

GCF_AUC_lda<-simple_auc(ROC_GCF_TPR_lda,ROC_GCF_FPR_lda)


ROC_GC_lda<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("gender",  "cohort"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GC_lda[[i]]<-ROC
}

ROC_GC_TPR_lda<-vector()
ROC_GC_FPR_lda<-vector()
ROC_GC_PPV_lda<-vector()
ROC_GC_SR_lda<-vector()
for (i in 1:100){
  ROC_GC_TPR_lda[i]<-ROC_GC_lda[[i]]$TPR[1]
  ROC_GC_FPR_lda[i]<-ROC_GC_lda[[i]]$FPR[1]
  ROC_GC_PPV_lda[i]<-ROC_GC_lda[[i]]$PPV[1]
  ROC_GC_SR_lda[i]<-ROC_GC_lda[[i]]$SR[1]
}

GC_AUC_lda<-simple_auc(ROC_GC_TPR_lda,ROC_GC_FPR_lda)

ROC_GF_lda<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("gender",  "fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GF_lda[[i]]<-ROC
}
ROC_GF_TPR_lda<-vector()
ROC_GF_FPR_lda<-vector()
ROC_GF_PPV_lda<-vector()
ROC_GF_SR_lda<-vector()
for (i in 1:100){
  ROC_GF_TPR_lda[i]<-ROC_GF_lda[[i]]$TPR[1]
  ROC_GF_FPR_lda[i]<-ROC_GF_lda[[i]]$FPR[1]
  ROC_GF_PPV_lda[i]<-ROC_GF_lda[[i]]$PPV[1]
  ROC_GF_SR_lda[i]<-ROC_GF_lda[[i]]$SR[1]
}

GF_AUC_lda<-simple_auc(ROC_GF_TPR_lda,ROC_GF_FPR_lda)

ROC_CF_lda<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("cohort",  "fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GF_lda[[i]]<-ROC
}
ROC_CF_TPR_lda<-vector()
ROC_CF_FPR_lda<-vector()
ROC_CF_PPV_lda<-vector()
ROC_CF_SR_lda<-vector()
for (i in 1:100){
  ROC_CF_TPR_lda[i]<-ROC_CF_lda[[i]]$TPR[1]
  ROC_CF_FPR_lda[i]<-ROC_CF_lda[[i]]$FPR[1]
  ROC_CF_PPV_lda[i]<-ROC_CF_lda[[i]]$PPV[1]
  ROC_CF_SR_lda[i]<-ROC_CF_lda[[i]]$SR[1]
}

CF_AUC_lda<-simple_auc(ROC_CF_TPR_lda,ROC_CF_FPR_lda)

ROC_G_lda<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("gender"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_G_lda[[i]]<-ROC
}

ROC_G_TPR_lda<-vector()
ROC_G_FPR_lda<-vector()
ROC_G_PPV_lda<-vector()
ROC_G_SR_lda<-vector()
for (i in 1:100){
  ROC_G_TPR_lda[i]<-ROC_G_lda[[i]]$TPR[1]
  ROC_G_FPR_lda[i]<-ROC_G_lda[[i]]$FPR[1]
  ROC_G_PPV_lda[i]<-ROC_G_lda[[i]]$PPV[1]
  ROC_G_SR_lda[i]<-ROC_G_lda[[i]]$SR[1]
}

G_AUC_lda<-simple_auc(ROC_G_TPR_lda,ROC_G_FPR_lda)


ROC_C_lda<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("cohort"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_C_lda[[i]]<-ROC
}
ROC_C_TPR_lda<-vector()
ROC_C_FPR_lda<-vector()
ROC_C_PPV_lda<-vector()
ROC_C_SR_lda<-vector()
for (i in 1:100){
  ROC_C_TPR_lda[i]<-ROC_C_lda[[i]]$TPR[1]
  ROC_C_FPR_lda[i]<-ROC_C_lda[[i]]$FPR[1]
  ROC_C_PPV_lda[i]<-ROC_C_lda[[i]]$PPV[1]
  ROC_C_SR_lda[i]<-ROC_C_lda[[i]]$SR[1]
}

C_AUC_lda<-simple_auc(ROC_C_TPR_lda,ROC_C_FPR_lda)

ROC_F_lda<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_F_lda[[i]]<-ROC
}

ROC_F_TPR_lda<-vector()
ROC_F_FPR_lda<-vector()
ROC_F_PPV_lda<-vector()
ROC_F_SR_lda<-vector()
for (i in 1:100){
  ROC_F_TPR_lda[i]<-ROC_F_lda[[i]]$TPR[1]
  ROC_F_FPR_lda[i]<-ROC_F_lda[[i]]$FPR[1]
  ROC_F_PPV_lda[i]<-ROC_F_lda[[i]]$PPV[1]
  ROC_F_SR_lda[i]<-ROC_F_lda[[i]]$SR[1]
}

F_AUC_lda<-simple_auc(ROC_F_TPR_lda,ROC_F_FPR_lda)

##Linear Discriminant Analysis Just passed
ROC_GCF_lda<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS, outcome="justpass",predictors = c("gender", "cohort", "fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GCF_lda[[i]]<-ROC
}
ROC_GCF_TPR_lda<-vector()
ROC_GCF_FPR_lda<-vector()
ROC_GCF_PPV_lda<-vector()
ROC_GCF_SR_lda<-vector()
for (i in 1:100){
  ROC_GCF_TPR_lda[i]<-ROC_GCF_lda[[i]]$TPR[1]
  ROC_GCF_FPR_lda[i]<-ROC_GCF_lda[[i]]$FPR[1]
  ROC_GCF_PPV_lda[i]<-ROC_GCF_lda[[i]]$PPV[1]
  ROC_GCF_SR_lda[i]<-ROC_GCF_lda[[i]]$SR[1]
}

GCF_AUC_justpass_lda<-simple_auc(ROC_GCF_TPR_lda,ROC_GCF_FPR_lda)


ROC_GC_lda<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS, outcome="justpass",predictors = c("gender",  "cohort"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GC_lda[[i]]<-ROC
}

ROC_GC_TPR_lda<-vector()
ROC_GC_FPR_lda<-vector()
ROC_GC_PPV_lda<-vector()
ROC_GC_SR_lda<-vector()
for (i in 1:100){
  ROC_GC_TPR_lda[i]<-ROC_GC_lda[[i]]$TPR[1]
  ROC_GC_FPR_lda[i]<-ROC_GC_lda[[i]]$FPR[1]
  ROC_GC_PPV_lda[i]<-ROC_GC_lda[[i]]$PPV[1]
  ROC_GC_SR_lda[i]<-ROC_GC_lda[[i]]$SR[1]
}

GC_AUC_justpass_lda<-simple_auc(ROC_GC_TPR_lda,ROC_GC_FPR_lda)

ROC_GF_lda<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS, outcome="justpass",predictors = c("gender",  "fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GF_lda[[i]]<-ROC
}
ROC_GF_TPR_lda<-vector()
ROC_GF_FPR_lda<-vector()
ROC_GF_PPV_lda<-vector()
ROC_GF_SR_lda<-vector()
for (i in 1:100){
  ROC_GF_TPR_lda[i]<-ROC_GF_lda[[i]]$TPR[1]
  ROC_GF_FPR_lda[i]<-ROC_GF_lda[[i]]$FPR[1]
  ROC_GF_PPV_lda[i]<-ROC_GF_lda[[i]]$PPV[1]
  ROC_GF_SR_lda[i]<-ROC_GF_lda[[i]]$SR[1]
}

GF_AUC_justpass_lda<-simple_auc(ROC_GF_TPR_lda,ROC_GF_FPR_lda)

ROC_CF_lda<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS, outcome="justpass",predictors = c("cohort",  "fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GF_lda[[i]]<-ROC
}
ROC_CF_TPR_lda<-vector()
ROC_CF_FPR_lda<-vector()
ROC_CF_PPV_lda<-vector()
ROC_CF_SR_lda<-vector()
for (i in 1:100){
  ROC_CF_TPR_lda[i]<-ROC_CF_lda[[i]]$TPR[1]
  ROC_CF_FPR_lda[i]<-ROC_CF_lda[[i]]$FPR[1]
  ROC_CF_PPV_lda[i]<-ROC_CF_lda[[i]]$PPV[1]
  ROC_CF_SR_lda[i]<-ROC_CF_lda[[i]]$SR[1]
}

CF_AUC_justpass_lda<-simple_auc(ROC_CF_TPR_lda,ROC_CF_FPR_lda)

ROC_G_lda<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS, outcome="justpass",predictors = c("gender"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_G_lda[[i]]<-ROC
}

ROC_G_TPR_lda<-vector()
ROC_G_FPR_lda<-vector()
ROC_G_PPV_lda<-vector()
ROC_G_SR_lda<-vector()
for (i in 1:100){
  ROC_G_TPR_lda[i]<-ROC_G_lda[[i]]$TPR[1]
  ROC_G_FPR_lda[i]<-ROC_G_lda[[i]]$FPR[1]
  ROC_G_PPV_lda[i]<-ROC_G_lda[[i]]$PPV[1]
  ROC_G_SR_lda[i]<-ROC_G_lda[[i]]$SR[1]
}

G_AUC_justpass_lda<-simple_auc(ROC_G_TPR_lda,ROC_G_FPR_lda)


ROC_C_lda<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS, outcome="justpass",predictors = c("cohort"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_C_lda[[i]]<-ROC
}
ROC_C_TPR_lda<-vector()
ROC_C_FPR_lda<-vector()
ROC_C_PPV_lda<-vector()
ROC_C_SR_lda<-vector()
for (i in 1:100){
  ROC_C_TPR_lda[i]<-ROC_C_lda[[i]]$TPR[1]
  ROC_C_FPR_lda[i]<-ROC_C_lda[[i]]$FPR[1]
  ROC_C_PPV_lda[i]<-ROC_C_lda[[i]]$PPV[1]
  ROC_C_SR_lda[i]<-ROC_C_lda[[i]]$SR[1]
}

C_AUC_justpass_lda<-simple_auc(ROC_C_TPR_lda,ROC_C_FPR_lda)

ROC_F_lda<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS, outcome="justpass",predictors = c("fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_F_lda[[i]]<-ROC
}

ROC_F_TPR_lda<-vector()
ROC_F_FPR_lda<-vector()
ROC_F_PPV_lda<-vector()
ROC_F_SR_lda<-vector()
for (i in 1:100){
  ROC_F_TPR_lda[i]<-ROC_F_lda[[i]]$TPR[1]
  ROC_F_FPR_lda[i]<-ROC_F_lda[[i]]$FPR[1]
  ROC_F_PPV_lda[i]<-ROC_F_lda[[i]]$PPV[1]
  ROC_F_SR_lda[i]<-ROC_F_lda[[i]]$SR[1]
}

F_AUC_justpass_lda<-simple_auc(ROC_F_TPR_lda,ROC_F_FPR_lda)

save(ROC_GCF_lda,ROC_GC_lda,ROC_GF_lda, ROC_CF_lda,ROC_F_lda,ROC_C_lda,ROC_F_lda,
     ROC_GCF_justpass_lda,ROC_GC_justpass_lda,ROC_GF_justpass_lda, ROC_CF_justpass_lda,ROC_F_justpass_lda,ROC_C_justpass_lda,ROC_F_justpass_lda,file="data/ROC_NN_ldareg.Rdata")


##########QDA############
##########QDA############
##########QDA############
##########QDA############
##########QDA############
###linear discriminant analysis passed
ROC_GCF_qda<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("gender", "cohort", "fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GCF_qda[[i]]<-ROC
}
ROC_GCF_TPR_qda<-vector()
ROC_GCF_FPR_qda<-vector()
ROC_GCF_PPV_qda<-vector()
ROC_GCF_SR_qda<-vector()
for (i in 1:100){
  ROC_GCF_TPR_qda[i]<-ROC_GCF_qda[[i]]$TPR[1]
  ROC_GCF_FPR_qda[i]<-ROC_GCF_qda[[i]]$FPR[1]
  ROC_GCF_PPV_qda[i]<-ROC_GCF_qda[[i]]$PPV[1]
  ROC_GCF_SR_qda[i]<-ROC_GCF_qda[[i]]$SR[1]
}

GCF_AUC_qda<-simple_auc(ROC_GCF_TPR_qda,ROC_GCF_FPR_qda)


ROC_GC_qda<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("gender",  "cohort"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GC_qda[[i]]<-ROC
}

ROC_GC_TPR_qda<-vector()
ROC_GC_FPR_qda<-vector()
ROC_GC_PPV_qda<-vector()
ROC_GC_SR_qda<-vector()
for (i in 1:100){
  ROC_GC_TPR_qda[i]<-ROC_GC_qda[[i]]$TPR[1]
  ROC_GC_FPR_qda[i]<-ROC_GC_qda[[i]]$FPR[1]
  ROC_GC_PPV_qda[i]<-ROC_GC_qda[[i]]$PPV[1]
  ROC_GC_SR_qda[i]<-ROC_GC_qda[[i]]$SR[1]
}

GC_AUC_qda<-simple_auc(ROC_GC_TPR_qda,ROC_GC_FPR_qda)

ROC_GF_qda<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("gender",  "fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GF_qda[[i]]<-ROC
}
ROC_GF_TPR_qda<-vector()
ROC_GF_FPR_qda<-vector()
ROC_GF_PPV_qda<-vector()
ROC_GF_SR_qda<-vector()
for (i in 1:100){
  ROC_GF_TPR_qda[i]<-ROC_GF_qda[[i]]$TPR[1]
  ROC_GF_FPR_qda[i]<-ROC_GF_qda[[i]]$FPR[1]
  ROC_GF_PPV_qda[i]<-ROC_GF_qda[[i]]$PPV[1]
  ROC_GF_SR_qda[i]<-ROC_GF_qda[[i]]$SR[1]
}

GF_AUC_qda<-simple_auc(ROC_GF_TPR_qda,ROC_GF_FPR_qda)

ROC_CF_qda<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("cohort",  "fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GF_qda[[i]]<-ROC
}
ROC_CF_TPR_qda<-vector()
ROC_CF_FPR_qda<-vector()
ROC_CF_PPV_qda<-vector()
ROC_CF_SR_qda<-vector()
for (i in 1:100){
  ROC_CF_TPR_qda[i]<-ROC_CF_qda[[i]]$TPR[1]
  ROC_CF_FPR_qda[i]<-ROC_CF_qda[[i]]$FPR[1]
  ROC_CF_PPV_qda[i]<-ROC_CF_qda[[i]]$PPV[1]
  ROC_CF_SR_qda[i]<-ROC_CF_qda[[i]]$SR[1]
}

CF_AUC_qda<-simple_auc(ROC_CF_TPR_qda,ROC_CF_FPR_qda)

ROC_G_qda<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("gender"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_G_qda[[i]]<-ROC
}

ROC_G_TPR_qda<-vector()
ROC_G_FPR_qda<-vector()
ROC_G_PPV_qda<-vector()
ROC_G_SR_qda<-vector()
for (i in 1:100){
  ROC_G_TPR_qda[i]<-ROC_G_qda[[i]]$TPR[1]
  ROC_G_FPR_qda[i]<-ROC_G_qda[[i]]$FPR[1]
  ROC_G_PPV_qda[i]<-ROC_G_qda[[i]]$PPV[1]
  ROC_G_SR_qda[i]<-ROC_G_qda[[i]]$SR[1]
}

G_AUC_qda<-simple_auc(ROC_G_TPR_qda,ROC_G_FPR_qda)


ROC_C_qda<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("cohort"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_C_qda[[i]]<-ROC
}
ROC_C_TPR_qda<-vector()
ROC_C_FPR_qda<-vector()
ROC_C_PPV_qda<-vector()
ROC_C_SR_qda<-vector()
for (i in 1:100){
  ROC_C_TPR_qda[i]<-ROC_C_qda[[i]]$TPR[1]
  ROC_C_FPR_qda[i]<-ROC_C_qda[[i]]$FPR[1]
  ROC_C_PPV_qda[i]<-ROC_C_qda[[i]]$PPV[1]
  ROC_C_SR_qda[i]<-ROC_C_qda[[i]]$SR[1]
}

C_AUC_qda<-simple_auc(ROC_C_TPR_qda,ROC_C_FPR_qda)

ROC_F_qda<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_F_qda[[i]]<-ROC
}

ROC_F_TPR_qda<-vector()
ROC_F_FPR_qda<-vector()
ROC_F_PPV_qda<-vector()
ROC_F_SR_qda<-vector()
for (i in 1:100){
  ROC_F_TPR_qda[i]<-ROC_F_qda[[i]]$TPR[1]
  ROC_F_FPR_qda[i]<-ROC_F_qda[[i]]$FPR[1]
  ROC_F_PPV_qda[i]<-ROC_F_qda[[i]]$PPV[1]
  ROC_F_SR_qda[i]<-ROC_F_qda[[i]]$SR[1]
}

F_AUC_qda<-simple_auc(ROC_F_TPR_qda,ROC_F_FPR_qda)

##Linear Discriminant Analysis Just passed
ROC_GCF_qda<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS, outcome="justpass",predictors = c("gender", "cohort", "fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GCF_qda[[i]]<-ROC
}
ROC_GCF_TPR_qda<-vector()
ROC_GCF_FPR_qda<-vector()
ROC_GCF_PPV_qda<-vector()
ROC_GCF_SR_qda<-vector()
for (i in 1:100){
  ROC_GCF_TPR_qda[i]<-ROC_GCF_qda[[i]]$TPR[1]
  ROC_GCF_FPR_qda[i]<-ROC_GCF_qda[[i]]$FPR[1]
  ROC_GCF_PPV_qda[i]<-ROC_GCF_qda[[i]]$PPV[1]
  ROC_GCF_SR_qda[i]<-ROC_GCF_qda[[i]]$SR[1]
}

GCF_AUC_justpass_qda<-simple_auc(ROC_GCF_TPR_qda,ROC_GCF_FPR_qda)


ROC_GC_qda<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS, outcome="justpass",predictors = c("gender",  "cohort"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GC_qda[[i]]<-ROC
}

ROC_GC_TPR_qda<-vector()
ROC_GC_FPR_qda<-vector()
ROC_GC_PPV_qda<-vector()
ROC_GC_SR_qda<-vector()
for (i in 1:100){
  ROC_GC_TPR_qda[i]<-ROC_GC_qda[[i]]$TPR[1]
  ROC_GC_FPR_qda[i]<-ROC_GC_qda[[i]]$FPR[1]
  ROC_GC_PPV_qda[i]<-ROC_GC_qda[[i]]$PPV[1]
  ROC_GC_SR_qda[i]<-ROC_GC_qda[[i]]$SR[1]
}

GC_AUC_justpass_qda<-simple_auc(ROC_GC_TPR_qda,ROC_GC_FPR_qda)

ROC_GF_qda<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS, outcome="justpass",predictors = c("gender",  "fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GF_qda[[i]]<-ROC
}
ROC_GF_TPR_qda<-vector()
ROC_GF_FPR_qda<-vector()
ROC_GF_PPV_qda<-vector()
ROC_GF_SR_qda<-vector()
for (i in 1:100){
  ROC_GF_TPR_qda[i]<-ROC_GF_qda[[i]]$TPR[1]
  ROC_GF_FPR_qda[i]<-ROC_GF_qda[[i]]$FPR[1]
  ROC_GF_PPV_qda[i]<-ROC_GF_qda[[i]]$PPV[1]
  ROC_GF_SR_qda[i]<-ROC_GF_qda[[i]]$SR[1]
}

GF_AUC_justpass_qda<-simple_auc(ROC_GF_TPR_qda,ROC_GF_FPR_qda)

ROC_CF_qda<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS, outcome="justpass",predictors = c("cohort",  "fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_GF_qda[[i]]<-ROC
}
ROC_CF_TPR_qda<-vector()
ROC_CF_FPR_qda<-vector()
ROC_CF_PPV_qda<-vector()
ROC_CF_SR_qda<-vector()
for (i in 1:100){
  ROC_CF_TPR_qda[i]<-ROC_CF_qda[[i]]$TPR[1]
  ROC_CF_FPR_qda[i]<-ROC_CF_qda[[i]]$FPR[1]
  ROC_CF_PPV_qda[i]<-ROC_CF_qda[[i]]$PPV[1]
  ROC_CF_SR_qda[i]<-ROC_CF_qda[[i]]$SR[1]
}

CF_AUC_justpass_qda<-simple_auc(ROC_CF_TPR_qda,ROC_CF_FPR_qda)

ROC_G_qda<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS, outcome="justpass",predictors = c("gender"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_G_qda[[i]]<-ROC
}

ROC_G_TPR_qda<-vector()
ROC_G_FPR_qda<-vector()
ROC_G_PPV_qda<-vector()
ROC_G_SR_qda<-vector()
for (i in 1:100){
  ROC_G_TPR_qda[i]<-ROC_G_qda[[i]]$TPR[1]
  ROC_G_FPR_qda[i]<-ROC_G_qda[[i]]$FPR[1]
  ROC_G_PPV_qda[i]<-ROC_G_qda[[i]]$PPV[1]
  ROC_G_SR_qda[i]<-ROC_G_qda[[i]]$SR[1]
}

G_AUC_justpass_qda<-simple_auc(ROC_G_TPR_qda,ROC_G_FPR_qda)


ROC_C_qda<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS, outcome="justpass",predictors = c("cohort"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_C_qda[[i]]<-ROC
}
ROC_C_TPR_qda<-vector()
ROC_C_FPR_qda<-vector()
ROC_C_PPV_qda<-vector()
ROC_C_SR_qda<-vector()
for (i in 1:100){
  ROC_C_TPR_qda[i]<-ROC_C_qda[[i]]$TPR[1]
  ROC_C_FPR_qda[i]<-ROC_C_qda[[i]]$FPR[1]
  ROC_C_PPV_qda[i]<-ROC_C_qda[[i]]$PPV[1]
  ROC_C_SR_qda[i]<-ROC_C_qda[[i]]$SR[1]
}

C_AUC_justpass_qda<-simple_auc(ROC_C_TPR_qda,ROC_C_FPR_qda)

ROC_F_qda<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS, outcome="justpass",predictors = c("fci_pre"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_F_qda[[i]]<-ROC
}

ROC_F_TPR_qda<-vector()
ROC_F_FPR_qda<-vector()
ROC_F_PPV_qda<-vector()
ROC_F_SR_qda<-vector()
for (i in 1:100){
  ROC_F_TPR_qda[i]<-ROC_F_qda[[i]]$TPR[1]
  ROC_F_FPR_qda[i]<-ROC_F_qda[[i]]$FPR[1]
  ROC_F_PPV_qda[i]<-ROC_F_qda[[i]]$PPV[1]
  ROC_F_SR_qda[i]<-ROC_F_qda[[i]]$SR[1]
}

F_AUC_justpass_qda<-simple_auc(ROC_F_TPR_qda,ROC_F_FPR_qda)

save(ROC_GCF_qda,ROC_GC_qda,ROC_GF_qda, ROC_CF_qda,ROC_F_qda,ROC_C_qda,ROC_F_qda,
     ROC_GCF_justpass_qda,ROC_GC_justpass_qda,ROC_GF_justpass_qda, ROC_CF_justpass_qda,ROC_F_justpass_qda,ROC_C_justpass_qda,ROC_F_justpass_qda,file="data/ROC_NN_qdareg.Rdata")

##########KNN############
##########KNN############
##########KNN############
##########KNN############
##########KNN############

## Models without networks measures

rm(list = ls())

library(igraph)
library(dplyr)
library(class)   # for knn
library(tidyr)
library(ggplot2)  # for plotting success rates
library(MASS)


# Import pass/fail centrality data
loadvars <- load("data/centPassFail.Rdata")
load("data/centrality_data_frames.Rdata")
load("data/ROC_logreg.Rdata")
source("code/jackknife_functions.R")
source("code/ROC_functions.R")

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
  ROC_CF_log[[i]]<-ROC
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
  ROC_CF_log[[i]]<-ROC
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
     ROC_GCF_justpass_log,ROC_GC_justpass_log,ROC_GF_justpass_log, ROC_CF_justpass_log,
     ROC_F_justpass_log,ROC_C_justpass_log,ROC_F_justpass_log,file="data/ROC_AUC/ROC09_NN_logreg.Rdata")

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
  ROC_CF_lda[[i]]<-ROC
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
  ROC_CF_lda[[i]]<-ROC
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
     ROC_GCF_justpass_lda,ROC_GC_justpass_lda,ROC_GF_justpass_lda, ROC_CF_justpass_lda,
     ROC_F_justpass_lda,ROC_C_justpass_lda,ROC_F_justpass_lda,file="data/ROC_AUC/ROC09_NN_ldareg.Rdata")


##########QDA############
##########QDA############
##########QDA############
##########QDA############
##########QDA############
###quadratic discriminant analysis passed
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
  ROC_CF_qda[[i]]<-ROC
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

##Quadratic Discriminant Analysis Just passed
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
  ROC_CF_qda[[i]]<-ROC
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
     ROC_GCF_justpass_qda,ROC_GC_justpass_qda,ROC_GF_justpass_qda, ROC_CF_justpass_qda,
     ROC_F_justpass_qda,ROC_C_justpass_qda,ROC_F_justpass_qda,file="data/ROC_AUC/ROC09_NN_qdareg.Rdata")

##########KNN############
##########KNN############
##########KNN############
##########KNN############
##########KNN############
###K-Nearest Neigbors
GCF_knn<-list()
for(i in 1:10){
  predPS_x<-jackPredKNN(centPS, predictors = c("gender", "cohort", "fci_pre"), nK=i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  GCF_knn[[i]]<-ROC
}
GCF_TPR_knn<-vector()
GCF_FPR_knn<-vector()
GCF_PPV_knn<-vector()
GCF_SR_knn<-vector()
for (i in 1:10){
  GCF_TPR_knn[i]<-GCF_knn[[i]]$TPR[1]
  GCF_FPR_knn[i]<-GCF_knn[[i]]$FPR[1]
  GCF_PPV_knn[i]<-GCF_knn[[i]]$PPV[1]
  GCF_SR_knn[i]<-GCF_knn[[i]]$SR[1]
}

GC_knn<-list()
for(i in 1:10){
  predPS_x<-jackPredKNN(centPS, predictors = c("gender", "cohort"), nK=i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  GC_knn[[i]]<-ROC
}
GC_TPR_knn<-vector()
GC_FPR_knn<-vector()
GC_PPV_knn<-vector()
GC_SR_knn<-vector()
for (i in 1:10){
  GC_TPR_knn[i]<-GC_knn[[i]]$TPR[1]
  GC_FPR_knn[i]<-GC_knn[[i]]$FPR[1]
  GC_PPV_knn[i]<-GC_knn[[i]]$PPV[1]
  GC_SR_knn[i]<-GC_knn[[i]]$SR[1]
}

GF_knn<-list()
for(i in 1:10){
  predPS_x<-jackPredKNN(centPS, predictors = c("gender", "fci_pre"), nK=i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  GF_knn[[i]]<-ROC
}
GF_TPR_knn<-vector()
GF_FPR_knn<-vector()
GF_PPV_knn<-vector()
GF_SR_knn<-vector()
for (i in 1:10){
  GF_TPR_knn[i]<-GF_knn[[i]]$TPR[1]
  GF_FPR_knn[i]<-GF_knn[[i]]$FPR[1]
  GF_PPV_knn[i]<-GF_knn[[i]]$PPV[1]
  GF_SR_knn[i]<-GF_knn[[i]]$SR[1]
}
CF_knn<-list()
for(i in 1:10){
  predPS_x<-jackPredKNN(centPS, predictors = c("cohort", "fci_pre"), nK=i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  CF_knn[[i]]<-ROC
}
CF_TPR_knn<-vector()
CF_FPR_knn<-vector()
CF_PPV_knn<-vector()
CF_SR_knn<-vector()
for (i in 1:10){
  CF_TPR_knn[i]<-CF_knn[[i]]$TPR[1]
  CF_FPR_knn[i]<-CF_knn[[i]]$FPR[1]
  CF_PPV_knn[i]<-CF_knn[[i]]$PPV[1]
  CF_SR_knn[i]<-CF_knn[[i]]$SR[1]
}

F_knn<-list()
for(i in 1:10){
  predPS_x<-jackPredKNN(centPS, predictors = c("fci_pre"), nK=i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  F_knn[[i]]<-ROC
}
F_TPR_knn<-vector()
F_FPR_knn<-vector()
F_PPV_knn<-vector()
F_SR_knn<-vector()
for (i in 1:10){
  F_TPR_knn[i]<-F_knn[[i]]$TPR[1]
  F_FPR_knn[i]<-F_knn[[i]]$FPR[1]
  F_PPV_knn[i]<-F_knn[[i]]$PPV[1]
  F_SR_knn[i]<-F_knn[[i]]$SR[1]
}
C_knn<-list()
for(i in 1:10){
  predPS_x<-jackPredKNN(centPS, predictors = c("cohort"), nK=i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  C_knn[[i]]<-ROC
}
C_TPR_knn<-vector()
C_FPR_knn<-vector()
C_PPV_knn<-vector()
C_SR_knn<-vector()
for (i in 1:10){
  C_TPR_knn[i]<-C_knn[[i]]$TPR[1]
  C_FPR_knn[i]<-C_knn[[i]]$FPR[1]
  C_PPV_knn[i]<-C_knn[[i]]$PPV[1]
  C_SR_knn[i]<-C_knn[[i]]$SR[1]
}
G_knn<-list()
for(i in 1:10){
  predPS_x<-jackPredKNN(centPS, predictors = c("gender"), nK=i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  G_knn[[i]]<-ROC
}
G_TPR_knn<-vector()
G_FPR_knn<-vector()
G_PPV_knn<-vector()
G_SR_knn<-vector()
for (i in 1:10){
  G_TPR_knn[i]<-G_knn[[i]]$TPR[1]
  G_FPR_knn[i]<-G_knn[[i]]$FPR[1]
  G_PPV_knn[i]<-G_knn[[i]]$PPV[1]
  G_SR_knn[i]<-G_knn[[i]]$SR[1]
}

###K-Nearest Neigbors just passed
GCF_knn_JP<-list()
for(i in 1:10){
  predPS_x<-jackPredKNN(centPS, outcome="justpass", predictors = c("gender", "cohort", "fci_pre"), nK=i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  GCF_knn_JP[[i]]<-ROC
}
GCF_TPR_knn_JP<-vector()
GCF_FPR_knn_JP<-vector()
GCF_PPV_knn_JP<-vector()
GCF_SR_knn_JP<-vector()
for (i in 1:10){
  GCF_TPR_knn_JP[i]<-GCF_knn_JP[[i]]$TPR[1]
  GCF_FPR_knn_JP[i]<-GCF_knn_JP[[i]]$FPR[1]
  GCF_PPV_knn_JP[i]<-GCF_knn_JP[[i]]$PPV[1]
  GCF_SR_knn_JP[i]<-GCF_knn_JP[[i]]$SR[1]
}

GC_knn_JP<-list()
for(i in 1:10){
  predPS_x<-jackPredKNN(centPS, outcome="justpass", predictors = c("gender", "cohort"), nK=i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  GC_knn_JP[[i]]<-ROC
}
GC_TPR_knn_JP<-vector()
GC_FPR_knn_JP<-vector()
GC_PPV_knn_JP<-vector()
GC_SR_knn_JP<-vector()
for (i in 1:10){
  GC_TPR_knn_JP[i]<-GC_knn_JP[[i]]$TPR[1]
  GC_FPR_knn_JP[i]<-GC_knn_JP[[i]]$FPR[1]
  GC_PPV_knn_JP[i]<-GC_knn_JP[[i]]$PPV[1]
  GC_SR_knn_JP[i]<-GC_knn_JP[[i]]$SR[1]
}

GF_knn_JP<-list()
for(i in 1:10){
  predPS_x<-jackPredKNN(centPS, outcome="justpass",predictors = c("gender", "fci_pre"), nK=i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  GF_knn_JP[[i]]<-ROC
}
GF_TPR_knn_JP<-vector()
GF_FPR_knn_JP<-vector()
GF_PPV_knn_JP<-vector()
GF_SR_knn_JP<-vector()
for (i in 1:10){
  GF_TPR_knn_JP[i]<-GF_knn_JP[[i]]$TPR[1]
  GF_FPR_knn_JP[i]<-GF_knn_JP[[i]]$FPR[1]
  GF_PPV_knn_JP[i]<-GF_knn_JP[[i]]$PPV[1]
  GF_SR_knn_JP[i]<-GF_knn_JP[[i]]$SR[1]
}
CF_knn_JP<-list()
for(i in 1:10){
  predPS_x<-jackPredKNN(centPS, outcome="justpass",predictors = c("cohort", "fci_pre"), nK=i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  CF_knn_JP[[i]]<-ROC
}
CF_TPR_knn_JP<-vector()
CF_FPR_knn_JP<-vector()
CF_PPV_knn_JP<-vector()
CF_SR_knn_JP<-vector()
for (i in 1:10){
  CF_TPR_knn_JP[i]<-CF_knn_JP[[i]]$TPR[1]
  CF_FPR_knn_JP[i]<-CF_knn_JP[[i]]$FPR[1]
  CF_PPV_knn_JP[i]<-CF_knn_JP[[i]]$PPV[1]
  CF_SR_knn_JP[i]<-CF_knn_JP[[i]]$SR[1]
}

F_knn_JP<-list()
for(i in 1:10){
  predPS_x<-jackPredKNN(centPS, outcome="justpass",predictors = c("fci_pre"), nK=i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  F_knn_JP[[i]]<-ROC
}
F_TPR_knn_JP<-vector()
F_FPR_knn_JP<-vector()
F_PPV_knn_JP<-vector()
F_SR_knn_JP<-vector()
for (i in 1:10){
  F_TPR_knn_JP[i]<-F_knn_JP[[i]]$TPR[1]
  F_FPR_knn_JP[i]<-F_knn_JP[[i]]$FPR[1]
  F_PPV_knn_JP[i]<-F_knn_JP[[i]]$PPV[1]
  F_SR_knn_JP[i]<-F_knn_JP[[i]]$SR[1]
}
C_knn_JP<-list()
for(i in 1:10){
  predPS_x<-jackPredKNN(centPS, outcome="justpass",predictors = c("cohort"), nK=i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  C_knn_JP[[i]]<-ROC
}
C_TPR_knn_JP<-vector()
C_FPR_knn_JP<-vector()
C_PPV_knn_JP<-vector()
C_SR_knn_JP<-vector()
for (i in 1:10){
  C_TPR_knn_JP[i]<-C_knn_JP[[i]]$TPR[1]
  C_FPR_knn_JP[i]<-C_knn_JP[[i]]$FPR[1]
  C_PPV_knn_JP[i]<-C_knn_JP[[i]]$PPV[1]
  C_SR_knn_JP[i]<-C_knn_JP[[i]]$SR[1]
}
G_knn_JP<-list()
for(i in 1:10){
  predPS_x<-jackPredKNN(centPS, outcome="justpass", predictors = c("gender"), nK=i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  G_knn_JP[[i]]<-ROC
}
G_TPR_knn_JP<-vector()
G_FPR_knn_JP<-vector()
G_PPV_knn_JP<-vector()
G_SR_knn_JP<-vector()
for (i in 1:10){
  G_TPR_knn_JP[i]<-G_knn_JP[[i]]$TPR[1]
  G_FPR_knn_JP[i]<-G_knn_JP[[i]]$FPR[1]
  G_PPV_knn_JP[i]<-G_knn_JP[[i]]$PPV[1]
  G_SR_knn_JP[i]<-G_knn_JP[[i]]$SR[1]
}


logRegP<-c(GCF_AUC_log,GF_AUC_log,GC_AUC_log,CF_AUC_log,F_AUC_log,G_AUC_log,C_AUC_log)
ldaP<-c(GCF_AUC_lda,GF_AUC_lda,GC_AUC_lda,CF_AUC_lda,F_AUC_lda,G_AUC_lda,C_AUC_lda)
qdaP<-c(GCF_AUC_qda,GF_AUC_qda,GC_AUC_qda,CF_AUC_qda,F_AUC_qda,G_AUC_qda,C_AUC_qda)
KNNP<-c(max(GCF_SR_knn),max(GF_SR_knn),max(GC_SR_knn),max(CF_SR_knn),max(F_SR_knn),max(G_SR_knn),max(C_SR_knn))
OnlyBackground<-data.frame(logistic=logRegP, LDA=ldaP,QDA=qdaP,KNN=KNNP)
rownames(OnlyBackground)<-c("GCF","GF","GC","CF","F","G","C")



logRegP<-c(GCF_AUC_justpass_log,GF_AUC_justpass_log,GC_AUC_justpass_log,CF_AUC_justpass_log,F_AUC_justpass_log,G_AUC_justpass_log,C_AUC_justpass_log)
ldaP<-c(GCF_AUC_justpass_lda,GF_AUC_justpass_lda,GC_AUC_justpass_lda,CF_AUC_justpass_lda,F_AUC_justpass_lda,G_AUC_justpass_lda,C_AUC_justpass_lda)
qdaP<-c(GCF_AUC_justpass_qda,GF_AUC_justpass_qda,GC_AUC_justpass_qda,CF_AUC_justpass_qda,F_AUC_justpass_qda,G_AUC_justpass_qda,C_AUC_justpass_qda)
KNNP<-c(max(GCF_SR_knn_JP),max(GF_SR_knn_JP),max(GC_SR_knn_JP),max(CF_SR_knn_JP),max(F_SR_knn_JP),max(G_SR_knn_JP),max(C_SR_knn_JP))
OnlyBackground_JP<-data.frame(logistic=logRegP, LDA=ldaP,QDA=qdaP,KNN=KNNP)
rownames(OnlyBackground_JP)<-c("GCF","GF","GC","CF","F","G","C")

save(OnlyBackground,OnlyBackground_JP,file="data/ROC_AUC/AUC09_SR_NN_fcipre.Rdata")



#Here we run linear discriminant analysis on only network variables
#Gender, Cohort, and FCI_pre are not part of the models
#The script ultimately ends with AUC values for each layer for each week for 
#passed and just passed conditions

rm(list = ls())

library(igraph)
library(dplyr)



# Import pass/fail centrality data
#loadvars <- load("data/centPassFail.Rdata")
load("data/centrality_data_frames.Rdata")
load("data/ROC_ldareg.Rdata")

## Run jackknife logistic regression 

# Turn long data frame into list of weekly frames
centPS <- dfPS %>% group_split(Week)
centCD <- dfCD %>% group_split(Week)
centICS <- dfICS %>% group_split(Week)

source("code/jackknife_functions.R")
source("code/ROC_functions.R")


#RUN
ROC_PS_lda<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_lda[[i]]<-ROC
}

ROC_CD_lda<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD,predictors = c("PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_lda[[i]]<-ROC
}

ROC_ICS_lda<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS,predictors = c("PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_lda[[i]]<-ROC
}

ROC_PS_justpass_lda<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,outcome = "justpass",predictors = c("PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_lda[[i]]<-ROC
}

ROC_CD_justpass_lda<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD, outcome = "justpass",predictors = c("PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_lda[[i]]<-ROC
}

ROC_ICS_justpass_lda<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS, outcome = "justpass",predictors = c("PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_lda[[i]]<-ROC
}


save(ROC_PS_lda,ROC_CD_lda,ROC_ICS_lda, ROC_PS_justpass_lda,ROC_CD_justpass_lda,ROC_ICS_justpass_lda,file="data/ROC_NB_ldareg.Rdata")



ROC_PS_TPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_FPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_PPV<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_SR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_PS_TPR[i,1:7]<-ROC_PS_lda[[i]]$TPR[1:7]
  ROC_PS_FPR[i,1:7]<-ROC_PS_lda[[i]]$FPR[1:7]
  ROC_PS_PPV[i,1:7]<-ROC_PS_lda[[i]]$PPV[1:7]
  ROC_PS_SR[i,1:7]<-ROC_PS_lda[[i]]$SR[1:7]
}

ROC_CD_TPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_FPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_PPV<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_SR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_CD_TPR[i,1:7]<-ROC_CD_lda[[i]]$TPR[1:7]
  ROC_CD_FPR[i,1:7]<-ROC_CD_lda[[i]]$FPR[1:7]
  ROC_CD_PPV[i,1:7]<-ROC_CD_lda[[i]]$PPV[1:7]
  ROC_CD_SR[i,1:7]<-ROC_CD_lda[[i]]$SR[1:7]
}

ROC_ICS_TPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_FPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_PPV<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_SR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_ICS_TPR[i,1:7]<-ROC_ICS_lda[[i]]$TPR[1:7]
  ROC_ICS_FPR[i,1:7]<-ROC_ICS_lda[[i]]$FPR[1:7]
  ROC_ICS_PPV[i,1:7]<-ROC_ICS_lda[[i]]$PPV[1:7]
  ROC_ICS_SR[i,1:7]<-ROC_ICS_lda[[i]]$SR[1:7]
}

ROC_PS_justpass_TPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_justpass_FPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_justpass_PPV<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_justpass_SR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_PS_justpass_TPR[i,1:7]<-ROC_PS_justpass_lda[[i]]$TPR[1:7]
  ROC_PS_justpass_FPR[i,1:7]<-ROC_PS_justpass_lda[[i]]$FPR[1:7]
  ROC_PS_justpass_PPV[i,1:7]<-ROC_PS_justpass_lda[[i]]$PPV[1:7]
  ROC_PS_justpass_SR[i,1:7]<-ROC_PS_justpass_lda[[i]]$SR[1:7]
}

ROC_CD_justpass_TPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_justpass_FPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_justpass_PPV<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_justpass_SR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_CD_justpass_TPR[i,1:7]<-ROC_CD_justpass_lda[[i]]$TPR[1:7]
  ROC_CD_justpass_FPR[i,1:7]<-ROC_CD_justpass_lda[[i]]$FPR[1:7]
  ROC_CD_justpass_PPV[i,1:7]<-ROC_CD_justpass_lda[[i]]$PPV[1:7]
  ROC_CD_justpass_SR[i,1:7]<-ROC_CD_justpass_lda[[i]]$SR[1:7]
}

ROC_ICS_justpass_TPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_justpass_FPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_justpass_PPV<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_justpass_SR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_ICS_justpass_TPR[i,1:7]<-ROC_ICS_justpass_lda[[i]]$TPR[1:7]
  ROC_ICS_justpass_FPR[i,1:7]<-ROC_ICS_justpass_lda[[i]]$FPR[1:7]
  ROC_ICS_justpass_PPV[i,1:7]<-ROC_ICS_justpass_lda[[i]]$PPV[1:7]
  ROC_ICS_justpass_SR[i,1:7]<-ROC_ICS_justpass_lda[[i]]$SR[1:7]
}

PS_AUC_NB_lda<-vector()
for (i in 1:7){
  PS_AUC_NB_lda[i]<-simple_auc(ROC_PS_TPR[,i],ROC_PS_FPR[,i])
}

CD_AUC_NB_lda<-vector()
for (i in 1:7){
  CD_AUC_NB_lda[i]<-simple_auc(ROC_CD_TPR[,i],ROC_CD_FPR[,i])
}

ICS_AUC_NB_lda<-vector()
for (i in 1:7){
  ICS_AUC_NB_lda[i]<-simple_auc(ROC_ICS_TPR[,i],ROC_ICS_FPR[,i])
}

PS_AUC_JP_NB_lda<-vector()
for (i in 1:7){
  PS_AUC_JP_NB_lda[i]<-simple_auc(ROC_PS_justpass_TPR[,i],ROC_PS_justpass_FPR[,i])
}

CD_AUC_JP_NB_lda<-vector()
for (i in 1:7){
  CD_AUC_JP_NB_lda[i]<-simple_auc(ROC_CD_justpass_TPR[,i],ROC_CD_justpass_FPR[,i])
}

ICS_AUC_JP_NB_lda<-vector()
for (i in 1:7){
  ICS_AUC_JP_NB_lda[i]<-simple_auc(ROC_ICS_justpass_TPR[,i],ROC_ICS_justpass_FPR[,i])
}
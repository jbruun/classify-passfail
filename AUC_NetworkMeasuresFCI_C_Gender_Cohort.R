#Logistic Regression
## Models with network  measures and FCI pre classes

rm(list = ls())
tstart<-Sys.time()

library(igraph)
library(dplyr)
library(class)   # for knn
library(tidyr)
library(ggplot2)  # for plotting success rates
library(MASS)

load("data/centrality_data_frames.Rdata")
# Turn long data frame into list of weekly frames
centPS <- dfPS %>% group_split(Week)
centCD <- dfCD %>% group_split(Week)
centICS <- dfICS %>% group_split(Week)

source("code/jackknife_functions.R")
source("code/ROC_functions.R")

#########LOGISTIC REGRESSION###########
#########LOGISTIC REGRESSION###########
#########LOGISTIC REGRESSION###########
#########LOGISTIC REGRESSION###########

######ALL NETWORK PREDICTORS######

ROC_PS_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log[[i]]<-ROC
}

ROC_CD_log<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log[[i]]<-ROC
}

ROC_ICS_log<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log[[i]]<-ROC
}

ROC_PS_justpass_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log[[i]]<-ROC
}

ROC_CD_justpass_log<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log[[i]]<-ROC
}

ROC_ICS_justpass_log<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log[[i]]<-ROC
}

######PAGERANK TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_log_PRTE<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_PRTE[[i]]<-ROC
}

ROC_CD_log_PRTE<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_PRTE[[i]]<-ROC
}

ROC_ICS_log_PRTE<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_PRTE[[i]]<-ROC
}

ROC_PS_justpass_log_PRTE<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_PRTE[[i]]<-ROC
}

ROC_CD_justpass_log_PRTE<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_PRTE[[i]]<-ROC
}

ROC_ICS_justpass_log_PRTE<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log_PRTE[[i]]<-ROC
}

######PAGERANK HIDE NETWORK PREDICTORS######

ROC_PS_log_PRH<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_PRH[[i]]<-ROC
}

ROC_CD_log_PRH<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_PRH[[i]]<-ROC
}

ROC_ICS_log_PRH<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_PRH[[i]]<-ROC
}

ROC_PS_justpass_log_PRH<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_PRH[[i]]<-ROC
}

ROC_CD_justpass_log_PRH<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_PRH[[i]]<-ROC
}

ROC_ICS_justpass_log_PRH<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log_PRH[[i]]<-ROC
}

######TARGET ENTROPY HIDE NETWORK PREDICTORS######

ROC_PS_log_TEH<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_TEH[[i]]<-ROC
}

ROC_CD_log_TEH<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_TEH[[i]]<-ROC
}

ROC_ICS_log_TEH<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_TEH[[i]]<-ROC
}

ROC_PS_justpass_log_TEH<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_TEH[[i]]<-ROC
}

ROC_CD_justpass_log_TEH<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_TEH[[i]]<-ROC
}

ROC_ICS_justpass_log_TEH<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log_TEH[[i]]<-ROC
}

######TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_log_TE<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("cohort","gender","fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_TE[[i]]<-ROC
}

ROC_CD_log_TE<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("cohort","gender","fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_TE[[i]]<-ROC
}

ROC_ICS_log_TE<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("cohort","gender","fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_TE[[i]]<-ROC
}

ROC_PS_justpass_log_TE<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_TE[[i]]<-ROC
}

ROC_CD_justpass_log_TE<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_TE[[i]]<-ROC
}

ROC_ICS_justpass_log_TE<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log_TE[[i]]<-ROC
}

######HIDE NETWORK PREDICTORS######

ROC_PS_log_H<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("cohort","gender","fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_H[[i]]<-ROC
}

ROC_CD_log_H<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("cohort","gender","fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_H[[i]]<-ROC
}

ROC_ICS_log_H<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("cohort","gender","fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_H[[i]]<-ROC
}

ROC_PS_justpass_log_H<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_H[[i]]<-ROC
}

ROC_CD_justpass_log_H<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_H[[i]]<-ROC
}

ROC_ICS_justpass_log_H<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log_H[[i]]<-ROC
}

######PAGERANK NETWORK PREDICTORS######

ROC_PS_log_PR<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("cohort","gender","fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_PR[[i]]<-ROC
}

ROC_CD_log_PR<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("cohort","gender","fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_PR[[i]]<-ROC
}

ROC_ICS_log_PR<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("cohort","gender","fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_PR[[i]]<-ROC
}

ROC_PS_justpass_log_PR<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_PR[[i]]<-ROC
}

ROC_CD_justpass_log_PR<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_PR[[i]]<-ROC
}

ROC_ICS_justpass_log_PR<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log_PR[[i]]<-ROC
}

save(ROC_PS_log,ROC_CD_log,ROC_ICS_log, ROC_PS_justpass_log,ROC_CD_justpass_log,ROC_ICS_justpass_log,
     ROC_PS_log_PRTE,ROC_CD_log_PRTE,ROC_ICS_log_PRTE, ROC_PS_justpass_log_PRTE,ROC_CD_justpass_log_PRTE,ROC_ICS_justpass_log_PRTE,
     ROC_PS_log_PRH,ROC_CD_log_PRH,ROC_ICS_log_PRH, ROC_PS_justpass_log_PRH,ROC_CD_justpass_log_PRH,ROC_ICS_justpass_log_PRH,
     ROC_PS_log_TEH,ROC_CD_log_TEH,ROC_ICS_log_TEH, ROC_PS_justpass_log_TEH,ROC_CD_justpass_log_TEH,ROC_ICS_justpass_log_TEH,
     ROC_PS_log_H,ROC_CD_log_H,ROC_ICS_log_H, ROC_PS_justpass_log_H,ROC_CD_justpass_log_H,ROC_ICS_justpass_log_H,
     ROC_PS_log_TE,ROC_CD_log_TE,ROC_ICS_log_TE, ROC_PS_justpass_log_TE,ROC_CD_justpass_log_TE,ROC_ICS_justpass_log_TE,
     ROC_PS_log_PR,ROC_CD_log_PR,ROC_ICS_log_PR, ROC_PS_justpass_log_PR,ROC_CD_justpass_log_PR,ROC_ICS_justpass_log_PR,
     file="data/ROC_NetworkMeasuresFCI_C_gender_cohort_logreg.Rdata")

tend<-Sys.time()
tend-tstart

t1<-Sys.time()
#########LINEAR DISCRIMINANT ANALYSIS###########
#########LINEAR DISCRIMINANT ANALYSIS###########
#########LINEAR DISCRIMINANT ANALYSIS ###########
#########LINEAR DISCRIMINANT ANALYSIS ###########

######ALL NETWORK PREDICTORS######

ROC_PS_lda<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_lda[[i]]<-ROC
}

ROC_CD_lda<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_lda[[i]]<-ROC
}

ROC_ICS_lda<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_lda[[i]]<-ROC
}

ROC_PS_justpass_lda<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_lda[[i]]<-ROC
}

ROC_CD_justpass_lda<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_lda[[i]]<-ROC
}

ROC_ICS_justpass_lda<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_lda[[i]]<-ROC
}

######PAGERANK TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_lda_PRTE<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_lda_PRTE[[i]]<-ROC
}

ROC_CD_lda_PRTE<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_lda_PRTE[[i]]<-ROC
}

ROC_ICS_lda_PRTE<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_lda_PRTE[[i]]<-ROC
}

ROC_PS_justpass_lda_PRTE<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_lda_PRTE[[i]]<-ROC
}

ROC_CD_justpass_lda_PRTE<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_lda_PRTE[[i]]<-ROC
}

ROC_ICS_justpass_lda_PRTE<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_lda_PRTE[[i]]<-ROC
}

######PAGERANK HIDE NETWORK PREDICTORS######

ROC_PS_lda_PRH<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_lda_PRH[[i]]<-ROC
}

ROC_CD_lda_PRH<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD,predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_lda_PRH[[i]]<-ROC
}

ROC_ICS_lda_PRH<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS,predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_lda_PRH[[i]]<-ROC
}

ROC_PS_justpass_lda_PRH<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_lda_PRH[[i]]<-ROC
}

ROC_CD_justpass_lda_PRH<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_lda_PRH[[i]]<-ROC
}

ROC_ICS_justpass_lda_PRH<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_lda_PRH[[i]]<-ROC
}

######TARGET ENTROPY HIDE NETWORK PREDICTORS######

ROC_PS_lda_TEH<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_lda_TEH[[i]]<-ROC
}

ROC_CD_lda_TEH<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD,predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_lda_TEH[[i]]<-ROC
}

ROC_ICS_lda_TEH<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS,predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_lda_TEH[[i]]<-ROC
}

ROC_PS_justpass_lda_TEH<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_lda_TEH[[i]]<-ROC
}

ROC_CD_justpass_lda_TEH<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_lda_TEH[[i]]<-ROC
}

ROC_ICS_justpass_lda_TEH<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_lda_TEH[[i]]<-ROC
}

######TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_lda_TE<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("cohort","gender","fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_lda_TE[[i]]<-ROC
}

ROC_CD_lda_TE<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD,predictors = c("cohort","gender","fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_lda_TE[[i]]<-ROC
}

ROC_ICS_lda_TE<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS,predictors = c("cohort","gender","fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_lda_TE[[i]]<-ROC
}

ROC_PS_justpass_lda_TE<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_lda_TE[[i]]<-ROC
}

ROC_CD_justpass_lda_TE<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_lda_TE[[i]]<-ROC
}

ROC_ICS_justpass_lda_TE<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_lda_TE[[i]]<-ROC
}

######HIDE NETWORK PREDICTORS######

ROC_PS_lda_H<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("cohort","gender","fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_lda_H[[i]]<-ROC
}

ROC_CD_lda_H<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD,predictors = c("cohort","gender","fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_lda_H[[i]]<-ROC
}

ROC_ICS_lda_H<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS,predictors = c("cohort","gender","fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_lda_H[[i]]<-ROC
}

ROC_PS_justpass_lda_H<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_lda_H[[i]]<-ROC
}

ROC_CD_justpass_lda_H<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_lda_H[[i]]<-ROC
}

ROC_ICS_justpass_lda_H<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_lda_H[[i]]<-ROC
}

######TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_lda_PR<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("cohort","gender","fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_lda_PR[[i]]<-ROC
}

ROC_CD_lda_PR<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD,predictors = c("cohort","gender","fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_lda_PR[[i]]<-ROC
}

ROC_ICS_lda_PR<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS,predictors = c("cohort","gender","fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_lda_PR[[i]]<-ROC
}

ROC_PS_justpass_lda_PR<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_lda_PR[[i]]<-ROC
}

ROC_CD_justpass_lda_PR<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_lda_PR[[i]]<-ROC
}

ROC_ICS_justpass_lda_PR<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_lda_PR[[i]]<-ROC
}

save(ROC_PS_lda,ROC_CD_lda,ROC_ICS_lda, ROC_PS_justpass_lda,ROC_CD_justpass_lda,ROC_ICS_justpass_lda,
     ROC_PS_lda_PRTE,ROC_CD_lda_PRTE,ROC_ICS_lda_PRTE, ROC_PS_justpass_lda_PRTE,ROC_CD_justpass_lda_PRTE,ROC_ICS_justpass_lda_PRTE,
     ROC_PS_lda_PRH,ROC_CD_lda_PRH,ROC_ICS_lda_PRH, ROC_PS_justpass_lda_PRH,ROC_CD_justpass_lda_PRH,ROC_ICS_justpass_lda_PRH,
     ROC_PS_lda_TEH,ROC_CD_lda_TEH,ROC_ICS_lda_TEH, ROC_PS_justpass_lda_TEH,ROC_CD_justpass_lda_TEH,ROC_ICS_justpass_lda_TEH,
     ROC_PS_lda_H,ROC_CD_lda_H,ROC_ICS_lda_H, ROC_PS_justpass_lda_H,ROC_CD_justpass_lda_H,ROC_ICS_justpass_lda_H,
     ROC_PS_lda_TE,ROC_CD_lda_TE,ROC_ICS_lda_TE, ROC_PS_justpass_lda_TE,ROC_CD_justpass_lda_TE,ROC_ICS_justpass_lda_TE,
     ROC_PS_lda_PR,ROC_CD_lda_PR,ROC_ICS_lda_PR, ROC_PS_justpass_lda_PR,ROC_CD_justpass_lda_PR,ROC_ICS_justpass_lda_PR,
     file="data/ROC_NetworkMeasuresFCI_C_gender_cohort_lda.Rdata")



#########QUADRATIC DISCRIMINANT ANALYSIS###########
#########QUADRATIC DISCRIMINANT ANALYSIS###########
#########QUADRATIC DISCRIMINANT ANALYSIS ###########
#########QUADRATIC DISCRIMINANT ANALYSIS ###########

######ALL NETWORK PREDICTORS######

ROC_PS_qda<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_qda[[i]]<-ROC
}

ROC_CD_qda<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_qda[[i]]<-ROC
}

ROC_ICS_qda<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_qda[[i]]<-ROC
}

ROC_PS_justpass_qda<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_qda[[i]]<-ROC
}

ROC_CD_justpass_qda<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_qda[[i]]<-ROC
}

ROC_ICS_justpass_qda<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_qda[[i]]<-ROC
}

######PAGERANK TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_qda_PRTE<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_qda_PRTE[[i]]<-ROC
}

ROC_CD_qda_PRTE<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_qda_PRTE[[i]]<-ROC
}

ROC_ICS_qda_PRTE<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_qda_PRTE[[i]]<-ROC
}

ROC_PS_justpass_qda_PRTE<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_qda_PRTE[[i]]<-ROC
}

ROC_CD_justpass_qda_PRTE<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_qda_PRTE[[i]]<-ROC
}

ROC_ICS_justpass_qda_PRTE<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_qda_PRTE[[i]]<-ROC
}

######PAGERANK HIDE NETWORK PREDICTORS######

ROC_PS_qda_PRH<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_qda_PRH[[i]]<-ROC
}

ROC_CD_qda_PRH<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD,predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_qda_PRH[[i]]<-ROC
}

ROC_ICS_qda_PRH<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS,predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_qda_PRH[[i]]<-ROC
}

ROC_PS_justpass_qda_PRH<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_qda_PRH[[i]]<-ROC
}

ROC_CD_justpass_qda_PRH<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_qda_PRH[[i]]<-ROC
}

ROC_ICS_justpass_qda_PRH<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_qda_PRH[[i]]<-ROC
}

######TARGET ENTROPY HIDE NETWORK PREDICTORS######

ROC_PS_qda_TEH<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_qda_TEH[[i]]<-ROC
}

ROC_CD_qda_TEH<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD,predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_qda_TEH[[i]]<-ROC
}

ROC_ICS_qda_TEH<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS,predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_qda_TEH[[i]]<-ROC
}

ROC_PS_justpass_qda_TEH<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_qda_TEH[[i]]<-ROC
}

ROC_CD_justpass_qda_TEH<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_qda_TEH[[i]]<-ROC
}

ROC_ICS_justpass_qda_TEH<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_qda_TEH[[i]]<-ROC
}

######TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_qda_TE<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("cohort","gender","fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_qda_TE[[i]]<-ROC
}

ROC_CD_qda_TE<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD,predictors = c("cohort","gender","fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_qda_TE[[i]]<-ROC
}

ROC_ICS_qda_TE<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS,predictors = c("cohort","gender","fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_qda_TE[[i]]<-ROC
}

ROC_PS_justpass_qda_TE<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_qda_TE[[i]]<-ROC
}

ROC_CD_justpass_qda_TE<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_qda_TE[[i]]<-ROC
}

ROC_ICS_justpass_qda_TE<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_qda_TE[[i]]<-ROC
}

######HIDE NETWORK PREDICTORS######

ROC_PS_qda_H<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("cohort","gender","fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_qda_H[[i]]<-ROC
}

ROC_CD_qda_H<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD,predictors = c("cohort","gender","fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_qda_H[[i]]<-ROC
}

ROC_ICS_qda_H<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS,predictors = c("cohort","gender","fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_qda_H[[i]]<-ROC
}

ROC_PS_justpass_qda_H<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_qda_H[[i]]<-ROC
}

ROC_CD_justpass_qda_H<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_qda_H[[i]]<-ROC
}

ROC_ICS_justpass_qda_H<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_qda_H[[i]]<-ROC
}

######TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_qda_PR<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("cohort","gender","fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_qda_PR[[i]]<-ROC
}

ROC_CD_qda_PR<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD,predictors = c("cohort","gender","fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_qda_PR[[i]]<-ROC
}

ROC_ICS_qda_PR<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS,predictors = c("cohort","gender","fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_qda_PR[[i]]<-ROC
}

ROC_PS_justpass_qda_PR<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_qda_PR[[i]]<-ROC
}

ROC_CD_justpass_qda_PR<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_qda_PR[[i]]<-ROC
}

ROC_ICS_justpass_qda_PR<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_qda_PR[[i]]<-ROC
}

save(ROC_PS_qda,ROC_CD_qda,ROC_ICS_qda, ROC_PS_justpass_qda,ROC_CD_justpass_qda,ROC_ICS_justpass_qda,
     ROC_PS_qda_PRTE,ROC_CD_qda_PRTE,ROC_ICS_qda_PRTE, ROC_PS_justpass_qda_PRTE,ROC_CD_justpass_qda_PRTE,ROC_ICS_justpass_qda_PRTE,
     ROC_PS_qda_PRH,ROC_CD_qda_PRH,ROC_ICS_qda_PRH, ROC_PS_justpass_qda_PRH,ROC_CD_justpass_qda_PRH,ROC_ICS_justpass_qda_PRH,
     ROC_PS_qda_TEH,ROC_CD_qda_TEH,ROC_ICS_qda_TEH, ROC_PS_justpass_qda_TEH,ROC_CD_justpass_qda_TEH,ROC_ICS_justpass_qda_TEH,
     ROC_PS_qda_H,ROC_CD_qda_H,ROC_ICS_qda_H, ROC_PS_justpass_qda_H,ROC_CD_justpass_qda_H,ROC_ICS_justpass_qda_H,
     ROC_PS_qda_TE,ROC_CD_qda_TE,ROC_ICS_qda_TE, ROC_PS_justpass_qda_TE,ROC_CD_justpass_qda_TE,ROC_ICS_justpass_qda_TE,
     ROC_PS_qda_PR,ROC_CD_qda_PR,ROC_ICS_qda_PR, ROC_PS_justpass_qda_PR,ROC_CD_justpass_qda_PR,ROC_ICS_justpass_qda_PR,
     file="data/ROC_NetworkMeasuresFCI_C_gender_cohort_qda.Rdata")



#########K NEAREST NEIGHBORS###########
#########K NEAREST NEIGHBORS###########
#########K NEAREST NEIGHBORS###########
#########K NEAREST NEIGHBORS###########

######ALL NETWORK PREDICTORS######
t1<-Sys.time()
ROC_PS_knn<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_knn[[i]]<-ROC
}

ROC_CD_knn<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn[[i]]<-ROC
}

ROC_ICS_knn<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn[[i]]<-ROC
}

ROC_PS_justpass_knn<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn[[i]]<-ROC
}

ROC_CD_justpass_knn<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn[[i]]<-ROC
}

ROC_ICS_justpass_knn<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn[[i]]<-ROC
}

######PAGERANK TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_knn_PRTE<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_knn_PRTE[[i]]<-ROC
}

ROC_CD_knn_PRTE<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_PRTE[[i]]<-ROC
}

ROC_ICS_knn_PRTE<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_PRTE[[i]]<-ROC
}

ROC_PS_justpass_knn_PRTE<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_PRTE[[i]]<-ROC
}

ROC_CD_justpass_knn_PRTE<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_PRTE[[i]]<-ROC
}

ROC_ICS_justpass_knn_PRTE<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_PRTE[[i]]<-ROC
}

######PAGERANK HIDE NETWORK PREDICTORS######

ROC_PS_knn_PRH<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_knn_PRH[[i]]<-ROC
}

ROC_CD_knn_PRH<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_PRH[[i]]<-ROC
}

ROC_ICS_knn_PRH<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_PRH[[i]]<-ROC
}

ROC_PS_justpass_knn_PRH<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_PRH[[i]]<-ROC
}

ROC_CD_justpass_knn_PRH<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_PRH[[i]]<-ROC
}

ROC_ICS_justpass_knn_PRH<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_PRH[[i]]<-ROC
}

######TARGET ENTROPY HIDE NETWORK PREDICTORS######

ROC_PS_knn_TEH<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_knn_TEH[[i]]<-ROC
}

ROC_CD_knn_TEH<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_TEH[[i]]<-ROC
}

ROC_ICS_knn_TEH<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_TEH[[i]]<-ROC
}

ROC_PS_justpass_knn_TEH<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_TEH[[i]]<-ROC
}

ROC_CD_justpass_knn_TEH<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_TEH[[i]]<-ROC
}

ROC_ICS_justpass_knn_TEH<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt","Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_TEH[[i]]<-ROC
}

######TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_knn_TE<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,predictors = c("cohort","gender","fci_pre_c","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_knn_TE[[i]]<-ROC
}

ROC_CD_knn_TE<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("cohort","gender","fci_pre_c","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_TE[[i]]<-ROC
}

ROC_ICS_knn_TE<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("cohort","gender","fci_pre_c","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_TE[[i]]<-ROC
}

ROC_PS_justpass_knn_TE<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_TE[[i]]<-ROC
}

ROC_CD_justpass_knn_TE<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_TE[[i]]<-ROC
}

ROC_ICS_justpass_knn_TE<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_TE[[i]]<-ROC
}

######HIDE NETWORK PREDICTORS######

ROC_PS_knn_H<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,predictors = c("cohort","gender","fci_pre_c","Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_knn_H[[i]]<-ROC
}

ROC_CD_knn_H<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("cohort","gender","fci_pre_c","Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_H[[i]]<-ROC
}

ROC_ICS_knn_H<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("cohort","gender","fci_pre_c","Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_H[[i]]<-ROC
}

ROC_PS_justpass_knn_H<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_H[[i]]<-ROC
}

ROC_CD_justpass_knn_H<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_H[[i]]<-ROC
}

ROC_ICS_justpass_knn_H<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_H[[i]]<-ROC
}

######TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_knn_PR<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,predictors = c("cohort","gender","fci_pre_c","PageRank"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_knn_PR[[i]]<-ROC
}

ROC_CD_knn_PR<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("cohort","gender","fci_pre_c","PageRank"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_PR[[i]]<-ROC
}

ROC_ICS_knn_PR<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("cohort","gender","fci_pre_c","PageRank"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_PR[[i]]<-ROC
}

ROC_PS_justpass_knn_PR<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_PR[[i]]<-ROC
}

ROC_CD_justpass_knn_PR<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_PR[[i]]<-ROC
}

ROC_ICS_justpass_knn_PR<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("cohort","gender","fci_pre_c","PageRank"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_PR[[i]]<-ROC
}

save(ROC_PS_knn,ROC_CD_knn,ROC_ICS_knn, ROC_PS_justpass_knn,ROC_CD_justpass_knn,ROC_ICS_justpass_knn,
     ROC_PS_knn_PRTE,ROC_CD_knn_PRTE,ROC_ICS_knn_PRTE, ROC_PS_justpass_knn_PRTE,ROC_CD_justpass_knn_PRTE,ROC_ICS_justpass_knn_PRTE,
     ROC_PS_knn_PRH,ROC_CD_knn_PRH,ROC_ICS_knn_PRH, ROC_PS_justpass_knn_PRH,ROC_CD_justpass_knn_PRH,ROC_ICS_justpass_knn_PRH,
     ROC_PS_knn_TEH,ROC_CD_knn_TEH,ROC_ICS_knn_TEH, ROC_PS_justpass_knn_TEH,ROC_CD_justpass_knn_TEH,ROC_ICS_justpass_knn_TEH,
     ROC_PS_knn_H,ROC_CD_knn_H,ROC_ICS_knn_H, ROC_PS_justpass_knn_H,ROC_CD_justpass_knn_H,ROC_ICS_justpass_knn_H,
     ROC_PS_knn_TE,ROC_CD_knn_TE,ROC_ICS_knn_TE, ROC_PS_justpass_knn_TE,ROC_CD_justpass_knn_TE,ROC_ICS_justpass_knn_TE,
     ROC_PS_knn_PR,ROC_CD_knn_PR,ROC_ICS_knn_PR, ROC_PS_justpass_knn_PR,ROC_CD_justpass_knn_PR,ROC_ICS_justpass_knn_PR,
     file="data/ROC_NetworkMeasuresFCI_C_gender_cohort_knn.Rdata")

t2<-Sys.time()
t2-t1

#######################################################
###LOAD CALCULATIONS FROM log reg, lda, qdua and KNN###
#######################################################
load("data/ROC_NetworkMeasuresFCI_C_gender_cohort_logreg.Rdata")
load("data/ROC_NetworkMeasuresFCI_C_gender_cohort_lda.Rdata")
load("data/ROC_NetworkMeasuresFCI_C_gender_cohort_qda.Rdata")
load("data/ROC_NetworkMeasuresFCI_C_gender_cohort_knn.Rdata")

#OUTCOME = PASSED
#Logistic Regression

ROC_PS_log_pfm<-performanceMeasures(ROC_PS_log)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_log_all<-AUC(ROC_PS_log_pfm[[1]],ROC_PS_log_pfm[[2]])

ROC_CD_log_pfm<-performanceMeasures(ROC_CD_log)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_log_all<-AUC(ROC_CD_log_pfm[[1]],ROC_CD_log_pfm[[2]])

ROC_ICS_log_pfm<-performanceMeasures(ROC_ICS_log)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_log_all<-AUC(ROC_ICS_log_pfm[[1]],ROC_ICS_log_pfm[[2]])

ROC_PS_log_pfm_PRTE<-performanceMeasures(ROC_PS_log_PRTE)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_log_PRTE<-AUC(ROC_PS_log_pfm_PRTE[[1]],ROC_PS_log_pfm_PRTE[[2]])

ROC_CD_log_pfm_PRTE<-performanceMeasures(ROC_CD_log_PRTE)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_log_PRTE<-AUC(ROC_CD_log_pfm_PRTE[[1]],ROC_CD_log_pfm_PRTE[[2]])

ROC_ICS_log_pfm_PRTE<-performanceMeasures(ROC_ICS_log_PRTE)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_log_PRTE<-AUC(ROC_ICS_log_pfm_PRTE[[1]],ROC_ICS_log_pfm_PRTE[[2]])


ROC_PS_log_pfm_PRH<-performanceMeasures(ROC_PS_log_PRH)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_log_PRH<-AUC(ROC_PS_log_pfm_PRH[[1]],ROC_PS_log_pfm_PRH[[2]])

ROC_CD_log_pfm_PRH<-performanceMeasures(ROC_CD_log_PRH)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_log_PRH<-AUC(ROC_CD_log_pfm_PRH[[1]],ROC_CD_log_pfm_PRH[[2]])

ROC_ICS_log_pfm_PRH<-performanceMeasures(ROC_ICS_log_PRH)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_log_PRH<-AUC(ROC_ICS_log_pfm_PRH[[1]],ROC_ICS_log_pfm_PRH[[2]])


ROC_PS_log_pfm_TEH<-performanceMeasures(ROC_PS_log_TEH)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_log_TEH<-AUC(ROC_PS_log_pfm_TEH[[1]],ROC_PS_log_pfm_TEH[[2]])

ROC_CD_log_pfm_TEH<-performanceMeasures(ROC_CD_log_TEH)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_log_TEH<-AUC(ROC_CD_log_pfm_TEH[[1]],ROC_CD_log_pfm_TEH[[2]])

ROC_ICS_log_pfm_TEH<-performanceMeasures(ROC_ICS_log_TEH)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_log_TEH<-AUC(ROC_ICS_log_pfm_TEH[[1]],ROC_ICS_log_pfm_TEH[[2]])

ROC_PS_log_pfm_PR<-performanceMeasures(ROC_PS_log_PR)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_log_PR<-AUC(ROC_PS_log_pfm_PR[[1]],ROC_PS_log_pfm_PR[[2]])

ROC_CD_log_pfm_PR<-performanceMeasures(ROC_CD_log_PR)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_log_PR<-AUC(ROC_CD_log_pfm_PR[[1]],ROC_CD_log_pfm_PR[[2]])

ROC_ICS_log_pfm_PR<-performanceMeasures(ROC_ICS_log_PR)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_log_PR<-AUC(ROC_ICS_log_pfm_PR[[1]],ROC_ICS_log_pfm_PR[[2]])

ROC_PS_log_pfm_TE<-performanceMeasures(ROC_PS_log_TE)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_log_TE<-AUC(ROC_PS_log_pfm_TE[[1]],ROC_PS_log_pfm_TE[[2]])

ROC_CD_log_pfm_TE<-performanceMeasures(ROC_CD_log_TE)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_log_TE<-AUC(ROC_CD_log_pfm_TE[[1]],ROC_CD_log_pfm_TE[[2]])

ROC_ICS_log_pfm_TE<-performanceMeasures(ROC_ICS_log_TE)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_log_TE<-AUC(ROC_ICS_log_pfm_TE[[1]],ROC_ICS_log_pfm_TE[[2]])

ROC_PS_log_pfm_H<-performanceMeasures(ROC_PS_log_H)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_log_H<-AUC(ROC_PS_log_pfm_H[[1]],ROC_PS_log_pfm_H[[2]])

ROC_CD_log_pfm_H<-performanceMeasures(ROC_CD_log_H)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_log_H<-AUC(ROC_CD_log_pfm_H[[1]],ROC_CD_log_pfm_H[[2]])

ROC_ICS_log_pfm_H<-performanceMeasures(ROC_ICS_log_H)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_log_H<-AUC(ROC_ICS_log_pfm_H[[1]],ROC_ICS_log_pfm_H[[2]])


#########LDA

ROC_PS_lda_pfm<-performanceMeasures(ROC_PS_log)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_lda_all<-AUC(ROC_PS_lda_pfm[[1]],ROC_PS_lda_pfm[[2]])

ROC_CD_lda_pfm<-performanceMeasures(ROC_CD_log)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_lda_all<-AUC(ROC_CD_lda_pfm[[1]],ROC_CD_lda_pfm[[2]])

ROC_ICS_lda_pfm<-performanceMeasures(ROC_ICS_log)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_lda_all<-AUC(ROC_ICS_lda_pfm[[1]],ROC_ICS_lda_pfm[[2]])

ROC_PS_lda_pfm_PRTE<-performanceMeasures(ROC_PS_lda_PRTE)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_lda_PRTE<-AUC(ROC_PS_lda_pfm_PRTE[[1]],ROC_PS_lda_pfm_PRTE[[2]])

ROC_CD_lda_pfm_PRTE<-performanceMeasures(ROC_CD_lda_PRTE)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_lda_PRTE<-AUC(ROC_CD_lda_pfm_PRTE[[1]],ROC_CD_lda_pfm_PRTE[[2]])

ROC_ICS_lda_pfm_PRTE<-performanceMeasures(ROC_ICS_lda_PRTE)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_lda_PRTE<-AUC(ROC_ICS_lda_pfm_PRTE[[1]],ROC_ICS_lda_pfm_PRTE[[2]])


ROC_PS_lda_pfm_PRH<-performanceMeasures(ROC_PS_lda_PRH)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_lda_PRH<-AUC(ROC_PS_lda_pfm_PRH[[1]],ROC_PS_lda_pfm_PRH[[2]])

ROC_CD_lda_pfm_PRH<-performanceMeasures(ROC_CD_lda_PRH)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_lda_PRH<-AUC(ROC_CD_lda_pfm_PRH[[1]],ROC_CD_lda_pfm_PRH[[2]])

ROC_ICS_lda_pfm_PRH<-performanceMeasures(ROC_ICS_lda_PRH)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_lda_PRH<-AUC(ROC_ICS_lda_pfm_PRH[[1]],ROC_ICS_lda_pfm_PRH[[2]])


ROC_PS_lda_pfm_TEH<-performanceMeasures(ROC_PS_lda_TEH)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_lda_TEH<-AUC(ROC_PS_lda_pfm_TEH[[1]],ROC_PS_lda_pfm_TEH[[2]])

ROC_CD_lda_pfm_TEH<-performanceMeasures(ROC_CD_lda_TEH)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_lda_TEH<-AUC(ROC_CD_lda_pfm_TEH[[1]],ROC_CD_lda_pfm_TEH[[2]])

ROC_ICS_lda_pfm_TEH<-performanceMeasures(ROC_ICS_lda_TEH)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_lda_TEH<-AUC(ROC_ICS_lda_pfm_TEH[[1]],ROC_ICS_lda_pfm_TEH[[2]])

ROC_PS_lda_pfm_PR<-performanceMeasures(ROC_PS_lda_PR)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_lda_PR<-AUC(ROC_PS_lda_pfm_PR[[1]],ROC_PS_lda_pfm_PR[[2]])

ROC_CD_lda_pfm_PR<-performanceMeasures(ROC_CD_lda_PR)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_lda_PR<-AUC(ROC_CD_lda_pfm_PR[[1]],ROC_CD_lda_pfm_PR[[2]])

ROC_ICS_lda_pfm_PR<-performanceMeasures(ROC_ICS_lda_PR)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_lda_PR<-AUC(ROC_ICS_lda_pfm_PR[[1]],ROC_ICS_lda_pfm_PR[[2]])

ROC_PS_lda_pfm_TE<-performanceMeasures(ROC_PS_lda_TE)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_lda_TE<-AUC(ROC_PS_lda_pfm_TE[[1]],ROC_PS_lda_pfm_TE[[2]])

ROC_CD_lda_pfm_TE<-performanceMeasures(ROC_CD_lda_TE)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_lda_TE<-AUC(ROC_CD_lda_pfm_TE[[1]],ROC_CD_lda_pfm_TE[[2]])

ROC_ICS_lda_pfm_TE<-performanceMeasures(ROC_ICS_lda_TE)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_lda_TE<-AUC(ROC_ICS_lda_pfm_TE[[1]],ROC_ICS_lda_pfm_TE[[2]])

ROC_PS_lda_pfm_H<-performanceMeasures(ROC_PS_lda_H)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_lda_H<-AUC(ROC_PS_lda_pfm_H[[1]],ROC_PS_lda_pfm_H[[2]])

ROC_CD_lda_pfm_H<-performanceMeasures(ROC_CD_lda_H)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_lda_H<-AUC(ROC_CD_lda_pfm_H[[1]],ROC_CD_lda_pfm_H[[2]])

ROC_ICS_lda_pfm_H<-performanceMeasures(ROC_ICS_lda_H)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_lda_H<-AUC(ROC_ICS_lda_pfm_H[[1]],ROC_ICS_lda_pfm_H[[2]])

#QDA

ROC_PS_qda_pfm<-performanceMeasures(ROC_PS_log)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_qda_all<-AUC(ROC_PS_qda_pfm[[1]],ROC_PS_qda_pfm[[2]])

ROC_CD_qda_pfm<-performanceMeasures(ROC_CD_log)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_qda_all<-AUC(ROC_CD_qda_pfm[[1]],ROC_CD_qda_pfm[[2]])

ROC_ICS_qda_pfm<-performanceMeasures(ROC_ICS_log)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_qda_all<-AUC(ROC_ICS_qda_pfm[[1]],ROC_ICS_qda_pfm[[2]])

ROC_PS_qda_pfm_PRTE<-performanceMeasures(ROC_PS_qda_PRTE)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_qda_PRTE<-AUC(ROC_PS_qda_pfm_PRTE[[1]],ROC_PS_qda_pfm_PRTE[[2]])

ROC_CD_qda_pfm_PRTE<-performanceMeasures(ROC_CD_qda_PRTE)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_qda_PRTE<-AUC(ROC_CD_qda_pfm_PRTE[[1]],ROC_CD_qda_pfm_PRTE[[2]])

ROC_ICS_qda_pfm_PRTE<-performanceMeasures(ROC_ICS_qda_PRTE)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_qda_PRTE<-AUC(ROC_ICS_qda_pfm_PRTE[[1]],ROC_ICS_qda_pfm_PRTE[[2]])


ROC_PS_qda_pfm_PRH<-performanceMeasures(ROC_PS_qda_PRH)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_qda_PRH<-AUC(ROC_PS_qda_pfm_PRH[[1]],ROC_PS_qda_pfm_PRH[[2]])

ROC_CD_qda_pfm_PRH<-performanceMeasures(ROC_CD_qda_PRH)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_qda_PRH<-AUC(ROC_CD_qda_pfm_PRH[[1]],ROC_CD_qda_pfm_PRH[[2]])

ROC_ICS_qda_pfm_PRH<-performanceMeasures(ROC_ICS_qda_PRH)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_qda_PRH<-AUC(ROC_ICS_qda_pfm_PRH[[1]],ROC_ICS_qda_pfm_PRH[[2]])


ROC_PS_qda_pfm_TEH<-performanceMeasures(ROC_PS_qda_TEH)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_qda_TEH<-AUC(ROC_PS_qda_pfm_TEH[[1]],ROC_PS_qda_pfm_TEH[[2]])

ROC_CD_qda_pfm_TEH<-performanceMeasures(ROC_CD_qda_TEH)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_qda_TEH<-AUC(ROC_CD_qda_pfm_TEH[[1]],ROC_CD_qda_pfm_TEH[[2]])

ROC_ICS_qda_pfm_TEH<-performanceMeasures(ROC_ICS_qda_TEH)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_qda_TEH<-AUC(ROC_ICS_qda_pfm_TEH[[1]],ROC_ICS_qda_pfm_TEH[[2]])

ROC_PS_qda_pfm_PR<-performanceMeasures(ROC_PS_qda_PR)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_qda_PR<-AUC(ROC_PS_qda_pfm_PR[[1]],ROC_PS_qda_pfm_PR[[2]])

ROC_CD_qda_pfm_PR<-performanceMeasures(ROC_CD_qda_PR)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_qda_PR<-AUC(ROC_CD_qda_pfm_PR[[1]],ROC_CD_qda_pfm_PR[[2]])

ROC_ICS_qda_pfm_PR<-performanceMeasures(ROC_ICS_qda_PR)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_qda_PR<-AUC(ROC_ICS_qda_pfm_PR[[1]],ROC_ICS_qda_pfm_PR[[2]])

ROC_PS_qda_pfm_TE<-performanceMeasures(ROC_PS_qda_TE)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_qda_TE<-AUC(ROC_PS_qda_pfm_TE[[1]],ROC_PS_qda_pfm_TE[[2]])

ROC_CD_qda_pfm_TE<-performanceMeasures(ROC_CD_qda_TE)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_qda_TE<-AUC(ROC_CD_qda_pfm_TE[[1]],ROC_CD_qda_pfm_TE[[2]])

ROC_ICS_qda_pfm_TE<-performanceMeasures(ROC_ICS_qda_TE)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_qda_TE<-AUC(ROC_ICS_qda_pfm_TE[[1]],ROC_ICS_qda_pfm_TE[[2]])

ROC_PS_qda_pfm_H<-performanceMeasures(ROC_PS_qda_H)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_qda_H<-AUC(ROC_PS_qda_pfm_H[[1]],ROC_PS_qda_pfm_H[[2]])

ROC_CD_qda_pfm_H<-performanceMeasures(ROC_CD_qda_H)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_qda_H<-AUC(ROC_CD_qda_pfm_H[[1]],ROC_CD_qda_pfm_H[[2]])

ROC_ICS_qda_pfm_H<-performanceMeasures(ROC_ICS_qda_H)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_qda_H<-AUC(ROC_ICS_qda_pfm_H[[1]],ROC_ICS_qda_pfm_H[[2]])

#KNN
ROC_PS_knn_pfm<-performanceMeasures(ROC_PS_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_cohort_knn_all<-colMeans(ROC_PS_knn_pfm[[4]])

ROC_CD_knn_pfm<-performanceMeasures(ROC_CD_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_cohort_knn_all<-colMeans(ROC_CD_knn_pfm[[4]])

ROC_ICS_knn_pfm<-performanceMeasures(ROC_ICS_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_cohort_knn_all<-colMeans(ROC_ICS_knn_pfm[[4]])

ROC_PS_knn_pfm_PRTE<-performanceMeasures(ROC_PS_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_cohort_knn_PRTE<-colMeans(ROC_PS_knn_pfm_PRTE[[4]])

ROC_CD_knn_pfm_PRTE<-performanceMeasures(ROC_CD_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_cohort_knn_PRTE<-colMeans(ROC_CD_knn_pfm_PRTE[[4]])

ROC_ICS_knn_pfm_PRTE<-performanceMeasures(ROC_ICS_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_cohort_knn_PRTE<-colMeans(ROC_ICS_knn_pfm_PRTE[[4]])

ROC_PS_knn_pfm_PRH<-performanceMeasures(ROC_PS_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_cohort_knn_PRH<-colMeans(ROC_PS_knn_pfm_PRH[[4]])

ROC_CD_knn_pfm_PRH<-performanceMeasures(ROC_CD_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_cohort_knn_PRH<-colMeans(ROC_CD_knn_pfm_PRH[[4]])

ROC_ICS_knn_pfm_PRH<-performanceMeasures(ROC_ICS_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_cohort_knn_PRH<-colMeans(ROC_ICS_knn_pfm_PRH[[4]])

ROC_PS_knn_pfm_TEH<-performanceMeasures(ROC_PS_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_cohort_knn_TEH<-colMeans(ROC_PS_knn_pfm_TEH[[4]])

ROC_CD_knn_pfm_TEH<-performanceMeasures(ROC_CD_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_cohort_knn_TEH<-colMeans(ROC_CD_knn_pfm_TEH[[4]])

ROC_ICS_knn_pfm_TEH<-performanceMeasures(ROC_ICS_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_cohort_knn_TEH<-colMeans(ROC_ICS_knn_pfm_TEH[[4]])

ROC_PS_knn_pfm_PR<-performanceMeasures(ROC_PS_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_cohort_knn_PR<-colMeans(ROC_PS_knn_pfm_PR[[4]])

ROC_CD_knn_pfm_PR<-performanceMeasures(ROC_CD_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_cohort_knn_PR<-colMeans(ROC_CD_knn_pfm_PR[[4]])

ROC_ICS_knn_pfm_PR<-performanceMeasures(ROC_ICS_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_cohort_knn_PR<-colMeans(ROC_ICS_knn_pfm_PR[[4]])

ROC_PS_knn_pfm_TE<-performanceMeasures(ROC_PS_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_cohort_knn_TE<-colMeans(ROC_PS_knn_pfm_TE[[4]])

ROC_CD_knn_pfm_TE<-performanceMeasures(ROC_CD_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_cohort_knn_TE<-colMeans(ROC_CD_knn_pfm_TE[[4]])

ROC_ICS_knn_pfm_TE<-performanceMeasures(ROC_ICS_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_cohort_knn_TE<-colMeans(ROC_ICS_knn_pfm_TE[[4]])

ROC_PS_knn_pfm_H<-performanceMeasures(ROC_PS_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_cohort_knn_all<-colMeans(ROC_PS_knn_pfm_H[[4]])

ROC_CD_knn_pfm_H<-performanceMeasures(ROC_CD_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_cohort_knn_H<-colMeans(ROC_CD_knn_pfm_H[[4]])

ROC_ICS_knn_pfm_H<-performanceMeasures(ROC_ICS_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_cohort_knn_H<-colMeans(ROC_ICS_knn_pfm_H[[4]])

#OUTCOME = JUSTPASSED
#Logistic Regression

ROC_PS_justpass_log_pfm<-performanceMeasures(ROC_PS_justpass_log)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_log_all<-AUC(ROC_PS_justpass_log_pfm[[1]],ROC_PS_justpass_log_pfm[[2]])

ROC_CD_justpass_log_pfm<-performanceMeasures(ROC_CD_justpass_log)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_log_all<-AUC(ROC_CD_justpass_log_pfm[[1]],ROC_CD_justpass_log_pfm[[2]])

ROC_ICS_justpass_log_pfm<-performanceMeasures(ROC_ICS_justpass_log)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_log_all<-AUC(ROC_ICS_justpass_log_pfm[[1]],ROC_ICS_justpass_log_pfm[[2]])



ROC_PS_justpass_log_pfm_PRTE<-performanceMeasures(ROC_PS_justpass_log_PRTE)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_log_PRTE<-AUC(ROC_PS_justpass_log_pfm_PRTE[[1]],ROC_PS_justpass_log_pfm_PRTE[[2]])

ROC_CD_justpass_log_pfm_PRTE<-performanceMeasures(ROC_CD_justpass_log_PRTE)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_log_PRTE<-AUC(ROC_CD_justpass_log_pfm_PRTE[[1]],ROC_CD_justpass_log_pfm_PRTE[[2]])

ROC_ICS_justpass_log_pfm_PRTE<-performanceMeasures(ROC_ICS_justpass_log_PRTE)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_log_PRTE<-AUC(ROC_ICS_justpass_log_pfm_PRTE[[1]],ROC_ICS_justpass_log_pfm_PRTE[[2]])


ROC_PS_justpass_log_pfm_PRH<-performanceMeasures(ROC_PS_justpass_log_PRH)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_log_PRH<-AUC(ROC_PS_justpass_log_pfm_PRH[[1]],ROC_PS_justpass_log_pfm_PRH[[2]])

ROC_CD_justpass_log_pfm_PRH<-performanceMeasures(ROC_CD_justpass_log_PRH)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_log_PRH<-AUC(ROC_CD_justpass_log_pfm_PRH[[1]],ROC_CD_justpass_log_pfm_PRH[[2]])

ROC_ICS_justpass_log_pfm_PRH<-performanceMeasures(ROC_ICS_justpass_log_PRH)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_log_PRH<-AUC(ROC_ICS_justpass_log_pfm_PRH[[1]],ROC_ICS_justpass_log_pfm_PRH[[2]])


ROC_PS_justpass_log_pfm_TEH<-performanceMeasures(ROC_PS_justpass_log_TEH)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_log_TEH<-AUC(ROC_PS_justpass_log_pfm_TEH[[1]],ROC_PS_justpass_log_pfm_TEH[[2]])

ROC_CD_justpass_log_pfm_TEH<-performanceMeasures(ROC_CD_justpass_log_TEH)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_log_TEH<-AUC(ROC_CD_justpass_log_pfm_TEH[[1]],ROC_CD_justpass_log_pfm_TEH[[2]])

ROC_ICS_justpass_log_pfm_TEH<-performanceMeasures(ROC_ICS_justpass_log_TEH)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_log_TEH<-AUC(ROC_ICS_justpass_log_pfm_TEH[[1]],ROC_ICS_justpass_log_pfm_TEH[[2]])

ROC_PS_justpass_log_pfm_PR<-performanceMeasures(ROC_PS_justpass_log_PR)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_log_PR<-AUC(ROC_PS_justpass_log_pfm_PR[[1]],ROC_PS_justpass_log_pfm_PR[[2]])

ROC_CD_justpass_log_pfm_PR<-performanceMeasures(ROC_CD_justpass_log_PR)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_log_PR<-AUC(ROC_CD_justpass_log_pfm_PR[[1]],ROC_CD_justpass_log_pfm_PR[[2]])

ROC_ICS_justpass_log_pfm_PR<-performanceMeasures(ROC_ICS_justpass_log_PR)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_log_PR<-AUC(ROC_ICS_justpass_log_pfm_PR[[1]],ROC_ICS_justpass_log_pfm_PR[[2]])

ROC_PS_justpass_log_pfm_TE<-performanceMeasures(ROC_PS_justpass_log_TE)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_log_TE<-AUC(ROC_PS_justpass_log_pfm_TE[[1]],ROC_PS_justpass_log_pfm_TE[[2]])

ROC_CD_justpass_log_pfm_TE<-performanceMeasures(ROC_CD_justpass_log_TE)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_log_TE<-AUC(ROC_CD_justpass_log_pfm_TE[[1]],ROC_CD_justpass_log_pfm_TE[[2]])

ROC_ICS_justpass_log_pfm_TE<-performanceMeasures(ROC_ICS_justpass_log_TE)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_log_TE<-AUC(ROC_ICS_justpass_log_pfm_TE[[1]],ROC_ICS_justpass_log_pfm_TE[[2]])

ROC_PS_justpass_log_pfm_H<-performanceMeasures(ROC_PS_justpass_log_H)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_log_H<-AUC(ROC_PS_justpass_log_pfm_H[[1]],ROC_PS_justpass_log_pfm_H[[2]])

ROC_CD_justpass_log_pfm_H<-performanceMeasures(ROC_CD_justpass_log_H)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_log_H<-AUC(ROC_CD_justpass_log_pfm_H[[1]],ROC_CD_justpass_log_pfm_H[[2]])

ROC_ICS_justpass_log_pfm_H<-performanceMeasures(ROC_ICS_justpass_log_H)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_log_H<-AUC(ROC_ICS_justpass_log_pfm_H[[1]],ROC_ICS_justpass_log_pfm_H[[2]])


#########LDA

ROC_PS_justpass_lda_pfm<-performanceMeasures(ROC_PS_justpass_log)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_lda_all<-AUC(ROC_PS_justpass_lda_pfm[[1]],ROC_PS_justpass_lda_pfm[[2]])

ROC_CD_justpass_lda_pfm<-performanceMeasures(ROC_CD_justpass_log)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_lda_all<-AUC(ROC_CD_justpass_lda_pfm[[1]],ROC_CD_justpass_lda_pfm[[2]])

ROC_ICS_justpass_lda_pfm<-performanceMeasures(ROC_ICS_justpass_log)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_lda_all<-AUC(ROC_ICS_justpass_lda_pfm[[1]],ROC_ICS_justpass_lda_pfm[[2]])

ROC_PS_justpass_lda_pfm_PRTE<-performanceMeasures(ROC_PS_justpass_lda_PRTE)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_lda_PRTE<-AUC(ROC_PS_justpass_lda_pfm_PRTE[[1]],ROC_PS_justpass_lda_pfm_PRTE[[2]])

ROC_CD_justpass_lda_pfm_PRTE<-performanceMeasures(ROC_CD_justpass_lda_PRTE)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_lda_PRTE<-AUC(ROC_CD_justpass_lda_pfm_PRTE[[1]],ROC_CD_justpass_lda_pfm_PRTE[[2]])

ROC_ICS_justpass_lda_pfm_PRTE<-performanceMeasures(ROC_ICS_justpass_lda_PRTE)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_lda_PRTE<-AUC(ROC_ICS_justpass_lda_pfm_PRTE[[1]],ROC_ICS_justpass_lda_pfm_PRTE[[2]])


ROC_PS_justpass_lda_pfm_PRH<-performanceMeasures(ROC_PS_justpass_lda_PRH)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_lda_PRH<-AUC(ROC_PS_justpass_lda_pfm_PRH[[1]],ROC_PS_justpass_lda_pfm_PRH[[2]])

ROC_CD_justpass_lda_pfm_PRH<-performanceMeasures(ROC_CD_justpass_lda_PRH)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_lda_PRH<-AUC(ROC_CD_justpass_lda_pfm_PRH[[1]],ROC_CD_justpass_lda_pfm_PRH[[2]])

ROC_ICS_justpass_lda_pfm_PRH<-performanceMeasures(ROC_ICS_justpass_lda_PRH)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_lda_PRH<-AUC(ROC_ICS_justpass_lda_pfm_PRH[[1]],ROC_ICS_justpass_lda_pfm_PRH[[2]])


ROC_PS_justpass_lda_pfm_TEH<-performanceMeasures(ROC_PS_justpass_lda_TEH)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_lda_TEH<-AUC(ROC_PS_justpass_lda_pfm_TEH[[1]],ROC_PS_justpass_lda_pfm_TEH[[2]])

ROC_CD_justpass_lda_pfm_TEH<-performanceMeasures(ROC_CD_justpass_lda_TEH)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_lda_TEH<-AUC(ROC_CD_justpass_lda_pfm_TEH[[1]],ROC_CD_justpass_lda_pfm_TEH[[2]])

ROC_ICS_justpass_lda_pfm_TEH<-performanceMeasures(ROC_ICS_justpass_lda_TEH)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_lda_TEH<-AUC(ROC_ICS_justpass_lda_pfm_TEH[[1]],ROC_ICS_justpass_lda_pfm_TEH[[2]])

ROC_PS_justpass_lda_pfm_PR<-performanceMeasures(ROC_PS_justpass_lda_PR)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_lda_PR<-AUC(ROC_PS_justpass_lda_pfm_PR[[1]],ROC_PS_justpass_lda_pfm_PR[[2]])

ROC_CD_justpass_lda_pfm_PR<-performanceMeasures(ROC_CD_justpass_lda_PR)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_lda_PR<-AUC(ROC_CD_justpass_lda_pfm_PR[[1]],ROC_CD_justpass_lda_pfm_PR[[2]])

ROC_ICS_justpass_lda_pfm_PR<-performanceMeasures(ROC_ICS_justpass_lda_PR)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_lda_PR<-AUC(ROC_ICS_justpass_lda_pfm_PR[[1]],ROC_ICS_justpass_lda_pfm_PR[[2]])

ROC_PS_justpass_lda_pfm_TE<-performanceMeasures(ROC_PS_justpass_lda_TE)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_lda_TE<-AUC(ROC_PS_justpass_lda_pfm_TE[[1]],ROC_PS_justpass_lda_pfm_TE[[2]])

ROC_CD_justpass_lda_pfm_TE<-performanceMeasures(ROC_CD_justpass_lda_TE)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_lda_TE<-AUC(ROC_CD_justpass_lda_pfm_TE[[1]],ROC_CD_justpass_lda_pfm_TE[[2]])

ROC_ICS_justpass_lda_pfm_TE<-performanceMeasures(ROC_ICS_justpass_lda_TE)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_lda_TE<-AUC(ROC_ICS_justpass_lda_pfm_TE[[1]],ROC_ICS_justpass_lda_pfm_TE[[2]])

ROC_PS_justpass_lda_pfm_H<-performanceMeasures(ROC_PS_justpass_lda_H)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_lda_H<-AUC(ROC_PS_justpass_lda_pfm_H[[1]],ROC_PS_justpass_lda_pfm_H[[2]])

ROC_CD_justpass_lda_pfm_H<-performanceMeasures(ROC_CD_justpass_lda_H)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_lda_H<-AUC(ROC_CD_justpass_lda_pfm_H[[1]],ROC_CD_justpass_lda_pfm_H[[2]])

ROC_ICS_justpass_lda_pfm_H<-performanceMeasures(ROC_ICS_justpass_lda_H)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_lda_H<-AUC(ROC_ICS_justpass_lda_pfm_H[[1]],ROC_ICS_justpass_lda_pfm_H[[2]])

#QDA

ROC_PS_justpass_qda_pfm<-performanceMeasures(ROC_PS_justpass_log)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_qda_all<-AUC(ROC_PS_justpass_qda_pfm[[1]],ROC_PS_justpass_qda_pfm[[2]])

ROC_CD_justpass_qda_pfm<-performanceMeasures(ROC_CD_justpass_log)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_qda_all<-AUC(ROC_CD_justpass_qda_pfm[[1]],ROC_CD_justpass_qda_pfm[[2]])

ROC_ICS_justpass_qda_pfm<-performanceMeasures(ROC_ICS_justpass_log)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_qda_all<-AUC(ROC_ICS_justpass_qda_pfm[[1]],ROC_ICS_justpass_qda_pfm[[2]])

ROC_PS_justpass_qda_pfm_PRTE<-performanceMeasures(ROC_PS_justpass_qda_PRTE)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_qda_PRTE<-AUC(ROC_PS_justpass_qda_pfm_PRTE[[1]],ROC_PS_justpass_qda_pfm_PRTE[[2]])

ROC_CD_justpass_qda_pfm_PRTE<-performanceMeasures(ROC_CD_justpass_qda_PRTE)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_qda_PRTE<-AUC(ROC_CD_justpass_qda_pfm_PRTE[[1]],ROC_CD_justpass_qda_pfm_PRTE[[2]])

ROC_ICS_justpass_qda_pfm_PRTE<-performanceMeasures(ROC_ICS_justpass_qda_PRTE)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_qda_PRTE<-AUC(ROC_ICS_justpass_qda_pfm_PRTE[[1]],ROC_ICS_justpass_qda_pfm_PRTE[[2]])


ROC_PS_justpass_qda_pfm_PRH<-performanceMeasures(ROC_PS_justpass_qda_PRH)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_qda_PRH<-AUC(ROC_PS_justpass_qda_pfm_PRH[[1]],ROC_PS_justpass_qda_pfm_PRH[[2]])

ROC_CD_justpass_qda_pfm_PRH<-performanceMeasures(ROC_CD_justpass_qda_PRH)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_qda_PRH<-AUC(ROC_CD_justpass_qda_pfm_PRH[[1]],ROC_CD_justpass_qda_pfm_PRH[[2]])

ROC_ICS_justpass_qda_pfm_PRH<-performanceMeasures(ROC_ICS_justpass_qda_PRH)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_qda_PRH<-AUC(ROC_ICS_justpass_qda_pfm_PRH[[1]],ROC_ICS_justpass_qda_pfm_PRH[[2]])


ROC_PS_justpass_qda_pfm_TEH<-performanceMeasures(ROC_PS_justpass_qda_TEH)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_qda_TEH<-AUC(ROC_PS_justpass_qda_pfm_TEH[[1]],ROC_PS_justpass_qda_pfm_TEH[[2]])

ROC_CD_justpass_qda_pfm_TEH<-performanceMeasures(ROC_CD_justpass_qda_TEH)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_qda_TEH<-AUC(ROC_CD_justpass_qda_pfm_TEH[[1]],ROC_CD_justpass_qda_pfm_TEH[[2]])

ROC_ICS_justpass_qda_pfm_TEH<-performanceMeasures(ROC_ICS_justpass_qda_TEH)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_qda_TEH<-AUC(ROC_ICS_justpass_qda_pfm_TEH[[1]],ROC_ICS_justpass_qda_pfm_TEH[[2]])

ROC_PS_justpass_qda_pfm_PR<-performanceMeasures(ROC_PS_justpass_qda_PR)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_qda_PR<-AUC(ROC_PS_justpass_qda_pfm_PR[[1]],ROC_PS_justpass_qda_pfm_PR[[2]])

ROC_CD_justpass_qda_pfm_PR<-performanceMeasures(ROC_CD_justpass_qda_PR)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_qda_PR<-AUC(ROC_CD_justpass_qda_pfm_PR[[1]],ROC_CD_justpass_qda_pfm_PR[[2]])

ROC_ICS_justpass_qda_pfm_PR<-performanceMeasures(ROC_ICS_justpass_qda_PR)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_qda_PR<-AUC(ROC_ICS_justpass_qda_pfm_PR[[1]],ROC_ICS_justpass_qda_pfm_PR[[2]])

ROC_PS_justpass_qda_pfm_TE<-performanceMeasures(ROC_PS_justpass_qda_TE)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_qda_TE<-AUC(ROC_PS_justpass_qda_pfm_TE[[1]],ROC_PS_justpass_qda_pfm_TE[[2]])

ROC_CD_justpass_qda_pfm_TE<-performanceMeasures(ROC_CD_justpass_qda_TE)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_qda_TE<-AUC(ROC_CD_justpass_qda_pfm_TE[[1]],ROC_CD_justpass_qda_pfm_TE[[2]])

ROC_ICS_justpass_qda_pfm_TE<-performanceMeasures(ROC_ICS_justpass_qda_TE)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_qda_TE<-AUC(ROC_ICS_justpass_qda_pfm_TE[[1]],ROC_ICS_justpass_qda_pfm_TE[[2]])

ROC_PS_justpass_qda_pfm_H<-performanceMeasures(ROC_PS_justpass_qda_H)
PS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_qda_H<-AUC(ROC_PS_justpass_qda_pfm_H[[1]],ROC_PS_justpass_qda_pfm_H[[2]])

ROC_CD_justpass_qda_pfm_H<-performanceMeasures(ROC_CD_justpass_qda_H)
CD_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_qda_H<-AUC(ROC_CD_justpass_qda_pfm_H[[1]],ROC_CD_justpass_qda_pfm_H[[2]])

ROC_ICS_justpass_qda_pfm_H<-performanceMeasures(ROC_ICS_justpass_qda_H)
ICS_AUC_NetworkMeasuresFCI_C_gender_cohort_justpass_qda_H<-AUC(ROC_ICS_justpass_qda_pfm_H[[1]],ROC_ICS_justpass_qda_pfm_H[[2]])

#KNN
ROC_PS_justpass_knn_pfm<-performanceMeasures(ROC_PS_justpass_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_cohort_justpass_knn_all<-colMeans(ROC_PS_justpass_knn_pfm[[4]])

ROC_CD_justpass_knn_pfm<-performanceMeasures(ROC_CD_justpass_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_cohort_justpass_knn_all<-colMeans(ROC_CD_justpass_knn_pfm[[4]])

ROC_ICS_justpass_knn_pfm<-performanceMeasures(ROC_ICS_justpass_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_cohort_justpass_knn_all<-colMeans(ROC_ICS_justpass_knn_pfm[[4]])

ROC_PS_justpass_knn_pfm_PRTE<-performanceMeasures(ROC_PS_justpass_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_cohort_justpass_knn_PRTE<-colMeans(ROC_PS_justpass_knn_pfm_PRTE[[4]])

ROC_CD_justpass_knn_pfm_PRTE<-performanceMeasures(ROC_CD_justpass_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_cohort_justpass_knn_PRTE<-colMeans(ROC_CD_justpass_knn_pfm_PRTE[[4]])

ROC_ICS_justpass_knn_pfm_PRTE<-performanceMeasures(ROC_ICS_justpass_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_cohort_justpass_knn_PRTE<-colMeans(ROC_ICS_justpass_knn_pfm_PRTE[[4]])

ROC_PS_justpass_knn_pfm_PRH<-performanceMeasures(ROC_PS_justpass_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_cohort_justpass_knn_PRH<-colMeans(ROC_PS_justpass_knn_pfm_PRH[[4]])

ROC_CD_justpass_knn_pfm_PRH<-performanceMeasures(ROC_CD_justpass_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_cohort_justpass_knn_PRH<-colMeans(ROC_CD_justpass_knn_pfm_PRH[[4]])

ROC_ICS_justpass_knn_pfm_PRH<-performanceMeasures(ROC_ICS_justpass_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_cohort_justpass_knn_PRH<-colMeans(ROC_ICS_justpass_knn_pfm_PRH[[4]])

ROC_PS_justpass_knn_pfm_TEH<-performanceMeasures(ROC_PS_justpass_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_cohort_justpass_knn_TEH<-colMeans(ROC_PS_justpass_knn_pfm_TEH[[4]])

ROC_CD_justpass_knn_pfm_TEH<-performanceMeasures(ROC_CD_justpass_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_cohort_justpass_knn_TEH<-colMeans(ROC_CD_justpass_knn_pfm_TEH[[4]])

ROC_ICS_justpass_knn_pfm_TEH<-performanceMeasures(ROC_ICS_justpass_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_cohort_justpass_knn_TEH<-colMeans(ROC_ICS_justpass_knn_pfm_TEH[[4]])

ROC_PS_justpass_knn_pfm_PR<-performanceMeasures(ROC_PS_justpass_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_cohort_justpass_knn_PR<-colMeans(ROC_PS_justpass_knn_pfm_PR[[4]])

ROC_CD_justpass_knn_pfm_PR<-performanceMeasures(ROC_CD_justpass_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_cohort_justpass_knn_PR<-colMeans(ROC_CD_justpass_knn_pfm_PR[[4]])

ROC_ICS_justpass_knn_pfm_PR<-performanceMeasures(ROC_ICS_justpass_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_cohort_justpass_knn_PR<-colMeans(ROC_ICS_justpass_knn_pfm_PR[[4]])

ROC_PS_justpass_knn_pfm_TE<-performanceMeasures(ROC_PS_justpass_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_cohort_justpass_knn_TE<-colMeans(ROC_PS_justpass_knn_pfm_TE[[4]])

ROC_CD_justpass_knn_pfm_TE<-performanceMeasures(ROC_CD_justpass_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_cohort_justpass_knn_TE<-colMeans(ROC_CD_justpass_knn_pfm_TE[[4]])

ROC_ICS_justpass_knn_pfm_TE<-performanceMeasures(ROC_ICS_justpass_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_cohort_justpass_knn_TE<-colMeans(ROC_ICS_justpass_knn_pfm_TE[[4]])

ROC_PS_justpass_knn_pfm_H<-performanceMeasures(ROC_PS_justpass_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_cohort_justpass_knn_all<-colMeans(ROC_PS_justpass_knn_pfm_H[[4]])

ROC_CD_justpass_knn_pfm_H<-performanceMeasures(ROC_CD_justpass_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_cohort_justpass_knn_H<-colMeans(ROC_CD_justpass_knn_pfm_H[[4]])

ROC_ICS_justpass_knn_pfm_H<-performanceMeasures(ROC_ICS_justpass_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_cohort_justpass_knn_H<-colMeans(ROC_ICS_justpass_knn_pfm_H[[4]])

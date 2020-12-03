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
  predPS_x<-jackPredLog(centPS,predictors = c("cohort","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log[[i]]<-ROC
}

ROC_CD_log<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("cohort","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log[[i]]<-ROC
}

ROC_ICS_log<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("cohort","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log[[i]]<-ROC
}

ROC_PS_justpass_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("cohort","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log[[i]]<-ROC
}

ROC_CD_justpass_log<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("cohort","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log[[i]]<-ROC
}

ROC_ICS_justpass_log<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("cohort","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log[[i]]<-ROC
}

######PAGERANK TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_log_PRTE<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("cohort","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_PRTE[[i]]<-ROC
}

ROC_CD_log_PRTE<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("cohort","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_PRTE[[i]]<-ROC
}

ROC_ICS_log_PRTE<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("cohort","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_PRTE[[i]]<-ROC
}

ROC_PS_justpass_log_PRTE<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("cohort","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_PRTE[[i]]<-ROC
}

ROC_CD_justpass_log_PRTE<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("cohort","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_PRTE[[i]]<-ROC
}

ROC_ICS_justpass_log_PRTE<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("cohort","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log_PRTE[[i]]<-ROC
}

######PAGERANK HIDE NETWORK PREDICTORS######

ROC_PS_log_PRH<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("cohort","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_PRH[[i]]<-ROC
}

ROC_CD_log_PRH<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("cohort","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_PRH[[i]]<-ROC
}

ROC_ICS_log_PRH<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("cohort","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_PRH[[i]]<-ROC
}

ROC_PS_justpass_log_PRH<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("cohort","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_PRH[[i]]<-ROC
}

ROC_CD_justpass_log_PRH<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("cohort","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_PRH[[i]]<-ROC
}

ROC_ICS_justpass_log_PRH<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("cohort","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log_PRH[[i]]<-ROC
}

######TARGET ENTROPY HIDE NETWORK PREDICTORS######

ROC_PS_log_TEH<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("cohort","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_TEH[[i]]<-ROC
}

ROC_CD_log_TEH<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("cohort","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_TEH[[i]]<-ROC
}

ROC_ICS_log_TEH<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("cohort","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_TEH[[i]]<-ROC
}

ROC_PS_justpass_log_TEH<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("cohort","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_TEH[[i]]<-ROC
}

ROC_CD_justpass_log_TEH<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("cohort","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_TEH[[i]]<-ROC
}

ROC_ICS_justpass_log_TEH<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("cohort","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log_TEH[[i]]<-ROC
}

######TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_log_TE<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("cohort","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_TE[[i]]<-ROC
}

ROC_CD_log_TE<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("cohort","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_TE[[i]]<-ROC
}

ROC_ICS_log_TE<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("cohort","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_TE[[i]]<-ROC
}

ROC_PS_justpass_log_TE<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("cohort","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_TE[[i]]<-ROC
}

ROC_CD_justpass_log_TE<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("cohort","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_TE[[i]]<-ROC
}

ROC_ICS_justpass_log_TE<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("cohort","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log_TE[[i]]<-ROC
}

######HIDE NETWORK PREDICTORS######

ROC_PS_log_H<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("cohort","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_H[[i]]<-ROC
}

ROC_CD_log_H<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("cohort","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_H[[i]]<-ROC
}

ROC_ICS_log_H<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("cohort","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_H[[i]]<-ROC
}

ROC_PS_justpass_log_H<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("cohort","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_H[[i]]<-ROC
}

ROC_CD_justpass_log_H<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("cohort","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_H[[i]]<-ROC
}

ROC_ICS_justpass_log_H<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("cohort","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log_H[[i]]<-ROC
}

######PAGERANK NETWORK PREDICTORS######

ROC_PS_log_PR<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("cohort","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_PR[[i]]<-ROC
}

ROC_CD_log_PR<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("cohort","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_PR[[i]]<-ROC
}

ROC_ICS_log_PR<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("cohort","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_PR[[i]]<-ROC
}

ROC_PS_justpass_log_PR<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("cohort","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_PR[[i]]<-ROC
}

ROC_CD_justpass_log_PR<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("cohort","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_PR[[i]]<-ROC
}

ROC_ICS_justpass_log_PR<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("cohort","PageRank"),p=i/100)
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
     file="data/ROC_AUC/ROC_AUC/ROC_NetworkMeasuresCohort_logreg.Rdata")

tend<-Sys.time()
tend-tstart

t1<-Sys.time()
## Leave one out cross-validation
##Made ready for ROC curves
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

predPS_log_PTHF<-jackPredLog(centPS,predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"))
rocPS_log_PTHF<-list()
rocPS_log_PTHF[[1]]<-roc(predPS_log_PTHF$pass,as.numeric(predPS_log_PTHF$Week1),auc=T,ci=T)
rocPS_log_PTHF[[2]]<-roc(predPS_log_PTHF$pass,as.numeric(predPS_log_PTHF$Week2),auc=T,ci=T)
rocPS_log_PTHF[[3]]<-roc(predPS_log_PTHF$pass,as.numeric(predPS_log_PTHF$Week3),auc=T,ci=T)
rocPS_log_PTHF[[4]]<-roc(predPS_log_PTHF$pass,as.numeric(predPS_log_PTHF$Week4),auc=T,ci=T)
rocPS_log_PTHF[[5]]<-roc(predPS_log_PTHF$pass,as.numeric(predPS_log_PTHF$Week5),auc=T,ci=T)
rocPS_log_PTHF[[6]]<-roc(predPS_log_PTHF$pass,as.numeric(predPS_log_PTHF$Week6),auc=T,ci=T)
rocPS_log_PTHF[[7]]<-roc(predPS_log_PTHF$pass,as.numeric(predPS_log_PTHF$Week7),auc=T,ci=T)
PS_log_PTHF_auc<-c(rocPS_log_PTHF[[1]]$auc,rocPS_log_PTHF[[2]]$auc,rocPS_log_PTHF[[3]]$auc,rocPS_log_PTHF[[4]]$auc,
                  rocPS_log_PTHF[[5]]$auc,rocPS_log_PTHF[[6]]$auc,rocPS_log_PTHF[[7]]$auc)
PS_log_PTHF_ciL<-c(rocPS_log_PTHF[[1]]$ci[1],rocPS_log_PTHF[[2]]$ci[1],rocPS_log_PTHF[[3]]$ci[1],rocPS_log_PTHF[[4]]$ci[1],
                  rocPS_log_PTHF[[5]]$ci[1],rocPS_log_PTHF[[6]]$ci[1],rocPS_log_PTHF[[7]]$ci[1])
PS_log_PTHF_ciH<-c(rocPS_log_PTHF[[1]]$ci[3],rocPS_log_PTHF[[2]]$ci[3],rocPS_log_PTHF[[3]]$ci[3],rocPS_log_PTHF[[4]]$ci[3],
                  rocPS_log_PTHF[[5]]$ci[3],rocPS_log_PTHF[[6]]$ci[3],rocPS_log_PTHF[[7]]$ci[3])
x<-c(1:7)
plot(x, PS_log_PTHF_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Weeks", ylab="AUC and CI",
     main="AUC with Confidence Intervals",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_log_PTHF_ciL, x, PS_log_PTHF_ciH, length=0.05, angle=90, code=3)

plot(rocPS_log_PTHF[[1]])
lines(rocPS_log_PTHF[[2]],col="yellow")
lines(rocPS_log_PTHF[[3]],col="blue")
lines(rocPS_log_PTHF[[4]],col="magenta")
lines(rocPS_log_PTHF[[5]],col="red")
lines(rocPS_log_PTHF[[6]],col="green")
lines(rocPS_log_PTHF[[7]],col="purple")


ROC_PS_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log[[i]]<-ROC
}

ROC_CD_log<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log[[i]]<-ROC
}

ROC_ICS_log<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log[[i]]<-ROC
}

ROC_PS_justpass_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log[[i]]<-ROC
}

ROC_CD_justpass_log<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log[[i]]<-ROC
}

ROC_ICS_justpass_log<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log[[i]]<-ROC
}

######PAGERANK TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_log_PRTE<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_PRTE[[i]]<-ROC
}

ROC_CD_log_PRTE<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_PRTE[[i]]<-ROC
}

ROC_ICS_log_PRTE<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_PRTE[[i]]<-ROC
}

ROC_PS_justpass_log_PRTE<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_PRTE[[i]]<-ROC
}

ROC_CD_justpass_log_PRTE<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_PRTE[[i]]<-ROC
}

ROC_ICS_justpass_log_PRTE<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log_PRTE[[i]]<-ROC
}

######PAGERANK HIDE NETWORK PREDICTORS######

ROC_PS_log_PRH<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_PRH[[i]]<-ROC
}

ROC_CD_log_PRH<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_PRH[[i]]<-ROC
}

ROC_ICS_log_PRH<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_PRH[[i]]<-ROC
}

ROC_PS_justpass_log_PRH<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_PRH[[i]]<-ROC
}

ROC_CD_justpass_log_PRH<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_PRH[[i]]<-ROC
}

ROC_ICS_justpass_log_PRH<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log_PRH[[i]]<-ROC
}

######TARGET ENTROPY HIDE NETWORK PREDICTORS######

ROC_PS_log_TEH<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_TEH[[i]]<-ROC
}

ROC_CD_log_TEH<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_TEH[[i]]<-ROC
}

ROC_ICS_log_TEH<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_TEH[[i]]<-ROC
}

ROC_PS_justpass_log_TEH<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_TEH[[i]]<-ROC
}

ROC_CD_justpass_log_TEH<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_TEH[[i]]<-ROC
}

ROC_ICS_justpass_log_TEH<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log_TEH[[i]]<-ROC
}

######TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_log_TE<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_TE[[i]]<-ROC
}

ROC_CD_log_TE<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_TE[[i]]<-ROC
}

ROC_ICS_log_TE<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_TE[[i]]<-ROC
}

ROC_PS_justpass_log_TE<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_TE[[i]]<-ROC
}

ROC_CD_justpass_log_TE<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_TE[[i]]<-ROC
}

ROC_ICS_justpass_log_TE<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log_TE[[i]]<-ROC
}

######HIDE NETWORK PREDICTORS######

ROC_PS_log_H<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_H[[i]]<-ROC
}

ROC_CD_log_H<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_H[[i]]<-ROC
}

ROC_ICS_log_H<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_H[[i]]<-ROC
}

ROC_PS_justpass_log_H<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_H[[i]]<-ROC
}

ROC_CD_justpass_log_H<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_H[[i]]<-ROC
}

ROC_ICS_justpass_log_H<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log_H[[i]]<-ROC
}

######PAGERANK NETWORK PREDICTORS######

ROC_PS_log_PR<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_PR[[i]]<-ROC
}

ROC_CD_log_PR<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_PR[[i]]<-ROC
}

ROC_ICS_log_PR<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_PR[[i]]<-ROC
}

ROC_PS_justpass_log_PR<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_PR[[i]]<-ROC
}

ROC_CD_justpass_log_PR<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_PR[[i]]<-ROC
}

ROC_ICS_justpass_log_PR<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("fci_pre_c","PageRank"),p=i/100)
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
     file="data/ROC_AUC/ROC02_NetworkMeasuresFCI_C_log.Rdata")

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
  predPS_x<-jackPredLDA(centPS,predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_lda[[i]]<-ROC
}

ROC_CD_lda<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD,predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_lda[[i]]<-ROC
}

ROC_ICS_lda<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS,predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_lda[[i]]<-ROC
}

ROC_PS_justpass_lda<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_lda[[i]]<-ROC
}

ROC_CD_justpass_lda<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD, outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_lda[[i]]<-ROC
}

ROC_ICS_justpass_lda<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS, outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_lda[[i]]<-ROC
}

######PAGERANK TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_lda_PRTE<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_lda_PRTE[[i]]<-ROC
}

ROC_CD_lda_PRTE<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD,predictors = c("fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_lda_PRTE[[i]]<-ROC
}

ROC_ICS_lda_PRTE<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS,predictors = c("fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_lda_PRTE[[i]]<-ROC
}

ROC_PS_justpass_lda_PRTE<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_lda_PRTE[[i]]<-ROC
}

ROC_CD_justpass_lda_PRTE<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD, outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_lda_PRTE[[i]]<-ROC
}

ROC_ICS_justpass_lda_PRTE<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS, outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_lda_PRTE[[i]]<-ROC
}

######PAGERANK HIDE NETWORK PREDICTORS######

ROC_PS_lda_PRH<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_lda_PRH[[i]]<-ROC
}

ROC_CD_lda_PRH<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD,predictors = c("fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_lda_PRH[[i]]<-ROC
}

ROC_ICS_lda_PRH<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS,predictors = c("fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_lda_PRH[[i]]<-ROC
}

ROC_PS_justpass_lda_PRH<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,outcome = "justpass",predictors = c("fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_lda_PRH[[i]]<-ROC
}

ROC_CD_justpass_lda_PRH<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD, outcome = "justpass",predictors = c("fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_lda_PRH[[i]]<-ROC
}

ROC_ICS_justpass_lda_PRH<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS, outcome = "justpass",predictors = c("fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_lda_PRH[[i]]<-ROC
}

######TARGET ENTROPY HIDE NETWORK PREDICTORS######

ROC_PS_lda_TEH<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_lda_TEH[[i]]<-ROC
}

ROC_CD_lda_TEH<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD,predictors = c("fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_lda_TEH[[i]]<-ROC
}

ROC_ICS_lda_TEH<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS,predictors = c("fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_lda_TEH[[i]]<-ROC
}

ROC_PS_justpass_lda_TEH<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,outcome = "justpass",predictors = c("fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_lda_TEH[[i]]<-ROC
}

ROC_CD_justpass_lda_TEH<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD, outcome = "justpass",predictors = c("fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_lda_TEH[[i]]<-ROC
}

ROC_ICS_justpass_lda_TEH<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS, outcome = "justpass",predictors = c("fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_lda_TEH[[i]]<-ROC
}

######TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_lda_TE<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_lda_TE[[i]]<-ROC
}

ROC_CD_lda_TE<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD,predictors = c("fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_lda_TE[[i]]<-ROC
}

ROC_ICS_lda_TE<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS,predictors = c("fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_lda_TE[[i]]<-ROC
}

ROC_PS_justpass_lda_TE<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,outcome = "justpass",predictors = c("fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_lda_TE[[i]]<-ROC
}

ROC_CD_justpass_lda_TE<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD, outcome = "justpass",predictors = c("fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_lda_TE[[i]]<-ROC
}

ROC_ICS_justpass_lda_TE<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS, outcome = "justpass",predictors = c("fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_lda_TE[[i]]<-ROC
}

######HIDE NETWORK PREDICTORS######

ROC_PS_lda_H<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_lda_H[[i]]<-ROC
}

ROC_CD_lda_H<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD,predictors = c("fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_lda_H[[i]]<-ROC
}

ROC_ICS_lda_H<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS,predictors = c("fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_lda_H[[i]]<-ROC
}

ROC_PS_justpass_lda_H<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,outcome = "justpass",predictors = c("fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_lda_H[[i]]<-ROC
}

ROC_CD_justpass_lda_H<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD, outcome = "justpass",predictors = c("fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_lda_H[[i]]<-ROC
}

ROC_ICS_justpass_lda_H<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS, outcome = "justpass",predictors = c("fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_lda_H[[i]]<-ROC
}

######TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_lda_PR<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_lda_PR[[i]]<-ROC
}

ROC_CD_lda_PR<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD,predictors = c("fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_lda_PR[[i]]<-ROC
}

ROC_ICS_lda_PR<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS,predictors = c("fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_lda_PR[[i]]<-ROC
}

ROC_PS_justpass_lda_PR<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,outcome = "justpass",predictors = c("fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_lda_PR[[i]]<-ROC
}

ROC_CD_justpass_lda_PR<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD, outcome = "justpass",predictors = c("fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_lda_PR[[i]]<-ROC
}

ROC_ICS_justpass_lda_PR<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS, outcome = "justpass",predictors = c("fci_pre_c","PageRank"),p=i/100)
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
     file="data/ROC_AUC/ROC02_NetworkMeasuresFCI_C_lda.Rdata")



#########QUADRATIC DISCRIMINANT ANALYSIS###########
#########QUADRATIC DISCRIMINANT ANALYSIS###########
#########QUADRATIC DISCRIMINANT ANALYSIS ###########
#########QUADRATIC DISCRIMINANT ANALYSIS ###########

######ALL NETWORK PREDICTORS######

ROC_PS_qda<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_qda[[i]]<-ROC
}

ROC_CD_qda<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD,predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_qda[[i]]<-ROC
}

ROC_ICS_qda<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS,predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_qda[[i]]<-ROC
}

ROC_PS_justpass_qda<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_qda[[i]]<-ROC
}

ROC_CD_justpass_qda<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD, outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_qda[[i]]<-ROC
}

ROC_ICS_justpass_qda<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS, outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_qda[[i]]<-ROC
}

######PAGERANK TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_qda_PRTE<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_qda_PRTE[[i]]<-ROC
}

ROC_CD_qda_PRTE<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD,predictors = c("fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_qda_PRTE[[i]]<-ROC
}

ROC_ICS_qda_PRTE<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS,predictors = c("fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_qda_PRTE[[i]]<-ROC
}

ROC_PS_justpass_qda_PRTE<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_qda_PRTE[[i]]<-ROC
}

ROC_CD_justpass_qda_PRTE<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD, outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_qda_PRTE[[i]]<-ROC
}

ROC_ICS_justpass_qda_PRTE<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS, outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_qda_PRTE[[i]]<-ROC
}

######PAGERANK HIDE NETWORK PREDICTORS######

ROC_PS_qda_PRH<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_qda_PRH[[i]]<-ROC
}

ROC_CD_qda_PRH<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD,predictors = c("fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_qda_PRH[[i]]<-ROC
}

ROC_ICS_qda_PRH<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS,predictors = c("fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_qda_PRH[[i]]<-ROC
}

ROC_PS_justpass_qda_PRH<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,outcome = "justpass",predictors = c("fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_qda_PRH[[i]]<-ROC
}

ROC_CD_justpass_qda_PRH<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD, outcome = "justpass",predictors = c("fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_qda_PRH[[i]]<-ROC
}

ROC_ICS_justpass_qda_PRH<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS, outcome = "justpass",predictors = c("fci_pre_c","PageRank","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_qda_PRH[[i]]<-ROC
}

######TARGET ENTROPY HIDE NETWORK PREDICTORS######

ROC_PS_qda_TEH<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_qda_TEH[[i]]<-ROC
}

ROC_CD_qda_TEH<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD,predictors = c("fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_qda_TEH[[i]]<-ROC
}

ROC_ICS_qda_TEH<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS,predictors = c("fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_qda_TEH[[i]]<-ROC
}

ROC_PS_justpass_qda_TEH<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,outcome = "justpass",predictors = c("fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_qda_TEH[[i]]<-ROC
}

ROC_CD_justpass_qda_TEH<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD, outcome = "justpass",predictors = c("fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_qda_TEH[[i]]<-ROC
}

ROC_ICS_justpass_qda_TEH<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS, outcome = "justpass",predictors = c("fci_pre_c","tarEnt","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_qda_TEH[[i]]<-ROC
}

######TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_qda_TE<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_qda_TE[[i]]<-ROC
}

ROC_CD_qda_TE<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD,predictors = c("fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_qda_TE[[i]]<-ROC
}

ROC_ICS_qda_TE<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS,predictors = c("fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_qda_TE[[i]]<-ROC
}

ROC_PS_justpass_qda_TE<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,outcome = "justpass",predictors = c("fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_qda_TE[[i]]<-ROC
}

ROC_CD_justpass_qda_TE<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD, outcome = "justpass",predictors = c("fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_qda_TE[[i]]<-ROC
}

ROC_ICS_justpass_qda_TE<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS, outcome = "justpass",predictors = c("fci_pre_c","tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_qda_TE[[i]]<-ROC
}

######HIDE NETWORK PREDICTORS######

ROC_PS_qda_H<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_qda_H[[i]]<-ROC
}

ROC_CD_qda_H<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD,predictors = c("fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_qda_H[[i]]<-ROC
}

ROC_ICS_qda_H<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS,predictors = c("fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_qda_H[[i]]<-ROC
}

ROC_PS_justpass_qda_H<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,outcome = "justpass",predictors = c("fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_qda_H[[i]]<-ROC
}

ROC_CD_justpass_qda_H<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD, outcome = "justpass",predictors = c("fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_qda_H[[i]]<-ROC
}

ROC_ICS_justpass_qda_H<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS, outcome = "justpass",predictors = c("fci_pre_c","Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_qda_H[[i]]<-ROC
}

######TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_qda_PR<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_qda_PR[[i]]<-ROC
}

ROC_CD_qda_PR<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD,predictors = c("fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_qda_PR[[i]]<-ROC
}

ROC_ICS_qda_PR<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS,predictors = c("fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_qda_PR[[i]]<-ROC
}

ROC_PS_justpass_qda_PR<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,outcome = "justpass",predictors = c("fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_qda_PR[[i]]<-ROC
}

ROC_CD_justpass_qda_PR<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD, outcome = "justpass",predictors = c("fci_pre_c","PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_qda_PR[[i]]<-ROC
}

ROC_ICS_justpass_qda_PR<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS, outcome = "justpass",predictors = c("fci_pre_c","PageRank"),p=i/100)
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
     file="data/ROC_AUC/ROC02_NetworkMeasuresFCI_C_qda.Rdata")



#########K NEAREST NEIGHBORS###########
#########K NEAREST NEIGHBORS###########
#########K NEAREST NEIGHBORS###########
#########K NEAREST NEIGHBORS###########

######ALL NETWORK PREDICTORS######
t1<-Sys.time()
ROC_PS_knn<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_knn[[i]]<-ROC
}

ROC_CD_knn<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn[[i]]<-ROC
}

ROC_ICS_knn<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn[[i]]<-ROC
}

ROC_PS_justpass_knn<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn[[i]]<-ROC
}

ROC_CD_justpass_knn<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn[[i]]<-ROC
}

ROC_ICS_justpass_knn<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn[[i]]<-ROC
}

######PAGERANK TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_knn_PRTE<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,predictors = c("fci_pre_c","PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_knn_PRTE[[i]]<-ROC
}

ROC_CD_knn_PRTE<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("fci_pre_c","PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_PRTE[[i]]<-ROC
}

ROC_ICS_knn_PRTE<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("fci_pre_c","PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_PRTE[[i]]<-ROC
}

ROC_PS_justpass_knn_PRTE<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_PRTE[[i]]<-ROC
}

ROC_CD_justpass_knn_PRTE<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_PRTE[[i]]<-ROC
}

ROC_ICS_justpass_knn_PRTE<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("fci_pre_c","PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_PRTE[[i]]<-ROC
}

######PAGERANK HIDE NETWORK PREDICTORS######

ROC_PS_knn_PRH<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,predictors = c("fci_pre_c","PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_knn_PRH[[i]]<-ROC
}

ROC_CD_knn_PRH<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("fci_pre_c","PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_PRH[[i]]<-ROC
}

ROC_ICS_knn_PRH<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("fci_pre_c","PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_PRH[[i]]<-ROC
}

ROC_PS_justpass_knn_PRH<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("fci_pre_c","PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_PRH[[i]]<-ROC
}

ROC_CD_justpass_knn_PRH<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("fci_pre_c","PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_PRH[[i]]<-ROC
}

ROC_ICS_justpass_knn_PRH<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("fci_pre_c","PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_PRH[[i]]<-ROC
}

######TARGET ENTROPY HIDE NETWORK PREDICTORS######

ROC_PS_knn_TEH<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,predictors = c("fci_pre_c","tarEnt","Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_knn_TEH[[i]]<-ROC
}

ROC_CD_knn_TEH<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("fci_pre_c","tarEnt","Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_TEH[[i]]<-ROC
}

ROC_ICS_knn_TEH<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("fci_pre_c","tarEnt","Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_TEH[[i]]<-ROC
}

ROC_PS_justpass_knn_TEH<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("fci_pre_c","tarEnt","Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_TEH[[i]]<-ROC
}

ROC_CD_justpass_knn_TEH<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("fci_pre_c","tarEnt","Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_TEH[[i]]<-ROC
}

ROC_ICS_justpass_knn_TEH<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("fci_pre_c","tarEnt","Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_TEH[[i]]<-ROC
}

######TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_knn_TE<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,predictors = c("fci_pre_c","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_knn_TE[[i]]<-ROC
}

ROC_CD_knn_TE<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("fci_pre_c","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_TE[[i]]<-ROC
}

ROC_ICS_knn_TE<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("fci_pre_c","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_TE[[i]]<-ROC
}

ROC_PS_justpass_knn_TE<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("fci_pre_c","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_TE[[i]]<-ROC
}

ROC_CD_justpass_knn_TE<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("fci_pre_c","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_TE[[i]]<-ROC
}

ROC_ICS_justpass_knn_TE<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("fci_pre_c","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_TE[[i]]<-ROC
}

######HIDE NETWORK PREDICTORS######

ROC_PS_knn_H<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,predictors = c("fci_pre_c","Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_knn_H[[i]]<-ROC
}

ROC_CD_knn_H<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("fci_pre_c","Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_H[[i]]<-ROC
}

ROC_ICS_knn_H<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("fci_pre_c","Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_H[[i]]<-ROC
}

ROC_PS_justpass_knn_H<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("fci_pre_c","Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_H[[i]]<-ROC
}

ROC_CD_justpass_knn_H<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("fci_pre_c","Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_H[[i]]<-ROC
}

ROC_ICS_justpass_knn_H<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("fci_pre_c","Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_H[[i]]<-ROC
}

######TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_knn_PR<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,predictors = c("fci_pre_c","PageRank"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_knn_PR[[i]]<-ROC
}

ROC_CD_knn_PR<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("fci_pre_c","PageRank"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_PR[[i]]<-ROC
}

ROC_ICS_knn_PR<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("fci_pre_c","PageRank"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_PR[[i]]<-ROC
}

ROC_PS_justpass_knn_PR<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("fci_pre_c","PageRank"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_PR[[i]]<-ROC
}

ROC_CD_justpass_knn_PR<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("fci_pre_c","PageRank"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_PR[[i]]<-ROC
}

ROC_ICS_justpass_knn_PR<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("fci_pre_c","PageRank"),nK = i)
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
     file="data/ROC_AUC/ROC02_NetworkMeasuresFCI_C_knn.Rdata")

t2<-Sys.time()
t2-t1

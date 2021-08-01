## Leave one out cross-validation
##Made ready for ROC curves
##Models without background measures

####Setup####
rm(list = ls())
t1<-Sys.time()

library(igraph)
library(boot)
library(pROC)
library(dplyr)
library(class)   # for knn
library(tidyr)
library(ggplot2)  # for plotting success rates
library(MASS)
library(boot)

load("data/centrality_data_frames.Rdata")
# Turn long data frame into list of weekly frames
centPS <- dfPS %>% group_split(Week)
centCD <- dfCD %>% group_split(Week)
centICS <- dfICS %>% group_split(Week)

source("code/jackknife_functionsAlternate.R")
source("code/ROC_functions.R")

lazy<-table(centPS[[1]]$pass)[2]/sum(table(centPS[[1]]$pass))
lazy_jp<-table(centPS[[1]]$justpass)[2]/sum(table(centPS[[1]]$justpass))
pass<-centPS[[1]]$pass
justpass<-centPS[[1]]$justpass
#######################################
#########LOGISTIC REGRESSION###########
#######################################
######Run models and calculate AUC (with CI)####
######PR-TE-H Models######
predPS_log_PTH<-jackPredLog(centPS,predictors = c("cohort","PageRank","tarEnt", "Hide"))
rocPS_log_PTH<-list()
rocPS_log_PTH[[1]]<-roc(predPS_log_PTH$pass,as.numeric(predPS_log_PTH$Week1),auc=T,ci=T)
rocPS_log_PTH[[2]]<-roc(predPS_log_PTH$pass,as.numeric(predPS_log_PTH$Week2),auc=T,ci=T)
rocPS_log_PTH[[3]]<-roc(predPS_log_PTH$pass,as.numeric(predPS_log_PTH$Week3),auc=T,ci=T)
rocPS_log_PTH[[4]]<-roc(predPS_log_PTH$pass,as.numeric(predPS_log_PTH$Week4),auc=T,ci=T)
rocPS_log_PTH[[5]]<-roc(predPS_log_PTH$pass,as.numeric(predPS_log_PTH$Week5),auc=T,ci=T)
rocPS_log_PTH[[6]]<-roc(predPS_log_PTH$pass,as.numeric(predPS_log_PTH$Week6),auc=T,ci=T)
rocPS_log_PTH[[7]]<-roc(predPS_log_PTH$pass,as.numeric(predPS_log_PTH$Week7),auc=T,ci=T)
PS_log_PTH_auc<-c(rocPS_log_PTH[[1]]$auc,rocPS_log_PTH[[2]]$auc,rocPS_log_PTH[[3]]$auc,rocPS_log_PTH[[4]]$auc,
                  rocPS_log_PTH[[5]]$auc,rocPS_log_PTH[[6]]$auc,rocPS_log_PTH[[7]]$auc)
PS_log_PTH_ciL<-c(rocPS_log_PTH[[1]]$ci[1],rocPS_log_PTH[[2]]$ci[1],rocPS_log_PTH[[3]]$ci[1],rocPS_log_PTH[[4]]$ci[1],
                  rocPS_log_PTH[[5]]$ci[1],rocPS_log_PTH[[6]]$ci[1],rocPS_log_PTH[[7]]$ci[1])
PS_log_PTH_ciH<-c(rocPS_log_PTH[[1]]$ci[3],rocPS_log_PTH[[2]]$ci[3],rocPS_log_PTH[[3]]$ci[3],rocPS_log_PTH[[4]]$ci[3],
                  rocPS_log_PTH[[5]]$ci[3],rocPS_log_PTH[[6]]$ci[3],rocPS_log_PTH[[7]]$ci[3])

predCD_log_PTH<-jackPredLog(centCD,predictors = c("cohort","PageRank","tarEnt", "Hide"))
rocCD_log_PTH<-list()
rocCD_log_PTH[[1]]<-roc(predCD_log_PTH$pass,as.numeric(predCD_log_PTH$Week1),auc=T,ci=T)
rocCD_log_PTH[[2]]<-roc(predCD_log_PTH$pass,as.numeric(predCD_log_PTH$Week2),auc=T,ci=T)
rocCD_log_PTH[[3]]<-roc(predCD_log_PTH$pass,as.numeric(predCD_log_PTH$Week3),auc=T,ci=T)
rocCD_log_PTH[[4]]<-roc(predCD_log_PTH$pass,as.numeric(predCD_log_PTH$Week4),auc=T,ci=T)
rocCD_log_PTH[[5]]<-roc(predCD_log_PTH$pass,as.numeric(predCD_log_PTH$Week5),auc=T,ci=T)
rocCD_log_PTH[[6]]<-roc(predCD_log_PTH$pass,as.numeric(predCD_log_PTH$Week6),auc=T,ci=T)
rocCD_log_PTH[[7]]<-roc(predCD_log_PTH$pass,as.numeric(predCD_log_PTH$Week7),auc=T,ci=T)
CD_log_PTH_auc<-c(rocCD_log_PTH[[1]]$auc,rocCD_log_PTH[[2]]$auc,rocCD_log_PTH[[3]]$auc,rocCD_log_PTH[[4]]$auc,
                  rocCD_log_PTH[[5]]$auc,rocCD_log_PTH[[6]]$auc,rocCD_log_PTH[[7]]$auc)
CD_log_PTH_ciL<-c(rocCD_log_PTH[[1]]$ci[1],rocCD_log_PTH[[2]]$ci[1],rocCD_log_PTH[[3]]$ci[1],rocCD_log_PTH[[4]]$ci[1],
                  rocCD_log_PTH[[5]]$ci[1],rocCD_log_PTH[[6]]$ci[1],rocCD_log_PTH[[7]]$ci[1])
CD_log_PTH_ciH<-c(rocCD_log_PTH[[1]]$ci[3],rocCD_log_PTH[[2]]$ci[3],rocCD_log_PTH[[3]]$ci[3],rocCD_log_PTH[[4]]$ci[3],
                  rocCD_log_PTH[[5]]$ci[3],rocCD_log_PTH[[6]]$ci[3],rocCD_log_PTH[[7]]$ci[3])



predICS_log_PTH<-jackPredLog(centICS,predictors = c("cohort","PageRank","tarEnt", "Hide"))
rocICS_log_PTH<-list()
rocICS_log_PTH[[1]]<-roc(predICS_log_PTH$pass,as.numeric(predICS_log_PTH$Week1),auc=T,ci=T)
rocICS_log_PTH[[2]]<-roc(predICS_log_PTH$pass,as.numeric(predICS_log_PTH$Week2),auc=T,ci=T)
rocICS_log_PTH[[3]]<-roc(predICS_log_PTH$pass,as.numeric(predICS_log_PTH$Week3),auc=T,ci=T)
rocICS_log_PTH[[4]]<-roc(predICS_log_PTH$pass,as.numeric(predICS_log_PTH$Week4),auc=T,ci=T)
rocICS_log_PTH[[5]]<-roc(predICS_log_PTH$pass,as.numeric(predICS_log_PTH$Week5),auc=T,ci=T)
rocICS_log_PTH[[6]]<-roc(predICS_log_PTH$pass,as.numeric(predICS_log_PTH$Week6),auc=T,ci=T)
rocICS_log_PTH[[7]]<-roc(predICS_log_PTH$pass,as.numeric(predICS_log_PTH$Week7),auc=T,ci=T)
ICS_log_PTH_auc<-c(rocICS_log_PTH[[1]]$auc,rocICS_log_PTH[[2]]$auc,rocICS_log_PTH[[3]]$auc,rocICS_log_PTH[[4]]$auc,
                   rocICS_log_PTH[[5]]$auc,rocICS_log_PTH[[6]]$auc,rocICS_log_PTH[[7]]$auc)
ICS_log_PTH_ciL<-c(rocICS_log_PTH[[1]]$ci[1],rocICS_log_PTH[[2]]$ci[1],rocICS_log_PTH[[3]]$ci[1],rocICS_log_PTH[[4]]$ci[1],
                   rocICS_log_PTH[[5]]$ci[1],rocICS_log_PTH[[6]]$ci[1],rocICS_log_PTH[[7]]$ci[1])
ICS_log_PTH_ciH<-c(rocICS_log_PTH[[1]]$ci[3],rocICS_log_PTH[[2]]$ci[3],rocICS_log_PTH[[3]]$ci[3],rocICS_log_PTH[[4]]$ci[3],
                   rocICS_log_PTH[[5]]$ci[3],rocICS_log_PTH[[6]]$ci[3],rocICS_log_PTH[[7]]$ci[3])




###JUST PASS
predPS_jp_log_PTH<-jackPredLog(centPS,outcome = "justpass",predictors = c("cohort","PageRank","tarEnt", "Hide"))
rocPS_jp_log_PTH<-list()
rocPS_jp_log_PTH[[1]]<-roc(predPS_jp_log_PTH$justpass,as.numeric(predPS_jp_log_PTH$Week1),auc=T,ci=T)
rocPS_jp_log_PTH[[2]]<-roc(predPS_jp_log_PTH$justpass,as.numeric(predPS_jp_log_PTH$Week2),auc=T,ci=T)
rocPS_jp_log_PTH[[3]]<-roc(predPS_jp_log_PTH$justpass,as.numeric(predPS_jp_log_PTH$Week3),auc=T,ci=T)
rocPS_jp_log_PTH[[4]]<-roc(predPS_jp_log_PTH$justpass,as.numeric(predPS_jp_log_PTH$Week4),auc=T,ci=T)
rocPS_jp_log_PTH[[5]]<-roc(predPS_jp_log_PTH$justpass,as.numeric(predPS_jp_log_PTH$Week5),auc=T,ci=T)
rocPS_jp_log_PTH[[6]]<-roc(predPS_jp_log_PTH$justpass,as.numeric(predPS_jp_log_PTH$Week6),auc=T,ci=T)
rocPS_jp_log_PTH[[7]]<-roc(predPS_jp_log_PTH$justpass,as.numeric(predPS_jp_log_PTH$Week7),auc=T,ci=T)

PS_jp_log_PTH_auc<-c(rocPS_jp_log_PTH[[1]]$auc,rocPS_jp_log_PTH[[2]]$auc,rocPS_jp_log_PTH[[3]]$auc,rocPS_jp_log_PTH[[4]]$auc,
                     rocPS_jp_log_PTH[[5]]$auc,rocPS_jp_log_PTH[[6]]$auc,rocPS_jp_log_PTH[[7]]$auc)
PS_jp_log_PTH_ciL<-c(rocPS_jp_log_PTH[[1]]$ci[1],rocPS_jp_log_PTH[[2]]$ci[1],rocPS_jp_log_PTH[[3]]$ci[1],rocPS_jp_log_PTH[[4]]$ci[1],
                     rocPS_jp_log_PTH[[5]]$ci[1],rocPS_jp_log_PTH[[6]]$ci[1],rocPS_jp_log_PTH[[7]]$ci[1])
PS_jp_log_PTH_ciH<-c(rocPS_jp_log_PTH[[1]]$ci[3],rocPS_jp_log_PTH[[2]]$ci[3],rocPS_jp_log_PTH[[3]]$ci[3],rocPS_jp_log_PTH[[4]]$ci[3],
                     rocPS_jp_log_PTH[[5]]$ci[3],rocPS_jp_log_PTH[[6]]$ci[3],rocPS_jp_log_PTH[[7]]$ci[3])

predCD_jp_log_PTH<-jackPredLog(centCD, outcome = "justpass",predictors = c("cohort","PageRank","tarEnt", "Hide"))
rocCD_jp_log_PTH<-list()
rocCD_jp_log_PTH[[1]]<-roc(predCD_jp_log_PTH$justpass,as.numeric(predCD_jp_log_PTH$Week1),auc=T,ci=T)
rocCD_jp_log_PTH[[2]]<-roc(predCD_jp_log_PTH$justpass,as.numeric(predCD_jp_log_PTH$Week2),auc=T,ci=T)
rocCD_jp_log_PTH[[3]]<-roc(predCD_jp_log_PTH$justpass,as.numeric(predCD_jp_log_PTH$Week3),auc=T,ci=T)
rocCD_jp_log_PTH[[4]]<-roc(predCD_jp_log_PTH$justpass,as.numeric(predCD_jp_log_PTH$Week4),auc=T,ci=T)
rocCD_jp_log_PTH[[5]]<-roc(predCD_jp_log_PTH$justpass,as.numeric(predCD_jp_log_PTH$Week5),auc=T,ci=T)
rocCD_jp_log_PTH[[6]]<-roc(predCD_jp_log_PTH$justpass,as.numeric(predCD_jp_log_PTH$Week6),auc=T,ci=T)
rocCD_jp_log_PTH[[7]]<-roc(predCD_jp_log_PTH$justpass,as.numeric(predCD_jp_log_PTH$Week7),auc=T,ci=T)
CD_jp_log_PTH_auc<-c(rocCD_jp_log_PTH[[1]]$auc,rocCD_jp_log_PTH[[2]]$auc,rocCD_jp_log_PTH[[3]]$auc,rocCD_jp_log_PTH[[4]]$auc,
                     rocCD_jp_log_PTH[[5]]$auc,rocCD_jp_log_PTH[[6]]$auc,rocCD_jp_log_PTH[[7]]$auc)
CD_jp_log_PTH_ciL<-c(rocCD_jp_log_PTH[[1]]$ci[1],rocCD_jp_log_PTH[[2]]$ci[1],rocCD_jp_log_PTH[[3]]$ci[1],rocCD_jp_log_PTH[[4]]$ci[1],
                     rocCD_jp_log_PTH[[5]]$ci[1],rocCD_jp_log_PTH[[6]]$ci[1],rocCD_jp_log_PTH[[7]]$ci[1])
CD_jp_log_PTH_ciH<-c(rocCD_jp_log_PTH[[1]]$ci[3],rocCD_jp_log_PTH[[2]]$ci[3],rocCD_jp_log_PTH[[3]]$ci[3],rocCD_jp_log_PTH[[4]]$ci[3],
                     rocCD_jp_log_PTH[[5]]$ci[3],rocCD_jp_log_PTH[[6]]$ci[3],rocCD_jp_log_PTH[[7]]$ci[3])


predICS_jp_log_PTH<-jackPredLog(centICS, outcome = "justpass",predictors = c("cohort","PageRank","tarEnt", "Hide"))
rocICS_jp_log_PTH<-list()
rocICS_jp_log_PTH[[1]]<-roc(predICS_jp_log_PTH$justpass,as.numeric(predICS_jp_log_PTH$Week1),auc=T,ci=T)
rocICS_jp_log_PTH[[2]]<-roc(predICS_jp_log_PTH$justpass,as.numeric(predICS_jp_log_PTH$Week2),auc=T,ci=T)
rocICS_jp_log_PTH[[3]]<-roc(predICS_jp_log_PTH$justpass,as.numeric(predICS_jp_log_PTH$Week3),auc=T,ci=T)
rocICS_jp_log_PTH[[4]]<-roc(predICS_jp_log_PTH$justpass,as.numeric(predICS_jp_log_PTH$Week4),auc=T,ci=T)
rocICS_jp_log_PTH[[5]]<-roc(predICS_jp_log_PTH$justpass,as.numeric(predICS_jp_log_PTH$Week5),auc=T,ci=T)
rocICS_jp_log_PTH[[6]]<-roc(predICS_jp_log_PTH$justpass,as.numeric(predICS_jp_log_PTH$Week6),auc=T,ci=T)
rocICS_jp_log_PTH[[7]]<-roc(predICS_jp_log_PTH$justpass,as.numeric(predICS_jp_log_PTH$Week7),auc=T,ci=T)
ICS_jp_log_PTH_auc<-c(rocICS_jp_log_PTH[[1]]$auc,rocICS_jp_log_PTH[[2]]$auc,rocICS_jp_log_PTH[[3]]$auc,rocICS_jp_log_PTH[[4]]$auc,
                      rocICS_jp_log_PTH[[5]]$auc,rocICS_jp_log_PTH[[6]]$auc,rocICS_jp_log_PTH[[7]]$auc)
ICS_jp_log_PTH_ciL<-c(rocICS_jp_log_PTH[[1]]$ci[1],rocICS_jp_log_PTH[[2]]$ci[1],rocICS_jp_log_PTH[[3]]$ci[1],rocICS_jp_log_PTH[[4]]$ci[1],
                      rocICS_jp_log_PTH[[5]]$ci[1],rocICS_jp_log_PTH[[6]]$ci[1],rocICS_jp_log_PTH[[7]]$ci[1])
ICS_jp_log_PTH_ciH<-c(rocICS_jp_log_PTH[[1]]$ci[3],rocICS_jp_log_PTH[[2]]$ci[3],rocICS_jp_log_PTH[[3]]$ci[3],rocICS_jp_log_PTH[[4]]$ci[3],
                      rocICS_jp_log_PTH[[5]]$ci[3],rocICS_jp_log_PTH[[6]]$ci[3],rocICS_jp_log_PTH[[7]]$ci[3])


######PR TE models######
predPS_log_PT<-jackPredLog(centPS,predictors = c("cohort","PageRank","tarEnt"))
rocPS_log_PT<-list()
rocPS_log_PT[[1]]<-roc(predPS_log_PT$pass,as.numeric(predPS_log_PT$Week1),auc=T,ci=T)
rocPS_log_PT[[2]]<-roc(predPS_log_PT$pass,as.numeric(predPS_log_PT$Week2),auc=T,ci=T)
rocPS_log_PT[[3]]<-roc(predPS_log_PT$pass,as.numeric(predPS_log_PT$Week3),auc=T,ci=T)
rocPS_log_PT[[4]]<-roc(predPS_log_PT$pass,as.numeric(predPS_log_PT$Week4),auc=T,ci=T)
rocPS_log_PT[[5]]<-roc(predPS_log_PT$pass,as.numeric(predPS_log_PT$Week5),auc=T,ci=T)
rocPS_log_PT[[6]]<-roc(predPS_log_PT$pass,as.numeric(predPS_log_PT$Week6),auc=T,ci=T)
rocPS_log_PT[[7]]<-roc(predPS_log_PT$pass,as.numeric(predPS_log_PT$Week7),auc=T,ci=T)
PS_log_PT_auc<-c(rocPS_log_PT[[1]]$auc,rocPS_log_PT[[2]]$auc,rocPS_log_PT[[3]]$auc,rocPS_log_PT[[4]]$auc,
                 rocPS_log_PT[[5]]$auc,rocPS_log_PT[[6]]$auc,rocPS_log_PT[[7]]$auc)
PS_log_PT_ciL<-c(rocPS_log_PT[[1]]$ci[1],rocPS_log_PT[[2]]$ci[1],rocPS_log_PT[[3]]$ci[1],rocPS_log_PT[[4]]$ci[1],
                 rocPS_log_PT[[5]]$ci[1],rocPS_log_PT[[6]]$ci[1],rocPS_log_PT[[7]]$ci[1])
PS_log_PT_ciH<-c(rocPS_log_PT[[1]]$ci[3],rocPS_log_PT[[2]]$ci[3],rocPS_log_PT[[3]]$ci[3],rocPS_log_PT[[4]]$ci[3],
                 rocPS_log_PT[[5]]$ci[3],rocPS_log_PT[[6]]$ci[3],rocPS_log_PT[[7]]$ci[3])
##CD layer
predCD_log_PT<-jackPredLog(centCD,predictors = c("cohort","PageRank","tarEnt"))
rocCD_log_PT<-list()
rocCD_log_PT[[1]]<-roc(predCD_log_PT$pass,as.numeric(predCD_log_PT$Week1),auc=T,ci=T)
rocCD_log_PT[[2]]<-roc(predCD_log_PT$pass,as.numeric(predCD_log_PT$Week2),auc=T,ci=T)
rocCD_log_PT[[3]]<-roc(predCD_log_PT$pass,as.numeric(predCD_log_PT$Week3),auc=T,ci=T)
rocCD_log_PT[[4]]<-roc(predCD_log_PT$pass,as.numeric(predCD_log_PT$Week4),auc=T,ci=T)
rocCD_log_PT[[5]]<-roc(predCD_log_PT$pass,as.numeric(predCD_log_PT$Week5),auc=T,ci=T)
rocCD_log_PT[[6]]<-roc(predCD_log_PT$pass,as.numeric(predCD_log_PT$Week6),auc=T,ci=T)
rocCD_log_PT[[7]]<-roc(predCD_log_PT$pass,as.numeric(predCD_log_PT$Week7),auc=T,ci=T)
CD_log_PT_auc<-c(rocCD_log_PT[[1]]$auc,rocCD_log_PT[[2]]$auc,rocCD_log_PT[[3]]$auc,rocCD_log_PT[[4]]$auc,
                 rocCD_log_PT[[5]]$auc,rocCD_log_PT[[6]]$auc,rocCD_log_PT[[7]]$auc)
CD_log_PT_ciL<-c(rocCD_log_PT[[1]]$ci[1],rocCD_log_PT[[2]]$ci[1],rocCD_log_PT[[3]]$ci[1],rocCD_log_PT[[4]]$ci[1],
                 rocCD_log_PT[[5]]$ci[1],rocCD_log_PT[[6]]$ci[1],rocCD_log_PT[[7]]$ci[1])
CD_log_PT_ciH<-c(rocCD_log_PT[[1]]$ci[3],rocCD_log_PT[[2]]$ci[3],rocCD_log_PT[[3]]$ci[3],rocCD_log_PT[[4]]$ci[3],
                 rocCD_log_PT[[5]]$ci[3],rocCD_log_PT[[6]]$ci[3],rocCD_log_PT[[7]]$ci[3])

#ICS layer
predICS_log_PT<-jackPredLog(centICS,predictors = c("cohort","PageRank","tarEnt"))
rocICS_log_PT<-list()
rocICS_log_PT[[1]]<-roc(predICS_log_PT$pass,as.numeric(predICS_log_PT$Week1),auc=T,ci=T)
rocICS_log_PT[[2]]<-roc(predICS_log_PT$pass,as.numeric(predICS_log_PT$Week2),auc=T,ci=T)
rocICS_log_PT[[3]]<-roc(predICS_log_PT$pass,as.numeric(predICS_log_PT$Week3),auc=T,ci=T)
rocICS_log_PT[[4]]<-roc(predICS_log_PT$pass,as.numeric(predICS_log_PT$Week4),auc=T,ci=T)
rocICS_log_PT[[5]]<-roc(predICS_log_PT$pass,as.numeric(predICS_log_PT$Week5),auc=T,ci=T)
rocICS_log_PT[[6]]<-roc(predICS_log_PT$pass,as.numeric(predICS_log_PT$Week6),auc=T,ci=T)
rocICS_log_PT[[7]]<-roc(predICS_log_PT$pass,as.numeric(predICS_log_PT$Week7),auc=T,ci=T)
ICS_log_PT_auc<-c(rocICS_log_PT[[1]]$auc,rocICS_log_PT[[2]]$auc,rocICS_log_PT[[3]]$auc,rocICS_log_PT[[4]]$auc,
                  rocICS_log_PT[[5]]$auc,rocICS_log_PT[[6]]$auc,rocICS_log_PT[[7]]$auc)
ICS_log_PT_ciL<-c(rocICS_log_PT[[1]]$ci[1],rocICS_log_PT[[2]]$ci[1],rocICS_log_PT[[3]]$ci[1],rocICS_log_PT[[4]]$ci[1],
                  rocICS_log_PT[[5]]$ci[1],rocICS_log_PT[[6]]$ci[1],rocICS_log_PT[[7]]$ci[1])
ICS_log_PT_ciH<-c(rocICS_log_PT[[1]]$ci[3],rocICS_log_PT[[2]]$ci[3],rocICS_log_PT[[3]]$ci[3],rocICS_log_PT[[4]]$ci[3],
                  rocICS_log_PT[[5]]$ci[3],rocICS_log_PT[[6]]$ci[3],rocICS_log_PT[[7]]$ci[3])



###JUST PASS
predPS_jp_log_PT<-jackPredLog(centPS,outcome = "justpass",predictors = c("cohort","PageRank","tarEnt"))
rocPS_jp_log_PT<-list()
rocPS_jp_log_PT[[1]]<-roc(predPS_jp_log_PT$justpass,as.numeric(predPS_jp_log_PT$Week1),auc=T,ci=T)
rocPS_jp_log_PT[[2]]<-roc(predPS_jp_log_PT$justpass,as.numeric(predPS_jp_log_PT$Week2),auc=T,ci=T)
rocPS_jp_log_PT[[3]]<-roc(predPS_jp_log_PT$justpass,as.numeric(predPS_jp_log_PT$Week3),auc=T,ci=T)
rocPS_jp_log_PT[[4]]<-roc(predPS_jp_log_PT$justpass,as.numeric(predPS_jp_log_PT$Week4),auc=T,ci=T)
rocPS_jp_log_PT[[5]]<-roc(predPS_jp_log_PT$justpass,as.numeric(predPS_jp_log_PT$Week5),auc=T,ci=T)
rocPS_jp_log_PT[[6]]<-roc(predPS_jp_log_PT$justpass,as.numeric(predPS_jp_log_PT$Week6),auc=T,ci=T)
rocPS_jp_log_PT[[7]]<-roc(predPS_jp_log_PT$justpass,as.numeric(predPS_jp_log_PT$Week7),auc=T,ci=T)

PS_jp_log_PT_auc<-c(rocPS_jp_log_PT[[1]]$auc,rocPS_jp_log_PT[[2]]$auc,rocPS_jp_log_PT[[3]]$auc,rocPS_jp_log_PT[[4]]$auc,
                    rocPS_jp_log_PT[[5]]$auc,rocPS_jp_log_PT[[6]]$auc,rocPS_jp_log_PT[[7]]$auc)
PS_jp_log_PT_ciL<-c(rocPS_jp_log_PT[[1]]$ci[1],rocPS_jp_log_PT[[2]]$ci[1],rocPS_jp_log_PT[[3]]$ci[1],rocPS_jp_log_PT[[4]]$ci[1],
                    rocPS_jp_log_PT[[5]]$ci[1],rocPS_jp_log_PT[[6]]$ci[1],rocPS_jp_log_PT[[7]]$ci[1])
PS_jp_log_PT_ciH<-c(rocPS_jp_log_PT[[1]]$ci[3],rocPS_jp_log_PT[[2]]$ci[3],rocPS_jp_log_PT[[3]]$ci[3],rocPS_jp_log_PT[[4]]$ci[3],
                    rocPS_jp_log_PT[[5]]$ci[3],rocPS_jp_log_PT[[6]]$ci[3],rocPS_jp_log_PT[[7]]$ci[3])

predCD_jp_log_PT<-jackPredLog(centCD, outcome = "justpass",predictors = c("cohort","PageRank","tarEnt"))
rocCD_jp_log_PT<-list()
rocCD_jp_log_PT[[1]]<-roc(predCD_jp_log_PT$justpass,as.numeric(predCD_jp_log_PT$Week1),auc=T,ci=T)
rocCD_jp_log_PT[[2]]<-roc(predCD_jp_log_PT$justpass,as.numeric(predCD_jp_log_PT$Week2),auc=T,ci=T)
rocCD_jp_log_PT[[3]]<-roc(predCD_jp_log_PT$justpass,as.numeric(predCD_jp_log_PT$Week3),auc=T,ci=T)
rocCD_jp_log_PT[[4]]<-roc(predCD_jp_log_PT$justpass,as.numeric(predCD_jp_log_PT$Week4),auc=T,ci=T)
rocCD_jp_log_PT[[5]]<-roc(predCD_jp_log_PT$justpass,as.numeric(predCD_jp_log_PT$Week5),auc=T,ci=T)
rocCD_jp_log_PT[[6]]<-roc(predCD_jp_log_PT$justpass,as.numeric(predCD_jp_log_PT$Week6),auc=T,ci=T)
rocCD_jp_log_PT[[7]]<-roc(predCD_jp_log_PT$justpass,as.numeric(predCD_jp_log_PT$Week7),auc=T,ci=T)
CD_jp_log_PT_auc<-c(rocCD_jp_log_PT[[1]]$auc,rocCD_jp_log_PT[[2]]$auc,rocCD_jp_log_PT[[3]]$auc,rocCD_jp_log_PT[[4]]$auc,
                    rocCD_jp_log_PT[[5]]$auc,rocCD_jp_log_PT[[6]]$auc,rocCD_jp_log_PT[[7]]$auc)
CD_jp_log_PT_ciL<-c(rocCD_jp_log_PT[[1]]$ci[1],rocCD_jp_log_PT[[2]]$ci[1],rocCD_jp_log_PT[[3]]$ci[1],rocCD_jp_log_PT[[4]]$ci[1],
                    rocCD_jp_log_PT[[5]]$ci[1],rocCD_jp_log_PT[[6]]$ci[1],rocCD_jp_log_PT[[7]]$ci[1])
CD_jp_log_PT_ciH<-c(rocCD_jp_log_PT[[1]]$ci[3],rocCD_jp_log_PT[[2]]$ci[3],rocCD_jp_log_PT[[3]]$ci[3],rocCD_jp_log_PT[[4]]$ci[3],
                    rocCD_jp_log_PT[[5]]$ci[3],rocCD_jp_log_PT[[6]]$ci[3],rocCD_jp_log_PT[[7]]$ci[3])


predICS_jp_log_PT<-jackPredLog(centICS, outcome = "justpass",predictors = c("cohort","PageRank","tarEnt"))
rocICS_jp_log_PT<-list()
rocICS_jp_log_PT[[1]]<-roc(predICS_jp_log_PT$justpass,as.numeric(predICS_jp_log_PT$Week1),auc=T,ci=T)
rocICS_jp_log_PT[[2]]<-roc(predICS_jp_log_PT$justpass,as.numeric(predICS_jp_log_PT$Week2),auc=T,ci=T)
rocICS_jp_log_PT[[3]]<-roc(predICS_jp_log_PT$justpass,as.numeric(predICS_jp_log_PT$Week3),auc=T,ci=T)
rocICS_jp_log_PT[[4]]<-roc(predICS_jp_log_PT$justpass,as.numeric(predICS_jp_log_PT$Week4),auc=T,ci=T)
rocICS_jp_log_PT[[5]]<-roc(predICS_jp_log_PT$justpass,as.numeric(predICS_jp_log_PT$Week5),auc=T,ci=T)
rocICS_jp_log_PT[[6]]<-roc(predICS_jp_log_PT$justpass,as.numeric(predICS_jp_log_PT$Week6),auc=T,ci=T)
rocICS_jp_log_PT[[7]]<-roc(predICS_jp_log_PT$justpass,as.numeric(predICS_jp_log_PT$Week7),auc=T,ci=T)
ICS_jp_log_PT_auc<-c(rocICS_jp_log_PT[[1]]$auc,rocICS_jp_log_PT[[2]]$auc,rocICS_jp_log_PT[[3]]$auc,rocICS_jp_log_PT[[4]]$auc,
                     rocICS_jp_log_PT[[5]]$auc,rocICS_jp_log_PT[[6]]$auc,rocICS_jp_log_PT[[7]]$auc)
ICS_jp_log_PT_ciL<-c(rocICS_jp_log_PT[[1]]$ci[1],rocICS_jp_log_PT[[2]]$ci[1],rocICS_jp_log_PT[[3]]$ci[1],rocICS_jp_log_PT[[4]]$ci[1],
                     rocICS_jp_log_PT[[5]]$ci[1],rocICS_jp_log_PT[[6]]$ci[1],rocICS_jp_log_PT[[7]]$ci[1])
ICS_jp_log_PT_ciH<-c(rocICS_jp_log_PT[[1]]$ci[3],rocICS_jp_log_PT[[2]]$ci[3],rocICS_jp_log_PT[[3]]$ci[3],rocICS_jp_log_PT[[4]]$ci[3],
                     rocICS_jp_log_PT[[5]]$ci[3],rocICS_jp_log_PT[[6]]$ci[3],rocICS_jp_log_PT[[7]]$ci[3])




######PR H Models######
predPS_log_PH<-jackPredLog(centPS,predictors = c("cohort","PageRank", "Hide"))
rocPS_log_PH<-list()
rocPS_log_PH[[1]]<-roc(predPS_log_PH$pass,as.numeric(predPS_log_PH$Week1),auc=T,ci=T)
rocPS_log_PH[[2]]<-roc(predPS_log_PH$pass,as.numeric(predPS_log_PH$Week2),auc=T,ci=T)
rocPS_log_PH[[3]]<-roc(predPS_log_PH$pass,as.numeric(predPS_log_PH$Week3),auc=T,ci=T)
rocPS_log_PH[[4]]<-roc(predPS_log_PH$pass,as.numeric(predPS_log_PH$Week4),auc=T,ci=T)
rocPS_log_PH[[5]]<-roc(predPS_log_PH$pass,as.numeric(predPS_log_PH$Week5),auc=T,ci=T)
rocPS_log_PH[[6]]<-roc(predPS_log_PH$pass,as.numeric(predPS_log_PH$Week6),auc=T,ci=T)
rocPS_log_PH[[7]]<-roc(predPS_log_PH$pass,as.numeric(predPS_log_PH$Week7),auc=T,ci=T)
PS_log_PH_auc<-c(rocPS_log_PH[[1]]$auc,rocPS_log_PH[[2]]$auc,rocPS_log_PH[[3]]$auc,rocPS_log_PH[[4]]$auc,
                 rocPS_log_PH[[5]]$auc,rocPS_log_PH[[6]]$auc,rocPS_log_PH[[7]]$auc)
PS_log_PH_ciL<-c(rocPS_log_PH[[1]]$ci[1],rocPS_log_PH[[2]]$ci[1],rocPS_log_PH[[3]]$ci[1],rocPS_log_PH[[4]]$ci[1],
                 rocPS_log_PH[[5]]$ci[1],rocPS_log_PH[[6]]$ci[1],rocPS_log_PH[[7]]$ci[1])
PS_log_PH_ciH<-c(rocPS_log_PH[[1]]$ci[3],rocPS_log_PH[[2]]$ci[3],rocPS_log_PH[[3]]$ci[3],rocPS_log_PH[[4]]$ci[3],
                 rocPS_log_PH[[5]]$ci[3],rocPS_log_PH[[6]]$ci[3],rocPS_log_PH[[7]]$ci[3])
##CD layer
predCD_log_PH<-jackPredLog(centCD,predictors = c("cohort","PageRank", "Hide"))
rocCD_log_PH<-list()
rocCD_log_PH[[1]]<-roc(predCD_log_PH$pass,as.numeric(predCD_log_PH$Week1),auc=T,ci=T)
rocCD_log_PH[[2]]<-roc(predCD_log_PH$pass,as.numeric(predCD_log_PH$Week2),auc=T,ci=T)
rocCD_log_PH[[3]]<-roc(predCD_log_PH$pass,as.numeric(predCD_log_PH$Week3),auc=T,ci=T)
rocCD_log_PH[[4]]<-roc(predCD_log_PH$pass,as.numeric(predCD_log_PH$Week4),auc=T,ci=T)
rocCD_log_PH[[5]]<-roc(predCD_log_PH$pass,as.numeric(predCD_log_PH$Week5),auc=T,ci=T)
rocCD_log_PH[[6]]<-roc(predCD_log_PH$pass,as.numeric(predCD_log_PH$Week6),auc=T,ci=T)
rocCD_log_PH[[7]]<-roc(predCD_log_PH$pass,as.numeric(predCD_log_PH$Week7),auc=T,ci=T)
CD_log_PH_auc<-c(rocCD_log_PH[[1]]$auc,rocCD_log_PH[[2]]$auc,rocCD_log_PH[[3]]$auc,rocCD_log_PH[[4]]$auc,
                 rocCD_log_PH[[5]]$auc,rocCD_log_PH[[6]]$auc,rocCD_log_PH[[7]]$auc)
CD_log_PH_ciL<-c(rocCD_log_PH[[1]]$ci[1],rocCD_log_PH[[2]]$ci[1],rocCD_log_PH[[3]]$ci[1],rocCD_log_PH[[4]]$ci[1],
                 rocCD_log_PH[[5]]$ci[1],rocCD_log_PH[[6]]$ci[1],rocCD_log_PH[[7]]$ci[1])
CD_log_PH_ciH<-c(rocCD_log_PH[[1]]$ci[3],rocCD_log_PH[[2]]$ci[3],rocCD_log_PH[[3]]$ci[3],rocCD_log_PH[[4]]$ci[3],
                 rocCD_log_PH[[5]]$ci[3],rocCD_log_PH[[6]]$ci[3],rocCD_log_PH[[7]]$ci[3])

#ICS layer
predICS_log_PH<-jackPredLog(centICS,predictors = c("cohort","PageRank", "Hide"))
rocICS_log_PH<-list()
rocICS_log_PH[[1]]<-roc(predICS_log_PH$pass,as.numeric(predICS_log_PH$Week1),auc=T,ci=T)
rocICS_log_PH[[2]]<-roc(predICS_log_PH$pass,as.numeric(predICS_log_PH$Week2),auc=T,ci=T)
rocICS_log_PH[[3]]<-roc(predICS_log_PH$pass,as.numeric(predICS_log_PH$Week3),auc=T,ci=T)
rocICS_log_PH[[4]]<-roc(predICS_log_PH$pass,as.numeric(predICS_log_PH$Week4),auc=T,ci=T)
rocICS_log_PH[[5]]<-roc(predICS_log_PH$pass,as.numeric(predICS_log_PH$Week5),auc=T,ci=T)
rocICS_log_PH[[6]]<-roc(predICS_log_PH$pass,as.numeric(predICS_log_PH$Week6),auc=T,ci=T)
rocICS_log_PH[[7]]<-roc(predICS_log_PH$pass,as.numeric(predICS_log_PH$Week7),auc=T,ci=T)
ICS_log_PH_auc<-c(rocICS_log_PH[[1]]$auc,rocICS_log_PH[[2]]$auc,rocICS_log_PH[[3]]$auc,rocICS_log_PH[[4]]$auc,
                  rocICS_log_PH[[5]]$auc,rocICS_log_PH[[6]]$auc,rocICS_log_PH[[7]]$auc)
ICS_log_PH_ciL<-c(rocICS_log_PH[[1]]$ci[1],rocICS_log_PH[[2]]$ci[1],rocICS_log_PH[[3]]$ci[1],rocICS_log_PH[[4]]$ci[1],
                  rocICS_log_PH[[5]]$ci[1],rocICS_log_PH[[6]]$ci[1],rocICS_log_PH[[7]]$ci[1])
ICS_log_PH_ciH<-c(rocICS_log_PH[[1]]$ci[3],rocICS_log_PH[[2]]$ci[3],rocICS_log_PH[[3]]$ci[3],rocICS_log_PH[[4]]$ci[3],
                  rocICS_log_PH[[5]]$ci[3],rocICS_log_PH[[6]]$ci[3],rocICS_log_PH[[7]]$ci[3])



###JUST PASS
##PS layer
predPS_jp_log_PH<-jackPredLog(centPS,outcome = "justpass",predictors = c("cohort","PageRank", "Hide"))
rocPS_jp_log_PH<-list()
rocPS_jp_log_PH[[1]]<-roc(predPS_jp_log_PH$justpass,as.numeric(predPS_jp_log_PH$Week1),auc=T,ci=T)
rocPS_jp_log_PH[[2]]<-roc(predPS_jp_log_PH$justpass,as.numeric(predPS_jp_log_PH$Week2),auc=T,ci=T)
rocPS_jp_log_PH[[3]]<-roc(predPS_jp_log_PH$justpass,as.numeric(predPS_jp_log_PH$Week3),auc=T,ci=T)
rocPS_jp_log_PH[[4]]<-roc(predPS_jp_log_PH$justpass,as.numeric(predPS_jp_log_PH$Week4),auc=T,ci=T)
rocPS_jp_log_PH[[5]]<-roc(predPS_jp_log_PH$justpass,as.numeric(predPS_jp_log_PH$Week5),auc=T,ci=T)
rocPS_jp_log_PH[[6]]<-roc(predPS_jp_log_PH$justpass,as.numeric(predPS_jp_log_PH$Week6),auc=T,ci=T)
rocPS_jp_log_PH[[7]]<-roc(predPS_jp_log_PH$justpass,as.numeric(predPS_jp_log_PH$Week7),auc=T,ci=T)

PS_jp_log_PH_auc<-c(rocPS_jp_log_PH[[1]]$auc,rocPS_jp_log_PH[[2]]$auc,rocPS_jp_log_PH[[3]]$auc,rocPS_jp_log_PH[[4]]$auc,
                    rocPS_jp_log_PH[[5]]$auc,rocPS_jp_log_PH[[6]]$auc,rocPS_jp_log_PH[[7]]$auc)
PS_jp_log_PH_ciL<-c(rocPS_jp_log_PH[[1]]$ci[1],rocPS_jp_log_PH[[2]]$ci[1],rocPS_jp_log_PH[[3]]$ci[1],rocPS_jp_log_PH[[4]]$ci[1],
                    rocPS_jp_log_PH[[5]]$ci[1],rocPS_jp_log_PH[[6]]$ci[1],rocPS_jp_log_PH[[7]]$ci[1])
PS_jp_log_PH_ciH<-c(rocPS_jp_log_PH[[1]]$ci[3],rocPS_jp_log_PH[[2]]$ci[3],rocPS_jp_log_PH[[3]]$ci[3],rocPS_jp_log_PH[[4]]$ci[3],
                    rocPS_jp_log_PH[[5]]$ci[3],rocPS_jp_log_PH[[6]]$ci[3],rocPS_jp_log_PH[[7]]$ci[3])
#CD Layer
predCD_jp_log_PH<-jackPredLog(centCD, outcome = "justpass",predictors = c("cohort","PageRank", "Hide"))
rocCD_jp_log_PH<-list()
rocCD_jp_log_PH[[1]]<-roc(predCD_jp_log_PH$justpass,as.numeric(predCD_jp_log_PH$Week1),auc=T,ci=T)
rocCD_jp_log_PH[[2]]<-roc(predCD_jp_log_PH$justpass,as.numeric(predCD_jp_log_PH$Week2),auc=T,ci=T)
rocCD_jp_log_PH[[3]]<-roc(predCD_jp_log_PH$justpass,as.numeric(predCD_jp_log_PH$Week3),auc=T,ci=T)
rocCD_jp_log_PH[[4]]<-roc(predCD_jp_log_PH$justpass,as.numeric(predCD_jp_log_PH$Week4),auc=T,ci=T)
rocCD_jp_log_PH[[5]]<-roc(predCD_jp_log_PH$justpass,as.numeric(predCD_jp_log_PH$Week5),auc=T,ci=T)
rocCD_jp_log_PH[[6]]<-roc(predCD_jp_log_PH$justpass,as.numeric(predCD_jp_log_PH$Week6),auc=T,ci=T)
rocCD_jp_log_PH[[7]]<-roc(predCD_jp_log_PH$justpass,as.numeric(predCD_jp_log_PH$Week7),auc=T,ci=T)
CD_jp_log_PH_auc<-c(rocCD_jp_log_PH[[1]]$auc,rocCD_jp_log_PH[[2]]$auc,rocCD_jp_log_PH[[3]]$auc,rocCD_jp_log_PH[[4]]$auc,
                    rocCD_jp_log_PH[[5]]$auc,rocCD_jp_log_PH[[6]]$auc,rocCD_jp_log_PH[[7]]$auc)
CD_jp_log_PH_ciL<-c(rocCD_jp_log_PH[[1]]$ci[1],rocCD_jp_log_PH[[2]]$ci[1],rocCD_jp_log_PH[[3]]$ci[1],rocCD_jp_log_PH[[4]]$ci[1],
                    rocCD_jp_log_PH[[5]]$ci[1],rocCD_jp_log_PH[[6]]$ci[1],rocCD_jp_log_PH[[7]]$ci[1])
CD_jp_log_PH_ciH<-c(rocCD_jp_log_PH[[1]]$ci[3],rocCD_jp_log_PH[[2]]$ci[3],rocCD_jp_log_PH[[3]]$ci[3],rocCD_jp_log_PH[[4]]$ci[3],
                    rocCD_jp_log_PH[[5]]$ci[3],rocCD_jp_log_PH[[6]]$ci[3],rocCD_jp_log_PH[[7]]$ci[3])

#ICS layer
predICS_jp_log_PH<-jackPredLog(centICS, outcome = "justpass",predictors = c("cohort","PageRank", "Hide"))
rocICS_jp_log_PH<-list()
rocICS_jp_log_PH[[1]]<-roc(predICS_jp_log_PH$justpass,as.numeric(predICS_jp_log_PH$Week1),auc=T,ci=T)
rocICS_jp_log_PH[[2]]<-roc(predICS_jp_log_PH$justpass,as.numeric(predICS_jp_log_PH$Week2),auc=T,ci=T)
rocICS_jp_log_PH[[3]]<-roc(predICS_jp_log_PH$justpass,as.numeric(predICS_jp_log_PH$Week3),auc=T,ci=T)
rocICS_jp_log_PH[[4]]<-roc(predICS_jp_log_PH$justpass,as.numeric(predICS_jp_log_PH$Week4),auc=T,ci=T)
rocICS_jp_log_PH[[5]]<-roc(predICS_jp_log_PH$justpass,as.numeric(predICS_jp_log_PH$Week5),auc=T,ci=T)
rocICS_jp_log_PH[[6]]<-roc(predICS_jp_log_PH$justpass,as.numeric(predICS_jp_log_PH$Week6),auc=T,ci=T)
rocICS_jp_log_PH[[7]]<-roc(predICS_jp_log_PH$justpass,as.numeric(predICS_jp_log_PH$Week7),auc=T,ci=T)
ICS_jp_log_PH_auc<-c(rocICS_jp_log_PH[[1]]$auc,rocICS_jp_log_PH[[2]]$auc,rocICS_jp_log_PH[[3]]$auc,rocICS_jp_log_PH[[4]]$auc,
                     rocICS_jp_log_PH[[5]]$auc,rocICS_jp_log_PH[[6]]$auc,rocICS_jp_log_PH[[7]]$auc)
ICS_jp_log_PH_ciL<-c(rocICS_jp_log_PH[[1]]$ci[1],rocICS_jp_log_PH[[2]]$ci[1],rocICS_jp_log_PH[[3]]$ci[1],rocICS_jp_log_PH[[4]]$ci[1],
                     rocICS_jp_log_PH[[5]]$ci[1],rocICS_jp_log_PH[[6]]$ci[1],rocICS_jp_log_PH[[7]]$ci[1])
ICS_jp_log_PH_ciH<-c(rocICS_jp_log_PH[[1]]$ci[3],rocICS_jp_log_PH[[2]]$ci[3],rocICS_jp_log_PH[[3]]$ci[3],rocICS_jp_log_PH[[4]]$ci[3],
                     rocICS_jp_log_PH[[5]]$ci[3],rocICS_jp_log_PH[[6]]$ci[3],rocICS_jp_log_PH[[7]]$ci[3])


######TE-H models######
#PS layer
predPS_log_TH<-jackPredLog(centPS,predictors = c("cohort","tarEnt", "Hide"))
rocPS_log_TH<-list()
rocPS_log_TH[[1]]<-roc(predPS_log_TH$pass,as.numeric(predPS_log_TH$Week1),auc=T,ci=T)
rocPS_log_TH[[2]]<-roc(predPS_log_TH$pass,as.numeric(predPS_log_TH$Week2),auc=T,ci=T)
rocPS_log_TH[[3]]<-roc(predPS_log_TH$pass,as.numeric(predPS_log_TH$Week3),auc=T,ci=T)
rocPS_log_TH[[4]]<-roc(predPS_log_TH$pass,as.numeric(predPS_log_TH$Week4),auc=T,ci=T)
rocPS_log_TH[[5]]<-roc(predPS_log_TH$pass,as.numeric(predPS_log_TH$Week5),auc=T,ci=T)
rocPS_log_TH[[6]]<-roc(predPS_log_TH$pass,as.numeric(predPS_log_TH$Week6),auc=T,ci=T)
rocPS_log_TH[[7]]<-roc(predPS_log_TH$pass,as.numeric(predPS_log_TH$Week7),auc=T,ci=T)
PS_log_TH_auc<-c(rocPS_log_TH[[1]]$auc,rocPS_log_TH[[2]]$auc,rocPS_log_TH[[3]]$auc,rocPS_log_TH[[4]]$auc,
                 rocPS_log_TH[[5]]$auc,rocPS_log_TH[[6]]$auc,rocPS_log_TH[[7]]$auc)
PS_log_TH_ciL<-c(rocPS_log_TH[[1]]$ci[1],rocPS_log_TH[[2]]$ci[1],rocPS_log_TH[[3]]$ci[1],rocPS_log_TH[[4]]$ci[1],
                 rocPS_log_TH[[5]]$ci[1],rocPS_log_TH[[6]]$ci[1],rocPS_log_TH[[7]]$ci[1])
PS_log_TH_ciH<-c(rocPS_log_TH[[1]]$ci[3],rocPS_log_TH[[2]]$ci[3],rocPS_log_TH[[3]]$ci[3],rocPS_log_TH[[4]]$ci[3],
                 rocPS_log_TH[[5]]$ci[3],rocPS_log_TH[[6]]$ci[3],rocPS_log_TH[[7]]$ci[3])
##CD layer
predCD_log_TH<-jackPredLog(centCD,predictors = c("cohort","tarEnt", "Hide"))
rocCD_log_TH<-list()
rocCD_log_TH[[1]]<-roc(predCD_log_TH$pass,as.numeric(predCD_log_TH$Week1),auc=T,ci=T)
rocCD_log_TH[[2]]<-roc(predCD_log_TH$pass,as.numeric(predCD_log_TH$Week2),auc=T,ci=T)
rocCD_log_TH[[3]]<-roc(predCD_log_TH$pass,as.numeric(predCD_log_TH$Week3),auc=T,ci=T)
rocCD_log_TH[[4]]<-roc(predCD_log_TH$pass,as.numeric(predCD_log_TH$Week4),auc=T,ci=T)
rocCD_log_TH[[5]]<-roc(predCD_log_TH$pass,as.numeric(predCD_log_TH$Week5),auc=T,ci=T)
rocCD_log_TH[[6]]<-roc(predCD_log_TH$pass,as.numeric(predCD_log_TH$Week6),auc=T,ci=T)
rocCD_log_TH[[7]]<-roc(predCD_log_TH$pass,as.numeric(predCD_log_TH$Week7),auc=T,ci=T)
CD_log_TH_auc<-c(rocCD_log_TH[[1]]$auc,rocCD_log_TH[[2]]$auc,rocCD_log_TH[[3]]$auc,rocCD_log_TH[[4]]$auc,
                 rocCD_log_TH[[5]]$auc,rocCD_log_TH[[6]]$auc,rocCD_log_TH[[7]]$auc)
CD_log_TH_ciL<-c(rocCD_log_TH[[1]]$ci[1],rocCD_log_TH[[2]]$ci[1],rocCD_log_TH[[3]]$ci[1],rocCD_log_TH[[4]]$ci[1],
                 rocCD_log_TH[[5]]$ci[1],rocCD_log_TH[[6]]$ci[1],rocCD_log_TH[[7]]$ci[1])
CD_log_TH_ciH<-c(rocCD_log_TH[[1]]$ci[3],rocCD_log_TH[[2]]$ci[3],rocCD_log_TH[[3]]$ci[3],rocCD_log_TH[[4]]$ci[3],
                 rocCD_log_TH[[5]]$ci[3],rocCD_log_TH[[6]]$ci[3],rocCD_log_TH[[7]]$ci[3])

#ICS layer
predICS_log_TH<-jackPredLog(centICS,predictors = c("cohort","tarEnt", "Hide"))
rocICS_log_TH<-list()
rocICS_log_TH[[1]]<-roc(predICS_log_TH$pass,as.numeric(predICS_log_TH$Week1),auc=T,ci=T)
rocICS_log_TH[[2]]<-roc(predICS_log_TH$pass,as.numeric(predICS_log_TH$Week2),auc=T,ci=T)
rocICS_log_TH[[3]]<-roc(predICS_log_TH$pass,as.numeric(predICS_log_TH$Week3),auc=T,ci=T)
rocICS_log_TH[[4]]<-roc(predICS_log_TH$pass,as.numeric(predICS_log_TH$Week4),auc=T,ci=T)
rocICS_log_TH[[5]]<-roc(predICS_log_TH$pass,as.numeric(predICS_log_TH$Week5),auc=T,ci=T)
rocICS_log_TH[[6]]<-roc(predICS_log_TH$pass,as.numeric(predICS_log_TH$Week6),auc=T,ci=T)
rocICS_log_TH[[7]]<-roc(predICS_log_TH$pass,as.numeric(predICS_log_TH$Week7),auc=T,ci=T)
ICS_log_TH_auc<-c(rocICS_log_TH[[1]]$auc,rocICS_log_TH[[2]]$auc,rocICS_log_TH[[3]]$auc,rocICS_log_TH[[4]]$auc,
                  rocICS_log_TH[[5]]$auc,rocICS_log_TH[[6]]$auc,rocICS_log_TH[[7]]$auc)
ICS_log_TH_ciL<-c(rocICS_log_TH[[1]]$ci[1],rocICS_log_TH[[2]]$ci[1],rocICS_log_TH[[3]]$ci[1],rocICS_log_TH[[4]]$ci[1],
                  rocICS_log_TH[[5]]$ci[1],rocICS_log_TH[[6]]$ci[1],rocICS_log_TH[[7]]$ci[1])
ICS_log_TH_ciH<-c(rocICS_log_TH[[1]]$ci[3],rocICS_log_TH[[2]]$ci[3],rocICS_log_TH[[3]]$ci[3],rocICS_log_TH[[4]]$ci[3],
                  rocICS_log_TH[[5]]$ci[3],rocICS_log_TH[[6]]$ci[3],rocICS_log_TH[[7]]$ci[3])


###JUST PASS
#PS Layer
predPS_jp_log_TH<-jackPredLog(centPS,outcome = "justpass",predictors = c("cohort","tarEnt", "Hide"))
rocPS_jp_log_TH<-list()
rocPS_jp_log_TH[[1]]<-roc(predPS_jp_log_TH$justpass,as.numeric(predPS_jp_log_TH$Week1),auc=T,ci=T)
rocPS_jp_log_TH[[2]]<-roc(predPS_jp_log_TH$justpass,as.numeric(predPS_jp_log_TH$Week2),auc=T,ci=T)
rocPS_jp_log_TH[[3]]<-roc(predPS_jp_log_TH$justpass,as.numeric(predPS_jp_log_TH$Week3),auc=T,ci=T)
rocPS_jp_log_TH[[4]]<-roc(predPS_jp_log_TH$justpass,as.numeric(predPS_jp_log_TH$Week4),auc=T,ci=T)
rocPS_jp_log_TH[[5]]<-roc(predPS_jp_log_TH$justpass,as.numeric(predPS_jp_log_TH$Week5),auc=T,ci=T)
rocPS_jp_log_TH[[6]]<-roc(predPS_jp_log_TH$justpass,as.numeric(predPS_jp_log_TH$Week6),auc=T,ci=T)
rocPS_jp_log_TH[[7]]<-roc(predPS_jp_log_TH$justpass,as.numeric(predPS_jp_log_TH$Week7),auc=T,ci=T)

PS_jp_log_TH_auc<-c(rocPS_jp_log_TH[[1]]$auc,rocPS_jp_log_TH[[2]]$auc,rocPS_jp_log_TH[[3]]$auc,rocPS_jp_log_TH[[4]]$auc,
                    rocPS_jp_log_TH[[5]]$auc,rocPS_jp_log_TH[[6]]$auc,rocPS_jp_log_TH[[7]]$auc)
PS_jp_log_TH_ciL<-c(rocPS_jp_log_TH[[1]]$ci[1],rocPS_jp_log_TH[[2]]$ci[1],rocPS_jp_log_TH[[3]]$ci[1],rocPS_jp_log_TH[[4]]$ci[1],
                    rocPS_jp_log_TH[[5]]$ci[1],rocPS_jp_log_TH[[6]]$ci[1],rocPS_jp_log_TH[[7]]$ci[1])
PS_jp_log_TH_ciH<-c(rocPS_jp_log_TH[[1]]$ci[3],rocPS_jp_log_TH[[2]]$ci[3],rocPS_jp_log_TH[[3]]$ci[3],rocPS_jp_log_TH[[4]]$ci[3],
                    rocPS_jp_log_TH[[5]]$ci[3],rocPS_jp_log_TH[[6]]$ci[3],rocPS_jp_log_TH[[7]]$ci[3])
#CD layer
predCD_jp_log_TH<-jackPredLog(centCD, outcome = "justpass",predictors = c("cohort","tarEnt", "Hide"))
rocCD_jp_log_TH<-list()
rocCD_jp_log_TH[[1]]<-roc(predCD_jp_log_TH$justpass,as.numeric(predCD_jp_log_TH$Week1),auc=T,ci=T)
rocCD_jp_log_TH[[2]]<-roc(predCD_jp_log_TH$justpass,as.numeric(predCD_jp_log_TH$Week2),auc=T,ci=T)
rocCD_jp_log_TH[[3]]<-roc(predCD_jp_log_TH$justpass,as.numeric(predCD_jp_log_TH$Week3),auc=T,ci=T)
rocCD_jp_log_TH[[4]]<-roc(predCD_jp_log_TH$justpass,as.numeric(predCD_jp_log_TH$Week4),auc=T,ci=T)
rocCD_jp_log_TH[[5]]<-roc(predCD_jp_log_TH$justpass,as.numeric(predCD_jp_log_TH$Week5),auc=T,ci=T)
rocCD_jp_log_TH[[6]]<-roc(predCD_jp_log_TH$justpass,as.numeric(predCD_jp_log_TH$Week6),auc=T,ci=T)
rocCD_jp_log_TH[[7]]<-roc(predCD_jp_log_TH$justpass,as.numeric(predCD_jp_log_TH$Week7),auc=T,ci=T)
CD_jp_log_TH_auc<-c(rocCD_jp_log_TH[[1]]$auc,rocCD_jp_log_TH[[2]]$auc,rocCD_jp_log_TH[[3]]$auc,rocCD_jp_log_TH[[4]]$auc,
                    rocCD_jp_log_TH[[5]]$auc,rocCD_jp_log_TH[[6]]$auc,rocCD_jp_log_TH[[7]]$auc)
CD_jp_log_TH_ciL<-c(rocCD_jp_log_TH[[1]]$ci[1],rocCD_jp_log_TH[[2]]$ci[1],rocCD_jp_log_TH[[3]]$ci[1],rocCD_jp_log_TH[[4]]$ci[1],
                    rocCD_jp_log_TH[[5]]$ci[1],rocCD_jp_log_TH[[6]]$ci[1],rocCD_jp_log_TH[[7]]$ci[1])
CD_jp_log_TH_ciH<-c(rocCD_jp_log_TH[[1]]$ci[3],rocCD_jp_log_TH[[2]]$ci[3],rocCD_jp_log_TH[[3]]$ci[3],rocCD_jp_log_TH[[4]]$ci[3],
                    rocCD_jp_log_TH[[5]]$ci[3],rocCD_jp_log_TH[[6]]$ci[3],rocCD_jp_log_TH[[7]]$ci[3])

#ICS Layer
predICS_jp_log_TH<-jackPredLog(centICS, outcome = "justpass",predictors = c("cohort","tarEnt", "Hide"))
rocICS_jp_log_TH<-list()
rocICS_jp_log_TH[[1]]<-roc(predICS_jp_log_TH$justpass,as.numeric(predICS_jp_log_TH$Week1),auc=T,ci=T)
rocICS_jp_log_TH[[2]]<-roc(predICS_jp_log_TH$justpass,as.numeric(predICS_jp_log_TH$Week2),auc=T,ci=T)
rocICS_jp_log_TH[[3]]<-roc(predICS_jp_log_TH$justpass,as.numeric(predICS_jp_log_TH$Week3),auc=T,ci=T)
rocICS_jp_log_TH[[4]]<-roc(predICS_jp_log_TH$justpass,as.numeric(predICS_jp_log_TH$Week4),auc=T,ci=T)
rocICS_jp_log_TH[[5]]<-roc(predICS_jp_log_TH$justpass,as.numeric(predICS_jp_log_TH$Week5),auc=T,ci=T)
rocICS_jp_log_TH[[6]]<-roc(predICS_jp_log_TH$justpass,as.numeric(predICS_jp_log_TH$Week6),auc=T,ci=T)
rocICS_jp_log_TH[[7]]<-roc(predICS_jp_log_TH$justpass,as.numeric(predICS_jp_log_TH$Week7),auc=T,ci=T)
ICS_jp_log_TH_auc<-c(rocICS_jp_log_TH[[1]]$auc,rocICS_jp_log_TH[[2]]$auc,rocICS_jp_log_TH[[3]]$auc,rocICS_jp_log_TH[[4]]$auc,
                     rocICS_jp_log_TH[[5]]$auc,rocICS_jp_log_TH[[6]]$auc,rocICS_jp_log_TH[[7]]$auc)
ICS_jp_log_TH_ciL<-c(rocICS_jp_log_TH[[1]]$ci[1],rocICS_jp_log_TH[[2]]$ci[1],rocICS_jp_log_TH[[3]]$ci[1],rocICS_jp_log_TH[[4]]$ci[1],
                     rocICS_jp_log_TH[[5]]$ci[1],rocICS_jp_log_TH[[6]]$ci[1],rocICS_jp_log_TH[[7]]$ci[1])
ICS_jp_log_TH_ciH<-c(rocICS_jp_log_TH[[1]]$ci[3],rocICS_jp_log_TH[[2]]$ci[3],rocICS_jp_log_TH[[3]]$ci[3],rocICS_jp_log_TH[[4]]$ci[3],
                     rocICS_jp_log_TH[[5]]$ci[3],rocICS_jp_log_TH[[6]]$ci[3],rocICS_jp_log_TH[[7]]$ci[3])

######TE models######
#PS layer
predPS_log_T<-jackPredLog(centPS,predictors = c("cohort","tarEnt"))
rocPS_log_T<-list()
rocPS_log_T[[1]]<-roc(predPS_log_T$pass,as.numeric(predPS_log_T$Week1),auc=T,ci=T)
rocPS_log_T[[2]]<-roc(predPS_log_T$pass,as.numeric(predPS_log_T$Week2),auc=T,ci=T)
rocPS_log_T[[3]]<-roc(predPS_log_T$pass,as.numeric(predPS_log_T$Week3),auc=T,ci=T)
rocPS_log_T[[4]]<-roc(predPS_log_T$pass,as.numeric(predPS_log_T$Week4),auc=T,ci=T)
rocPS_log_T[[5]]<-roc(predPS_log_T$pass,as.numeric(predPS_log_T$Week5),auc=T,ci=T)
rocPS_log_T[[6]]<-roc(predPS_log_T$pass,as.numeric(predPS_log_T$Week6),auc=T,ci=T)
rocPS_log_T[[7]]<-roc(predPS_log_T$pass,as.numeric(predPS_log_T$Week7),auc=T,ci=T)
PS_log_T_auc<-c(rocPS_log_T[[1]]$auc,rocPS_log_T[[2]]$auc,rocPS_log_T[[3]]$auc,rocPS_log_T[[4]]$auc,
                rocPS_log_T[[5]]$auc,rocPS_log_T[[6]]$auc,rocPS_log_T[[7]]$auc)
PS_log_T_ciL<-c(rocPS_log_T[[1]]$ci[1],rocPS_log_T[[2]]$ci[1],rocPS_log_T[[3]]$ci[1],rocPS_log_T[[4]]$ci[1],
                rocPS_log_T[[5]]$ci[1],rocPS_log_T[[6]]$ci[1],rocPS_log_T[[7]]$ci[1])
PS_log_T_ciH<-c(rocPS_log_T[[1]]$ci[3],rocPS_log_T[[2]]$ci[3],rocPS_log_T[[3]]$ci[3],rocPS_log_T[[4]]$ci[3],
                rocPS_log_T[[5]]$ci[3],rocPS_log_T[[6]]$ci[3],rocPS_log_T[[7]]$ci[3])
##CD layer
predCD_log_T<-jackPredLog(centCD,predictors = c("cohort","tarEnt"))
rocCD_log_T<-list()
rocCD_log_T[[1]]<-roc(predCD_log_T$pass,as.numeric(predCD_log_T$Week1),auc=T,ci=T)
rocCD_log_T[[2]]<-roc(predCD_log_T$pass,as.numeric(predCD_log_T$Week2),auc=T,ci=T)
rocCD_log_T[[3]]<-roc(predCD_log_T$pass,as.numeric(predCD_log_T$Week3),auc=T,ci=T)
rocCD_log_T[[4]]<-roc(predCD_log_T$pass,as.numeric(predCD_log_T$Week4),auc=T,ci=T)
rocCD_log_T[[5]]<-roc(predCD_log_T$pass,as.numeric(predCD_log_T$Week5),auc=T,ci=T)
rocCD_log_T[[6]]<-roc(predCD_log_T$pass,as.numeric(predCD_log_T$Week6),auc=T,ci=T)
rocCD_log_T[[7]]<-roc(predCD_log_T$pass,as.numeric(predCD_log_T$Week7),auc=T,ci=T)
CD_log_T_auc<-c(rocCD_log_T[[1]]$auc,rocCD_log_T[[2]]$auc,rocCD_log_T[[3]]$auc,rocCD_log_T[[4]]$auc,
                rocCD_log_T[[5]]$auc,rocCD_log_T[[6]]$auc,rocCD_log_T[[7]]$auc)
CD_log_T_ciL<-c(rocCD_log_T[[1]]$ci[1],rocCD_log_T[[2]]$ci[1],rocCD_log_T[[3]]$ci[1],rocCD_log_T[[4]]$ci[1],
                rocCD_log_T[[5]]$ci[1],rocCD_log_T[[6]]$ci[1],rocCD_log_T[[7]]$ci[1])
CD_log_T_ciH<-c(rocCD_log_T[[1]]$ci[3],rocCD_log_T[[2]]$ci[3],rocCD_log_T[[3]]$ci[3],rocCD_log_T[[4]]$ci[3],
                rocCD_log_T[[5]]$ci[3],rocCD_log_T[[6]]$ci[3],rocCD_log_T[[7]]$ci[3])

#ICS layer
predICS_log_T<-jackPredLog(centICS,predictors = c("cohort","tarEnt"))
rocICS_log_T<-list()
rocICS_log_T[[1]]<-roc(predICS_log_T$pass,as.numeric(predICS_log_T$Week1),auc=T,ci=T)
rocICS_log_T[[2]]<-roc(predICS_log_T$pass,as.numeric(predICS_log_T$Week2),auc=T,ci=T)
rocICS_log_T[[3]]<-roc(predICS_log_T$pass,as.numeric(predICS_log_T$Week3),auc=T,ci=T)
rocICS_log_T[[4]]<-roc(predICS_log_T$pass,as.numeric(predICS_log_T$Week4),auc=T,ci=T)
rocICS_log_T[[5]]<-roc(predICS_log_T$pass,as.numeric(predICS_log_T$Week5),auc=T,ci=T)
rocICS_log_T[[6]]<-roc(predICS_log_T$pass,as.numeric(predICS_log_T$Week6),auc=T,ci=T)
rocICS_log_T[[7]]<-roc(predICS_log_T$pass,as.numeric(predICS_log_T$Week7),auc=T,ci=T)
ICS_log_T_auc<-c(rocICS_log_T[[1]]$auc,rocICS_log_T[[2]]$auc,rocICS_log_T[[3]]$auc,rocICS_log_T[[4]]$auc,
                 rocICS_log_T[[5]]$auc,rocICS_log_T[[6]]$auc,rocICS_log_T[[7]]$auc)
ICS_log_T_ciL<-c(rocICS_log_T[[1]]$ci[1],rocICS_log_T[[2]]$ci[1],rocICS_log_T[[3]]$ci[1],rocICS_log_T[[4]]$ci[1],
                 rocICS_log_T[[5]]$ci[1],rocICS_log_T[[6]]$ci[1],rocICS_log_T[[7]]$ci[1])
ICS_log_T_ciH<-c(rocICS_log_T[[1]]$ci[3],rocICS_log_T[[2]]$ci[3],rocICS_log_T[[3]]$ci[3],rocICS_log_T[[4]]$ci[3],
                 rocICS_log_T[[5]]$ci[3],rocICS_log_T[[6]]$ci[3],rocICS_log_T[[7]]$ci[3])

###JUST PASS
#PS layer
predPS_jp_log_T<-jackPredLog(centPS,outcome = "justpass",predictors = c("cohort","tarEnt"))
rocPS_jp_log_T<-list()
rocPS_jp_log_T[[1]]<-roc(predPS_jp_log_T$justpass,as.numeric(predPS_jp_log_T$Week1),auc=T,ci=T)
rocPS_jp_log_T[[2]]<-roc(predPS_jp_log_T$justpass,as.numeric(predPS_jp_log_T$Week2),auc=T,ci=T)
rocPS_jp_log_T[[3]]<-roc(predPS_jp_log_T$justpass,as.numeric(predPS_jp_log_T$Week3),auc=T,ci=T)
rocPS_jp_log_T[[4]]<-roc(predPS_jp_log_T$justpass,as.numeric(predPS_jp_log_T$Week4),auc=T,ci=T)
rocPS_jp_log_T[[5]]<-roc(predPS_jp_log_T$justpass,as.numeric(predPS_jp_log_T$Week5),auc=T,ci=T)
rocPS_jp_log_T[[6]]<-roc(predPS_jp_log_T$justpass,as.numeric(predPS_jp_log_T$Week6),auc=T,ci=T)
rocPS_jp_log_T[[7]]<-roc(predPS_jp_log_T$justpass,as.numeric(predPS_jp_log_T$Week7),auc=T,ci=T)

PS_jp_log_T_auc<-c(rocPS_jp_log_T[[1]]$auc,rocPS_jp_log_T[[2]]$auc,rocPS_jp_log_T[[3]]$auc,rocPS_jp_log_T[[4]]$auc,
                   rocPS_jp_log_T[[5]]$auc,rocPS_jp_log_T[[6]]$auc,rocPS_jp_log_T[[7]]$auc)
PS_jp_log_T_ciL<-c(rocPS_jp_log_T[[1]]$ci[1],rocPS_jp_log_T[[2]]$ci[1],rocPS_jp_log_T[[3]]$ci[1],rocPS_jp_log_T[[4]]$ci[1],
                   rocPS_jp_log_T[[5]]$ci[1],rocPS_jp_log_T[[6]]$ci[1],rocPS_jp_log_T[[7]]$ci[1])
PS_jp_log_T_ciH<-c(rocPS_jp_log_T[[1]]$ci[3],rocPS_jp_log_T[[2]]$ci[3],rocPS_jp_log_T[[3]]$ci[3],rocPS_jp_log_T[[4]]$ci[3],
                   rocPS_jp_log_T[[5]]$ci[3],rocPS_jp_log_T[[6]]$ci[3],rocPS_jp_log_T[[7]]$ci[3])
#CD layer
predCD_jp_log_T<-jackPredLog(centCD, outcome = "justpass",predictors = c("cohort","tarEnt"))
rocCD_jp_log_T<-list()
rocCD_jp_log_T[[1]]<-roc(predCD_jp_log_T$justpass,as.numeric(predCD_jp_log_T$Week1),auc=T,ci=T)
rocCD_jp_log_T[[2]]<-roc(predCD_jp_log_T$justpass,as.numeric(predCD_jp_log_T$Week2),auc=T,ci=T)
rocCD_jp_log_T[[3]]<-roc(predCD_jp_log_T$justpass,as.numeric(predCD_jp_log_T$Week3),auc=T,ci=T)
rocCD_jp_log_T[[4]]<-roc(predCD_jp_log_T$justpass,as.numeric(predCD_jp_log_T$Week4),auc=T,ci=T)
rocCD_jp_log_T[[5]]<-roc(predCD_jp_log_T$justpass,as.numeric(predCD_jp_log_T$Week5),auc=T,ci=T)
rocCD_jp_log_T[[6]]<-roc(predCD_jp_log_T$justpass,as.numeric(predCD_jp_log_T$Week6),auc=T,ci=T)
rocCD_jp_log_T[[7]]<-roc(predCD_jp_log_T$justpass,as.numeric(predCD_jp_log_T$Week7),auc=T,ci=T)
CD_jp_log_T_auc<-c(rocCD_jp_log_T[[1]]$auc,rocCD_jp_log_T[[2]]$auc,rocCD_jp_log_T[[3]]$auc,rocCD_jp_log_T[[4]]$auc,
                   rocCD_jp_log_T[[5]]$auc,rocCD_jp_log_T[[6]]$auc,rocCD_jp_log_T[[7]]$auc)
CD_jp_log_T_ciL<-c(rocCD_jp_log_T[[1]]$ci[1],rocCD_jp_log_T[[2]]$ci[1],rocCD_jp_log_T[[3]]$ci[1],rocCD_jp_log_T[[4]]$ci[1],
                   rocCD_jp_log_T[[5]]$ci[1],rocCD_jp_log_T[[6]]$ci[1],rocCD_jp_log_T[[7]]$ci[1])
CD_jp_log_T_ciH<-c(rocCD_jp_log_T[[1]]$ci[3],rocCD_jp_log_T[[2]]$ci[3],rocCD_jp_log_T[[3]]$ci[3],rocCD_jp_log_T[[4]]$ci[3],
                   rocCD_jp_log_T[[5]]$ci[3],rocCD_jp_log_T[[6]]$ci[3],rocCD_jp_log_T[[7]]$ci[3])
#ICS layer
predICS_jp_log_T<-jackPredLog(centICS, outcome = "justpass",predictors = c("cohort","tarEnt"))
rocICS_jp_log_T<-list()
rocICS_jp_log_T[[1]]<-roc(predICS_jp_log_T$justpass,as.numeric(predICS_jp_log_T$Week1),auc=T,ci=T)
rocICS_jp_log_T[[2]]<-roc(predICS_jp_log_T$justpass,as.numeric(predICS_jp_log_T$Week2),auc=T,ci=T)
rocICS_jp_log_T[[3]]<-roc(predICS_jp_log_T$justpass,as.numeric(predICS_jp_log_T$Week3),auc=T,ci=T)
rocICS_jp_log_T[[4]]<-roc(predICS_jp_log_T$justpass,as.numeric(predICS_jp_log_T$Week4),auc=T,ci=T)
rocICS_jp_log_T[[5]]<-roc(predICS_jp_log_T$justpass,as.numeric(predICS_jp_log_T$Week5),auc=T,ci=T)
rocICS_jp_log_T[[6]]<-roc(predICS_jp_log_T$justpass,as.numeric(predICS_jp_log_T$Week6),auc=T,ci=T)
rocICS_jp_log_T[[7]]<-roc(predICS_jp_log_T$justpass,as.numeric(predICS_jp_log_T$Week7),auc=T,ci=T)
ICS_jp_log_T_auc<-c(rocICS_jp_log_T[[1]]$auc,rocICS_jp_log_T[[2]]$auc,rocICS_jp_log_T[[3]]$auc,rocICS_jp_log_T[[4]]$auc,
                    rocICS_jp_log_T[[5]]$auc,rocICS_jp_log_T[[6]]$auc,rocICS_jp_log_T[[7]]$auc)
ICS_jp_log_T_ciL<-c(rocICS_jp_log_T[[1]]$ci[1],rocICS_jp_log_T[[2]]$ci[1],rocICS_jp_log_T[[3]]$ci[1],rocICS_jp_log_T[[4]]$ci[1],
                    rocICS_jp_log_T[[5]]$ci[1],rocICS_jp_log_T[[6]]$ci[1],rocICS_jp_log_T[[7]]$ci[1])
ICS_jp_log_T_ciH<-c(rocICS_jp_log_T[[1]]$ci[3],rocICS_jp_log_T[[2]]$ci[3],rocICS_jp_log_T[[3]]$ci[3],rocICS_jp_log_T[[4]]$ci[3],
                    rocICS_jp_log_T[[5]]$ci[3],rocICS_jp_log_T[[6]]$ci[3],rocICS_jp_log_T[[7]]$ci[3])



######H models######
#PS layer
predPS_log_H<-jackPredLog(centPS,predictors = c("cohort", "Hide"))
rocPS_log_H<-list()
rocPS_log_H[[1]]<-roc(predPS_log_H$pass,as.numeric(predPS_log_H$Week1),auc=T,ci=T)
rocPS_log_H[[2]]<-roc(predPS_log_H$pass,as.numeric(predPS_log_H$Week2),auc=T,ci=T)
rocPS_log_H[[3]]<-roc(predPS_log_H$pass,as.numeric(predPS_log_H$Week3),auc=T,ci=T)
rocPS_log_H[[4]]<-roc(predPS_log_H$pass,as.numeric(predPS_log_H$Week4),auc=T,ci=T)
rocPS_log_H[[5]]<-roc(predPS_log_H$pass,as.numeric(predPS_log_H$Week5),auc=T,ci=T)
rocPS_log_H[[6]]<-roc(predPS_log_H$pass,as.numeric(predPS_log_H$Week6),auc=T,ci=T)
rocPS_log_H[[7]]<-roc(predPS_log_H$pass,as.numeric(predPS_log_H$Week7),auc=T,ci=T)
PS_log_H_auc<-c(rocPS_log_H[[1]]$auc,rocPS_log_H[[2]]$auc,rocPS_log_H[[3]]$auc,rocPS_log_H[[4]]$auc,
                rocPS_log_H[[5]]$auc,rocPS_log_H[[6]]$auc,rocPS_log_H[[7]]$auc)
PS_log_H_ciL<-c(rocPS_log_H[[1]]$ci[1],rocPS_log_H[[2]]$ci[1],rocPS_log_H[[3]]$ci[1],rocPS_log_H[[4]]$ci[1],
                rocPS_log_H[[5]]$ci[1],rocPS_log_H[[6]]$ci[1],rocPS_log_H[[7]]$ci[1])
PS_log_H_ciH<-c(rocPS_log_H[[1]]$ci[3],rocPS_log_H[[2]]$ci[3],rocPS_log_H[[3]]$ci[3],rocPS_log_H[[4]]$ci[3],
                rocPS_log_H[[5]]$ci[3],rocPS_log_H[[6]]$ci[3],rocPS_log_H[[7]]$ci[3])
##CD layer
predCD_log_H<-jackPredLog(centCD,predictors = c("cohort", "Hide"))
rocCD_log_H<-list()
rocCD_log_H[[1]]<-roc(predCD_log_H$pass,as.numeric(predCD_log_H$Week1),auc=T,ci=T)
rocCD_log_H[[2]]<-roc(predCD_log_H$pass,as.numeric(predCD_log_H$Week2),auc=T,ci=T)
rocCD_log_H[[3]]<-roc(predCD_log_H$pass,as.numeric(predCD_log_H$Week3),auc=T,ci=T)
rocCD_log_H[[4]]<-roc(predCD_log_H$pass,as.numeric(predCD_log_H$Week4),auc=T,ci=T)
rocCD_log_H[[5]]<-roc(predCD_log_H$pass,as.numeric(predCD_log_H$Week5),auc=T,ci=T)
rocCD_log_H[[6]]<-roc(predCD_log_H$pass,as.numeric(predCD_log_H$Week6),auc=T,ci=T)
rocCD_log_H[[7]]<-roc(predCD_log_H$pass,as.numeric(predCD_log_H$Week7),auc=T,ci=T)
CD_log_H_auc<-c(rocCD_log_H[[1]]$auc,rocCD_log_H[[2]]$auc,rocCD_log_H[[3]]$auc,rocCD_log_H[[4]]$auc,
                rocCD_log_H[[5]]$auc,rocCD_log_H[[6]]$auc,rocCD_log_H[[7]]$auc)
CD_log_H_ciL<-c(rocCD_log_H[[1]]$ci[1],rocCD_log_H[[2]]$ci[1],rocCD_log_H[[3]]$ci[1],rocCD_log_H[[4]]$ci[1],
                rocCD_log_H[[5]]$ci[1],rocCD_log_H[[6]]$ci[1],rocCD_log_H[[7]]$ci[1])
CD_log_H_ciH<-c(rocCD_log_H[[1]]$ci[3],rocCD_log_H[[2]]$ci[3],rocCD_log_H[[3]]$ci[3],rocCD_log_H[[4]]$ci[3],
                rocCD_log_H[[5]]$ci[3],rocCD_log_H[[6]]$ci[3],rocCD_log_H[[7]]$ci[3])

#ICS layer
predICS_log_H<-jackPredLog(centICS,predictors = c("cohort", "Hide"))
rocICS_log_H<-list()
rocICS_log_H[[1]]<-roc(predICS_log_H$pass,as.numeric(predICS_log_H$Week1),auc=T,ci=T)
rocICS_log_H[[2]]<-roc(predICS_log_H$pass,as.numeric(predICS_log_H$Week2),auc=T,ci=T)
rocICS_log_H[[3]]<-roc(predICS_log_H$pass,as.numeric(predICS_log_H$Week3),auc=T,ci=T)
rocICS_log_H[[4]]<-roc(predICS_log_H$pass,as.numeric(predICS_log_H$Week4),auc=T,ci=T)
rocICS_log_H[[5]]<-roc(predICS_log_H$pass,as.numeric(predICS_log_H$Week5),auc=T,ci=T)
rocICS_log_H[[6]]<-roc(predICS_log_H$pass,as.numeric(predICS_log_H$Week6),auc=T,ci=T)
rocICS_log_H[[7]]<-roc(predICS_log_H$pass,as.numeric(predICS_log_H$Week7),auc=T,ci=T)
ICS_log_H_auc<-c(rocICS_log_H[[1]]$auc,rocICS_log_H[[2]]$auc,rocICS_log_H[[3]]$auc,rocICS_log_H[[4]]$auc,
                 rocICS_log_H[[5]]$auc,rocICS_log_H[[6]]$auc,rocICS_log_H[[7]]$auc)
ICS_log_H_ciL<-c(rocICS_log_H[[1]]$ci[1],rocICS_log_H[[2]]$ci[1],rocICS_log_H[[3]]$ci[1],rocICS_log_H[[4]]$ci[1],
                 rocICS_log_H[[5]]$ci[1],rocICS_log_H[[6]]$ci[1],rocICS_log_H[[7]]$ci[1])
ICS_log_H_ciH<-c(rocICS_log_H[[1]]$ci[3],rocICS_log_H[[2]]$ci[3],rocICS_log_H[[3]]$ci[3],rocICS_log_H[[4]]$ci[3],
                 rocICS_log_H[[5]]$ci[3],rocICS_log_H[[6]]$ci[3],rocICS_log_H[[7]]$ci[3])


###JUST PASS
predPS_jp_log_H<-jackPredLog(centPS,outcome = "justpass",predictors = c("cohort", "Hide"))
rocPS_jp_log_H<-list()
rocPS_jp_log_H[[1]]<-roc(predPS_jp_log_H$justpass,as.numeric(predPS_jp_log_H$Week1),auc=T,ci=T)
rocPS_jp_log_H[[2]]<-roc(predPS_jp_log_H$justpass,as.numeric(predPS_jp_log_H$Week2),auc=T,ci=T)
rocPS_jp_log_H[[3]]<-roc(predPS_jp_log_H$justpass,as.numeric(predPS_jp_log_H$Week3),auc=T,ci=T)
rocPS_jp_log_H[[4]]<-roc(predPS_jp_log_H$justpass,as.numeric(predPS_jp_log_H$Week4),auc=T,ci=T)
rocPS_jp_log_H[[5]]<-roc(predPS_jp_log_H$justpass,as.numeric(predPS_jp_log_H$Week5),auc=T,ci=T)
rocPS_jp_log_H[[6]]<-roc(predPS_jp_log_H$justpass,as.numeric(predPS_jp_log_H$Week6),auc=T,ci=T)
rocPS_jp_log_H[[7]]<-roc(predPS_jp_log_H$justpass,as.numeric(predPS_jp_log_H$Week7),auc=T,ci=T)

PS_jp_log_H_auc<-c(rocPS_jp_log_H[[1]]$auc,rocPS_jp_log_H[[2]]$auc,rocPS_jp_log_H[[3]]$auc,rocPS_jp_log_H[[4]]$auc,
                   rocPS_jp_log_H[[5]]$auc,rocPS_jp_log_H[[6]]$auc,rocPS_jp_log_H[[7]]$auc)
PS_jp_log_H_ciL<-c(rocPS_jp_log_H[[1]]$ci[1],rocPS_jp_log_H[[2]]$ci[1],rocPS_jp_log_H[[3]]$ci[1],rocPS_jp_log_H[[4]]$ci[1],
                   rocPS_jp_log_H[[5]]$ci[1],rocPS_jp_log_H[[6]]$ci[1],rocPS_jp_log_H[[7]]$ci[1])
PS_jp_log_H_ciH<-c(rocPS_jp_log_H[[1]]$ci[3],rocPS_jp_log_H[[2]]$ci[3],rocPS_jp_log_H[[3]]$ci[3],rocPS_jp_log_H[[4]]$ci[3],
                   rocPS_jp_log_H[[5]]$ci[3],rocPS_jp_log_H[[6]]$ci[3],rocPS_jp_log_H[[7]]$ci[3])

predCD_jp_log_H<-jackPredLog(centCD, outcome = "justpass",predictors = c("cohort", "Hide"))
rocCD_jp_log_H<-list()
rocCD_jp_log_H[[1]]<-roc(predCD_jp_log_H$justpass,as.numeric(predCD_jp_log_H$Week1),auc=T,ci=T)
rocCD_jp_log_H[[2]]<-roc(predCD_jp_log_H$justpass,as.numeric(predCD_jp_log_H$Week2),auc=T,ci=T)
rocCD_jp_log_H[[3]]<-roc(predCD_jp_log_H$justpass,as.numeric(predCD_jp_log_H$Week3),auc=T,ci=T)
rocCD_jp_log_H[[4]]<-roc(predCD_jp_log_H$justpass,as.numeric(predCD_jp_log_H$Week4),auc=T,ci=T)
rocCD_jp_log_H[[5]]<-roc(predCD_jp_log_H$justpass,as.numeric(predCD_jp_log_H$Week5),auc=T,ci=T)
rocCD_jp_log_H[[6]]<-roc(predCD_jp_log_H$justpass,as.numeric(predCD_jp_log_H$Week6),auc=T,ci=T)
rocCD_jp_log_H[[7]]<-roc(predCD_jp_log_H$justpass,as.numeric(predCD_jp_log_H$Week7),auc=T,ci=T)
CD_jp_log_H_auc<-c(rocCD_jp_log_H[[1]]$auc,rocCD_jp_log_H[[2]]$auc,rocCD_jp_log_H[[3]]$auc,rocCD_jp_log_H[[4]]$auc,
                   rocCD_jp_log_H[[5]]$auc,rocCD_jp_log_H[[6]]$auc,rocCD_jp_log_H[[7]]$auc)
CD_jp_log_H_ciL<-c(rocCD_jp_log_H[[1]]$ci[1],rocCD_jp_log_H[[2]]$ci[1],rocCD_jp_log_H[[3]]$ci[1],rocCD_jp_log_H[[4]]$ci[1],
                   rocCD_jp_log_H[[5]]$ci[1],rocCD_jp_log_H[[6]]$ci[1],rocCD_jp_log_H[[7]]$ci[1])
CD_jp_log_H_ciH<-c(rocCD_jp_log_H[[1]]$ci[3],rocCD_jp_log_H[[2]]$ci[3],rocCD_jp_log_H[[3]]$ci[3],rocCD_jp_log_H[[4]]$ci[3],
                   rocCD_jp_log_H[[5]]$ci[3],rocCD_jp_log_H[[6]]$ci[3],rocCD_jp_log_H[[7]]$ci[3])


predICS_jp_log_H<-jackPredLog(centICS, outcome = "justpass",predictors = c("cohort", "Hide"))
rocICS_jp_log_H<-list()
rocICS_jp_log_H[[1]]<-roc(predICS_jp_log_H$justpass,as.numeric(predICS_jp_log_H$Week1),auc=T,ci=T)
rocICS_jp_log_H[[2]]<-roc(predICS_jp_log_H$justpass,as.numeric(predICS_jp_log_H$Week2),auc=T,ci=T)
rocICS_jp_log_H[[3]]<-roc(predICS_jp_log_H$justpass,as.numeric(predICS_jp_log_H$Week3),auc=T,ci=T)
rocICS_jp_log_H[[4]]<-roc(predICS_jp_log_H$justpass,as.numeric(predICS_jp_log_H$Week4),auc=T,ci=T)
rocICS_jp_log_H[[5]]<-roc(predICS_jp_log_H$justpass,as.numeric(predICS_jp_log_H$Week5),auc=T,ci=T)
rocICS_jp_log_H[[6]]<-roc(predICS_jp_log_H$justpass,as.numeric(predICS_jp_log_H$Week6),auc=T,ci=T)
rocICS_jp_log_H[[7]]<-roc(predICS_jp_log_H$justpass,as.numeric(predICS_jp_log_H$Week7),auc=T,ci=T)
ICS_jp_log_H_auc<-c(rocICS_jp_log_H[[1]]$auc,rocICS_jp_log_H[[2]]$auc,rocICS_jp_log_H[[3]]$auc,rocICS_jp_log_H[[4]]$auc,
                    rocICS_jp_log_H[[5]]$auc,rocICS_jp_log_H[[6]]$auc,rocICS_jp_log_H[[7]]$auc)
ICS_jp_log_H_ciL<-c(rocICS_jp_log_H[[1]]$ci[1],rocICS_jp_log_H[[2]]$ci[1],rocICS_jp_log_H[[3]]$ci[1],rocICS_jp_log_H[[4]]$ci[1],
                    rocICS_jp_log_H[[5]]$ci[1],rocICS_jp_log_H[[6]]$ci[1],rocICS_jp_log_H[[7]]$ci[1])
ICS_jp_log_H_ciH<-c(rocICS_jp_log_H[[1]]$ci[3],rocICS_jp_log_H[[2]]$ci[3],rocICS_jp_log_H[[3]]$ci[3],rocICS_jp_log_H[[4]]$ci[3],
                    rocICS_jp_log_H[[5]]$ci[3],rocICS_jp_log_H[[6]]$ci[3],rocICS_jp_log_H[[7]]$ci[3])


######PR models######
#PS layer
predPS_log_P<-jackPredLog(centPS,predictors = c("cohort","PageRank"))
rocPS_log_P<-list()
rocPS_log_P[[1]]<-roc(predPS_log_P$pass,as.numeric(predPS_log_P$Week1),auc=T,ci=T)
rocPS_log_P[[2]]<-roc(predPS_log_P$pass,as.numeric(predPS_log_P$Week2),auc=T,ci=T)
rocPS_log_P[[3]]<-roc(predPS_log_P$pass,as.numeric(predPS_log_P$Week3),auc=T,ci=T)
rocPS_log_P[[4]]<-roc(predPS_log_P$pass,as.numeric(predPS_log_P$Week4),auc=T,ci=T)
rocPS_log_P[[5]]<-roc(predPS_log_P$pass,as.numeric(predPS_log_P$Week5),auc=T,ci=T)
rocPS_log_P[[6]]<-roc(predPS_log_P$pass,as.numeric(predPS_log_P$Week6),auc=T,ci=T)
rocPS_log_P[[7]]<-roc(predPS_log_P$pass,as.numeric(predPS_log_P$Week7),auc=T,ci=T)
PS_log_P_auc<-c(rocPS_log_P[[1]]$auc,rocPS_log_P[[2]]$auc,rocPS_log_P[[3]]$auc,rocPS_log_P[[4]]$auc,
                rocPS_log_P[[5]]$auc,rocPS_log_P[[6]]$auc,rocPS_log_P[[7]]$auc)
PS_log_P_ciL<-c(rocPS_log_P[[1]]$ci[1],rocPS_log_P[[2]]$ci[1],rocPS_log_P[[3]]$ci[1],rocPS_log_P[[4]]$ci[1],
                rocPS_log_P[[5]]$ci[1],rocPS_log_P[[6]]$ci[1],rocPS_log_P[[7]]$ci[1])
PS_log_P_ciH<-c(rocPS_log_P[[1]]$ci[3],rocPS_log_P[[2]]$ci[3],rocPS_log_P[[3]]$ci[3],rocPS_log_P[[4]]$ci[3],
                rocPS_log_P[[5]]$ci[3],rocPS_log_P[[6]]$ci[3],rocPS_log_P[[7]]$ci[3])
##CD layer
predCD_log_P<-jackPredLog(centCD,predictors = c("cohort","PageRank"))
rocCD_log_P<-list()
rocCD_log_P[[1]]<-roc(predCD_log_P$pass,as.numeric(predCD_log_P$Week1),auc=T,ci=T)
rocCD_log_P[[2]]<-roc(predCD_log_P$pass,as.numeric(predCD_log_P$Week2),auc=T,ci=T)
rocCD_log_P[[3]]<-roc(predCD_log_P$pass,as.numeric(predCD_log_P$Week3),auc=T,ci=T)
rocCD_log_P[[4]]<-roc(predCD_log_P$pass,as.numeric(predCD_log_P$Week4),auc=T,ci=T)
rocCD_log_P[[5]]<-roc(predCD_log_P$pass,as.numeric(predCD_log_P$Week5),auc=T,ci=T)
rocCD_log_P[[6]]<-roc(predCD_log_P$pass,as.numeric(predCD_log_P$Week6),auc=T,ci=T)
rocCD_log_P[[7]]<-roc(predCD_log_P$pass,as.numeric(predCD_log_P$Week7),auc=T,ci=T)
CD_log_P_auc<-c(rocCD_log_P[[1]]$auc,rocCD_log_P[[2]]$auc,rocCD_log_P[[3]]$auc,rocCD_log_P[[4]]$auc,
                rocCD_log_P[[5]]$auc,rocCD_log_P[[6]]$auc,rocCD_log_P[[7]]$auc)
CD_log_P_ciL<-c(rocCD_log_P[[1]]$ci[1],rocCD_log_P[[2]]$ci[1],rocCD_log_P[[3]]$ci[1],rocCD_log_P[[4]]$ci[1],
                rocCD_log_P[[5]]$ci[1],rocCD_log_P[[6]]$ci[1],rocCD_log_P[[7]]$ci[1])
CD_log_P_ciH<-c(rocCD_log_P[[1]]$ci[3],rocCD_log_P[[2]]$ci[3],rocCD_log_P[[3]]$ci[3],rocCD_log_P[[4]]$ci[3],
                rocCD_log_P[[5]]$ci[3],rocCD_log_P[[6]]$ci[3],rocCD_log_P[[7]]$ci[3])

#ICS layer
predICS_log_P<-jackPredLog(centICS,predictors = c("cohort","PageRank"))
rocICS_log_P<-list()
rocICS_log_P[[1]]<-roc(predICS_log_P$pass,as.numeric(predICS_log_P$Week1),auc=T,ci=T)
rocICS_log_P[[2]]<-roc(predICS_log_P$pass,as.numeric(predICS_log_P$Week2),auc=T,ci=T)
rocICS_log_P[[3]]<-roc(predICS_log_P$pass,as.numeric(predICS_log_P$Week3),auc=T,ci=T)
rocICS_log_P[[4]]<-roc(predICS_log_P$pass,as.numeric(predICS_log_P$Week4),auc=T,ci=T)
rocICS_log_P[[5]]<-roc(predICS_log_P$pass,as.numeric(predICS_log_P$Week5),auc=T,ci=T)
rocICS_log_P[[6]]<-roc(predICS_log_P$pass,as.numeric(predICS_log_P$Week6),auc=T,ci=T)
rocICS_log_P[[7]]<-roc(predICS_log_P$pass,as.numeric(predICS_log_P$Week7),auc=T,ci=T)
ICS_log_P_auc<-c(rocICS_log_P[[1]]$auc,rocICS_log_P[[2]]$auc,rocICS_log_P[[3]]$auc,rocICS_log_P[[4]]$auc,
                 rocICS_log_P[[5]]$auc,rocICS_log_P[[6]]$auc,rocICS_log_P[[7]]$auc)
ICS_log_P_ciL<-c(rocICS_log_P[[1]]$ci[1],rocICS_log_P[[2]]$ci[1],rocICS_log_P[[3]]$ci[1],rocICS_log_P[[4]]$ci[1],
                 rocICS_log_P[[5]]$ci[1],rocICS_log_P[[6]]$ci[1],rocICS_log_P[[7]]$ci[1])
ICS_log_P_ciH<-c(rocICS_log_P[[1]]$ci[3],rocICS_log_P[[2]]$ci[3],rocICS_log_P[[3]]$ci[3],rocICS_log_P[[4]]$ci[3],
                 rocICS_log_P[[5]]$ci[3],rocICS_log_P[[6]]$ci[3],rocICS_log_P[[7]]$ci[3])

###JUST PASS
#PS layer
predPS_jp_log_P<-jackPredLog(centPS,outcome = "justpass",predictors = c("cohort","PageRank"))
rocPS_jp_log_P<-list()
rocPS_jp_log_P[[1]]<-roc(predPS_jp_log_P$justpass,as.numeric(predPS_jp_log_P$Week1),auc=T,ci=T)
rocPS_jp_log_P[[2]]<-roc(predPS_jp_log_P$justpass,as.numeric(predPS_jp_log_P$Week2),auc=T,ci=T)
rocPS_jp_log_P[[3]]<-roc(predPS_jp_log_P$justpass,as.numeric(predPS_jp_log_P$Week3),auc=T,ci=T)
rocPS_jp_log_P[[4]]<-roc(predPS_jp_log_P$justpass,as.numeric(predPS_jp_log_P$Week4),auc=T,ci=T)
rocPS_jp_log_P[[5]]<-roc(predPS_jp_log_P$justpass,as.numeric(predPS_jp_log_P$Week5),auc=T,ci=T)
rocPS_jp_log_P[[6]]<-roc(predPS_jp_log_P$justpass,as.numeric(predPS_jp_log_P$Week6),auc=T,ci=T)
rocPS_jp_log_P[[7]]<-roc(predPS_jp_log_P$justpass,as.numeric(predPS_jp_log_P$Week7),auc=T,ci=T)

PS_jp_log_P_auc<-c(rocPS_jp_log_P[[1]]$auc,rocPS_jp_log_P[[2]]$auc,rocPS_jp_log_P[[3]]$auc,rocPS_jp_log_P[[4]]$auc,
                   rocPS_jp_log_P[[5]]$auc,rocPS_jp_log_P[[6]]$auc,rocPS_jp_log_P[[7]]$auc)
PS_jp_log_P_ciL<-c(rocPS_jp_log_P[[1]]$ci[1],rocPS_jp_log_P[[2]]$ci[1],rocPS_jp_log_P[[3]]$ci[1],rocPS_jp_log_P[[4]]$ci[1],
                   rocPS_jp_log_P[[5]]$ci[1],rocPS_jp_log_P[[6]]$ci[1],rocPS_jp_log_P[[7]]$ci[1])
PS_jp_log_P_ciH<-c(rocPS_jp_log_P[[1]]$ci[3],rocPS_jp_log_P[[2]]$ci[3],rocPS_jp_log_P[[3]]$ci[3],rocPS_jp_log_P[[4]]$ci[3],
                   rocPS_jp_log_P[[5]]$ci[3],rocPS_jp_log_P[[6]]$ci[3],rocPS_jp_log_P[[7]]$ci[3])
#CD layer
predCD_jp_log_P<-jackPredLog(centCD, outcome = "justpass",predictors = c("cohort","PageRank"))
rocCD_jp_log_P<-list()
rocCD_jp_log_P[[1]]<-roc(predCD_jp_log_P$justpass,as.numeric(predCD_jp_log_P$Week1),auc=T,ci=T)
rocCD_jp_log_P[[2]]<-roc(predCD_jp_log_P$justpass,as.numeric(predCD_jp_log_P$Week2),auc=T,ci=T)
rocCD_jp_log_P[[3]]<-roc(predCD_jp_log_P$justpass,as.numeric(predCD_jp_log_P$Week3),auc=T,ci=T)
rocCD_jp_log_P[[4]]<-roc(predCD_jp_log_P$justpass,as.numeric(predCD_jp_log_P$Week4),auc=T,ci=T)
rocCD_jp_log_P[[5]]<-roc(predCD_jp_log_P$justpass,as.numeric(predCD_jp_log_P$Week5),auc=T,ci=T)
rocCD_jp_log_P[[6]]<-roc(predCD_jp_log_P$justpass,as.numeric(predCD_jp_log_P$Week6),auc=T,ci=T)
rocCD_jp_log_P[[7]]<-roc(predCD_jp_log_P$justpass,as.numeric(predCD_jp_log_P$Week7),auc=T,ci=T)
CD_jp_log_P_auc<-c(rocCD_jp_log_P[[1]]$auc,rocCD_jp_log_P[[2]]$auc,rocCD_jp_log_P[[3]]$auc,rocCD_jp_log_P[[4]]$auc,
                   rocCD_jp_log_P[[5]]$auc,rocCD_jp_log_P[[6]]$auc,rocCD_jp_log_P[[7]]$auc)
CD_jp_log_P_ciL<-c(rocCD_jp_log_P[[1]]$ci[1],rocCD_jp_log_P[[2]]$ci[1],rocCD_jp_log_P[[3]]$ci[1],rocCD_jp_log_P[[4]]$ci[1],
                   rocCD_jp_log_P[[5]]$ci[1],rocCD_jp_log_P[[6]]$ci[1],rocCD_jp_log_P[[7]]$ci[1])
CD_jp_log_P_ciH<-c(rocCD_jp_log_P[[1]]$ci[3],rocCD_jp_log_P[[2]]$ci[3],rocCD_jp_log_P[[3]]$ci[3],rocCD_jp_log_P[[4]]$ci[3],
                   rocCD_jp_log_P[[5]]$ci[3],rocCD_jp_log_P[[6]]$ci[3],rocCD_jp_log_P[[7]]$ci[3])

#ICS layer
predICS_jp_log_P<-jackPredLog(centICS, outcome = "justpass",predictors = c("cohort","PageRank"))
rocICS_jp_log_P<-list()
rocICS_jp_log_P[[1]]<-roc(predICS_jp_log_P$justpass,as.numeric(predICS_jp_log_P$Week1),auc=T,ci=T)
rocICS_jp_log_P[[2]]<-roc(predICS_jp_log_P$justpass,as.numeric(predICS_jp_log_P$Week2),auc=T,ci=T)
rocICS_jp_log_P[[3]]<-roc(predICS_jp_log_P$justpass,as.numeric(predICS_jp_log_P$Week3),auc=T,ci=T)
rocICS_jp_log_P[[4]]<-roc(predICS_jp_log_P$justpass,as.numeric(predICS_jp_log_P$Week4),auc=T,ci=T)
rocICS_jp_log_P[[5]]<-roc(predICS_jp_log_P$justpass,as.numeric(predICS_jp_log_P$Week5),auc=T,ci=T)
rocICS_jp_log_P[[6]]<-roc(predICS_jp_log_P$justpass,as.numeric(predICS_jp_log_P$Week6),auc=T,ci=T)
rocICS_jp_log_P[[7]]<-roc(predICS_jp_log_P$justpass,as.numeric(predICS_jp_log_P$Week7),auc=T,ci=T)
ICS_jp_log_P_auc<-c(rocICS_jp_log_P[[1]]$auc,rocICS_jp_log_P[[2]]$auc,rocICS_jp_log_P[[3]]$auc,rocICS_jp_log_P[[4]]$auc,
                    rocICS_jp_log_P[[5]]$auc,rocICS_jp_log_P[[6]]$auc,rocICS_jp_log_P[[7]]$auc)
ICS_jp_log_P_ciL<-c(rocICS_jp_log_P[[1]]$ci[1],rocICS_jp_log_P[[2]]$ci[1],rocICS_jp_log_P[[3]]$ci[1],rocICS_jp_log_P[[4]]$ci[1],
                    rocICS_jp_log_P[[5]]$ci[1],rocICS_jp_log_P[[6]]$ci[1],rocICS_jp_log_P[[7]]$ci[1])
ICS_jp_log_P_ciH<-c(rocICS_jp_log_P[[1]]$ci[3],rocICS_jp_log_P[[2]]$ci[3],rocICS_jp_log_P[[3]]$ci[3],rocICS_jp_log_P[[4]]$ci[3],
                    rocICS_jp_log_P[[5]]$ci[3],rocICS_jp_log_P[[6]]$ci[3],rocICS_jp_log_P[[7]]$ci[3])



####LogReg PLOTS####
####Mean over all models####
##pass-fail##
PS_log_RegAll<-data.frame(PS_log_PTH_auc,PS_log_PT_auc,PS_log_TH_auc,PS_log_PH_auc,PS_log_P_auc,PS_log_T_auc,PS_log_H_auc)
PS_log_RegAllciH<-data.frame(PS_log_PTH_ciH,PS_log_PT_ciH,PS_log_TH_ciH,PS_log_PH_ciH,PS_log_P_ciH,PS_log_T_ciH,PS_log_H_ciH)
PS_log_RegAllciL<-data.frame(PS_log_PTH_ciL,PS_log_PT_ciL,PS_log_TH_ciL,PS_log_PH_ciL,PS_log_P_ciL,PS_log_T_ciL,PS_log_H_ciL)

PS_log_RegSD<-sqrt(166)*(PS_log_RegAllciH-PS_log_RegAllciL)/3.92
PS_log_RegWeigths<-1/PS_log_RegSD^2
PS_log_RegWM<-rowSums(PS_log_RegAll*PS_log_RegWeigths)/rowSums(PS_log_RegWeigths)
PS_log_RegSEWM<-sqrt(1/rowSums(PS_log_RegWeigths))

CD_log_RegAll<-data.frame(CD_log_PTH_auc,CD_log_PT_auc,CD_log_TH_auc,CD_log_PH_auc,CD_log_P_auc,CD_log_T_auc,CD_log_H_auc)
CD_log_RegAllciH<-data.frame(CD_log_PTH_ciH,CD_log_PT_ciH,CD_log_TH_ciH,CD_log_PH_ciH,CD_log_P_ciH,CD_log_T_ciH,CD_log_H_ciH)
CD_log_RegAllciL<-data.frame(CD_log_PTH_ciL,CD_log_PT_ciL,CD_log_TH_ciL,CD_log_PH_ciL,CD_log_P_ciL,CD_log_T_ciL,CD_log_H_ciL)

CD_log_RegSD<-sqrt(166)*(CD_log_RegAllciH-CD_log_RegAllciL)/3.92
CD_log_RegWeigths<-1/CD_log_RegSD^2
CD_log_RegWM<-rowSums(CD_log_RegAll*CD_log_RegWeigths)/rowSums(CD_log_RegWeigths)
CD_log_RegSEWM<-sqrt(1/rowSums(CD_log_RegWeigths))

ICS_log_RegAll<-data.frame(ICS_log_PTH_auc,ICS_log_PT_auc,ICS_log_TH_auc,ICS_log_PH_auc,ICS_log_P_auc,ICS_log_T_auc,ICS_log_H_auc)
ICS_log_RegAllciH<-data.frame(ICS_log_PTH_ciH,ICS_log_PT_ciH,ICS_log_TH_ciH,ICS_log_PH_ciH,ICS_log_P_ciH,ICS_log_T_ciH,ICS_log_H_ciH)
ICS_log_RegAllciL<-data.frame(ICS_log_PTH_ciL,ICS_log_PT_ciL,ICS_log_TH_ciL,ICS_log_PH_ciL,ICS_log_P_ciL,ICS_log_T_ciL,ICS_log_H_ciL)

ICS_log_RegSD<-sqrt(166)*(ICS_log_RegAllciH-ICS_log_RegAllciL)/3.92
ICS_log_RegWeigths<-1/ICS_log_RegSD^2
ICS_log_RegWM<-rowSums(ICS_log_RegAll*ICS_log_RegWeigths)/rowSums(ICS_log_RegWeigths)
ICS_log_RegSEWM<-sqrt(1/rowSums(ICS_log_RegWeigths))

##just pass-fail##
PS_jp_log_RegAll<-data.frame(PS_jp_log_PTH_auc,PS_jp_log_PT_auc,PS_jp_log_TH_auc,PS_jp_log_PH_auc,PS_jp_log_P_auc,PS_jp_log_T_auc,PS_jp_log_H_auc)
PS_jp_log_RegAllciH<-data.frame(PS_jp_log_PTH_ciH,PS_jp_log_PT_ciH,PS_jp_log_TH_ciH,PS_jp_log_PH_ciH,PS_jp_log_P_ciH,PS_jp_log_T_ciH,PS_jp_log_H_ciH)
PS_jp_log_RegAllciL<-data.frame(PS_jp_log_PTH_ciL,PS_jp_log_PT_ciL,PS_jp_log_TH_ciL,PS_jp_log_PH_ciL,PS_jp_log_P_ciL,PS_jp_log_T_ciL,PS_jp_log_H_ciL)

PS_jp_log_RegSD<-sqrt(67)*(PS_jp_log_RegAllciH-PS_jp_log_RegAllciL)/3.92
PS_jp_log_RegWeigths<-1/PS_jp_log_RegSD^2
PS_jp_log_RegWM<-rowSums(PS_jp_log_RegAll*PS_jp_log_RegWeigths)/rowSums(PS_jp_log_RegWeigths)
PS_jp_log_RegSEWM<-sqrt(1/rowSums(PS_jp_log_RegWeigths))

CD_jp_log_RegAll<-data.frame(CD_jp_log_PTH_auc,CD_jp_log_PT_auc,CD_jp_log_TH_auc,CD_jp_log_PH_auc,CD_jp_log_P_auc,CD_jp_log_T_auc,CD_jp_log_H_auc)
CD_jp_log_RegAllciH<-data.frame(CD_jp_log_PTH_ciH,CD_jp_log_PT_ciH,CD_jp_log_TH_ciH,CD_jp_log_PH_ciH,CD_jp_log_P_ciH,CD_jp_log_T_ciH,CD_jp_log_H_ciH)
CD_jp_log_RegAllciL<-data.frame(CD_jp_log_PTH_ciL,CD_jp_log_PT_ciL,CD_jp_log_TH_ciL,CD_jp_log_PH_ciL,CD_jp_log_P_ciL,CD_jp_log_T_ciL,CD_jp_log_H_ciL)

CD_jp_log_RegSD<-sqrt(67)*(CD_jp_log_RegAllciH-CD_jp_log_RegAllciL)/3.92
CD_jp_log_RegWeigths<-1/CD_jp_log_RegSD^2 #Zero standard deviations become infinite here. They are probably a sign of an outlier.
CD_jp_log_RegWeigths[!is.finite(as.matrix(CD_jp_log_RegWeigths))]<-NA #Correction for zero standard deviations.
CD_jp_log_RegWM<-rowSums(CD_jp_log_RegAll*CD_jp_log_RegWeigths,na.rm = T)/rowSums(CD_jp_log_RegWeigths,na.rm = T)
CD_jp_log_RegSEWM<-sqrt(1/rowSums(CD_jp_log_RegWeigths,na.rm=T))

ICS_jp_log_RegAll<-data.frame(ICS_jp_log_PTH_auc,ICS_jp_log_PT_auc,ICS_jp_log_TH_auc,ICS_jp_log_PH_auc,ICS_jp_log_P_auc,ICS_jp_log_T_auc,ICS_jp_log_H_auc)
ICS_jp_log_RegAllciH<-data.frame(ICS_jp_log_PTH_ciH,ICS_jp_log_PT_ciH,ICS_jp_log_TH_ciH,ICS_jp_log_PH_ciH,ICS_jp_log_P_ciH,ICS_jp_log_T_ciH,ICS_jp_log_H_ciH)
ICS_jp_log_RegAllciL<-data.frame(ICS_jp_log_PTH_ciL,ICS_jp_log_PT_ciL,ICS_jp_log_TH_ciL,ICS_jp_log_PH_ciL,ICS_jp_log_P_ciL,ICS_jp_log_T_ciL,ICS_jp_log_H_ciL)

ICS_jp_log_RegSD<-sqrt(67)*(ICS_jp_log_RegAllciH-ICS_jp_log_RegAllciL)/3.92
ICS_jp_log_RegWeigths<-1/ICS_jp_log_RegSD^2
ICS_jp_log_RegWM<-rowSums(ICS_jp_log_RegAll*ICS_jp_log_RegWeigths)/rowSums(ICS_jp_log_RegWeigths)
ICS_jp_log_RegSEWM<-sqrt(1/rowSums(ICS_jp_log_RegWeigths))

####PLOTTING####
pdf(file = "plots/ROC_AUC_plots/AUC04_NetworkMeasuresCohort_log_weightedMeans.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,2))
x<-c(1:7)
plot(x, PS_log_RegWM,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Logistic Regression Pass-fail", sub="Weighted mean of all centrality models",type="b"
)
lines(x+0.1,CD_log_RegWM,pch=1,type="b",col="darkred")
lines(x-0.1,ICS_log_RegWM,pch=2,type="b",col="darkblue")
# hack: we draw arrows but with very special "arrowheads"

arrows(x, PS_log_RegWM-1.96*PS_log_RegSEWM/sqrt(166), x, PS_log_RegWM+1.96*PS_log_RegSEWM/sqrt(166), length=0.05, angle=90, code=3)
arrows(x+0.1, CD_log_RegWM-1.96*CD_log_RegSEWM/sqrt(166), x+0.1, CD_log_RegWM+1.96*CD_log_RegSEWM/sqrt(166), length=0.05, angle=90, code=3,col="darkred")
arrows(x-0.1, ICS_log_RegWM-1.96*ICS_log_RegSEWM/sqrt(166), x-0.1, ICS_log_RegWM+1.96*ICS_log_RegSEWM/sqrt(166), length=0.05, angle=90, code=3,col="darkblue")
legend(3, 0.3, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","darkred", "darkblue"), pch=c(19,1,2),lty=1, cex=0.8)
abline(h=lazy)

x<-c(1:7)
plot(x, PS_jp_log_RegWM,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Logistic Regression Just pass-fail", sub="Weighted mean of all centrality models",type="b"
)
lines(x+0.1,CD_jp_log_RegWM,pch=1,type="b",col="darkred")
lines(x-0.1,ICS_jp_log_RegWM,pch=2,type="b",col="darkblue")
# hack: we draw arrows but with very special "arrowheads"

arrows(x, PS_jp_log_RegWM-1.96*PS_jp_log_RegSEWM/sqrt(166), x, PS_jp_log_RegWM+1.96*PS_jp_log_RegSEWM/sqrt(166), length=0.05, angle=90, code=3)
arrows(x+0.1, CD_jp_log_RegWM-1.96*CD_jp_log_RegSEWM/sqrt(166), x+0.1, CD_jp_log_RegWM+1.96*CD_jp_log_RegSEWM/sqrt(166), length=0.05, angle=90, code=3,col="darkred")
arrows(x-0.1, ICS_jp_log_RegWM-1.96*ICS_jp_log_RegSEWM/sqrt(166), x-0.1, ICS_jp_log_RegWM+1.96*ICS_jp_log_RegSEWM/sqrt(166), length=0.05, angle=90, code=3,col="darkblue")
legend(3, 0.3, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","darkred", "darkblue"), pch=c(19,1,2),lty=1, cex=0.8)
abline(h=lazy_jp)
dev.off()
####ROC curves####
pdf(file = "plots/ROC_AUC_plots/ROC04_log_pf.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 14) # The height of the plot in inches
par(mfrow=c(4,2))
plot(rocCD_log_PTH[[1]], main="PTH")
lines(rocCD_log_PTH[[2]],col="yellow")
lines(rocCD_log_PTH[[3]],col="blue")
lines(rocCD_log_PTH[[4]],col="magenta")
lines(rocCD_log_PTH[[5]],col="red")
lines(rocCD_log_PTH[[6]],col="green")
lines(rocCD_log_PTH[[7]],col="purple")

plot(rocCD_log_PT[[1]], main="PT")
lines(rocCD_log_PT[[2]],col="yellow")
lines(rocCD_log_PT[[3]],col="blue")
lines(rocCD_log_PT[[4]],col="magenta")
lines(rocCD_log_PT[[5]],col="red")
lines(rocCD_log_PT[[6]],col="green")
lines(rocCD_log_PT[[7]],col="purple")

plot(rocCD_log_PH[[1]], main="PH")
lines(rocCD_log_PH[[2]],col="yellow")
lines(rocCD_log_PH[[3]],col="blue")
lines(rocCD_log_PH[[4]],col="magenta")
lines(rocCD_log_PH[[5]],col="red")
lines(rocCD_log_PH[[6]],col="green")
lines(rocCD_log_PH[[7]],col="purple")

plot(rocCD_log_TH[[1]], main="TH")
lines(rocCD_log_TH[[2]],col="yellow")
lines(rocCD_log_TH[[3]],col="blue")
lines(rocCD_log_TH[[4]],col="magenta")
lines(rocCD_log_TH[[5]],col="red")
lines(rocCD_log_TH[[6]],col="green")
lines(rocCD_log_TH[[7]],col="purple")

plot(rocCD_log_P[[1]], main="P")
lines(rocCD_log_P[[2]],col="yellow")
lines(rocCD_log_P[[3]],col="blue")
lines(rocCD_log_P[[4]],col="magenta")
lines(rocCD_log_P[[5]],col="red")
lines(rocCD_log_P[[6]],col="green")
lines(rocCD_log_P[[7]],col="purple")

plot(rocCD_log_T[[1]], main="T")
lines(rocCD_log_T[[2]],col="yellow")
lines(rocCD_log_T[[3]],col="blue")
lines(rocCD_log_T[[4]],col="magenta")
lines(rocCD_log_T[[5]],col="red")
lines(rocCD_log_T[[6]],col="green")
lines(rocCD_log_T[[7]],col="purple")

plot(rocCD_log_H[[1]], main="H")
lines(rocCD_log_H[[2]],col="yellow")
lines(rocCD_log_H[[3]],col="blue")
lines(rocCD_log_H[[4]],col="magenta")
lines(rocCD_log_H[[5]],col="red")
lines(rocCD_log_H[[6]],col="green")
lines(rocCD_log_H[[7]],col="purple")

plot.new()
legend(0,1,c("week 1","week 2","week 3","week 4"),
       col=c("black","yellow", "blue","magenta"),lty=1, cex=2)
legend(0.5,1,c("week 5","week 6","week 7"),
       col=c("red","green","purple"),lty=1, cex=2)

dev.off()
pdf(file = "plots/ROC_AUC_plots/ROC04_log_jpf.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 14) # The height of the plot in inches
par(mfrow=c(4,2))
plot(rocCD_jp_log_PTH[[1]])
lines(rocCD_jp_log_PTH[[2]],col="yellow")
lines(rocCD_jp_log_PTH[[3]],col="blue")
lines(rocCD_jp_log_PTH[[4]],col="magenta")
lines(rocCD_jp_log_PTH[[5]],col="red")
lines(rocCD_jp_log_PTH[[6]],col="green")
lines(rocCD_jp_log_PTH[[7]],col="purple")

plot(rocCD_jp_log_PT[[1]])
lines(rocCD_jp_log_PT[[2]],col="yellow")
lines(rocCD_jp_log_PT[[3]],col="blue")
lines(rocCD_jp_log_PT[[4]],col="magenta")
lines(rocCD_jp_log_PT[[5]],col="red")
lines(rocCD_jp_log_PT[[6]],col="green")
lines(rocCD_jp_log_PT[[7]],col="purple")

plot(rocCD_jp_log_PH[[1]])
lines(rocCD_jp_log_PH[[2]],col="yellow")
lines(rocCD_jp_log_PH[[3]],col="blue")
lines(rocCD_jp_log_PH[[4]],col="magenta")
lines(rocCD_jp_log_PH[[5]],col="red")
lines(rocCD_jp_log_PH[[6]],col="green")
lines(rocCD_jp_log_PH[[7]],col="purple")

plot(rocCD_jp_log_TH[[1]])
lines(rocCD_jp_log_TH[[2]],col="yellow")
lines(rocCD_jp_log_TH[[3]],col="blue")
lines(rocCD_jp_log_TH[[4]],col="magenta")
lines(rocCD_jp_log_TH[[5]],col="red")
lines(rocCD_jp_log_TH[[6]],col="green")
lines(rocCD_jp_log_TH[[7]],col="purple")

plot(rocCD_jp_log_P[[1]])
lines(rocCD_jp_log_P[[2]],col="yellow")
lines(rocCD_jp_log_P[[3]],col="blue")
lines(rocCD_jp_log_P[[4]],col="magenta")
lines(rocCD_jp_log_P[[5]],col="red")
lines(rocCD_jp_log_P[[6]],col="green")
lines(rocCD_jp_log_P[[7]],col="purple")

plot(rocCD_jp_log_T[[1]])
lines(rocCD_jp_log_T[[2]],col="yellow")
lines(rocCD_jp_log_T[[3]],col="blue")
lines(rocCD_jp_log_T[[4]],col="magenta")
lines(rocCD_jp_log_T[[5]],col="red")
lines(rocCD_jp_log_T[[6]],col="green")
lines(rocCD_jp_log_T[[7]],col="purple")

plot(rocCD_jp_log_H[[1]])
lines(rocCD_jp_log_T[[2]],col="yellow")
lines(rocCD_jp_log_T[[3]],col="blue")
lines(rocCD_jp_log_T[[4]],col="magenta")
lines(rocCD_jp_log_T[[5]],col="red")
lines(rocCD_jp_log_T[[6]],col="green")
lines(rocCD_jp_log_T[[7]],col="purple")

plot.new()
legend(0,1,c("week 1","week 2","week 3","week 4"),
       col=c("black","yellow", "blue","magenta"),lty=1, cex=2)
legend(0.5,1,c("week 5","week 6","week 7"),
       col=c("red","green","purple"),lty=1, cex=2)

dev.off()



################################################
#########LINEAR DISCRIMINANT ANALYSIS###########
################################################
######Run models and calculate AUC (with CI)####
######PR-TE-H Models######
predPS_lda_PTH<-jackPredLDA(centPS,predictors = c("cohort","PageRank","tarEnt", "Hide"))
rocPS_lda_PTH<-list()
rocPS_lda_PTH[[1]]<-roc(predPS_lda_PTH$pass,as.numeric(predPS_lda_PTH$Week1),auc=T,ci=T)
rocPS_lda_PTH[[2]]<-roc(predPS_lda_PTH$pass,as.numeric(predPS_lda_PTH$Week2),auc=T,ci=T)
rocPS_lda_PTH[[3]]<-roc(predPS_lda_PTH$pass,as.numeric(predPS_lda_PTH$Week3),auc=T,ci=T)
rocPS_lda_PTH[[4]]<-roc(predPS_lda_PTH$pass,as.numeric(predPS_lda_PTH$Week4),auc=T,ci=T)
rocPS_lda_PTH[[5]]<-roc(predPS_lda_PTH$pass,as.numeric(predPS_lda_PTH$Week5),auc=T,ci=T)
rocPS_lda_PTH[[6]]<-roc(predPS_lda_PTH$pass,as.numeric(predPS_lda_PTH$Week6),auc=T,ci=T)
rocPS_lda_PTH[[7]]<-roc(predPS_lda_PTH$pass,as.numeric(predPS_lda_PTH$Week7),auc=T,ci=T)
PS_lda_PTH_auc<-c(rocPS_lda_PTH[[1]]$auc,rocPS_lda_PTH[[2]]$auc,rocPS_lda_PTH[[3]]$auc,rocPS_lda_PTH[[4]]$auc,
                  rocPS_lda_PTH[[5]]$auc,rocPS_lda_PTH[[6]]$auc,rocPS_lda_PTH[[7]]$auc)
PS_lda_PTH_ciL<-c(rocPS_lda_PTH[[1]]$ci[1],rocPS_lda_PTH[[2]]$ci[1],rocPS_lda_PTH[[3]]$ci[1],rocPS_lda_PTH[[4]]$ci[1],
                  rocPS_lda_PTH[[5]]$ci[1],rocPS_lda_PTH[[6]]$ci[1],rocPS_lda_PTH[[7]]$ci[1])
PS_lda_PTH_ciH<-c(rocPS_lda_PTH[[1]]$ci[3],rocPS_lda_PTH[[2]]$ci[3],rocPS_lda_PTH[[3]]$ci[3],rocPS_lda_PTH[[4]]$ci[3],
                  rocPS_lda_PTH[[5]]$ci[3],rocPS_lda_PTH[[6]]$ci[3],rocPS_lda_PTH[[7]]$ci[3])

predCD_lda_PTH<-jackPredLDA(centCD,predictors = c("cohort","PageRank","tarEnt", "Hide"))
rocCD_lda_PTH<-list()
rocCD_lda_PTH[[1]]<-roc(predCD_lda_PTH$pass,as.numeric(predCD_lda_PTH$Week1),auc=T,ci=T)
rocCD_lda_PTH[[2]]<-roc(predCD_lda_PTH$pass,as.numeric(predCD_lda_PTH$Week2),auc=T,ci=T)
rocCD_lda_PTH[[3]]<-roc(predCD_lda_PTH$pass,as.numeric(predCD_lda_PTH$Week3),auc=T,ci=T)
rocCD_lda_PTH[[4]]<-roc(predCD_lda_PTH$pass,as.numeric(predCD_lda_PTH$Week4),auc=T,ci=T)
rocCD_lda_PTH[[5]]<-roc(predCD_lda_PTH$pass,as.numeric(predCD_lda_PTH$Week5),auc=T,ci=T)
rocCD_lda_PTH[[6]]<-roc(predCD_lda_PTH$pass,as.numeric(predCD_lda_PTH$Week6),auc=T,ci=T)
rocCD_lda_PTH[[7]]<-roc(predCD_lda_PTH$pass,as.numeric(predCD_lda_PTH$Week7),auc=T,ci=T)
CD_lda_PTH_auc<-c(rocCD_lda_PTH[[1]]$auc,rocCD_lda_PTH[[2]]$auc,rocCD_lda_PTH[[3]]$auc,rocCD_lda_PTH[[4]]$auc,
                  rocCD_lda_PTH[[5]]$auc,rocCD_lda_PTH[[6]]$auc,rocCD_lda_PTH[[7]]$auc)
CD_lda_PTH_ciL<-c(rocCD_lda_PTH[[1]]$ci[1],rocCD_lda_PTH[[2]]$ci[1],rocCD_lda_PTH[[3]]$ci[1],rocCD_lda_PTH[[4]]$ci[1],
                  rocCD_lda_PTH[[5]]$ci[1],rocCD_lda_PTH[[6]]$ci[1],rocCD_lda_PTH[[7]]$ci[1])
CD_lda_PTH_ciH<-c(rocCD_lda_PTH[[1]]$ci[3],rocCD_lda_PTH[[2]]$ci[3],rocCD_lda_PTH[[3]]$ci[3],rocCD_lda_PTH[[4]]$ci[3],
                  rocCD_lda_PTH[[5]]$ci[3],rocCD_lda_PTH[[6]]$ci[3],rocCD_lda_PTH[[7]]$ci[3])



predICS_lda_PTH<-jackPredLDA(centICS,predictors = c("cohort","PageRank","tarEnt", "Hide"))
rocICS_lda_PTH<-list()
rocICS_lda_PTH[[1]]<-roc(predICS_lda_PTH$pass,as.numeric(predICS_lda_PTH$Week1),auc=T,ci=T)
rocICS_lda_PTH[[2]]<-roc(predICS_lda_PTH$pass,as.numeric(predICS_lda_PTH$Week2),auc=T,ci=T)
rocICS_lda_PTH[[3]]<-roc(predICS_lda_PTH$pass,as.numeric(predICS_lda_PTH$Week3),auc=T,ci=T)
rocICS_lda_PTH[[4]]<-roc(predICS_lda_PTH$pass,as.numeric(predICS_lda_PTH$Week4),auc=T,ci=T)
rocICS_lda_PTH[[5]]<-roc(predICS_lda_PTH$pass,as.numeric(predICS_lda_PTH$Week5),auc=T,ci=T)
rocICS_lda_PTH[[6]]<-roc(predICS_lda_PTH$pass,as.numeric(predICS_lda_PTH$Week6),auc=T,ci=T)
rocICS_lda_PTH[[7]]<-roc(predICS_lda_PTH$pass,as.numeric(predICS_lda_PTH$Week7),auc=T,ci=T)
ICS_lda_PTH_auc<-c(rocICS_lda_PTH[[1]]$auc,rocICS_lda_PTH[[2]]$auc,rocICS_lda_PTH[[3]]$auc,rocICS_lda_PTH[[4]]$auc,
                   rocICS_lda_PTH[[5]]$auc,rocICS_lda_PTH[[6]]$auc,rocICS_lda_PTH[[7]]$auc)
ICS_lda_PTH_ciL<-c(rocICS_lda_PTH[[1]]$ci[1],rocICS_lda_PTH[[2]]$ci[1],rocICS_lda_PTH[[3]]$ci[1],rocICS_lda_PTH[[4]]$ci[1],
                   rocICS_lda_PTH[[5]]$ci[1],rocICS_lda_PTH[[6]]$ci[1],rocICS_lda_PTH[[7]]$ci[1])
ICS_lda_PTH_ciH<-c(rocICS_lda_PTH[[1]]$ci[3],rocICS_lda_PTH[[2]]$ci[3],rocICS_lda_PTH[[3]]$ci[3],rocICS_lda_PTH[[4]]$ci[3],
                   rocICS_lda_PTH[[5]]$ci[3],rocICS_lda_PTH[[6]]$ci[3],rocICS_lda_PTH[[7]]$ci[3])




###JUST PASS
predPS_jp_lda_PTH<-jackPredLDA(centPS,outcome = "justpass",predictors = c("cohort","PageRank","tarEnt", "Hide"))
rocPS_jp_lda_PTH<-list()
rocPS_jp_lda_PTH[[1]]<-roc(predPS_jp_lda_PTH$justpass,as.numeric(predPS_jp_lda_PTH$Week1),auc=T,ci=T)
rocPS_jp_lda_PTH[[2]]<-roc(predPS_jp_lda_PTH$justpass,as.numeric(predPS_jp_lda_PTH$Week2),auc=T,ci=T)
rocPS_jp_lda_PTH[[3]]<-roc(predPS_jp_lda_PTH$justpass,as.numeric(predPS_jp_lda_PTH$Week3),auc=T,ci=T)
rocPS_jp_lda_PTH[[4]]<-roc(predPS_jp_lda_PTH$justpass,as.numeric(predPS_jp_lda_PTH$Week4),auc=T,ci=T)
rocPS_jp_lda_PTH[[5]]<-roc(predPS_jp_lda_PTH$justpass,as.numeric(predPS_jp_lda_PTH$Week5),auc=T,ci=T)
rocPS_jp_lda_PTH[[6]]<-roc(predPS_jp_lda_PTH$justpass,as.numeric(predPS_jp_lda_PTH$Week6),auc=T,ci=T)
rocPS_jp_lda_PTH[[7]]<-roc(predPS_jp_lda_PTH$justpass,as.numeric(predPS_jp_lda_PTH$Week7),auc=T,ci=T)

PS_jp_lda_PTH_auc<-c(rocPS_jp_lda_PTH[[1]]$auc,rocPS_jp_lda_PTH[[2]]$auc,rocPS_jp_lda_PTH[[3]]$auc,rocPS_jp_lda_PTH[[4]]$auc,
                     rocPS_jp_lda_PTH[[5]]$auc,rocPS_jp_lda_PTH[[6]]$auc,rocPS_jp_lda_PTH[[7]]$auc)
PS_jp_lda_PTH_ciL<-c(rocPS_jp_lda_PTH[[1]]$ci[1],rocPS_jp_lda_PTH[[2]]$ci[1],rocPS_jp_lda_PTH[[3]]$ci[1],rocPS_jp_lda_PTH[[4]]$ci[1],
                     rocPS_jp_lda_PTH[[5]]$ci[1],rocPS_jp_lda_PTH[[6]]$ci[1],rocPS_jp_lda_PTH[[7]]$ci[1])
PS_jp_lda_PTH_ciH<-c(rocPS_jp_lda_PTH[[1]]$ci[3],rocPS_jp_lda_PTH[[2]]$ci[3],rocPS_jp_lda_PTH[[3]]$ci[3],rocPS_jp_lda_PTH[[4]]$ci[3],
                     rocPS_jp_lda_PTH[[5]]$ci[3],rocPS_jp_lda_PTH[[6]]$ci[3],rocPS_jp_lda_PTH[[7]]$ci[3])

predCD_jp_lda_PTH<-jackPredLDA(centCD, outcome = "justpass",predictors = c("cohort","PageRank","tarEnt", "Hide"))
rocCD_jp_lda_PTH<-list()
rocCD_jp_lda_PTH[[1]]<-roc(predCD_jp_lda_PTH$justpass,as.numeric(predCD_jp_lda_PTH$Week1),auc=T,ci=T)
rocCD_jp_lda_PTH[[2]]<-roc(predCD_jp_lda_PTH$justpass,as.numeric(predCD_jp_lda_PTH$Week2),auc=T,ci=T)
rocCD_jp_lda_PTH[[3]]<-roc(predCD_jp_lda_PTH$justpass,as.numeric(predCD_jp_lda_PTH$Week3),auc=T,ci=T)
rocCD_jp_lda_PTH[[4]]<-roc(predCD_jp_lda_PTH$justpass,as.numeric(predCD_jp_lda_PTH$Week4),auc=T,ci=T)
rocCD_jp_lda_PTH[[5]]<-roc(predCD_jp_lda_PTH$justpass,as.numeric(predCD_jp_lda_PTH$Week5),auc=T,ci=T)
rocCD_jp_lda_PTH[[6]]<-roc(predCD_jp_lda_PTH$justpass,as.numeric(predCD_jp_lda_PTH$Week6),auc=T,ci=T)
rocCD_jp_lda_PTH[[7]]<-roc(predCD_jp_lda_PTH$justpass,as.numeric(predCD_jp_lda_PTH$Week7),auc=T,ci=T)
CD_jp_lda_PTH_auc<-c(rocCD_jp_lda_PTH[[1]]$auc,rocCD_jp_lda_PTH[[2]]$auc,rocCD_jp_lda_PTH[[3]]$auc,rocCD_jp_lda_PTH[[4]]$auc,
                     rocCD_jp_lda_PTH[[5]]$auc,rocCD_jp_lda_PTH[[6]]$auc,rocCD_jp_lda_PTH[[7]]$auc)
CD_jp_lda_PTH_ciL<-c(rocCD_jp_lda_PTH[[1]]$ci[1],rocCD_jp_lda_PTH[[2]]$ci[1],rocCD_jp_lda_PTH[[3]]$ci[1],rocCD_jp_lda_PTH[[4]]$ci[1],
                     rocCD_jp_lda_PTH[[5]]$ci[1],rocCD_jp_lda_PTH[[6]]$ci[1],rocCD_jp_lda_PTH[[7]]$ci[1])
CD_jp_lda_PTH_ciH<-c(rocCD_jp_lda_PTH[[1]]$ci[3],rocCD_jp_lda_PTH[[2]]$ci[3],rocCD_jp_lda_PTH[[3]]$ci[3],rocCD_jp_lda_PTH[[4]]$ci[3],
                     rocCD_jp_lda_PTH[[5]]$ci[3],rocCD_jp_lda_PTH[[6]]$ci[3],rocCD_jp_lda_PTH[[7]]$ci[3])


predICS_jp_lda_PTH<-jackPredLDA(centICS, outcome = "justpass",predictors = c("cohort","PageRank","tarEnt", "Hide"))
rocICS_jp_lda_PTH<-list()
rocICS_jp_lda_PTH[[1]]<-roc(predICS_jp_lda_PTH$justpass,as.numeric(predICS_jp_lda_PTH$Week1),auc=T,ci=T)
rocICS_jp_lda_PTH[[2]]<-roc(predICS_jp_lda_PTH$justpass,as.numeric(predICS_jp_lda_PTH$Week2),auc=T,ci=T)
rocICS_jp_lda_PTH[[3]]<-roc(predICS_jp_lda_PTH$justpass,as.numeric(predICS_jp_lda_PTH$Week3),auc=T,ci=T)
rocICS_jp_lda_PTH[[4]]<-roc(predICS_jp_lda_PTH$justpass,as.numeric(predICS_jp_lda_PTH$Week4),auc=T,ci=T)
rocICS_jp_lda_PTH[[5]]<-roc(predICS_jp_lda_PTH$justpass,as.numeric(predICS_jp_lda_PTH$Week5),auc=T,ci=T)
rocICS_jp_lda_PTH[[6]]<-roc(predICS_jp_lda_PTH$justpass,as.numeric(predICS_jp_lda_PTH$Week6),auc=T,ci=T)
rocICS_jp_lda_PTH[[7]]<-roc(predICS_jp_lda_PTH$justpass,as.numeric(predICS_jp_lda_PTH$Week7),auc=T,ci=T)
ICS_jp_lda_PTH_auc<-c(rocICS_jp_lda_PTH[[1]]$auc,rocICS_jp_lda_PTH[[2]]$auc,rocICS_jp_lda_PTH[[3]]$auc,rocICS_jp_lda_PTH[[4]]$auc,
                      rocICS_jp_lda_PTH[[5]]$auc,rocICS_jp_lda_PTH[[6]]$auc,rocICS_jp_lda_PTH[[7]]$auc)
ICS_jp_lda_PTH_ciL<-c(rocICS_jp_lda_PTH[[1]]$ci[1],rocICS_jp_lda_PTH[[2]]$ci[1],rocICS_jp_lda_PTH[[3]]$ci[1],rocICS_jp_lda_PTH[[4]]$ci[1],
                      rocICS_jp_lda_PTH[[5]]$ci[1],rocICS_jp_lda_PTH[[6]]$ci[1],rocICS_jp_lda_PTH[[7]]$ci[1])
ICS_jp_lda_PTH_ciH<-c(rocICS_jp_lda_PTH[[1]]$ci[3],rocICS_jp_lda_PTH[[2]]$ci[3],rocICS_jp_lda_PTH[[3]]$ci[3],rocICS_jp_lda_PTH[[4]]$ci[3],
                      rocICS_jp_lda_PTH[[5]]$ci[3],rocICS_jp_lda_PTH[[6]]$ci[3],rocICS_jp_lda_PTH[[7]]$ci[3])


######PR TE models######
predPS_lda_PT<-jackPredLDA(centPS,predictors = c("cohort","PageRank","tarEnt"))
rocPS_lda_PT<-list()
rocPS_lda_PT[[1]]<-roc(predPS_lda_PT$pass,as.numeric(predPS_lda_PT$Week1),auc=T,ci=T)
rocPS_lda_PT[[2]]<-roc(predPS_lda_PT$pass,as.numeric(predPS_lda_PT$Week2),auc=T,ci=T)
rocPS_lda_PT[[3]]<-roc(predPS_lda_PT$pass,as.numeric(predPS_lda_PT$Week3),auc=T,ci=T)
rocPS_lda_PT[[4]]<-roc(predPS_lda_PT$pass,as.numeric(predPS_lda_PT$Week4),auc=T,ci=T)
rocPS_lda_PT[[5]]<-roc(predPS_lda_PT$pass,as.numeric(predPS_lda_PT$Week5),auc=T,ci=T)
rocPS_lda_PT[[6]]<-roc(predPS_lda_PT$pass,as.numeric(predPS_lda_PT$Week6),auc=T,ci=T)
rocPS_lda_PT[[7]]<-roc(predPS_lda_PT$pass,as.numeric(predPS_lda_PT$Week7),auc=T,ci=T)
PS_lda_PT_auc<-c(rocPS_lda_PT[[1]]$auc,rocPS_lda_PT[[2]]$auc,rocPS_lda_PT[[3]]$auc,rocPS_lda_PT[[4]]$auc,
                 rocPS_lda_PT[[5]]$auc,rocPS_lda_PT[[6]]$auc,rocPS_lda_PT[[7]]$auc)
PS_lda_PT_ciL<-c(rocPS_lda_PT[[1]]$ci[1],rocPS_lda_PT[[2]]$ci[1],rocPS_lda_PT[[3]]$ci[1],rocPS_lda_PT[[4]]$ci[1],
                 rocPS_lda_PT[[5]]$ci[1],rocPS_lda_PT[[6]]$ci[1],rocPS_lda_PT[[7]]$ci[1])
PS_lda_PT_ciH<-c(rocPS_lda_PT[[1]]$ci[3],rocPS_lda_PT[[2]]$ci[3],rocPS_lda_PT[[3]]$ci[3],rocPS_lda_PT[[4]]$ci[3],
                 rocPS_lda_PT[[5]]$ci[3],rocPS_lda_PT[[6]]$ci[3],rocPS_lda_PT[[7]]$ci[3])
##CD layer
predCD_lda_PT<-jackPredLDA(centCD,predictors = c("cohort","PageRank","tarEnt"))
rocCD_lda_PT<-list()
rocCD_lda_PT[[1]]<-roc(predCD_lda_PT$pass,as.numeric(predCD_lda_PT$Week1),auc=T,ci=T)
rocCD_lda_PT[[2]]<-roc(predCD_lda_PT$pass,as.numeric(predCD_lda_PT$Week2),auc=T,ci=T)
rocCD_lda_PT[[3]]<-roc(predCD_lda_PT$pass,as.numeric(predCD_lda_PT$Week3),auc=T,ci=T)
rocCD_lda_PT[[4]]<-roc(predCD_lda_PT$pass,as.numeric(predCD_lda_PT$Week4),auc=T,ci=T)
rocCD_lda_PT[[5]]<-roc(predCD_lda_PT$pass,as.numeric(predCD_lda_PT$Week5),auc=T,ci=T)
rocCD_lda_PT[[6]]<-roc(predCD_lda_PT$pass,as.numeric(predCD_lda_PT$Week6),auc=T,ci=T)
rocCD_lda_PT[[7]]<-roc(predCD_lda_PT$pass,as.numeric(predCD_lda_PT$Week7),auc=T,ci=T)
CD_lda_PT_auc<-c(rocCD_lda_PT[[1]]$auc,rocCD_lda_PT[[2]]$auc,rocCD_lda_PT[[3]]$auc,rocCD_lda_PT[[4]]$auc,
                 rocCD_lda_PT[[5]]$auc,rocCD_lda_PT[[6]]$auc,rocCD_lda_PT[[7]]$auc)
CD_lda_PT_ciL<-c(rocCD_lda_PT[[1]]$ci[1],rocCD_lda_PT[[2]]$ci[1],rocCD_lda_PT[[3]]$ci[1],rocCD_lda_PT[[4]]$ci[1],
                 rocCD_lda_PT[[5]]$ci[1],rocCD_lda_PT[[6]]$ci[1],rocCD_lda_PT[[7]]$ci[1])
CD_lda_PT_ciH<-c(rocCD_lda_PT[[1]]$ci[3],rocCD_lda_PT[[2]]$ci[3],rocCD_lda_PT[[3]]$ci[3],rocCD_lda_PT[[4]]$ci[3],
                 rocCD_lda_PT[[5]]$ci[3],rocCD_lda_PT[[6]]$ci[3],rocCD_lda_PT[[7]]$ci[3])

#ICS layer
predICS_lda_PT<-jackPredLDA(centICS,predictors = c("cohort","PageRank","tarEnt"))
rocICS_lda_PT<-list()
rocICS_lda_PT[[1]]<-roc(predICS_lda_PT$pass,as.numeric(predICS_lda_PT$Week1),auc=T,ci=T)
rocICS_lda_PT[[2]]<-roc(predICS_lda_PT$pass,as.numeric(predICS_lda_PT$Week2),auc=T,ci=T)
rocICS_lda_PT[[3]]<-roc(predICS_lda_PT$pass,as.numeric(predICS_lda_PT$Week3),auc=T,ci=T)
rocICS_lda_PT[[4]]<-roc(predICS_lda_PT$pass,as.numeric(predICS_lda_PT$Week4),auc=T,ci=T)
rocICS_lda_PT[[5]]<-roc(predICS_lda_PT$pass,as.numeric(predICS_lda_PT$Week5),auc=T,ci=T)
rocICS_lda_PT[[6]]<-roc(predICS_lda_PT$pass,as.numeric(predICS_lda_PT$Week6),auc=T,ci=T)
rocICS_lda_PT[[7]]<-roc(predICS_lda_PT$pass,as.numeric(predICS_lda_PT$Week7),auc=T,ci=T)
ICS_lda_PT_auc<-c(rocICS_lda_PT[[1]]$auc,rocICS_lda_PT[[2]]$auc,rocICS_lda_PT[[3]]$auc,rocICS_lda_PT[[4]]$auc,
                  rocICS_lda_PT[[5]]$auc,rocICS_lda_PT[[6]]$auc,rocICS_lda_PT[[7]]$auc)
ICS_lda_PT_ciL<-c(rocICS_lda_PT[[1]]$ci[1],rocICS_lda_PT[[2]]$ci[1],rocICS_lda_PT[[3]]$ci[1],rocICS_lda_PT[[4]]$ci[1],
                  rocICS_lda_PT[[5]]$ci[1],rocICS_lda_PT[[6]]$ci[1],rocICS_lda_PT[[7]]$ci[1])
ICS_lda_PT_ciH<-c(rocICS_lda_PT[[1]]$ci[3],rocICS_lda_PT[[2]]$ci[3],rocICS_lda_PT[[3]]$ci[3],rocICS_lda_PT[[4]]$ci[3],
                  rocICS_lda_PT[[5]]$ci[3],rocICS_lda_PT[[6]]$ci[3],rocICS_lda_PT[[7]]$ci[3])



###JUST PASS
predPS_jp_lda_PT<-jackPredLDA(centPS,outcome = "justpass",predictors = c("cohort","PageRank","tarEnt"))
rocPS_jp_lda_PT<-list()
rocPS_jp_lda_PT[[1]]<-roc(predPS_jp_lda_PT$justpass,as.numeric(predPS_jp_lda_PT$Week1),auc=T,ci=T)
rocPS_jp_lda_PT[[2]]<-roc(predPS_jp_lda_PT$justpass,as.numeric(predPS_jp_lda_PT$Week2),auc=T,ci=T)
rocPS_jp_lda_PT[[3]]<-roc(predPS_jp_lda_PT$justpass,as.numeric(predPS_jp_lda_PT$Week3),auc=T,ci=T)
rocPS_jp_lda_PT[[4]]<-roc(predPS_jp_lda_PT$justpass,as.numeric(predPS_jp_lda_PT$Week4),auc=T,ci=T)
rocPS_jp_lda_PT[[5]]<-roc(predPS_jp_lda_PT$justpass,as.numeric(predPS_jp_lda_PT$Week5),auc=T,ci=T)
rocPS_jp_lda_PT[[6]]<-roc(predPS_jp_lda_PT$justpass,as.numeric(predPS_jp_lda_PT$Week6),auc=T,ci=T)
rocPS_jp_lda_PT[[7]]<-roc(predPS_jp_lda_PT$justpass,as.numeric(predPS_jp_lda_PT$Week7),auc=T,ci=T)

PS_jp_lda_PT_auc<-c(rocPS_jp_lda_PT[[1]]$auc,rocPS_jp_lda_PT[[2]]$auc,rocPS_jp_lda_PT[[3]]$auc,rocPS_jp_lda_PT[[4]]$auc,
                    rocPS_jp_lda_PT[[5]]$auc,rocPS_jp_lda_PT[[6]]$auc,rocPS_jp_lda_PT[[7]]$auc)
PS_jp_lda_PT_ciL<-c(rocPS_jp_lda_PT[[1]]$ci[1],rocPS_jp_lda_PT[[2]]$ci[1],rocPS_jp_lda_PT[[3]]$ci[1],rocPS_jp_lda_PT[[4]]$ci[1],
                    rocPS_jp_lda_PT[[5]]$ci[1],rocPS_jp_lda_PT[[6]]$ci[1],rocPS_jp_lda_PT[[7]]$ci[1])
PS_jp_lda_PT_ciH<-c(rocPS_jp_lda_PT[[1]]$ci[3],rocPS_jp_lda_PT[[2]]$ci[3],rocPS_jp_lda_PT[[3]]$ci[3],rocPS_jp_lda_PT[[4]]$ci[3],
                    rocPS_jp_lda_PT[[5]]$ci[3],rocPS_jp_lda_PT[[6]]$ci[3],rocPS_jp_lda_PT[[7]]$ci[3])

predCD_jp_lda_PT<-jackPredLDA(centCD, outcome = "justpass",predictors = c("cohort","PageRank","tarEnt"))
rocCD_jp_lda_PT<-list()
rocCD_jp_lda_PT[[1]]<-roc(predCD_jp_lda_PT$justpass,as.numeric(predCD_jp_lda_PT$Week1),auc=T,ci=T)
rocCD_jp_lda_PT[[2]]<-roc(predCD_jp_lda_PT$justpass,as.numeric(predCD_jp_lda_PT$Week2),auc=T,ci=T)
rocCD_jp_lda_PT[[3]]<-roc(predCD_jp_lda_PT$justpass,as.numeric(predCD_jp_lda_PT$Week3),auc=T,ci=T)
rocCD_jp_lda_PT[[4]]<-roc(predCD_jp_lda_PT$justpass,as.numeric(predCD_jp_lda_PT$Week4),auc=T,ci=T)
rocCD_jp_lda_PT[[5]]<-roc(predCD_jp_lda_PT$justpass,as.numeric(predCD_jp_lda_PT$Week5),auc=T,ci=T)
rocCD_jp_lda_PT[[6]]<-roc(predCD_jp_lda_PT$justpass,as.numeric(predCD_jp_lda_PT$Week6),auc=T,ci=T)
rocCD_jp_lda_PT[[7]]<-roc(predCD_jp_lda_PT$justpass,as.numeric(predCD_jp_lda_PT$Week7),auc=T,ci=T)
CD_jp_lda_PT_auc<-c(rocCD_jp_lda_PT[[1]]$auc,rocCD_jp_lda_PT[[2]]$auc,rocCD_jp_lda_PT[[3]]$auc,rocCD_jp_lda_PT[[4]]$auc,
                    rocCD_jp_lda_PT[[5]]$auc,rocCD_jp_lda_PT[[6]]$auc,rocCD_jp_lda_PT[[7]]$auc)
CD_jp_lda_PT_ciL<-c(rocCD_jp_lda_PT[[1]]$ci[1],rocCD_jp_lda_PT[[2]]$ci[1],rocCD_jp_lda_PT[[3]]$ci[1],rocCD_jp_lda_PT[[4]]$ci[1],
                    rocCD_jp_lda_PT[[5]]$ci[1],rocCD_jp_lda_PT[[6]]$ci[1],rocCD_jp_lda_PT[[7]]$ci[1])
CD_jp_lda_PT_ciH<-c(rocCD_jp_lda_PT[[1]]$ci[3],rocCD_jp_lda_PT[[2]]$ci[3],rocCD_jp_lda_PT[[3]]$ci[3],rocCD_jp_lda_PT[[4]]$ci[3],
                    rocCD_jp_lda_PT[[5]]$ci[3],rocCD_jp_lda_PT[[6]]$ci[3],rocCD_jp_lda_PT[[7]]$ci[3])


predICS_jp_lda_PT<-jackPredLDA(centICS, outcome = "justpass",predictors = c("cohort","PageRank","tarEnt"))
rocICS_jp_lda_PT<-list()
rocICS_jp_lda_PT[[1]]<-roc(predICS_jp_lda_PT$justpass,as.numeric(predICS_jp_lda_PT$Week1),auc=T,ci=T)
rocICS_jp_lda_PT[[2]]<-roc(predICS_jp_lda_PT$justpass,as.numeric(predICS_jp_lda_PT$Week2),auc=T,ci=T)
rocICS_jp_lda_PT[[3]]<-roc(predICS_jp_lda_PT$justpass,as.numeric(predICS_jp_lda_PT$Week3),auc=T,ci=T)
rocICS_jp_lda_PT[[4]]<-roc(predICS_jp_lda_PT$justpass,as.numeric(predICS_jp_lda_PT$Week4),auc=T,ci=T)
rocICS_jp_lda_PT[[5]]<-roc(predICS_jp_lda_PT$justpass,as.numeric(predICS_jp_lda_PT$Week5),auc=T,ci=T)
rocICS_jp_lda_PT[[6]]<-roc(predICS_jp_lda_PT$justpass,as.numeric(predICS_jp_lda_PT$Week6),auc=T,ci=T)
rocICS_jp_lda_PT[[7]]<-roc(predICS_jp_lda_PT$justpass,as.numeric(predICS_jp_lda_PT$Week7),auc=T,ci=T)
ICS_jp_lda_PT_auc<-c(rocICS_jp_lda_PT[[1]]$auc,rocICS_jp_lda_PT[[2]]$auc,rocICS_jp_lda_PT[[3]]$auc,rocICS_jp_lda_PT[[4]]$auc,
                     rocICS_jp_lda_PT[[5]]$auc,rocICS_jp_lda_PT[[6]]$auc,rocICS_jp_lda_PT[[7]]$auc)
ICS_jp_lda_PT_ciL<-c(rocICS_jp_lda_PT[[1]]$ci[1],rocICS_jp_lda_PT[[2]]$ci[1],rocICS_jp_lda_PT[[3]]$ci[1],rocICS_jp_lda_PT[[4]]$ci[1],
                     rocICS_jp_lda_PT[[5]]$ci[1],rocICS_jp_lda_PT[[6]]$ci[1],rocICS_jp_lda_PT[[7]]$ci[1])
ICS_jp_lda_PT_ciH<-c(rocICS_jp_lda_PT[[1]]$ci[3],rocICS_jp_lda_PT[[2]]$ci[3],rocICS_jp_lda_PT[[3]]$ci[3],rocICS_jp_lda_PT[[4]]$ci[3],
                     rocICS_jp_lda_PT[[5]]$ci[3],rocICS_jp_lda_PT[[6]]$ci[3],rocICS_jp_lda_PT[[7]]$ci[3])




######PR H Models######
predPS_lda_PH<-jackPredLDA(centPS,predictors = c("cohort","PageRank", "Hide"))
rocPS_lda_PH<-list()
rocPS_lda_PH[[1]]<-roc(predPS_lda_PH$pass,as.numeric(predPS_lda_PH$Week1),auc=T,ci=T)
rocPS_lda_PH[[2]]<-roc(predPS_lda_PH$pass,as.numeric(predPS_lda_PH$Week2),auc=T,ci=T)
rocPS_lda_PH[[3]]<-roc(predPS_lda_PH$pass,as.numeric(predPS_lda_PH$Week3),auc=T,ci=T)
rocPS_lda_PH[[4]]<-roc(predPS_lda_PH$pass,as.numeric(predPS_lda_PH$Week4),auc=T,ci=T)
rocPS_lda_PH[[5]]<-roc(predPS_lda_PH$pass,as.numeric(predPS_lda_PH$Week5),auc=T,ci=T)
rocPS_lda_PH[[6]]<-roc(predPS_lda_PH$pass,as.numeric(predPS_lda_PH$Week6),auc=T,ci=T)
rocPS_lda_PH[[7]]<-roc(predPS_lda_PH$pass,as.numeric(predPS_lda_PH$Week7),auc=T,ci=T)
PS_lda_PH_auc<-c(rocPS_lda_PH[[1]]$auc,rocPS_lda_PH[[2]]$auc,rocPS_lda_PH[[3]]$auc,rocPS_lda_PH[[4]]$auc,
                 rocPS_lda_PH[[5]]$auc,rocPS_lda_PH[[6]]$auc,rocPS_lda_PH[[7]]$auc)
PS_lda_PH_ciL<-c(rocPS_lda_PH[[1]]$ci[1],rocPS_lda_PH[[2]]$ci[1],rocPS_lda_PH[[3]]$ci[1],rocPS_lda_PH[[4]]$ci[1],
                 rocPS_lda_PH[[5]]$ci[1],rocPS_lda_PH[[6]]$ci[1],rocPS_lda_PH[[7]]$ci[1])
PS_lda_PH_ciH<-c(rocPS_lda_PH[[1]]$ci[3],rocPS_lda_PH[[2]]$ci[3],rocPS_lda_PH[[3]]$ci[3],rocPS_lda_PH[[4]]$ci[3],
                 rocPS_lda_PH[[5]]$ci[3],rocPS_lda_PH[[6]]$ci[3],rocPS_lda_PH[[7]]$ci[3])
##CD layer
predCD_lda_PH<-jackPredLDA(centCD,predictors = c("cohort","PageRank", "Hide"))
rocCD_lda_PH<-list()
rocCD_lda_PH[[1]]<-roc(predCD_lda_PH$pass,as.numeric(predCD_lda_PH$Week1),auc=T,ci=T)
rocCD_lda_PH[[2]]<-roc(predCD_lda_PH$pass,as.numeric(predCD_lda_PH$Week2),auc=T,ci=T)
rocCD_lda_PH[[3]]<-roc(predCD_lda_PH$pass,as.numeric(predCD_lda_PH$Week3),auc=T,ci=T)
rocCD_lda_PH[[4]]<-roc(predCD_lda_PH$pass,as.numeric(predCD_lda_PH$Week4),auc=T,ci=T)
rocCD_lda_PH[[5]]<-roc(predCD_lda_PH$pass,as.numeric(predCD_lda_PH$Week5),auc=T,ci=T)
rocCD_lda_PH[[6]]<-roc(predCD_lda_PH$pass,as.numeric(predCD_lda_PH$Week6),auc=T,ci=T)
rocCD_lda_PH[[7]]<-roc(predCD_lda_PH$pass,as.numeric(predCD_lda_PH$Week7),auc=T,ci=T)
CD_lda_PH_auc<-c(rocCD_lda_PH[[1]]$auc,rocCD_lda_PH[[2]]$auc,rocCD_lda_PH[[3]]$auc,rocCD_lda_PH[[4]]$auc,
                 rocCD_lda_PH[[5]]$auc,rocCD_lda_PH[[6]]$auc,rocCD_lda_PH[[7]]$auc)
CD_lda_PH_ciL<-c(rocCD_lda_PH[[1]]$ci[1],rocCD_lda_PH[[2]]$ci[1],rocCD_lda_PH[[3]]$ci[1],rocCD_lda_PH[[4]]$ci[1],
                 rocCD_lda_PH[[5]]$ci[1],rocCD_lda_PH[[6]]$ci[1],rocCD_lda_PH[[7]]$ci[1])
CD_lda_PH_ciH<-c(rocCD_lda_PH[[1]]$ci[3],rocCD_lda_PH[[2]]$ci[3],rocCD_lda_PH[[3]]$ci[3],rocCD_lda_PH[[4]]$ci[3],
                 rocCD_lda_PH[[5]]$ci[3],rocCD_lda_PH[[6]]$ci[3],rocCD_lda_PH[[7]]$ci[3])

#ICS layer
predICS_lda_PH<-jackPredLDA(centICS,predictors = c("cohort","PageRank", "Hide"))
rocICS_lda_PH<-list()
rocICS_lda_PH[[1]]<-roc(predICS_lda_PH$pass,as.numeric(predICS_lda_PH$Week1),auc=T,ci=T)
rocICS_lda_PH[[2]]<-roc(predICS_lda_PH$pass,as.numeric(predICS_lda_PH$Week2),auc=T,ci=T)
rocICS_lda_PH[[3]]<-roc(predICS_lda_PH$pass,as.numeric(predICS_lda_PH$Week3),auc=T,ci=T)
rocICS_lda_PH[[4]]<-roc(predICS_lda_PH$pass,as.numeric(predICS_lda_PH$Week4),auc=T,ci=T)
rocICS_lda_PH[[5]]<-roc(predICS_lda_PH$pass,as.numeric(predICS_lda_PH$Week5),auc=T,ci=T)
rocICS_lda_PH[[6]]<-roc(predICS_lda_PH$pass,as.numeric(predICS_lda_PH$Week6),auc=T,ci=T)
rocICS_lda_PH[[7]]<-roc(predICS_lda_PH$pass,as.numeric(predICS_lda_PH$Week7),auc=T,ci=T)
ICS_lda_PH_auc<-c(rocICS_lda_PH[[1]]$auc,rocICS_lda_PH[[2]]$auc,rocICS_lda_PH[[3]]$auc,rocICS_lda_PH[[4]]$auc,
                  rocICS_lda_PH[[5]]$auc,rocICS_lda_PH[[6]]$auc,rocICS_lda_PH[[7]]$auc)
ICS_lda_PH_ciL<-c(rocICS_lda_PH[[1]]$ci[1],rocICS_lda_PH[[2]]$ci[1],rocICS_lda_PH[[3]]$ci[1],rocICS_lda_PH[[4]]$ci[1],
                  rocICS_lda_PH[[5]]$ci[1],rocICS_lda_PH[[6]]$ci[1],rocICS_lda_PH[[7]]$ci[1])
ICS_lda_PH_ciH<-c(rocICS_lda_PH[[1]]$ci[3],rocICS_lda_PH[[2]]$ci[3],rocICS_lda_PH[[3]]$ci[3],rocICS_lda_PH[[4]]$ci[3],
                  rocICS_lda_PH[[5]]$ci[3],rocICS_lda_PH[[6]]$ci[3],rocICS_lda_PH[[7]]$ci[3])



###JUST PASS
##PS layer
predPS_jp_lda_PH<-jackPredLDA(centPS,outcome = "justpass",predictors = c("cohort","PageRank", "Hide"))
rocPS_jp_lda_PH<-list()
rocPS_jp_lda_PH[[1]]<-roc(predPS_jp_lda_PH$justpass,as.numeric(predPS_jp_lda_PH$Week1),auc=T,ci=T)
rocPS_jp_lda_PH[[2]]<-roc(predPS_jp_lda_PH$justpass,as.numeric(predPS_jp_lda_PH$Week2),auc=T,ci=T)
rocPS_jp_lda_PH[[3]]<-roc(predPS_jp_lda_PH$justpass,as.numeric(predPS_jp_lda_PH$Week3),auc=T,ci=T)
rocPS_jp_lda_PH[[4]]<-roc(predPS_jp_lda_PH$justpass,as.numeric(predPS_jp_lda_PH$Week4),auc=T,ci=T)
rocPS_jp_lda_PH[[5]]<-roc(predPS_jp_lda_PH$justpass,as.numeric(predPS_jp_lda_PH$Week5),auc=T,ci=T)
rocPS_jp_lda_PH[[6]]<-roc(predPS_jp_lda_PH$justpass,as.numeric(predPS_jp_lda_PH$Week6),auc=T,ci=T)
rocPS_jp_lda_PH[[7]]<-roc(predPS_jp_lda_PH$justpass,as.numeric(predPS_jp_lda_PH$Week7),auc=T,ci=T)

PS_jp_lda_PH_auc<-c(rocPS_jp_lda_PH[[1]]$auc,rocPS_jp_lda_PH[[2]]$auc,rocPS_jp_lda_PH[[3]]$auc,rocPS_jp_lda_PH[[4]]$auc,
                    rocPS_jp_lda_PH[[5]]$auc,rocPS_jp_lda_PH[[6]]$auc,rocPS_jp_lda_PH[[7]]$auc)
PS_jp_lda_PH_ciL<-c(rocPS_jp_lda_PH[[1]]$ci[1],rocPS_jp_lda_PH[[2]]$ci[1],rocPS_jp_lda_PH[[3]]$ci[1],rocPS_jp_lda_PH[[4]]$ci[1],
                    rocPS_jp_lda_PH[[5]]$ci[1],rocPS_jp_lda_PH[[6]]$ci[1],rocPS_jp_lda_PH[[7]]$ci[1])
PS_jp_lda_PH_ciH<-c(rocPS_jp_lda_PH[[1]]$ci[3],rocPS_jp_lda_PH[[2]]$ci[3],rocPS_jp_lda_PH[[3]]$ci[3],rocPS_jp_lda_PH[[4]]$ci[3],
                    rocPS_jp_lda_PH[[5]]$ci[3],rocPS_jp_lda_PH[[6]]$ci[3],rocPS_jp_lda_PH[[7]]$ci[3])
#CD Layer
predCD_jp_lda_PH<-jackPredLDA(centCD, outcome = "justpass",predictors = c("cohort","PageRank", "Hide"))
rocCD_jp_lda_PH<-list()
rocCD_jp_lda_PH[[1]]<-roc(predCD_jp_lda_PH$justpass,as.numeric(predCD_jp_lda_PH$Week1),auc=T,ci=T)
rocCD_jp_lda_PH[[2]]<-roc(predCD_jp_lda_PH$justpass,as.numeric(predCD_jp_lda_PH$Week2),auc=T,ci=T)
rocCD_jp_lda_PH[[3]]<-roc(predCD_jp_lda_PH$justpass,as.numeric(predCD_jp_lda_PH$Week3),auc=T,ci=T)
rocCD_jp_lda_PH[[4]]<-roc(predCD_jp_lda_PH$justpass,as.numeric(predCD_jp_lda_PH$Week4),auc=T,ci=T)
rocCD_jp_lda_PH[[5]]<-roc(predCD_jp_lda_PH$justpass,as.numeric(predCD_jp_lda_PH$Week5),auc=T,ci=T)
rocCD_jp_lda_PH[[6]]<-roc(predCD_jp_lda_PH$justpass,as.numeric(predCD_jp_lda_PH$Week6),auc=T,ci=T)
rocCD_jp_lda_PH[[7]]<-roc(predCD_jp_lda_PH$justpass,as.numeric(predCD_jp_lda_PH$Week7),auc=T,ci=T)
CD_jp_lda_PH_auc<-c(rocCD_jp_lda_PH[[1]]$auc,rocCD_jp_lda_PH[[2]]$auc,rocCD_jp_lda_PH[[3]]$auc,rocCD_jp_lda_PH[[4]]$auc,
                    rocCD_jp_lda_PH[[5]]$auc,rocCD_jp_lda_PH[[6]]$auc,rocCD_jp_lda_PH[[7]]$auc)
CD_jp_lda_PH_ciL<-c(rocCD_jp_lda_PH[[1]]$ci[1],rocCD_jp_lda_PH[[2]]$ci[1],rocCD_jp_lda_PH[[3]]$ci[1],rocCD_jp_lda_PH[[4]]$ci[1],
                    rocCD_jp_lda_PH[[5]]$ci[1],rocCD_jp_lda_PH[[6]]$ci[1],rocCD_jp_lda_PH[[7]]$ci[1])
CD_jp_lda_PH_ciH<-c(rocCD_jp_lda_PH[[1]]$ci[3],rocCD_jp_lda_PH[[2]]$ci[3],rocCD_jp_lda_PH[[3]]$ci[3],rocCD_jp_lda_PH[[4]]$ci[3],
                    rocCD_jp_lda_PH[[5]]$ci[3],rocCD_jp_lda_PH[[6]]$ci[3],rocCD_jp_lda_PH[[7]]$ci[3])

#ICS layer
predICS_jp_lda_PH<-jackPredLDA(centICS, outcome = "justpass",predictors = c("cohort","PageRank", "Hide"))
rocICS_jp_lda_PH<-list()
rocICS_jp_lda_PH[[1]]<-roc(predICS_jp_lda_PH$justpass,as.numeric(predICS_jp_lda_PH$Week1),auc=T,ci=T)
rocICS_jp_lda_PH[[2]]<-roc(predICS_jp_lda_PH$justpass,as.numeric(predICS_jp_lda_PH$Week2),auc=T,ci=T)
rocICS_jp_lda_PH[[3]]<-roc(predICS_jp_lda_PH$justpass,as.numeric(predICS_jp_lda_PH$Week3),auc=T,ci=T)
rocICS_jp_lda_PH[[4]]<-roc(predICS_jp_lda_PH$justpass,as.numeric(predICS_jp_lda_PH$Week4),auc=T,ci=T)
rocICS_jp_lda_PH[[5]]<-roc(predICS_jp_lda_PH$justpass,as.numeric(predICS_jp_lda_PH$Week5),auc=T,ci=T)
rocICS_jp_lda_PH[[6]]<-roc(predICS_jp_lda_PH$justpass,as.numeric(predICS_jp_lda_PH$Week6),auc=T,ci=T)
rocICS_jp_lda_PH[[7]]<-roc(predICS_jp_lda_PH$justpass,as.numeric(predICS_jp_lda_PH$Week7),auc=T,ci=T)
ICS_jp_lda_PH_auc<-c(rocICS_jp_lda_PH[[1]]$auc,rocICS_jp_lda_PH[[2]]$auc,rocICS_jp_lda_PH[[3]]$auc,rocICS_jp_lda_PH[[4]]$auc,
                     rocICS_jp_lda_PH[[5]]$auc,rocICS_jp_lda_PH[[6]]$auc,rocICS_jp_lda_PH[[7]]$auc)
ICS_jp_lda_PH_ciL<-c(rocICS_jp_lda_PH[[1]]$ci[1],rocICS_jp_lda_PH[[2]]$ci[1],rocICS_jp_lda_PH[[3]]$ci[1],rocICS_jp_lda_PH[[4]]$ci[1],
                     rocICS_jp_lda_PH[[5]]$ci[1],rocICS_jp_lda_PH[[6]]$ci[1],rocICS_jp_lda_PH[[7]]$ci[1])
ICS_jp_lda_PH_ciH<-c(rocICS_jp_lda_PH[[1]]$ci[3],rocICS_jp_lda_PH[[2]]$ci[3],rocICS_jp_lda_PH[[3]]$ci[3],rocICS_jp_lda_PH[[4]]$ci[3],
                     rocICS_jp_lda_PH[[5]]$ci[3],rocICS_jp_lda_PH[[6]]$ci[3],rocICS_jp_lda_PH[[7]]$ci[3])


######TE-H models######
#PS layer
predPS_lda_TH<-jackPredLDA(centPS,predictors = c("cohort","tarEnt", "Hide"))
rocPS_lda_TH<-list()
rocPS_lda_TH[[1]]<-roc(predPS_lda_TH$pass,as.numeric(predPS_lda_TH$Week1),auc=T,ci=T)
rocPS_lda_TH[[2]]<-roc(predPS_lda_TH$pass,as.numeric(predPS_lda_TH$Week2),auc=T,ci=T)
rocPS_lda_TH[[3]]<-roc(predPS_lda_TH$pass,as.numeric(predPS_lda_TH$Week3),auc=T,ci=T)
rocPS_lda_TH[[4]]<-roc(predPS_lda_TH$pass,as.numeric(predPS_lda_TH$Week4),auc=T,ci=T)
rocPS_lda_TH[[5]]<-roc(predPS_lda_TH$pass,as.numeric(predPS_lda_TH$Week5),auc=T,ci=T)
rocPS_lda_TH[[6]]<-roc(predPS_lda_TH$pass,as.numeric(predPS_lda_TH$Week6),auc=T,ci=T)
rocPS_lda_TH[[7]]<-roc(predPS_lda_TH$pass,as.numeric(predPS_lda_TH$Week7),auc=T,ci=T)
PS_lda_TH_auc<-c(rocPS_lda_TH[[1]]$auc,rocPS_lda_TH[[2]]$auc,rocPS_lda_TH[[3]]$auc,rocPS_lda_TH[[4]]$auc,
                 rocPS_lda_TH[[5]]$auc,rocPS_lda_TH[[6]]$auc,rocPS_lda_TH[[7]]$auc)
PS_lda_TH_ciL<-c(rocPS_lda_TH[[1]]$ci[1],rocPS_lda_TH[[2]]$ci[1],rocPS_lda_TH[[3]]$ci[1],rocPS_lda_TH[[4]]$ci[1],
                 rocPS_lda_TH[[5]]$ci[1],rocPS_lda_TH[[6]]$ci[1],rocPS_lda_TH[[7]]$ci[1])
PS_lda_TH_ciH<-c(rocPS_lda_TH[[1]]$ci[3],rocPS_lda_TH[[2]]$ci[3],rocPS_lda_TH[[3]]$ci[3],rocPS_lda_TH[[4]]$ci[3],
                 rocPS_lda_TH[[5]]$ci[3],rocPS_lda_TH[[6]]$ci[3],rocPS_lda_TH[[7]]$ci[3])
##CD layer
predCD_lda_TH<-jackPredLDA(centCD,predictors = c("cohort","tarEnt", "Hide"))
rocCD_lda_TH<-list()
rocCD_lda_TH[[1]]<-roc(predCD_lda_TH$pass,as.numeric(predCD_lda_TH$Week1),auc=T,ci=T)
rocCD_lda_TH[[2]]<-roc(predCD_lda_TH$pass,as.numeric(predCD_lda_TH$Week2),auc=T,ci=T)
rocCD_lda_TH[[3]]<-roc(predCD_lda_TH$pass,as.numeric(predCD_lda_TH$Week3),auc=T,ci=T)
rocCD_lda_TH[[4]]<-roc(predCD_lda_TH$pass,as.numeric(predCD_lda_TH$Week4),auc=T,ci=T)
rocCD_lda_TH[[5]]<-roc(predCD_lda_TH$pass,as.numeric(predCD_lda_TH$Week5),auc=T,ci=T)
rocCD_lda_TH[[6]]<-roc(predCD_lda_TH$pass,as.numeric(predCD_lda_TH$Week6),auc=T,ci=T)
rocCD_lda_TH[[7]]<-roc(predCD_lda_TH$pass,as.numeric(predCD_lda_TH$Week7),auc=T,ci=T)
CD_lda_TH_auc<-c(rocCD_lda_TH[[1]]$auc,rocCD_lda_TH[[2]]$auc,rocCD_lda_TH[[3]]$auc,rocCD_lda_TH[[4]]$auc,
                 rocCD_lda_TH[[5]]$auc,rocCD_lda_TH[[6]]$auc,rocCD_lda_TH[[7]]$auc)
CD_lda_TH_ciL<-c(rocCD_lda_TH[[1]]$ci[1],rocCD_lda_TH[[2]]$ci[1],rocCD_lda_TH[[3]]$ci[1],rocCD_lda_TH[[4]]$ci[1],
                 rocCD_lda_TH[[5]]$ci[1],rocCD_lda_TH[[6]]$ci[1],rocCD_lda_TH[[7]]$ci[1])
CD_lda_TH_ciH<-c(rocCD_lda_TH[[1]]$ci[3],rocCD_lda_TH[[2]]$ci[3],rocCD_lda_TH[[3]]$ci[3],rocCD_lda_TH[[4]]$ci[3],
                 rocCD_lda_TH[[5]]$ci[3],rocCD_lda_TH[[6]]$ci[3],rocCD_lda_TH[[7]]$ci[3])

#ICS layer
predICS_lda_TH<-jackPredLDA(centICS,predictors = c("cohort","tarEnt", "Hide"))
rocICS_lda_TH<-list()
rocICS_lda_TH[[1]]<-roc(predICS_lda_TH$pass,as.numeric(predICS_lda_TH$Week1),auc=T,ci=T)
rocICS_lda_TH[[2]]<-roc(predICS_lda_TH$pass,as.numeric(predICS_lda_TH$Week2),auc=T,ci=T)
rocICS_lda_TH[[3]]<-roc(predICS_lda_TH$pass,as.numeric(predICS_lda_TH$Week3),auc=T,ci=T)
rocICS_lda_TH[[4]]<-roc(predICS_lda_TH$pass,as.numeric(predICS_lda_TH$Week4),auc=T,ci=T)
rocICS_lda_TH[[5]]<-roc(predICS_lda_TH$pass,as.numeric(predICS_lda_TH$Week5),auc=T,ci=T)
rocICS_lda_TH[[6]]<-roc(predICS_lda_TH$pass,as.numeric(predICS_lda_TH$Week6),auc=T,ci=T)
rocICS_lda_TH[[7]]<-roc(predICS_lda_TH$pass,as.numeric(predICS_lda_TH$Week7),auc=T,ci=T)
ICS_lda_TH_auc<-c(rocICS_lda_TH[[1]]$auc,rocICS_lda_TH[[2]]$auc,rocICS_lda_TH[[3]]$auc,rocICS_lda_TH[[4]]$auc,
                  rocICS_lda_TH[[5]]$auc,rocICS_lda_TH[[6]]$auc,rocICS_lda_TH[[7]]$auc)
ICS_lda_TH_ciL<-c(rocICS_lda_TH[[1]]$ci[1],rocICS_lda_TH[[2]]$ci[1],rocICS_lda_TH[[3]]$ci[1],rocICS_lda_TH[[4]]$ci[1],
                  rocICS_lda_TH[[5]]$ci[1],rocICS_lda_TH[[6]]$ci[1],rocICS_lda_TH[[7]]$ci[1])
ICS_lda_TH_ciH<-c(rocICS_lda_TH[[1]]$ci[3],rocICS_lda_TH[[2]]$ci[3],rocICS_lda_TH[[3]]$ci[3],rocICS_lda_TH[[4]]$ci[3],
                  rocICS_lda_TH[[5]]$ci[3],rocICS_lda_TH[[6]]$ci[3],rocICS_lda_TH[[7]]$ci[3])


###JUST PASS
#PS Layer
predPS_jp_lda_TH<-jackPredLDA(centPS,outcome = "justpass",predictors = c("cohort","tarEnt", "Hide"))
rocPS_jp_lda_TH<-list()
rocPS_jp_lda_TH[[1]]<-roc(predPS_jp_lda_TH$justpass,as.numeric(predPS_jp_lda_TH$Week1),auc=T,ci=T)
rocPS_jp_lda_TH[[2]]<-roc(predPS_jp_lda_TH$justpass,as.numeric(predPS_jp_lda_TH$Week2),auc=T,ci=T)
rocPS_jp_lda_TH[[3]]<-roc(predPS_jp_lda_TH$justpass,as.numeric(predPS_jp_lda_TH$Week3),auc=T,ci=T)
rocPS_jp_lda_TH[[4]]<-roc(predPS_jp_lda_TH$justpass,as.numeric(predPS_jp_lda_TH$Week4),auc=T,ci=T)
rocPS_jp_lda_TH[[5]]<-roc(predPS_jp_lda_TH$justpass,as.numeric(predPS_jp_lda_TH$Week5),auc=T,ci=T)
rocPS_jp_lda_TH[[6]]<-roc(predPS_jp_lda_TH$justpass,as.numeric(predPS_jp_lda_TH$Week6),auc=T,ci=T)
rocPS_jp_lda_TH[[7]]<-roc(predPS_jp_lda_TH$justpass,as.numeric(predPS_jp_lda_TH$Week7),auc=T,ci=T)

PS_jp_lda_TH_auc<-c(rocPS_jp_lda_TH[[1]]$auc,rocPS_jp_lda_TH[[2]]$auc,rocPS_jp_lda_TH[[3]]$auc,rocPS_jp_lda_TH[[4]]$auc,
                    rocPS_jp_lda_TH[[5]]$auc,rocPS_jp_lda_TH[[6]]$auc,rocPS_jp_lda_TH[[7]]$auc)
PS_jp_lda_TH_ciL<-c(rocPS_jp_lda_TH[[1]]$ci[1],rocPS_jp_lda_TH[[2]]$ci[1],rocPS_jp_lda_TH[[3]]$ci[1],rocPS_jp_lda_TH[[4]]$ci[1],
                    rocPS_jp_lda_TH[[5]]$ci[1],rocPS_jp_lda_TH[[6]]$ci[1],rocPS_jp_lda_TH[[7]]$ci[1])
PS_jp_lda_TH_ciH<-c(rocPS_jp_lda_TH[[1]]$ci[3],rocPS_jp_lda_TH[[2]]$ci[3],rocPS_jp_lda_TH[[3]]$ci[3],rocPS_jp_lda_TH[[4]]$ci[3],
                    rocPS_jp_lda_TH[[5]]$ci[3],rocPS_jp_lda_TH[[6]]$ci[3],rocPS_jp_lda_TH[[7]]$ci[3])
#CD layer
predCD_jp_lda_TH<-jackPredLDA(centCD, outcome = "justpass",predictors = c("cohort","tarEnt", "Hide"))
rocCD_jp_lda_TH<-list()
rocCD_jp_lda_TH[[1]]<-roc(predCD_jp_lda_TH$justpass,as.numeric(predCD_jp_lda_TH$Week1),auc=T,ci=T)
rocCD_jp_lda_TH[[2]]<-roc(predCD_jp_lda_TH$justpass,as.numeric(predCD_jp_lda_TH$Week2),auc=T,ci=T)
rocCD_jp_lda_TH[[3]]<-roc(predCD_jp_lda_TH$justpass,as.numeric(predCD_jp_lda_TH$Week3),auc=T,ci=T)
rocCD_jp_lda_TH[[4]]<-roc(predCD_jp_lda_TH$justpass,as.numeric(predCD_jp_lda_TH$Week4),auc=T,ci=T)
rocCD_jp_lda_TH[[5]]<-roc(predCD_jp_lda_TH$justpass,as.numeric(predCD_jp_lda_TH$Week5),auc=T,ci=T)
rocCD_jp_lda_TH[[6]]<-roc(predCD_jp_lda_TH$justpass,as.numeric(predCD_jp_lda_TH$Week6),auc=T,ci=T)
rocCD_jp_lda_TH[[7]]<-roc(predCD_jp_lda_TH$justpass,as.numeric(predCD_jp_lda_TH$Week7),auc=T,ci=T)
CD_jp_lda_TH_auc<-c(rocCD_jp_lda_TH[[1]]$auc,rocCD_jp_lda_TH[[2]]$auc,rocCD_jp_lda_TH[[3]]$auc,rocCD_jp_lda_TH[[4]]$auc,
                    rocCD_jp_lda_TH[[5]]$auc,rocCD_jp_lda_TH[[6]]$auc,rocCD_jp_lda_TH[[7]]$auc)
CD_jp_lda_TH_ciL<-c(rocCD_jp_lda_TH[[1]]$ci[1],rocCD_jp_lda_TH[[2]]$ci[1],rocCD_jp_lda_TH[[3]]$ci[1],rocCD_jp_lda_TH[[4]]$ci[1],
                    rocCD_jp_lda_TH[[5]]$ci[1],rocCD_jp_lda_TH[[6]]$ci[1],rocCD_jp_lda_TH[[7]]$ci[1])
CD_jp_lda_TH_ciH<-c(rocCD_jp_lda_TH[[1]]$ci[3],rocCD_jp_lda_TH[[2]]$ci[3],rocCD_jp_lda_TH[[3]]$ci[3],rocCD_jp_lda_TH[[4]]$ci[3],
                    rocCD_jp_lda_TH[[5]]$ci[3],rocCD_jp_lda_TH[[6]]$ci[3],rocCD_jp_lda_TH[[7]]$ci[3])

#ICS Layer
predICS_jp_lda_TH<-jackPredLDA(centICS, outcome = "justpass",predictors = c("cohort","tarEnt", "Hide"))
rocICS_jp_lda_TH<-list()
rocICS_jp_lda_TH[[1]]<-roc(predICS_jp_lda_TH$justpass,as.numeric(predICS_jp_lda_TH$Week1),auc=T,ci=T)
rocICS_jp_lda_TH[[2]]<-roc(predICS_jp_lda_TH$justpass,as.numeric(predICS_jp_lda_TH$Week2),auc=T,ci=T)
rocICS_jp_lda_TH[[3]]<-roc(predICS_jp_lda_TH$justpass,as.numeric(predICS_jp_lda_TH$Week3),auc=T,ci=T)
rocICS_jp_lda_TH[[4]]<-roc(predICS_jp_lda_TH$justpass,as.numeric(predICS_jp_lda_TH$Week4),auc=T,ci=T)
rocICS_jp_lda_TH[[5]]<-roc(predICS_jp_lda_TH$justpass,as.numeric(predICS_jp_lda_TH$Week5),auc=T,ci=T)
rocICS_jp_lda_TH[[6]]<-roc(predICS_jp_lda_TH$justpass,as.numeric(predICS_jp_lda_TH$Week6),auc=T,ci=T)
rocICS_jp_lda_TH[[7]]<-roc(predICS_jp_lda_TH$justpass,as.numeric(predICS_jp_lda_TH$Week7),auc=T,ci=T)
ICS_jp_lda_TH_auc<-c(rocICS_jp_lda_TH[[1]]$auc,rocICS_jp_lda_TH[[2]]$auc,rocICS_jp_lda_TH[[3]]$auc,rocICS_jp_lda_TH[[4]]$auc,
                     rocICS_jp_lda_TH[[5]]$auc,rocICS_jp_lda_TH[[6]]$auc,rocICS_jp_lda_TH[[7]]$auc)
ICS_jp_lda_TH_ciL<-c(rocICS_jp_lda_TH[[1]]$ci[1],rocICS_jp_lda_TH[[2]]$ci[1],rocICS_jp_lda_TH[[3]]$ci[1],rocICS_jp_lda_TH[[4]]$ci[1],
                     rocICS_jp_lda_TH[[5]]$ci[1],rocICS_jp_lda_TH[[6]]$ci[1],rocICS_jp_lda_TH[[7]]$ci[1])
ICS_jp_lda_TH_ciH<-c(rocICS_jp_lda_TH[[1]]$ci[3],rocICS_jp_lda_TH[[2]]$ci[3],rocICS_jp_lda_TH[[3]]$ci[3],rocICS_jp_lda_TH[[4]]$ci[3],
                     rocICS_jp_lda_TH[[5]]$ci[3],rocICS_jp_lda_TH[[6]]$ci[3],rocICS_jp_lda_TH[[7]]$ci[3])

######TE models######
#PS layer
predPS_lda_T<-jackPredLDA(centPS,predictors = c("cohort","tarEnt"))
rocPS_lda_T<-list()
rocPS_lda_T[[1]]<-roc(predPS_lda_T$pass,as.numeric(predPS_lda_T$Week1),auc=T,ci=T)
rocPS_lda_T[[2]]<-roc(predPS_lda_T$pass,as.numeric(predPS_lda_T$Week2),auc=T,ci=T)
rocPS_lda_T[[3]]<-roc(predPS_lda_T$pass,as.numeric(predPS_lda_T$Week3),auc=T,ci=T)
rocPS_lda_T[[4]]<-roc(predPS_lda_T$pass,as.numeric(predPS_lda_T$Week4),auc=T,ci=T)
rocPS_lda_T[[5]]<-roc(predPS_lda_T$pass,as.numeric(predPS_lda_T$Week5),auc=T,ci=T)
rocPS_lda_T[[6]]<-roc(predPS_lda_T$pass,as.numeric(predPS_lda_T$Week6),auc=T,ci=T)
rocPS_lda_T[[7]]<-roc(predPS_lda_T$pass,as.numeric(predPS_lda_T$Week7),auc=T,ci=T)
PS_lda_T_auc<-c(rocPS_lda_T[[1]]$auc,rocPS_lda_T[[2]]$auc,rocPS_lda_T[[3]]$auc,rocPS_lda_T[[4]]$auc,
                rocPS_lda_T[[5]]$auc,rocPS_lda_T[[6]]$auc,rocPS_lda_T[[7]]$auc)
PS_lda_T_ciL<-c(rocPS_lda_T[[1]]$ci[1],rocPS_lda_T[[2]]$ci[1],rocPS_lda_T[[3]]$ci[1],rocPS_lda_T[[4]]$ci[1],
                rocPS_lda_T[[5]]$ci[1],rocPS_lda_T[[6]]$ci[1],rocPS_lda_T[[7]]$ci[1])
PS_lda_T_ciH<-c(rocPS_lda_T[[1]]$ci[3],rocPS_lda_T[[2]]$ci[3],rocPS_lda_T[[3]]$ci[3],rocPS_lda_T[[4]]$ci[3],
                rocPS_lda_T[[5]]$ci[3],rocPS_lda_T[[6]]$ci[3],rocPS_lda_T[[7]]$ci[3])
##CD layer
predCD_lda_T<-jackPredLDA(centCD,predictors = c("cohort","tarEnt"))
rocCD_lda_T<-list()
rocCD_lda_T[[1]]<-roc(predCD_lda_T$pass,as.numeric(predCD_lda_T$Week1),auc=T,ci=T)
rocCD_lda_T[[2]]<-roc(predCD_lda_T$pass,as.numeric(predCD_lda_T$Week2),auc=T,ci=T)
rocCD_lda_T[[3]]<-roc(predCD_lda_T$pass,as.numeric(predCD_lda_T$Week3),auc=T,ci=T)
rocCD_lda_T[[4]]<-roc(predCD_lda_T$pass,as.numeric(predCD_lda_T$Week4),auc=T,ci=T)
rocCD_lda_T[[5]]<-roc(predCD_lda_T$pass,as.numeric(predCD_lda_T$Week5),auc=T,ci=T)
rocCD_lda_T[[6]]<-roc(predCD_lda_T$pass,as.numeric(predCD_lda_T$Week6),auc=T,ci=T)
rocCD_lda_T[[7]]<-roc(predCD_lda_T$pass,as.numeric(predCD_lda_T$Week7),auc=T,ci=T)
CD_lda_T_auc<-c(rocCD_lda_T[[1]]$auc,rocCD_lda_T[[2]]$auc,rocCD_lda_T[[3]]$auc,rocCD_lda_T[[4]]$auc,
                rocCD_lda_T[[5]]$auc,rocCD_lda_T[[6]]$auc,rocCD_lda_T[[7]]$auc)
CD_lda_T_ciL<-c(rocCD_lda_T[[1]]$ci[1],rocCD_lda_T[[2]]$ci[1],rocCD_lda_T[[3]]$ci[1],rocCD_lda_T[[4]]$ci[1],
                rocCD_lda_T[[5]]$ci[1],rocCD_lda_T[[6]]$ci[1],rocCD_lda_T[[7]]$ci[1])
CD_lda_T_ciH<-c(rocCD_lda_T[[1]]$ci[3],rocCD_lda_T[[2]]$ci[3],rocCD_lda_T[[3]]$ci[3],rocCD_lda_T[[4]]$ci[3],
                rocCD_lda_T[[5]]$ci[3],rocCD_lda_T[[6]]$ci[3],rocCD_lda_T[[7]]$ci[3])

#ICS layer
predICS_lda_T<-jackPredLDA(centICS,predictors = c("cohort","tarEnt"))
rocICS_lda_T<-list()
rocICS_lda_T[[1]]<-roc(predICS_lda_T$pass,as.numeric(predICS_lda_T$Week1),auc=T,ci=T)
rocICS_lda_T[[2]]<-roc(predICS_lda_T$pass,as.numeric(predICS_lda_T$Week2),auc=T,ci=T)
rocICS_lda_T[[3]]<-roc(predICS_lda_T$pass,as.numeric(predICS_lda_T$Week3),auc=T,ci=T)
rocICS_lda_T[[4]]<-roc(predICS_lda_T$pass,as.numeric(predICS_lda_T$Week4),auc=T,ci=T)
rocICS_lda_T[[5]]<-roc(predICS_lda_T$pass,as.numeric(predICS_lda_T$Week5),auc=T,ci=T)
rocICS_lda_T[[6]]<-roc(predICS_lda_T$pass,as.numeric(predICS_lda_T$Week6),auc=T,ci=T)
rocICS_lda_T[[7]]<-roc(predICS_lda_T$pass,as.numeric(predICS_lda_T$Week7),auc=T,ci=T)
ICS_lda_T_auc<-c(rocICS_lda_T[[1]]$auc,rocICS_lda_T[[2]]$auc,rocICS_lda_T[[3]]$auc,rocICS_lda_T[[4]]$auc,
                 rocICS_lda_T[[5]]$auc,rocICS_lda_T[[6]]$auc,rocICS_lda_T[[7]]$auc)
ICS_lda_T_ciL<-c(rocICS_lda_T[[1]]$ci[1],rocICS_lda_T[[2]]$ci[1],rocICS_lda_T[[3]]$ci[1],rocICS_lda_T[[4]]$ci[1],
                 rocICS_lda_T[[5]]$ci[1],rocICS_lda_T[[6]]$ci[1],rocICS_lda_T[[7]]$ci[1])
ICS_lda_T_ciH<-c(rocICS_lda_T[[1]]$ci[3],rocICS_lda_T[[2]]$ci[3],rocICS_lda_T[[3]]$ci[3],rocICS_lda_T[[4]]$ci[3],
                 rocICS_lda_T[[5]]$ci[3],rocICS_lda_T[[6]]$ci[3],rocICS_lda_T[[7]]$ci[3])

###JUST PASS
#PS layer
predPS_jp_lda_T<-jackPredLDA(centPS,outcome = "justpass",predictors = c("cohort","tarEnt"))
rocPS_jp_lda_T<-list()
rocPS_jp_lda_T[[1]]<-roc(predPS_jp_lda_T$justpass,as.numeric(predPS_jp_lda_T$Week1),auc=T,ci=T)
rocPS_jp_lda_T[[2]]<-roc(predPS_jp_lda_T$justpass,as.numeric(predPS_jp_lda_T$Week2),auc=T,ci=T)
rocPS_jp_lda_T[[3]]<-roc(predPS_jp_lda_T$justpass,as.numeric(predPS_jp_lda_T$Week3),auc=T,ci=T)
rocPS_jp_lda_T[[4]]<-roc(predPS_jp_lda_T$justpass,as.numeric(predPS_jp_lda_T$Week4),auc=T,ci=T)
rocPS_jp_lda_T[[5]]<-roc(predPS_jp_lda_T$justpass,as.numeric(predPS_jp_lda_T$Week5),auc=T,ci=T)
rocPS_jp_lda_T[[6]]<-roc(predPS_jp_lda_T$justpass,as.numeric(predPS_jp_lda_T$Week6),auc=T,ci=T)
rocPS_jp_lda_T[[7]]<-roc(predPS_jp_lda_T$justpass,as.numeric(predPS_jp_lda_T$Week7),auc=T,ci=T)

PS_jp_lda_T_auc<-c(rocPS_jp_lda_T[[1]]$auc,rocPS_jp_lda_T[[2]]$auc,rocPS_jp_lda_T[[3]]$auc,rocPS_jp_lda_T[[4]]$auc,
                   rocPS_jp_lda_T[[5]]$auc,rocPS_jp_lda_T[[6]]$auc,rocPS_jp_lda_T[[7]]$auc)
PS_jp_lda_T_ciL<-c(rocPS_jp_lda_T[[1]]$ci[1],rocPS_jp_lda_T[[2]]$ci[1],rocPS_jp_lda_T[[3]]$ci[1],rocPS_jp_lda_T[[4]]$ci[1],
                   rocPS_jp_lda_T[[5]]$ci[1],rocPS_jp_lda_T[[6]]$ci[1],rocPS_jp_lda_T[[7]]$ci[1])
PS_jp_lda_T_ciH<-c(rocPS_jp_lda_T[[1]]$ci[3],rocPS_jp_lda_T[[2]]$ci[3],rocPS_jp_lda_T[[3]]$ci[3],rocPS_jp_lda_T[[4]]$ci[3],
                   rocPS_jp_lda_T[[5]]$ci[3],rocPS_jp_lda_T[[6]]$ci[3],rocPS_jp_lda_T[[7]]$ci[3])
#CD layer
predCD_jp_lda_T<-jackPredLDA(centCD, outcome = "justpass",predictors = c("cohort","tarEnt"))
rocCD_jp_lda_T<-list()
rocCD_jp_lda_T[[1]]<-roc(predCD_jp_lda_T$justpass,as.numeric(predCD_jp_lda_T$Week1),auc=T,ci=T)
rocCD_jp_lda_T[[2]]<-roc(predCD_jp_lda_T$justpass,as.numeric(predCD_jp_lda_T$Week2),auc=T,ci=T)
rocCD_jp_lda_T[[3]]<-roc(predCD_jp_lda_T$justpass,as.numeric(predCD_jp_lda_T$Week3),auc=T,ci=T)
rocCD_jp_lda_T[[4]]<-roc(predCD_jp_lda_T$justpass,as.numeric(predCD_jp_lda_T$Week4),auc=T,ci=T)
rocCD_jp_lda_T[[5]]<-roc(predCD_jp_lda_T$justpass,as.numeric(predCD_jp_lda_T$Week5),auc=T,ci=T)
rocCD_jp_lda_T[[6]]<-roc(predCD_jp_lda_T$justpass,as.numeric(predCD_jp_lda_T$Week6),auc=T,ci=T)
rocCD_jp_lda_T[[7]]<-roc(predCD_jp_lda_T$justpass,as.numeric(predCD_jp_lda_T$Week7),auc=T,ci=T)
CD_jp_lda_T_auc<-c(rocCD_jp_lda_T[[1]]$auc,rocCD_jp_lda_T[[2]]$auc,rocCD_jp_lda_T[[3]]$auc,rocCD_jp_lda_T[[4]]$auc,
                   rocCD_jp_lda_T[[5]]$auc,rocCD_jp_lda_T[[6]]$auc,rocCD_jp_lda_T[[7]]$auc)
CD_jp_lda_T_ciL<-c(rocCD_jp_lda_T[[1]]$ci[1],rocCD_jp_lda_T[[2]]$ci[1],rocCD_jp_lda_T[[3]]$ci[1],rocCD_jp_lda_T[[4]]$ci[1],
                   rocCD_jp_lda_T[[5]]$ci[1],rocCD_jp_lda_T[[6]]$ci[1],rocCD_jp_lda_T[[7]]$ci[1])
CD_jp_lda_T_ciH<-c(rocCD_jp_lda_T[[1]]$ci[3],rocCD_jp_lda_T[[2]]$ci[3],rocCD_jp_lda_T[[3]]$ci[3],rocCD_jp_lda_T[[4]]$ci[3],
                   rocCD_jp_lda_T[[5]]$ci[3],rocCD_jp_lda_T[[6]]$ci[3],rocCD_jp_lda_T[[7]]$ci[3])
#ICS layer
predICS_jp_lda_T<-jackPredLDA(centICS, outcome = "justpass",predictors = c("cohort","tarEnt"))
rocICS_jp_lda_T<-list()
rocICS_jp_lda_T[[1]]<-roc(predICS_jp_lda_T$justpass,as.numeric(predICS_jp_lda_T$Week1),auc=T,ci=T)
rocICS_jp_lda_T[[2]]<-roc(predICS_jp_lda_T$justpass,as.numeric(predICS_jp_lda_T$Week2),auc=T,ci=T)
rocICS_jp_lda_T[[3]]<-roc(predICS_jp_lda_T$justpass,as.numeric(predICS_jp_lda_T$Week3),auc=T,ci=T)
rocICS_jp_lda_T[[4]]<-roc(predICS_jp_lda_T$justpass,as.numeric(predICS_jp_lda_T$Week4),auc=T,ci=T)
rocICS_jp_lda_T[[5]]<-roc(predICS_jp_lda_T$justpass,as.numeric(predICS_jp_lda_T$Week5),auc=T,ci=T)
rocICS_jp_lda_T[[6]]<-roc(predICS_jp_lda_T$justpass,as.numeric(predICS_jp_lda_T$Week6),auc=T,ci=T)
rocICS_jp_lda_T[[7]]<-roc(predICS_jp_lda_T$justpass,as.numeric(predICS_jp_lda_T$Week7),auc=T,ci=T)
ICS_jp_lda_T_auc<-c(rocICS_jp_lda_T[[1]]$auc,rocICS_jp_lda_T[[2]]$auc,rocICS_jp_lda_T[[3]]$auc,rocICS_jp_lda_T[[4]]$auc,
                    rocICS_jp_lda_T[[5]]$auc,rocICS_jp_lda_T[[6]]$auc,rocICS_jp_lda_T[[7]]$auc)
ICS_jp_lda_T_ciL<-c(rocICS_jp_lda_T[[1]]$ci[1],rocICS_jp_lda_T[[2]]$ci[1],rocICS_jp_lda_T[[3]]$ci[1],rocICS_jp_lda_T[[4]]$ci[1],
                    rocICS_jp_lda_T[[5]]$ci[1],rocICS_jp_lda_T[[6]]$ci[1],rocICS_jp_lda_T[[7]]$ci[1])
ICS_jp_lda_T_ciH<-c(rocICS_jp_lda_T[[1]]$ci[3],rocICS_jp_lda_T[[2]]$ci[3],rocICS_jp_lda_T[[3]]$ci[3],rocICS_jp_lda_T[[4]]$ci[3],
                    rocICS_jp_lda_T[[5]]$ci[3],rocICS_jp_lda_T[[6]]$ci[3],rocICS_jp_lda_T[[7]]$ci[3])



######H models######
#PS layer
predPS_lda_H<-jackPredLDA(centPS,predictors = c("cohort", "Hide"))
rocPS_lda_H<-list()
rocPS_lda_H[[1]]<-roc(predPS_lda_H$pass,as.numeric(predPS_lda_H$Week1),auc=T,ci=T)
rocPS_lda_H[[2]]<-roc(predPS_lda_H$pass,as.numeric(predPS_lda_H$Week2),auc=T,ci=T)
rocPS_lda_H[[3]]<-roc(predPS_lda_H$pass,as.numeric(predPS_lda_H$Week3),auc=T,ci=T)
rocPS_lda_H[[4]]<-roc(predPS_lda_H$pass,as.numeric(predPS_lda_H$Week4),auc=T,ci=T)
rocPS_lda_H[[5]]<-roc(predPS_lda_H$pass,as.numeric(predPS_lda_H$Week5),auc=T,ci=T)
rocPS_lda_H[[6]]<-roc(predPS_lda_H$pass,as.numeric(predPS_lda_H$Week6),auc=T,ci=T)
rocPS_lda_H[[7]]<-roc(predPS_lda_H$pass,as.numeric(predPS_lda_H$Week7),auc=T,ci=T)
PS_lda_H_auc<-c(rocPS_lda_H[[1]]$auc,rocPS_lda_H[[2]]$auc,rocPS_lda_H[[3]]$auc,rocPS_lda_H[[4]]$auc,
                rocPS_lda_H[[5]]$auc,rocPS_lda_H[[6]]$auc,rocPS_lda_H[[7]]$auc)
PS_lda_H_ciL<-c(rocPS_lda_H[[1]]$ci[1],rocPS_lda_H[[2]]$ci[1],rocPS_lda_H[[3]]$ci[1],rocPS_lda_H[[4]]$ci[1],
                rocPS_lda_H[[5]]$ci[1],rocPS_lda_H[[6]]$ci[1],rocPS_lda_H[[7]]$ci[1])
PS_lda_H_ciH<-c(rocPS_lda_H[[1]]$ci[3],rocPS_lda_H[[2]]$ci[3],rocPS_lda_H[[3]]$ci[3],rocPS_lda_H[[4]]$ci[3],
                rocPS_lda_H[[5]]$ci[3],rocPS_lda_H[[6]]$ci[3],rocPS_lda_H[[7]]$ci[3])
##CD layer
predCD_lda_H<-jackPredLDA(centCD,predictors = c("cohort", "Hide"))
rocCD_lda_H<-list()
rocCD_lda_H[[1]]<-roc(predCD_lda_H$pass,as.numeric(predCD_lda_H$Week1),auc=T,ci=T)
rocCD_lda_H[[2]]<-roc(predCD_lda_H$pass,as.numeric(predCD_lda_H$Week2),auc=T,ci=T)
rocCD_lda_H[[3]]<-roc(predCD_lda_H$pass,as.numeric(predCD_lda_H$Week3),auc=T,ci=T)
rocCD_lda_H[[4]]<-roc(predCD_lda_H$pass,as.numeric(predCD_lda_H$Week4),auc=T,ci=T)
rocCD_lda_H[[5]]<-roc(predCD_lda_H$pass,as.numeric(predCD_lda_H$Week5),auc=T,ci=T)
rocCD_lda_H[[6]]<-roc(predCD_lda_H$pass,as.numeric(predCD_lda_H$Week6),auc=T,ci=T)
rocCD_lda_H[[7]]<-roc(predCD_lda_H$pass,as.numeric(predCD_lda_H$Week7),auc=T,ci=T)
CD_lda_H_auc<-c(rocCD_lda_H[[1]]$auc,rocCD_lda_H[[2]]$auc,rocCD_lda_H[[3]]$auc,rocCD_lda_H[[4]]$auc,
                rocCD_lda_H[[5]]$auc,rocCD_lda_H[[6]]$auc,rocCD_lda_H[[7]]$auc)
CD_lda_H_ciL<-c(rocCD_lda_H[[1]]$ci[1],rocCD_lda_H[[2]]$ci[1],rocCD_lda_H[[3]]$ci[1],rocCD_lda_H[[4]]$ci[1],
                rocCD_lda_H[[5]]$ci[1],rocCD_lda_H[[6]]$ci[1],rocCD_lda_H[[7]]$ci[1])
CD_lda_H_ciH<-c(rocCD_lda_H[[1]]$ci[3],rocCD_lda_H[[2]]$ci[3],rocCD_lda_H[[3]]$ci[3],rocCD_lda_H[[4]]$ci[3],
                rocCD_lda_H[[5]]$ci[3],rocCD_lda_H[[6]]$ci[3],rocCD_lda_H[[7]]$ci[3])

#ICS layer
predICS_lda_H<-jackPredLDA(centICS,predictors = c("cohort", "Hide"))
rocICS_lda_H<-list()
rocICS_lda_H[[1]]<-roc(predICS_lda_H$pass,as.numeric(predICS_lda_H$Week1),auc=T,ci=T)
rocICS_lda_H[[2]]<-roc(predICS_lda_H$pass,as.numeric(predICS_lda_H$Week2),auc=T,ci=T)
rocICS_lda_H[[3]]<-roc(predICS_lda_H$pass,as.numeric(predICS_lda_H$Week3),auc=T,ci=T)
rocICS_lda_H[[4]]<-roc(predICS_lda_H$pass,as.numeric(predICS_lda_H$Week4),auc=T,ci=T)
rocICS_lda_H[[5]]<-roc(predICS_lda_H$pass,as.numeric(predICS_lda_H$Week5),auc=T,ci=T)
rocICS_lda_H[[6]]<-roc(predICS_lda_H$pass,as.numeric(predICS_lda_H$Week6),auc=T,ci=T)
rocICS_lda_H[[7]]<-roc(predICS_lda_H$pass,as.numeric(predICS_lda_H$Week7),auc=T,ci=T)
ICS_lda_H_auc<-c(rocICS_lda_H[[1]]$auc,rocICS_lda_H[[2]]$auc,rocICS_lda_H[[3]]$auc,rocICS_lda_H[[4]]$auc,
                 rocICS_lda_H[[5]]$auc,rocICS_lda_H[[6]]$auc,rocICS_lda_H[[7]]$auc)
ICS_lda_H_ciL<-c(rocICS_lda_H[[1]]$ci[1],rocICS_lda_H[[2]]$ci[1],rocICS_lda_H[[3]]$ci[1],rocICS_lda_H[[4]]$ci[1],
                 rocICS_lda_H[[5]]$ci[1],rocICS_lda_H[[6]]$ci[1],rocICS_lda_H[[7]]$ci[1])
ICS_lda_H_ciH<-c(rocICS_lda_H[[1]]$ci[3],rocICS_lda_H[[2]]$ci[3],rocICS_lda_H[[3]]$ci[3],rocICS_lda_H[[4]]$ci[3],
                 rocICS_lda_H[[5]]$ci[3],rocICS_lda_H[[6]]$ci[3],rocICS_lda_H[[7]]$ci[3])


###JUST PASS
predPS_jp_lda_H<-jackPredLDA(centPS,outcome = "justpass",predictors = c("cohort", "Hide"))
rocPS_jp_lda_H<-list()
rocPS_jp_lda_H[[1]]<-roc(predPS_jp_lda_H$justpass,as.numeric(predPS_jp_lda_H$Week1),auc=T,ci=T)
rocPS_jp_lda_H[[2]]<-roc(predPS_jp_lda_H$justpass,as.numeric(predPS_jp_lda_H$Week2),auc=T,ci=T)
rocPS_jp_lda_H[[3]]<-roc(predPS_jp_lda_H$justpass,as.numeric(predPS_jp_lda_H$Week3),auc=T,ci=T)
rocPS_jp_lda_H[[4]]<-roc(predPS_jp_lda_H$justpass,as.numeric(predPS_jp_lda_H$Week4),auc=T,ci=T)
rocPS_jp_lda_H[[5]]<-roc(predPS_jp_lda_H$justpass,as.numeric(predPS_jp_lda_H$Week5),auc=T,ci=T)
rocPS_jp_lda_H[[6]]<-roc(predPS_jp_lda_H$justpass,as.numeric(predPS_jp_lda_H$Week6),auc=T,ci=T)
rocPS_jp_lda_H[[7]]<-roc(predPS_jp_lda_H$justpass,as.numeric(predPS_jp_lda_H$Week7),auc=T,ci=T)

PS_jp_lda_H_auc<-c(rocPS_jp_lda_H[[1]]$auc,rocPS_jp_lda_H[[2]]$auc,rocPS_jp_lda_H[[3]]$auc,rocPS_jp_lda_H[[4]]$auc,
                   rocPS_jp_lda_H[[5]]$auc,rocPS_jp_lda_H[[6]]$auc,rocPS_jp_lda_H[[7]]$auc)
PS_jp_lda_H_ciL<-c(rocPS_jp_lda_H[[1]]$ci[1],rocPS_jp_lda_H[[2]]$ci[1],rocPS_jp_lda_H[[3]]$ci[1],rocPS_jp_lda_H[[4]]$ci[1],
                   rocPS_jp_lda_H[[5]]$ci[1],rocPS_jp_lda_H[[6]]$ci[1],rocPS_jp_lda_H[[7]]$ci[1])
PS_jp_lda_H_ciH<-c(rocPS_jp_lda_H[[1]]$ci[3],rocPS_jp_lda_H[[2]]$ci[3],rocPS_jp_lda_H[[3]]$ci[3],rocPS_jp_lda_H[[4]]$ci[3],
                   rocPS_jp_lda_H[[5]]$ci[3],rocPS_jp_lda_H[[6]]$ci[3],rocPS_jp_lda_H[[7]]$ci[3])

predCD_jp_lda_H<-jackPredLDA(centCD, outcome = "justpass",predictors = c("cohort", "Hide"))
rocCD_jp_lda_H<-list()
rocCD_jp_lda_H[[1]]<-roc(predCD_jp_lda_H$justpass,as.numeric(predCD_jp_lda_H$Week1),auc=T,ci=T)
rocCD_jp_lda_H[[2]]<-roc(predCD_jp_lda_H$justpass,as.numeric(predCD_jp_lda_H$Week2),auc=T,ci=T)
rocCD_jp_lda_H[[3]]<-roc(predCD_jp_lda_H$justpass,as.numeric(predCD_jp_lda_H$Week3),auc=T,ci=T)
rocCD_jp_lda_H[[4]]<-roc(predCD_jp_lda_H$justpass,as.numeric(predCD_jp_lda_H$Week4),auc=T,ci=T)
rocCD_jp_lda_H[[5]]<-roc(predCD_jp_lda_H$justpass,as.numeric(predCD_jp_lda_H$Week5),auc=T,ci=T)
rocCD_jp_lda_H[[6]]<-roc(predCD_jp_lda_H$justpass,as.numeric(predCD_jp_lda_H$Week6),auc=T,ci=T)
rocCD_jp_lda_H[[7]]<-roc(predCD_jp_lda_H$justpass,as.numeric(predCD_jp_lda_H$Week7),auc=T,ci=T)
CD_jp_lda_H_auc<-c(rocCD_jp_lda_H[[1]]$auc,rocCD_jp_lda_H[[2]]$auc,rocCD_jp_lda_H[[3]]$auc,rocCD_jp_lda_H[[4]]$auc,
                   rocCD_jp_lda_H[[5]]$auc,rocCD_jp_lda_H[[6]]$auc,rocCD_jp_lda_H[[7]]$auc)
CD_jp_lda_H_ciL<-c(rocCD_jp_lda_H[[1]]$ci[1],rocCD_jp_lda_H[[2]]$ci[1],rocCD_jp_lda_H[[3]]$ci[1],rocCD_jp_lda_H[[4]]$ci[1],
                   rocCD_jp_lda_H[[5]]$ci[1],rocCD_jp_lda_H[[6]]$ci[1],rocCD_jp_lda_H[[7]]$ci[1])
CD_jp_lda_H_ciH<-c(rocCD_jp_lda_H[[1]]$ci[3],rocCD_jp_lda_H[[2]]$ci[3],rocCD_jp_lda_H[[3]]$ci[3],rocCD_jp_lda_H[[4]]$ci[3],
                   rocCD_jp_lda_H[[5]]$ci[3],rocCD_jp_lda_H[[6]]$ci[3],rocCD_jp_lda_H[[7]]$ci[3])


predICS_jp_lda_H<-jackPredLDA(centICS, outcome = "justpass",predictors = c("cohort", "Hide"))
rocICS_jp_lda_H<-list()
rocICS_jp_lda_H[[1]]<-roc(predICS_jp_lda_H$justpass,as.numeric(predICS_jp_lda_H$Week1),auc=T,ci=T)
rocICS_jp_lda_H[[2]]<-roc(predICS_jp_lda_H$justpass,as.numeric(predICS_jp_lda_H$Week2),auc=T,ci=T)
rocICS_jp_lda_H[[3]]<-roc(predICS_jp_lda_H$justpass,as.numeric(predICS_jp_lda_H$Week3),auc=T,ci=T)
rocICS_jp_lda_H[[4]]<-roc(predICS_jp_lda_H$justpass,as.numeric(predICS_jp_lda_H$Week4),auc=T,ci=T)
rocICS_jp_lda_H[[5]]<-roc(predICS_jp_lda_H$justpass,as.numeric(predICS_jp_lda_H$Week5),auc=T,ci=T)
rocICS_jp_lda_H[[6]]<-roc(predICS_jp_lda_H$justpass,as.numeric(predICS_jp_lda_H$Week6),auc=T,ci=T)
rocICS_jp_lda_H[[7]]<-roc(predICS_jp_lda_H$justpass,as.numeric(predICS_jp_lda_H$Week7),auc=T,ci=T)
ICS_jp_lda_H_auc<-c(rocICS_jp_lda_H[[1]]$auc,rocICS_jp_lda_H[[2]]$auc,rocICS_jp_lda_H[[3]]$auc,rocICS_jp_lda_H[[4]]$auc,
                    rocICS_jp_lda_H[[5]]$auc,rocICS_jp_lda_H[[6]]$auc,rocICS_jp_lda_H[[7]]$auc)
ICS_jp_lda_H_ciL<-c(rocICS_jp_lda_H[[1]]$ci[1],rocICS_jp_lda_H[[2]]$ci[1],rocICS_jp_lda_H[[3]]$ci[1],rocICS_jp_lda_H[[4]]$ci[1],
                    rocICS_jp_lda_H[[5]]$ci[1],rocICS_jp_lda_H[[6]]$ci[1],rocICS_jp_lda_H[[7]]$ci[1])
ICS_jp_lda_H_ciH<-c(rocICS_jp_lda_H[[1]]$ci[3],rocICS_jp_lda_H[[2]]$ci[3],rocICS_jp_lda_H[[3]]$ci[3],rocICS_jp_lda_H[[4]]$ci[3],
                    rocICS_jp_lda_H[[5]]$ci[3],rocICS_jp_lda_H[[6]]$ci[3],rocICS_jp_lda_H[[7]]$ci[3])


######PR models######
#PS layer
predPS_lda_P<-jackPredLDA(centPS,predictors = c("cohort","PageRank"))
rocPS_lda_P<-list()
rocPS_lda_P[[1]]<-roc(predPS_lda_P$pass,as.numeric(predPS_lda_P$Week1),auc=T,ci=T)
rocPS_lda_P[[2]]<-roc(predPS_lda_P$pass,as.numeric(predPS_lda_P$Week2),auc=T,ci=T)
rocPS_lda_P[[3]]<-roc(predPS_lda_P$pass,as.numeric(predPS_lda_P$Week3),auc=T,ci=T)
rocPS_lda_P[[4]]<-roc(predPS_lda_P$pass,as.numeric(predPS_lda_P$Week4),auc=T,ci=T)
rocPS_lda_P[[5]]<-roc(predPS_lda_P$pass,as.numeric(predPS_lda_P$Week5),auc=T,ci=T)
rocPS_lda_P[[6]]<-roc(predPS_lda_P$pass,as.numeric(predPS_lda_P$Week6),auc=T,ci=T)
rocPS_lda_P[[7]]<-roc(predPS_lda_P$pass,as.numeric(predPS_lda_P$Week7),auc=T,ci=T)
PS_lda_P_auc<-c(rocPS_lda_P[[1]]$auc,rocPS_lda_P[[2]]$auc,rocPS_lda_P[[3]]$auc,rocPS_lda_P[[4]]$auc,
                rocPS_lda_P[[5]]$auc,rocPS_lda_P[[6]]$auc,rocPS_lda_P[[7]]$auc)
PS_lda_P_ciL<-c(rocPS_lda_P[[1]]$ci[1],rocPS_lda_P[[2]]$ci[1],rocPS_lda_P[[3]]$ci[1],rocPS_lda_P[[4]]$ci[1],
                rocPS_lda_P[[5]]$ci[1],rocPS_lda_P[[6]]$ci[1],rocPS_lda_P[[7]]$ci[1])
PS_lda_P_ciH<-c(rocPS_lda_P[[1]]$ci[3],rocPS_lda_P[[2]]$ci[3],rocPS_lda_P[[3]]$ci[3],rocPS_lda_P[[4]]$ci[3],
                rocPS_lda_P[[5]]$ci[3],rocPS_lda_P[[6]]$ci[3],rocPS_lda_P[[7]]$ci[3])
##CD layer
predCD_lda_P<-jackPredLDA(centCD,predictors = c("cohort","PageRank"))
rocCD_lda_P<-list()
rocCD_lda_P[[1]]<-roc(predCD_lda_P$pass,as.numeric(predCD_lda_P$Week1),auc=T,ci=T)
rocCD_lda_P[[2]]<-roc(predCD_lda_P$pass,as.numeric(predCD_lda_P$Week2),auc=T,ci=T)
rocCD_lda_P[[3]]<-roc(predCD_lda_P$pass,as.numeric(predCD_lda_P$Week3),auc=T,ci=T)
rocCD_lda_P[[4]]<-roc(predCD_lda_P$pass,as.numeric(predCD_lda_P$Week4),auc=T,ci=T)
rocCD_lda_P[[5]]<-roc(predCD_lda_P$pass,as.numeric(predCD_lda_P$Week5),auc=T,ci=T)
rocCD_lda_P[[6]]<-roc(predCD_lda_P$pass,as.numeric(predCD_lda_P$Week6),auc=T,ci=T)
rocCD_lda_P[[7]]<-roc(predCD_lda_P$pass,as.numeric(predCD_lda_P$Week7),auc=T,ci=T)
CD_lda_P_auc<-c(rocCD_lda_P[[1]]$auc,rocCD_lda_P[[2]]$auc,rocCD_lda_P[[3]]$auc,rocCD_lda_P[[4]]$auc,
                rocCD_lda_P[[5]]$auc,rocCD_lda_P[[6]]$auc,rocCD_lda_P[[7]]$auc)
CD_lda_P_ciL<-c(rocCD_lda_P[[1]]$ci[1],rocCD_lda_P[[2]]$ci[1],rocCD_lda_P[[3]]$ci[1],rocCD_lda_P[[4]]$ci[1],
                rocCD_lda_P[[5]]$ci[1],rocCD_lda_P[[6]]$ci[1],rocCD_lda_P[[7]]$ci[1])
CD_lda_P_ciH<-c(rocCD_lda_P[[1]]$ci[3],rocCD_lda_P[[2]]$ci[3],rocCD_lda_P[[3]]$ci[3],rocCD_lda_P[[4]]$ci[3],
                rocCD_lda_P[[5]]$ci[3],rocCD_lda_P[[6]]$ci[3],rocCD_lda_P[[7]]$ci[3])

#ICS layer
predICS_lda_P<-jackPredLDA(centICS,predictors = c("cohort","PageRank"))
rocICS_lda_P<-list()
rocICS_lda_P[[1]]<-roc(predICS_lda_P$pass,as.numeric(predICS_lda_P$Week1),auc=T,ci=T)
rocICS_lda_P[[2]]<-roc(predICS_lda_P$pass,as.numeric(predICS_lda_P$Week2),auc=T,ci=T)
rocICS_lda_P[[3]]<-roc(predICS_lda_P$pass,as.numeric(predICS_lda_P$Week3),auc=T,ci=T)
rocICS_lda_P[[4]]<-roc(predICS_lda_P$pass,as.numeric(predICS_lda_P$Week4),auc=T,ci=T)
rocICS_lda_P[[5]]<-roc(predICS_lda_P$pass,as.numeric(predICS_lda_P$Week5),auc=T,ci=T)
rocICS_lda_P[[6]]<-roc(predICS_lda_P$pass,as.numeric(predICS_lda_P$Week6),auc=T,ci=T)
rocICS_lda_P[[7]]<-roc(predICS_lda_P$pass,as.numeric(predICS_lda_P$Week7),auc=T,ci=T)
ICS_lda_P_auc<-c(rocICS_lda_P[[1]]$auc,rocICS_lda_P[[2]]$auc,rocICS_lda_P[[3]]$auc,rocICS_lda_P[[4]]$auc,
                 rocICS_lda_P[[5]]$auc,rocICS_lda_P[[6]]$auc,rocICS_lda_P[[7]]$auc)
ICS_lda_P_ciL<-c(rocICS_lda_P[[1]]$ci[1],rocICS_lda_P[[2]]$ci[1],rocICS_lda_P[[3]]$ci[1],rocICS_lda_P[[4]]$ci[1],
                 rocICS_lda_P[[5]]$ci[1],rocICS_lda_P[[6]]$ci[1],rocICS_lda_P[[7]]$ci[1])
ICS_lda_P_ciH<-c(rocICS_lda_P[[1]]$ci[3],rocICS_lda_P[[2]]$ci[3],rocICS_lda_P[[3]]$ci[3],rocICS_lda_P[[4]]$ci[3],
                 rocICS_lda_P[[5]]$ci[3],rocICS_lda_P[[6]]$ci[3],rocICS_lda_P[[7]]$ci[3])

###JUST PASS
#PS layer
predPS_jp_lda_P<-jackPredLDA(centPS,outcome = "justpass",predictors = c("cohort","PageRank"))
rocPS_jp_lda_P<-list()
rocPS_jp_lda_P[[1]]<-roc(predPS_jp_lda_P$justpass,as.numeric(predPS_jp_lda_P$Week1),auc=T,ci=T)
rocPS_jp_lda_P[[2]]<-roc(predPS_jp_lda_P$justpass,as.numeric(predPS_jp_lda_P$Week2),auc=T,ci=T)
rocPS_jp_lda_P[[3]]<-roc(predPS_jp_lda_P$justpass,as.numeric(predPS_jp_lda_P$Week3),auc=T,ci=T)
rocPS_jp_lda_P[[4]]<-roc(predPS_jp_lda_P$justpass,as.numeric(predPS_jp_lda_P$Week4),auc=T,ci=T)
rocPS_jp_lda_P[[5]]<-roc(predPS_jp_lda_P$justpass,as.numeric(predPS_jp_lda_P$Week5),auc=T,ci=T)
rocPS_jp_lda_P[[6]]<-roc(predPS_jp_lda_P$justpass,as.numeric(predPS_jp_lda_P$Week6),auc=T,ci=T)
rocPS_jp_lda_P[[7]]<-roc(predPS_jp_lda_P$justpass,as.numeric(predPS_jp_lda_P$Week7),auc=T,ci=T)

PS_jp_lda_P_auc<-c(rocPS_jp_lda_P[[1]]$auc,rocPS_jp_lda_P[[2]]$auc,rocPS_jp_lda_P[[3]]$auc,rocPS_jp_lda_P[[4]]$auc,
                   rocPS_jp_lda_P[[5]]$auc,rocPS_jp_lda_P[[6]]$auc,rocPS_jp_lda_P[[7]]$auc)
PS_jp_lda_P_ciL<-c(rocPS_jp_lda_P[[1]]$ci[1],rocPS_jp_lda_P[[2]]$ci[1],rocPS_jp_lda_P[[3]]$ci[1],rocPS_jp_lda_P[[4]]$ci[1],
                   rocPS_jp_lda_P[[5]]$ci[1],rocPS_jp_lda_P[[6]]$ci[1],rocPS_jp_lda_P[[7]]$ci[1])
PS_jp_lda_P_ciH<-c(rocPS_jp_lda_P[[1]]$ci[3],rocPS_jp_lda_P[[2]]$ci[3],rocPS_jp_lda_P[[3]]$ci[3],rocPS_jp_lda_P[[4]]$ci[3],
                   rocPS_jp_lda_P[[5]]$ci[3],rocPS_jp_lda_P[[6]]$ci[3],rocPS_jp_lda_P[[7]]$ci[3])
#CD layer
predCD_jp_lda_P<-jackPredLDA(centCD, outcome = "justpass",predictors = c("cohort","PageRank"))
rocCD_jp_lda_P<-list()
rocCD_jp_lda_P[[1]]<-roc(predCD_jp_lda_P$justpass,as.numeric(predCD_jp_lda_P$Week1),auc=T,ci=T)
rocCD_jp_lda_P[[2]]<-roc(predCD_jp_lda_P$justpass,as.numeric(predCD_jp_lda_P$Week2),auc=T,ci=T)
rocCD_jp_lda_P[[3]]<-roc(predCD_jp_lda_P$justpass,as.numeric(predCD_jp_lda_P$Week3),auc=T,ci=T)
rocCD_jp_lda_P[[4]]<-roc(predCD_jp_lda_P$justpass,as.numeric(predCD_jp_lda_P$Week4),auc=T,ci=T)
rocCD_jp_lda_P[[5]]<-roc(predCD_jp_lda_P$justpass,as.numeric(predCD_jp_lda_P$Week5),auc=T,ci=T)
rocCD_jp_lda_P[[6]]<-roc(predCD_jp_lda_P$justpass,as.numeric(predCD_jp_lda_P$Week6),auc=T,ci=T)
rocCD_jp_lda_P[[7]]<-roc(predCD_jp_lda_P$justpass,as.numeric(predCD_jp_lda_P$Week7),auc=T,ci=T)
CD_jp_lda_P_auc<-c(rocCD_jp_lda_P[[1]]$auc,rocCD_jp_lda_P[[2]]$auc,rocCD_jp_lda_P[[3]]$auc,rocCD_jp_lda_P[[4]]$auc,
                   rocCD_jp_lda_P[[5]]$auc,rocCD_jp_lda_P[[6]]$auc,rocCD_jp_lda_P[[7]]$auc)
CD_jp_lda_P_ciL<-c(rocCD_jp_lda_P[[1]]$ci[1],rocCD_jp_lda_P[[2]]$ci[1],rocCD_jp_lda_P[[3]]$ci[1],rocCD_jp_lda_P[[4]]$ci[1],
                   rocCD_jp_lda_P[[5]]$ci[1],rocCD_jp_lda_P[[6]]$ci[1],rocCD_jp_lda_P[[7]]$ci[1])
CD_jp_lda_P_ciH<-c(rocCD_jp_lda_P[[1]]$ci[3],rocCD_jp_lda_P[[2]]$ci[3],rocCD_jp_lda_P[[3]]$ci[3],rocCD_jp_lda_P[[4]]$ci[3],
                   rocCD_jp_lda_P[[5]]$ci[3],rocCD_jp_lda_P[[6]]$ci[3],rocCD_jp_lda_P[[7]]$ci[3])

#ICS layer
predICS_jp_lda_P<-jackPredLDA(centICS, outcome = "justpass",predictors = c("cohort","PageRank"))
rocICS_jp_lda_P<-list()
rocICS_jp_lda_P[[1]]<-roc(predICS_jp_lda_P$justpass,as.numeric(predICS_jp_lda_P$Week1),auc=T,ci=T)
rocICS_jp_lda_P[[2]]<-roc(predICS_jp_lda_P$justpass,as.numeric(predICS_jp_lda_P$Week2),auc=T,ci=T)
rocICS_jp_lda_P[[3]]<-roc(predICS_jp_lda_P$justpass,as.numeric(predICS_jp_lda_P$Week3),auc=T,ci=T)
rocICS_jp_lda_P[[4]]<-roc(predICS_jp_lda_P$justpass,as.numeric(predICS_jp_lda_P$Week4),auc=T,ci=T)
rocICS_jp_lda_P[[5]]<-roc(predICS_jp_lda_P$justpass,as.numeric(predICS_jp_lda_P$Week5),auc=T,ci=T)
rocICS_jp_lda_P[[6]]<-roc(predICS_jp_lda_P$justpass,as.numeric(predICS_jp_lda_P$Week6),auc=T,ci=T)
rocICS_jp_lda_P[[7]]<-roc(predICS_jp_lda_P$justpass,as.numeric(predICS_jp_lda_P$Week7),auc=T,ci=T)
ICS_jp_lda_P_auc<-c(rocICS_jp_lda_P[[1]]$auc,rocICS_jp_lda_P[[2]]$auc,rocICS_jp_lda_P[[3]]$auc,rocICS_jp_lda_P[[4]]$auc,
                    rocICS_jp_lda_P[[5]]$auc,rocICS_jp_lda_P[[6]]$auc,rocICS_jp_lda_P[[7]]$auc)
ICS_jp_lda_P_ciL<-c(rocICS_jp_lda_P[[1]]$ci[1],rocICS_jp_lda_P[[2]]$ci[1],rocICS_jp_lda_P[[3]]$ci[1],rocICS_jp_lda_P[[4]]$ci[1],
                    rocICS_jp_lda_P[[5]]$ci[1],rocICS_jp_lda_P[[6]]$ci[1],rocICS_jp_lda_P[[7]]$ci[1])
ICS_jp_lda_P_ciH<-c(rocICS_jp_lda_P[[1]]$ci[3],rocICS_jp_lda_P[[2]]$ci[3],rocICS_jp_lda_P[[3]]$ci[3],rocICS_jp_lda_P[[4]]$ci[3],
                    rocICS_jp_lda_P[[5]]$ci[3],rocICS_jp_lda_P[[6]]$ci[3],rocICS_jp_lda_P[[7]]$ci[3])



####LDA PLOTS####
####Mean over all models####
##pass-fail##
PS_lda_RegAll<-data.frame(PS_lda_PTH_auc,PS_lda_PT_auc,PS_lda_TH_auc,PS_lda_PH_auc,PS_lda_P_auc,PS_lda_T_auc,PS_lda_H_auc)
PS_lda_RegAllciH<-data.frame(PS_lda_PTH_ciH,PS_lda_PT_ciH,PS_lda_TH_ciH,PS_lda_PH_ciH,PS_lda_P_ciH,PS_lda_T_ciH,PS_lda_H_ciH)
PS_lda_RegAllciL<-data.frame(PS_lda_PTH_ciL,PS_lda_PT_ciL,PS_lda_TH_ciL,PS_lda_PH_ciL,PS_lda_P_ciL,PS_lda_T_ciL,PS_lda_H_ciL)

PS_lda_RegSD<-sqrt(166)*(PS_lda_RegAllciH-PS_lda_RegAllciL)/3.92
PS_lda_RegWeigths<-1/PS_lda_RegSD^2
PS_lda_RegWM<-rowSums(PS_lda_RegAll*PS_lda_RegWeigths)/rowSums(PS_lda_RegWeigths)
PS_lda_RegSEWM<-sqrt(1/rowSums(PS_lda_RegWeigths))

CD_lda_RegAll<-data.frame(CD_lda_PTH_auc,CD_lda_PT_auc,CD_lda_TH_auc,CD_lda_PH_auc,CD_lda_P_auc,CD_lda_T_auc,CD_lda_H_auc)
CD_lda_RegAllciH<-data.frame(CD_lda_PTH_ciH,CD_lda_PT_ciH,CD_lda_TH_ciH,CD_lda_PH_ciH,CD_lda_P_ciH,CD_lda_T_ciH,CD_lda_H_ciH)
CD_lda_RegAllciL<-data.frame(CD_lda_PTH_ciL,CD_lda_PT_ciL,CD_lda_TH_ciL,CD_lda_PH_ciL,CD_lda_P_ciL,CD_lda_T_ciL,CD_lda_H_ciL)

CD_lda_RegSD<-sqrt(166)*(CD_lda_RegAllciH-CD_lda_RegAllciL)/3.92
CD_lda_RegWeigths<-1/CD_lda_RegSD^2
CD_lda_RegWM<-rowSums(CD_lda_RegAll*CD_lda_RegWeigths)/rowSums(CD_lda_RegWeigths)
CD_lda_RegSEWM<-sqrt(1/rowSums(CD_lda_RegWeigths))

ICS_lda_RegAll<-data.frame(ICS_lda_PTH_auc,ICS_lda_PT_auc,ICS_lda_TH_auc,ICS_lda_PH_auc,ICS_lda_P_auc,ICS_lda_T_auc,ICS_lda_H_auc)
ICS_lda_RegAllciH<-data.frame(ICS_lda_PTH_ciH,ICS_lda_PT_ciH,ICS_lda_TH_ciH,ICS_lda_PH_ciH,ICS_lda_P_ciH,ICS_lda_T_ciH,ICS_lda_H_ciH)
ICS_lda_RegAllciL<-data.frame(ICS_lda_PTH_ciL,ICS_lda_PT_ciL,ICS_lda_TH_ciL,ICS_lda_PH_ciL,ICS_lda_P_ciL,ICS_lda_T_ciL,ICS_lda_H_ciL)

ICS_lda_RegSD<-sqrt(166)*(ICS_lda_RegAllciH-ICS_lda_RegAllciL)/3.92
ICS_lda_RegWeigths<-1/ICS_lda_RegSD^2
ICS_lda_RegWM<-rowSums(ICS_lda_RegAll*ICS_lda_RegWeigths)/rowSums(ICS_lda_RegWeigths)
ICS_lda_RegSEWM<-sqrt(1/rowSums(ICS_lda_RegWeigths))

##just pass-fail##
PS_jp_lda_RegAll<-data.frame(PS_jp_lda_PTH_auc,PS_jp_lda_PT_auc,PS_jp_lda_TH_auc,PS_jp_lda_PH_auc,PS_jp_lda_P_auc,PS_jp_lda_T_auc,PS_jp_lda_H_auc)
PS_jp_lda_RegAllciH<-data.frame(PS_jp_lda_PTH_ciH,PS_jp_lda_PT_ciH,PS_jp_lda_TH_ciH,PS_jp_lda_PH_ciH,PS_jp_lda_P_ciH,PS_jp_lda_T_ciH,PS_jp_lda_H_ciH)
PS_jp_lda_RegAllciL<-data.frame(PS_jp_lda_PTH_ciL,PS_jp_lda_PT_ciL,PS_jp_lda_TH_ciL,PS_jp_lda_PH_ciL,PS_jp_lda_P_ciL,PS_jp_lda_T_ciL,PS_jp_lda_H_ciL)

PS_jp_lda_RegSD<-sqrt(67)*(PS_jp_lda_RegAllciH-PS_jp_lda_RegAllciL)/3.92
PS_jp_lda_RegWeigths<-1/PS_jp_lda_RegSD^2
PS_jp_lda_RegWM<-rowSums(PS_jp_lda_RegAll*PS_jp_lda_RegWeigths)/rowSums(PS_jp_lda_RegWeigths)
PS_jp_lda_RegSEWM<-sqrt(1/rowSums(PS_jp_lda_RegWeigths))

CD_jp_lda_RegAll<-data.frame(CD_jp_lda_PTH_auc,CD_jp_lda_PT_auc,CD_jp_lda_TH_auc,CD_jp_lda_PH_auc,CD_jp_lda_P_auc,CD_jp_lda_T_auc,CD_jp_lda_H_auc)
CD_jp_lda_RegAllciH<-data.frame(CD_jp_lda_PTH_ciH,CD_jp_lda_PT_ciH,CD_jp_lda_TH_ciH,CD_jp_lda_PH_ciH,CD_jp_lda_P_ciH,CD_jp_lda_T_ciH,CD_jp_lda_H_ciH)
CD_jp_lda_RegAllciL<-data.frame(CD_jp_lda_PTH_ciL,CD_jp_lda_PT_ciL,CD_jp_lda_TH_ciL,CD_jp_lda_PH_ciL,CD_jp_lda_P_ciL,CD_jp_lda_T_ciL,CD_jp_lda_H_ciL)

CD_jp_lda_RegSD<-sqrt(67)*(CD_jp_lda_RegAllciH-CD_jp_lda_RegAllciL)/3.92
CD_jp_lda_RegWeigths<-1/CD_jp_lda_RegSD^2 #Zero standard deviations become infinite here. They are probably a sign of an outlier.
CD_jp_lda_RegWeigths[!is.finite(as.matrix(CD_jp_lda_RegWeigths))]<-NA #Correction for zero standard deviations.
CD_jp_lda_RegWM<-rowSums(CD_jp_lda_RegAll*CD_jp_lda_RegWeigths,na.rm = T)/rowSums(CD_jp_lda_RegWeigths,na.rm = T)
CD_jp_lda_RegSEWM<-sqrt(1/rowSums(CD_jp_lda_RegWeigths,na.rm=T))

ICS_jp_lda_RegAll<-data.frame(ICS_jp_lda_PTH_auc,ICS_jp_lda_PT_auc,ICS_jp_lda_TH_auc,ICS_jp_lda_PH_auc,ICS_jp_lda_P_auc,ICS_jp_lda_T_auc,ICS_jp_lda_H_auc)
ICS_jp_lda_RegAllciH<-data.frame(ICS_jp_lda_PTH_ciH,ICS_jp_lda_PT_ciH,ICS_jp_lda_TH_ciH,ICS_jp_lda_PH_ciH,ICS_jp_lda_P_ciH,ICS_jp_lda_T_ciH,ICS_jp_lda_H_ciH)
ICS_jp_lda_RegAllciL<-data.frame(ICS_jp_lda_PTH_ciL,ICS_jp_lda_PT_ciL,ICS_jp_lda_TH_ciL,ICS_jp_lda_PH_ciL,ICS_jp_lda_P_ciL,ICS_jp_lda_T_ciL,ICS_jp_lda_H_ciL)

ICS_jp_lda_RegSD<-sqrt(67)*(ICS_jp_lda_RegAllciH-ICS_jp_lda_RegAllciL)/3.92
ICS_jp_lda_RegWeigths<-1/ICS_jp_lda_RegSD^2
ICS_jp_lda_RegWM<-rowSums(ICS_jp_lda_RegAll*ICS_jp_lda_RegWeigths)/rowSums(ICS_jp_lda_RegWeigths)
ICS_jp_lda_RegSEWM<-sqrt(1/rowSums(ICS_jp_lda_RegWeigths))
####PLOTTING####
pdf(file = "plots/ROC_AUC_plots/AUC04_NetworkMeasuresCohort_lda_weightedMeans.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,2))
x<-c(1:7)
plot(x, PS_lda_RegWM,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="LDA Pass-fail", sub="Weighted mean of all centrality models",type="b"
)
lines(x+0.1,CD_lda_RegWM,pch=1,type="b",col="darkred")
lines(x-0.1,ICS_lda_RegWM,pch=2,type="b",col="darkblue")
# hack: we draw arrows but with very special "arrowheads"

arrows(x, PS_lda_RegWM-1.96*PS_lda_RegSEWM/sqrt(166), x, PS_lda_RegWM+1.96*PS_lda_RegSEWM/sqrt(166), length=0.05, angle=90, code=3)
arrows(x+0.1, CD_lda_RegWM-1.96*CD_lda_RegSEWM/sqrt(166), x+0.1, CD_lda_RegWM+1.96*CD_lda_RegSEWM/sqrt(166), length=0.05, angle=90, code=3,col="darkred")
arrows(x-0.1, ICS_lda_RegWM-1.96*ICS_lda_RegSEWM/sqrt(166), x-0.1, ICS_lda_RegWM+1.96*ICS_lda_RegSEWM/sqrt(166), length=0.05, angle=90, code=3,col="darkblue")
legend(3, 0.3, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","darkred", "darkblue"), pch=c(19,1,2),lty=1, cex=0.8)
abline(h=lazy)

x<-c(1:7)
plot(x, PS_jp_lda_RegWM,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="LDA Just pass-fail", sub="Weighted mean of all centrality models",type="b"
)
lines(x+0.1,CD_jp_lda_RegWM,pch=1,type="b",col="darkred")
lines(x-0.1,ICS_jp_lda_RegWM,pch=2,type="b",col="darkblue")
# hack: we draw arrows but with very special "arrowheads"

arrows(x, PS_jp_lda_RegWM-1.96*PS_jp_lda_RegSEWM/sqrt(166), x, PS_jp_lda_RegWM+1.96*PS_jp_lda_RegSEWM/sqrt(166), length=0.05, angle=90, code=3)
arrows(x+0.1, CD_jp_lda_RegWM-1.96*CD_jp_lda_RegSEWM/sqrt(166), x+0.1, CD_jp_lda_RegWM+1.96*CD_jp_lda_RegSEWM/sqrt(166), length=0.05, angle=90, code=3,col="darkred")
arrows(x-0.1, ICS_jp_lda_RegWM-1.96*ICS_jp_lda_RegSEWM/sqrt(166), x-0.1, ICS_jp_lda_RegWM+1.96*ICS_jp_lda_RegSEWM/sqrt(166), length=0.05, angle=90, code=3,col="darkblue")
legend(3, 0.3, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","darkred", "darkblue"), pch=c(19,1,2),lty=1, cex=0.8)
abline(h=lazy_jp)
dev.off()
####ROC curves####
pdf(file = "plots/ROC_AUC_plots/ROC04_lda_pf.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 14) # The height of the plot in inches
par(mfrow=c(4,2))
plot(rocCD_lda_PTH[[1]], main="PTH")
lines(rocCD_lda_PTH[[2]],col="yellow")
lines(rocCD_lda_PTH[[3]],col="blue")
lines(rocCD_lda_PTH[[4]],col="magenta")
lines(rocCD_lda_PTH[[5]],col="red")
lines(rocCD_lda_PTH[[6]],col="green")
lines(rocCD_lda_PTH[[7]],col="purple")

plot(rocCD_lda_PT[[1]], main="PT")
lines(rocCD_lda_PT[[2]],col="yellow")
lines(rocCD_lda_PT[[3]],col="blue")
lines(rocCD_lda_PT[[4]],col="magenta")
lines(rocCD_lda_PT[[5]],col="red")
lines(rocCD_lda_PT[[6]],col="green")
lines(rocCD_lda_PT[[7]],col="purple")

plot(rocCD_lda_PH[[1]], main="PH")
lines(rocCD_lda_PH[[2]],col="yellow")
lines(rocCD_lda_PH[[3]],col="blue")
lines(rocCD_lda_PH[[4]],col="magenta")
lines(rocCD_lda_PH[[5]],col="red")
lines(rocCD_lda_PH[[6]],col="green")
lines(rocCD_lda_PH[[7]],col="purple")

plot(rocCD_lda_TH[[1]], main="TH")
lines(rocCD_lda_TH[[2]],col="yellow")
lines(rocCD_lda_TH[[3]],col="blue")
lines(rocCD_lda_TH[[4]],col="magenta")
lines(rocCD_lda_TH[[5]],col="red")
lines(rocCD_lda_TH[[6]],col="green")
lines(rocCD_lda_TH[[7]],col="purple")

plot(rocCD_lda_P[[1]], main="P")
lines(rocCD_lda_P[[2]],col="yellow")
lines(rocCD_lda_P[[3]],col="blue")
lines(rocCD_lda_P[[4]],col="magenta")
lines(rocCD_lda_P[[5]],col="red")
lines(rocCD_lda_P[[6]],col="green")
lines(rocCD_lda_P[[7]],col="purple")

plot(rocCD_lda_T[[1]], main="T")
lines(rocCD_lda_T[[2]],col="yellow")
lines(rocCD_lda_T[[3]],col="blue")
lines(rocCD_lda_T[[4]],col="magenta")
lines(rocCD_lda_T[[5]],col="red")
lines(rocCD_lda_T[[6]],col="green")
lines(rocCD_lda_T[[7]],col="purple")

plot(rocCD_lda_H[[1]], main="H")
lines(rocCD_lda_H[[2]],col="yellow")
lines(rocCD_lda_H[[3]],col="blue")
lines(rocCD_lda_H[[4]],col="magenta")
lines(rocCD_lda_H[[5]],col="red")
lines(rocCD_lda_H[[6]],col="green")
lines(rocCD_lda_H[[7]],col="purple")

plot.new()
legend(0,1,c("week 1","week 2","week 3","week 4"),
       col=c("black","yellow", "blue","magenta"),lty=1, cex=0.8)
legend(0.5,1,c("week 5","week 6","week 7"),
       col=c("red","green","purple"),lty=1, cex=0.8)

dev.off()
pdf(file = "plots/ROC_AUC_plots/ROC04_lda_jpf.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 14) # The height of the plot in inches
par(mfrow=c(4,2))
plot(rocCD_jp_lda_PTH[[1]])
lines(rocCD_jp_lda_PTH[[2]],col="yellow")
lines(rocCD_jp_lda_PTH[[3]],col="blue")
lines(rocCD_jp_lda_PTH[[4]],col="magenta")
lines(rocCD_jp_lda_PTH[[5]],col="red")
lines(rocCD_jp_lda_PTH[[6]],col="green")
lines(rocCD_jp_lda_PTH[[7]],col="purple")

plot(rocCD_jp_lda_PT[[1]])
lines(rocCD_jp_lda_PT[[2]],col="yellow")
lines(rocCD_jp_lda_PT[[3]],col="blue")
lines(rocCD_jp_lda_PT[[4]],col="magenta")
lines(rocCD_jp_lda_PT[[5]],col="red")
lines(rocCD_jp_lda_PT[[6]],col="green")
lines(rocCD_jp_lda_PT[[7]],col="purple")

plot(rocCD_jp_lda_PH[[1]])
lines(rocCD_jp_lda_PH[[2]],col="yellow")
lines(rocCD_jp_lda_PH[[3]],col="blue")
lines(rocCD_jp_lda_PH[[4]],col="magenta")
lines(rocCD_jp_lda_PH[[5]],col="red")
lines(rocCD_jp_lda_PH[[6]],col="green")
lines(rocCD_jp_lda_PH[[7]],col="purple")

plot(rocCD_jp_lda_TH[[1]])
lines(rocCD_jp_lda_TH[[2]],col="yellow")
lines(rocCD_jp_lda_TH[[3]],col="blue")
lines(rocCD_jp_lda_TH[[4]],col="magenta")
lines(rocCD_jp_lda_TH[[5]],col="red")
lines(rocCD_jp_lda_TH[[6]],col="green")
lines(rocCD_jp_lda_TH[[7]],col="purple")

plot(rocCD_jp_lda_P[[1]])
lines(rocCD_jp_lda_P[[2]],col="yellow")
lines(rocCD_jp_lda_P[[3]],col="blue")
lines(rocCD_jp_lda_P[[4]],col="magenta")
lines(rocCD_jp_lda_P[[5]],col="red")
lines(rocCD_jp_lda_P[[6]],col="green")
lines(rocCD_jp_lda_P[[7]],col="purple")

plot(rocCD_jp_lda_T[[1]])
lines(rocCD_jp_lda_T[[2]],col="yellow")
lines(rocCD_jp_lda_T[[3]],col="blue")
lines(rocCD_jp_lda_T[[4]],col="magenta")
lines(rocCD_jp_lda_T[[5]],col="red")
lines(rocCD_jp_lda_T[[6]],col="green")
lines(rocCD_jp_lda_T[[7]],col="purple")

plot(rocCD_jp_lda_H[[1]])
lines(rocCD_jp_lda_T[[2]],col="yellow")
lines(rocCD_jp_lda_T[[3]],col="blue")
lines(rocCD_jp_lda_T[[4]],col="magenta")
lines(rocCD_jp_lda_T[[5]],col="red")
lines(rocCD_jp_lda_T[[6]],col="green")
lines(rocCD_jp_lda_T[[7]],col="purple")

plot.new()
legend(0,1,c("week 1","week 2","week 3","week 4"),
       col=c("black","yellow", "blue","magenta"),lty=1, cex=0.8)
legend(0.5,1,c("week 5","week 6","week 7"),
       col=c("red","green","purple"),lty=1, cex=0.8)

dev.off()


###################################################
#########QUADRATIC DISCRIMINANT ANALYSIS###########
###################################################
######Run models and calculate AUC (with CI)####
######PR-TE-H Models######
predPS_qda_PTH<-jackPredQDA(centPS,predictors = c("cohort","PageRank","tarEnt", "Hide"))
rocPS_qda_PTH<-list()
rocPS_qda_PTH[[1]]<-roc(predPS_qda_PTH$pass,as.numeric(predPS_qda_PTH$Week1),auc=T,ci=T)
rocPS_qda_PTH[[2]]<-roc(predPS_qda_PTH$pass,as.numeric(predPS_qda_PTH$Week2),auc=T,ci=T)
rocPS_qda_PTH[[3]]<-roc(predPS_qda_PTH$pass,as.numeric(predPS_qda_PTH$Week3),auc=T,ci=T)
rocPS_qda_PTH[[4]]<-roc(predPS_qda_PTH$pass,as.numeric(predPS_qda_PTH$Week4),auc=T,ci=T)
rocPS_qda_PTH[[5]]<-roc(predPS_qda_PTH$pass,as.numeric(predPS_qda_PTH$Week5),auc=T,ci=T)
rocPS_qda_PTH[[6]]<-roc(predPS_qda_PTH$pass,as.numeric(predPS_qda_PTH$Week6),auc=T,ci=T)
rocPS_qda_PTH[[7]]<-roc(predPS_qda_PTH$pass,as.numeric(predPS_qda_PTH$Week7),auc=T,ci=T)
PS_qda_PTH_auc<-c(rocPS_qda_PTH[[1]]$auc,rocPS_qda_PTH[[2]]$auc,rocPS_qda_PTH[[3]]$auc,rocPS_qda_PTH[[4]]$auc,
                  rocPS_qda_PTH[[5]]$auc,rocPS_qda_PTH[[6]]$auc,rocPS_qda_PTH[[7]]$auc)
PS_qda_PTH_ciL<-c(rocPS_qda_PTH[[1]]$ci[1],rocPS_qda_PTH[[2]]$ci[1],rocPS_qda_PTH[[3]]$ci[1],rocPS_qda_PTH[[4]]$ci[1],
                  rocPS_qda_PTH[[5]]$ci[1],rocPS_qda_PTH[[6]]$ci[1],rocPS_qda_PTH[[7]]$ci[1])
PS_qda_PTH_ciH<-c(rocPS_qda_PTH[[1]]$ci[3],rocPS_qda_PTH[[2]]$ci[3],rocPS_qda_PTH[[3]]$ci[3],rocPS_qda_PTH[[4]]$ci[3],
                  rocPS_qda_PTH[[5]]$ci[3],rocPS_qda_PTH[[6]]$ci[3],rocPS_qda_PTH[[7]]$ci[3])

predCD_qda_PTH<-jackPredQDA(centCD,predictors = c("cohort","PageRank","tarEnt", "Hide"))
rocCD_qda_PTH<-list()
rocCD_qda_PTH[[1]]<-roc(predCD_qda_PTH$pass,as.numeric(predCD_qda_PTH$Week1),auc=T,ci=T)
rocCD_qda_PTH[[2]]<-roc(predCD_qda_PTH$pass,as.numeric(predCD_qda_PTH$Week2),auc=T,ci=T)
rocCD_qda_PTH[[3]]<-roc(predCD_qda_PTH$pass,as.numeric(predCD_qda_PTH$Week3),auc=T,ci=T)
rocCD_qda_PTH[[4]]<-roc(predCD_qda_PTH$pass,as.numeric(predCD_qda_PTH$Week4),auc=T,ci=T)
rocCD_qda_PTH[[5]]<-roc(predCD_qda_PTH$pass,as.numeric(predCD_qda_PTH$Week5),auc=T,ci=T)
rocCD_qda_PTH[[6]]<-roc(predCD_qda_PTH$pass,as.numeric(predCD_qda_PTH$Week6),auc=T,ci=T)
rocCD_qda_PTH[[7]]<-roc(predCD_qda_PTH$pass,as.numeric(predCD_qda_PTH$Week7),auc=T,ci=T)
CD_qda_PTH_auc<-c(rocCD_qda_PTH[[1]]$auc,rocCD_qda_PTH[[2]]$auc,rocCD_qda_PTH[[3]]$auc,rocCD_qda_PTH[[4]]$auc,
                  rocCD_qda_PTH[[5]]$auc,rocCD_qda_PTH[[6]]$auc,rocCD_qda_PTH[[7]]$auc)
CD_qda_PTH_ciL<-c(rocCD_qda_PTH[[1]]$ci[1],rocCD_qda_PTH[[2]]$ci[1],rocCD_qda_PTH[[3]]$ci[1],rocCD_qda_PTH[[4]]$ci[1],
                  rocCD_qda_PTH[[5]]$ci[1],rocCD_qda_PTH[[6]]$ci[1],rocCD_qda_PTH[[7]]$ci[1])
CD_qda_PTH_ciH<-c(rocCD_qda_PTH[[1]]$ci[3],rocCD_qda_PTH[[2]]$ci[3],rocCD_qda_PTH[[3]]$ci[3],rocCD_qda_PTH[[4]]$ci[3],
                  rocCD_qda_PTH[[5]]$ci[3],rocCD_qda_PTH[[6]]$ci[3],rocCD_qda_PTH[[7]]$ci[3])



predICS_qda_PTH<-jackPredQDA(centICS,predictors = c("cohort","PageRank","tarEnt", "Hide"))
rocICS_qda_PTH<-list()
rocICS_qda_PTH[[1]]<-roc(predICS_qda_PTH$pass,as.numeric(predICS_qda_PTH$Week1),auc=T,ci=T)
rocICS_qda_PTH[[2]]<-roc(predICS_qda_PTH$pass,as.numeric(predICS_qda_PTH$Week2),auc=T,ci=T)
rocICS_qda_PTH[[3]]<-roc(predICS_qda_PTH$pass,as.numeric(predICS_qda_PTH$Week3),auc=T,ci=T)
rocICS_qda_PTH[[4]]<-roc(predICS_qda_PTH$pass,as.numeric(predICS_qda_PTH$Week4),auc=T,ci=T)
rocICS_qda_PTH[[5]]<-roc(predICS_qda_PTH$pass,as.numeric(predICS_qda_PTH$Week5),auc=T,ci=T)
rocICS_qda_PTH[[6]]<-roc(predICS_qda_PTH$pass,as.numeric(predICS_qda_PTH$Week6),auc=T,ci=T)
rocICS_qda_PTH[[7]]<-roc(predICS_qda_PTH$pass,as.numeric(predICS_qda_PTH$Week7),auc=T,ci=T)
ICS_qda_PTH_auc<-c(rocICS_qda_PTH[[1]]$auc,rocICS_qda_PTH[[2]]$auc,rocICS_qda_PTH[[3]]$auc,rocICS_qda_PTH[[4]]$auc,
                   rocICS_qda_PTH[[5]]$auc,rocICS_qda_PTH[[6]]$auc,rocICS_qda_PTH[[7]]$auc)
ICS_qda_PTH_ciL<-c(rocICS_qda_PTH[[1]]$ci[1],rocICS_qda_PTH[[2]]$ci[1],rocICS_qda_PTH[[3]]$ci[1],rocICS_qda_PTH[[4]]$ci[1],
                   rocICS_qda_PTH[[5]]$ci[1],rocICS_qda_PTH[[6]]$ci[1],rocICS_qda_PTH[[7]]$ci[1])
ICS_qda_PTH_ciH<-c(rocICS_qda_PTH[[1]]$ci[3],rocICS_qda_PTH[[2]]$ci[3],rocICS_qda_PTH[[3]]$ci[3],rocICS_qda_PTH[[4]]$ci[3],
                   rocICS_qda_PTH[[5]]$ci[3],rocICS_qda_PTH[[6]]$ci[3],rocICS_qda_PTH[[7]]$ci[3])




###JUST PASS
predPS_jp_qda_PTH<-jackPredQDA(centPS,outcome = "justpass",predictors = c("cohort","PageRank","tarEnt", "Hide"))
rocPS_jp_qda_PTH<-list()
rocPS_jp_qda_PTH[[1]]<-roc(predPS_jp_qda_PTH$justpass,as.numeric(predPS_jp_qda_PTH$Week1),auc=T,ci=T)
rocPS_jp_qda_PTH[[2]]<-roc(predPS_jp_qda_PTH$justpass,as.numeric(predPS_jp_qda_PTH$Week2),auc=T,ci=T)
rocPS_jp_qda_PTH[[3]]<-roc(predPS_jp_qda_PTH$justpass,as.numeric(predPS_jp_qda_PTH$Week3),auc=T,ci=T)
rocPS_jp_qda_PTH[[4]]<-roc(predPS_jp_qda_PTH$justpass,as.numeric(predPS_jp_qda_PTH$Week4),auc=T,ci=T)
rocPS_jp_qda_PTH[[5]]<-roc(predPS_jp_qda_PTH$justpass,as.numeric(predPS_jp_qda_PTH$Week5),auc=T,ci=T)
rocPS_jp_qda_PTH[[6]]<-roc(predPS_jp_qda_PTH$justpass,as.numeric(predPS_jp_qda_PTH$Week6),auc=T,ci=T)
rocPS_jp_qda_PTH[[7]]<-roc(predPS_jp_qda_PTH$justpass,as.numeric(predPS_jp_qda_PTH$Week7),auc=T,ci=T)

PS_jp_qda_PTH_auc<-c(rocPS_jp_qda_PTH[[1]]$auc,rocPS_jp_qda_PTH[[2]]$auc,rocPS_jp_qda_PTH[[3]]$auc,rocPS_jp_qda_PTH[[4]]$auc,
                     rocPS_jp_qda_PTH[[5]]$auc,rocPS_jp_qda_PTH[[6]]$auc,rocPS_jp_qda_PTH[[7]]$auc)
PS_jp_qda_PTH_ciL<-c(rocPS_jp_qda_PTH[[1]]$ci[1],rocPS_jp_qda_PTH[[2]]$ci[1],rocPS_jp_qda_PTH[[3]]$ci[1],rocPS_jp_qda_PTH[[4]]$ci[1],
                     rocPS_jp_qda_PTH[[5]]$ci[1],rocPS_jp_qda_PTH[[6]]$ci[1],rocPS_jp_qda_PTH[[7]]$ci[1])
PS_jp_qda_PTH_ciH<-c(rocPS_jp_qda_PTH[[1]]$ci[3],rocPS_jp_qda_PTH[[2]]$ci[3],rocPS_jp_qda_PTH[[3]]$ci[3],rocPS_jp_qda_PTH[[4]]$ci[3],
                     rocPS_jp_qda_PTH[[5]]$ci[3],rocPS_jp_qda_PTH[[6]]$ci[3],rocPS_jp_qda_PTH[[7]]$ci[3])

####RANK DEF. PROBLEM####
#predCD_jp_qda_PTH<-jackPredQDA(centCD, outcome = "justpass",predictors = c("cohort","PageRank","tarEnt", "Hide"))
#rocCD_jp_qda_PTH<-list()
#rocCD_jp_qda_PTH[[1]]<-roc(predCD_jp_qda_PTH$justpass,as.numeric(predCD_jp_qda_PTH$Week1),auc=T,ci=T)
#rocCD_jp_qda_PTH[[2]]<-roc(predCD_jp_qda_PTH$justpass,as.numeric(predCD_jp_qda_PTH$Week2),auc=T,ci=T)
#rocCD_jp_qda_PTH[[3]]<-roc(predCD_jp_qda_PTH$justpass,as.numeric(predCD_jp_qda_PTH$Week3),auc=T,ci=T)
#rocCD_jp_qda_PTH[[4]]<-roc(predCD_jp_qda_PTH$justpass,as.numeric(predCD_jp_qda_PTH$Week4),auc=T,ci=T)
#rocCD_jp_qda_PTH[[5]]<-roc(predCD_jp_qda_PTH$justpass,as.numeric(predCD_jp_qda_PTH$Week5),auc=T,ci=T)
#rocCD_jp_qda_PTH[[6]]<-roc(predCD_jp_qda_PTH$justpass,as.numeric(predCD_jp_qda_PTH$Week6),auc=T,ci=T)
#rocCD_jp_qda_PTH[[7]]<-roc(predCD_jp_qda_PTH$justpass,as.numeric(predCD_jp_qda_PTH$Week7),auc=T,ci=T)
#CD_jp_qda_PTH_auc<-c(rocCD_jp_qda_PTH[[1]]$auc,rocCD_jp_qda_PTH[[2]]$auc,rocCD_jp_qda_PTH[[3]]$auc,rocCD_jp_qda_PTH[[4]]$auc,
#                    rocCD_jp_qda_PTH[[5]]$auc,rocCD_jp_qda_PTH[[6]]$auc,rocCD_jp_qda_PTH[[7]]$auc)
#CD_jp_qda_PTH_ciL<-c(rocCD_jp_qda_PTH[[1]]$ci[1],rocCD_jp_qda_PTH[[2]]$ci[1],rocCD_jp_qda_PTH[[3]]$ci[1],rocCD_jp_qda_PTH[[4]]$ci[1],
#                     rocCD_jp_qda_PTH[[5]]$ci[1],rocCD_jp_qda_PTH[[6]]$ci[1],rocCD_jp_qda_PTH[[7]]$ci[1])
#CD_jp_qda_PTH_ciH<-c(rocCD_jp_qda_PTH[[1]]$ci[3],rocCD_jp_qda_PTH[[2]]$ci[3],rocCD_jp_qda_PTH[[3]]$ci[3],rocCD_jp_qda_PTH[[4]]$ci[3],
#                    rocCD_jp_qda_PTH[[5]]$ci[3],rocCD_jp_qda_PTH[[6]]$ci[3],rocCD_jp_qda_PTH[[7]]$ci[3])


predICS_jp_qda_PTH<-jackPredQDA(centICS, outcome = "justpass",predictors = c("cohort","PageRank","tarEnt", "Hide"))
rocICS_jp_qda_PTH<-list()
rocICS_jp_qda_PTH[[1]]<-roc(predICS_jp_qda_PTH$justpass,as.numeric(predICS_jp_qda_PTH$Week1),auc=T,ci=T)
rocICS_jp_qda_PTH[[2]]<-roc(predICS_jp_qda_PTH$justpass,as.numeric(predICS_jp_qda_PTH$Week2),auc=T,ci=T)
rocICS_jp_qda_PTH[[3]]<-roc(predICS_jp_qda_PTH$justpass,as.numeric(predICS_jp_qda_PTH$Week3),auc=T,ci=T)
rocICS_jp_qda_PTH[[4]]<-roc(predICS_jp_qda_PTH$justpass,as.numeric(predICS_jp_qda_PTH$Week4),auc=T,ci=T)
rocICS_jp_qda_PTH[[5]]<-roc(predICS_jp_qda_PTH$justpass,as.numeric(predICS_jp_qda_PTH$Week5),auc=T,ci=T)
rocICS_jp_qda_PTH[[6]]<-roc(predICS_jp_qda_PTH$justpass,as.numeric(predICS_jp_qda_PTH$Week6),auc=T,ci=T)
rocICS_jp_qda_PTH[[7]]<-roc(predICS_jp_qda_PTH$justpass,as.numeric(predICS_jp_qda_PTH$Week7),auc=T,ci=T)
ICS_jp_qda_PTH_auc<-c(rocICS_jp_qda_PTH[[1]]$auc,rocICS_jp_qda_PTH[[2]]$auc,rocICS_jp_qda_PTH[[3]]$auc,rocICS_jp_qda_PTH[[4]]$auc,
                      rocICS_jp_qda_PTH[[5]]$auc,rocICS_jp_qda_PTH[[6]]$auc,rocICS_jp_qda_PTH[[7]]$auc)
ICS_jp_qda_PTH_ciL<-c(rocICS_jp_qda_PTH[[1]]$ci[1],rocICS_jp_qda_PTH[[2]]$ci[1],rocICS_jp_qda_PTH[[3]]$ci[1],rocICS_jp_qda_PTH[[4]]$ci[1],
                      rocICS_jp_qda_PTH[[5]]$ci[1],rocICS_jp_qda_PTH[[6]]$ci[1],rocICS_jp_qda_PTH[[7]]$ci[1])
ICS_jp_qda_PTH_ciH<-c(rocICS_jp_qda_PTH[[1]]$ci[3],rocICS_jp_qda_PTH[[2]]$ci[3],rocICS_jp_qda_PTH[[3]]$ci[3],rocICS_jp_qda_PTH[[4]]$ci[3],
                      rocICS_jp_qda_PTH[[5]]$ci[3],rocICS_jp_qda_PTH[[6]]$ci[3],rocICS_jp_qda_PTH[[7]]$ci[3])


######PR TE models######
predPS_qda_PT<-jackPredQDA(centPS,predictors = c("cohort","PageRank","tarEnt"))
rocPS_qda_PT<-list()
rocPS_qda_PT[[1]]<-roc(predPS_qda_PT$pass,as.numeric(predPS_qda_PT$Week1),auc=T,ci=T)
rocPS_qda_PT[[2]]<-roc(predPS_qda_PT$pass,as.numeric(predPS_qda_PT$Week2),auc=T,ci=T)
rocPS_qda_PT[[3]]<-roc(predPS_qda_PT$pass,as.numeric(predPS_qda_PT$Week3),auc=T,ci=T)
rocPS_qda_PT[[4]]<-roc(predPS_qda_PT$pass,as.numeric(predPS_qda_PT$Week4),auc=T,ci=T)
rocPS_qda_PT[[5]]<-roc(predPS_qda_PT$pass,as.numeric(predPS_qda_PT$Week5),auc=T,ci=T)
rocPS_qda_PT[[6]]<-roc(predPS_qda_PT$pass,as.numeric(predPS_qda_PT$Week6),auc=T,ci=T)
rocPS_qda_PT[[7]]<-roc(predPS_qda_PT$pass,as.numeric(predPS_qda_PT$Week7),auc=T,ci=T)
PS_qda_PT_auc<-c(rocPS_qda_PT[[1]]$auc,rocPS_qda_PT[[2]]$auc,rocPS_qda_PT[[3]]$auc,rocPS_qda_PT[[4]]$auc,
                 rocPS_qda_PT[[5]]$auc,rocPS_qda_PT[[6]]$auc,rocPS_qda_PT[[7]]$auc)
PS_qda_PT_ciL<-c(rocPS_qda_PT[[1]]$ci[1],rocPS_qda_PT[[2]]$ci[1],rocPS_qda_PT[[3]]$ci[1],rocPS_qda_PT[[4]]$ci[1],
                 rocPS_qda_PT[[5]]$ci[1],rocPS_qda_PT[[6]]$ci[1],rocPS_qda_PT[[7]]$ci[1])
PS_qda_PT_ciH<-c(rocPS_qda_PT[[1]]$ci[3],rocPS_qda_PT[[2]]$ci[3],rocPS_qda_PT[[3]]$ci[3],rocPS_qda_PT[[4]]$ci[3],
                 rocPS_qda_PT[[5]]$ci[3],rocPS_qda_PT[[6]]$ci[3],rocPS_qda_PT[[7]]$ci[3])
##CD layer
predCD_qda_PT<-jackPredQDA(centCD,predictors = c("cohort","PageRank","tarEnt"))
rocCD_qda_PT<-list()
rocCD_qda_PT[[1]]<-roc(predCD_qda_PT$pass,as.numeric(predCD_qda_PT$Week1),auc=T,ci=T)
rocCD_qda_PT[[2]]<-roc(predCD_qda_PT$pass,as.numeric(predCD_qda_PT$Week2),auc=T,ci=T)
rocCD_qda_PT[[3]]<-roc(predCD_qda_PT$pass,as.numeric(predCD_qda_PT$Week3),auc=T,ci=T)
rocCD_qda_PT[[4]]<-roc(predCD_qda_PT$pass,as.numeric(predCD_qda_PT$Week4),auc=T,ci=T)
rocCD_qda_PT[[5]]<-roc(predCD_qda_PT$pass,as.numeric(predCD_qda_PT$Week5),auc=T,ci=T)
rocCD_qda_PT[[6]]<-roc(predCD_qda_PT$pass,as.numeric(predCD_qda_PT$Week6),auc=T,ci=T)
rocCD_qda_PT[[7]]<-roc(predCD_qda_PT$pass,as.numeric(predCD_qda_PT$Week7),auc=T,ci=T)
CD_qda_PT_auc<-c(rocCD_qda_PT[[1]]$auc,rocCD_qda_PT[[2]]$auc,rocCD_qda_PT[[3]]$auc,rocCD_qda_PT[[4]]$auc,
                 rocCD_qda_PT[[5]]$auc,rocCD_qda_PT[[6]]$auc,rocCD_qda_PT[[7]]$auc)
CD_qda_PT_ciL<-c(rocCD_qda_PT[[1]]$ci[1],rocCD_qda_PT[[2]]$ci[1],rocCD_qda_PT[[3]]$ci[1],rocCD_qda_PT[[4]]$ci[1],
                 rocCD_qda_PT[[5]]$ci[1],rocCD_qda_PT[[6]]$ci[1],rocCD_qda_PT[[7]]$ci[1])
CD_qda_PT_ciH<-c(rocCD_qda_PT[[1]]$ci[3],rocCD_qda_PT[[2]]$ci[3],rocCD_qda_PT[[3]]$ci[3],rocCD_qda_PT[[4]]$ci[3],
                 rocCD_qda_PT[[5]]$ci[3],rocCD_qda_PT[[6]]$ci[3],rocCD_qda_PT[[7]]$ci[3])

#ICS layer
predICS_qda_PT<-jackPredQDA(centICS,predictors = c("cohort","PageRank","tarEnt"))
rocICS_qda_PT<-list()
rocICS_qda_PT[[1]]<-roc(predICS_qda_PT$pass,as.numeric(predICS_qda_PT$Week1),auc=T,ci=T)
rocICS_qda_PT[[2]]<-roc(predICS_qda_PT$pass,as.numeric(predICS_qda_PT$Week2),auc=T,ci=T)
rocICS_qda_PT[[3]]<-roc(predICS_qda_PT$pass,as.numeric(predICS_qda_PT$Week3),auc=T,ci=T)
rocICS_qda_PT[[4]]<-roc(predICS_qda_PT$pass,as.numeric(predICS_qda_PT$Week4),auc=T,ci=T)
rocICS_qda_PT[[5]]<-roc(predICS_qda_PT$pass,as.numeric(predICS_qda_PT$Week5),auc=T,ci=T)
rocICS_qda_PT[[6]]<-roc(predICS_qda_PT$pass,as.numeric(predICS_qda_PT$Week6),auc=T,ci=T)
rocICS_qda_PT[[7]]<-roc(predICS_qda_PT$pass,as.numeric(predICS_qda_PT$Week7),auc=T,ci=T)
ICS_qda_PT_auc<-c(rocICS_qda_PT[[1]]$auc,rocICS_qda_PT[[2]]$auc,rocICS_qda_PT[[3]]$auc,rocICS_qda_PT[[4]]$auc,
                  rocICS_qda_PT[[5]]$auc,rocICS_qda_PT[[6]]$auc,rocICS_qda_PT[[7]]$auc)
ICS_qda_PT_ciL<-c(rocICS_qda_PT[[1]]$ci[1],rocICS_qda_PT[[2]]$ci[1],rocICS_qda_PT[[3]]$ci[1],rocICS_qda_PT[[4]]$ci[1],
                  rocICS_qda_PT[[5]]$ci[1],rocICS_qda_PT[[6]]$ci[1],rocICS_qda_PT[[7]]$ci[1])
ICS_qda_PT_ciH<-c(rocICS_qda_PT[[1]]$ci[3],rocICS_qda_PT[[2]]$ci[3],rocICS_qda_PT[[3]]$ci[3],rocICS_qda_PT[[4]]$ci[3],
                  rocICS_qda_PT[[5]]$ci[3],rocICS_qda_PT[[6]]$ci[3],rocICS_qda_PT[[7]]$ci[3])



###JUST PASS
predPS_jp_qda_PT<-jackPredQDA(centPS,outcome = "justpass",predictors = c("cohort","PageRank","tarEnt"))
rocPS_jp_qda_PT<-list()
rocPS_jp_qda_PT[[1]]<-roc(predPS_jp_qda_PT$justpass,as.numeric(predPS_jp_qda_PT$Week1),auc=T,ci=T)
rocPS_jp_qda_PT[[2]]<-roc(predPS_jp_qda_PT$justpass,as.numeric(predPS_jp_qda_PT$Week2),auc=T,ci=T)
rocPS_jp_qda_PT[[3]]<-roc(predPS_jp_qda_PT$justpass,as.numeric(predPS_jp_qda_PT$Week3),auc=T,ci=T)
rocPS_jp_qda_PT[[4]]<-roc(predPS_jp_qda_PT$justpass,as.numeric(predPS_jp_qda_PT$Week4),auc=T,ci=T)
rocPS_jp_qda_PT[[5]]<-roc(predPS_jp_qda_PT$justpass,as.numeric(predPS_jp_qda_PT$Week5),auc=T,ci=T)
rocPS_jp_qda_PT[[6]]<-roc(predPS_jp_qda_PT$justpass,as.numeric(predPS_jp_qda_PT$Week6),auc=T,ci=T)
rocPS_jp_qda_PT[[7]]<-roc(predPS_jp_qda_PT$justpass,as.numeric(predPS_jp_qda_PT$Week7),auc=T,ci=T)

PS_jp_qda_PT_auc<-c(rocPS_jp_qda_PT[[1]]$auc,rocPS_jp_qda_PT[[2]]$auc,rocPS_jp_qda_PT[[3]]$auc,rocPS_jp_qda_PT[[4]]$auc,
                    rocPS_jp_qda_PT[[5]]$auc,rocPS_jp_qda_PT[[6]]$auc,rocPS_jp_qda_PT[[7]]$auc)
PS_jp_qda_PT_ciL<-c(rocPS_jp_qda_PT[[1]]$ci[1],rocPS_jp_qda_PT[[2]]$ci[1],rocPS_jp_qda_PT[[3]]$ci[1],rocPS_jp_qda_PT[[4]]$ci[1],
                    rocPS_jp_qda_PT[[5]]$ci[1],rocPS_jp_qda_PT[[6]]$ci[1],rocPS_jp_qda_PT[[7]]$ci[1])
PS_jp_qda_PT_ciH<-c(rocPS_jp_qda_PT[[1]]$ci[3],rocPS_jp_qda_PT[[2]]$ci[3],rocPS_jp_qda_PT[[3]]$ci[3],rocPS_jp_qda_PT[[4]]$ci[3],
                    rocPS_jp_qda_PT[[5]]$ci[3],rocPS_jp_qda_PT[[6]]$ci[3],rocPS_jp_qda_PT[[7]]$ci[3])

####Rank def. problem####
#predCD_jp_qda_PT<-jackPredQDA(centCD, outcome = "justpass",predictors = c("cohort","PageRank","tarEnt"))
#rocCD_jp_qda_PT<-list()
#rocCD_jp_qda_PT[[1]]<-roc(predCD_jp_qda_PT$justpass,as.numeric(predCD_jp_qda_PT$Week1),auc=T,ci=T)
#rocCD_jp_qda_PT[[2]]<-roc(predCD_jp_qda_PT$justpass,as.numeric(predCD_jp_qda_PT$Week2),auc=T,ci=T)
#rocCD_jp_qda_PT[[3]]<-roc(predCD_jp_qda_PT$justpass,as.numeric(predCD_jp_qda_PT$Week3),auc=T,ci=T)
#rocCD_jp_qda_PT[[4]]<-roc(predCD_jp_qda_PT$justpass,as.numeric(predCD_jp_qda_PT$Week4),auc=T,ci=T)
#rocCD_jp_qda_PT[[5]]<-roc(predCD_jp_qda_PT$justpass,as.numeric(predCD_jp_qda_PT$Week5),auc=T,ci=T)
#rocCD_jp_qda_PT[[6]]<-roc(predCD_jp_qda_PT$justpass,as.numeric(predCD_jp_qda_PT$Week6),auc=T,ci=T)
#rocCD_jp_qda_PT[[7]]<-roc(predCD_jp_qda_PT$justpass,as.numeric(predCD_jp_qda_PT$Week7),auc=T,ci=T)
#CD_jp_qda_PT_auc<-c(rocCD_jp_qda_PT[[1]]$auc,rocCD_jp_qda_PT[[2]]$auc,rocCD_jp_qda_PT[[3]]$auc,rocCD_jp_qda_PT[[4]]$auc,
#                   rocCD_jp_qda_PT[[5]]$auc,rocCD_jp_qda_PT[[6]]$auc,rocCD_jp_qda_PT[[7]]$auc)
#CD_jp_qda_PT_ciL<-c(rocCD_jp_qda_PT[[1]]$ci[1],rocCD_jp_qda_PT[[2]]$ci[1],rocCD_jp_qda_PT[[3]]$ci[1],rocCD_jp_qda_PT[[4]]$ci[1],
#                    rocCD_jp_qda_PT[[5]]$ci[1],rocCD_jp_qda_PT[[6]]$ci[1],rocCD_jp_qda_PT[[7]]$ci[1])
#CD_jp_qda_PT_ciH<-c(rocCD_jp_qda_PT[[1]]$ci[3],rocCD_jp_qda_PT[[2]]$ci[3],rocCD_jp_qda_PT[[3]]$ci[3],rocCD_jp_qda_PT[[4]]$ci[3],
#                   rocCD_jp_qda_PT[[5]]$ci[3],rocCD_jp_qda_PT[[6]]$ci[3],rocCD_jp_qda_PT[[7]]$ci[3])


predICS_jp_qda_PT<-jackPredQDA(centICS, outcome = "justpass",predictors = c("cohort","PageRank","tarEnt"))
rocICS_jp_qda_PT<-list()
rocICS_jp_qda_PT[[1]]<-roc(predICS_jp_qda_PT$justpass,as.numeric(predICS_jp_qda_PT$Week1),auc=T,ci=T)
rocICS_jp_qda_PT[[2]]<-roc(predICS_jp_qda_PT$justpass,as.numeric(predICS_jp_qda_PT$Week2),auc=T,ci=T)
rocICS_jp_qda_PT[[3]]<-roc(predICS_jp_qda_PT$justpass,as.numeric(predICS_jp_qda_PT$Week3),auc=T,ci=T)
rocICS_jp_qda_PT[[4]]<-roc(predICS_jp_qda_PT$justpass,as.numeric(predICS_jp_qda_PT$Week4),auc=T,ci=T)
rocICS_jp_qda_PT[[5]]<-roc(predICS_jp_qda_PT$justpass,as.numeric(predICS_jp_qda_PT$Week5),auc=T,ci=T)
rocICS_jp_qda_PT[[6]]<-roc(predICS_jp_qda_PT$justpass,as.numeric(predICS_jp_qda_PT$Week6),auc=T,ci=T)
rocICS_jp_qda_PT[[7]]<-roc(predICS_jp_qda_PT$justpass,as.numeric(predICS_jp_qda_PT$Week7),auc=T,ci=T)
ICS_jp_qda_PT_auc<-c(rocICS_jp_qda_PT[[1]]$auc,rocICS_jp_qda_PT[[2]]$auc,rocICS_jp_qda_PT[[3]]$auc,rocICS_jp_qda_PT[[4]]$auc,
                     rocICS_jp_qda_PT[[5]]$auc,rocICS_jp_qda_PT[[6]]$auc,rocICS_jp_qda_PT[[7]]$auc)
ICS_jp_qda_PT_ciL<-c(rocICS_jp_qda_PT[[1]]$ci[1],rocICS_jp_qda_PT[[2]]$ci[1],rocICS_jp_qda_PT[[3]]$ci[1],rocICS_jp_qda_PT[[4]]$ci[1],
                     rocICS_jp_qda_PT[[5]]$ci[1],rocICS_jp_qda_PT[[6]]$ci[1],rocICS_jp_qda_PT[[7]]$ci[1])
ICS_jp_qda_PT_ciH<-c(rocICS_jp_qda_PT[[1]]$ci[3],rocICS_jp_qda_PT[[2]]$ci[3],rocICS_jp_qda_PT[[3]]$ci[3],rocICS_jp_qda_PT[[4]]$ci[3],
                     rocICS_jp_qda_PT[[5]]$ci[3],rocICS_jp_qda_PT[[6]]$ci[3],rocICS_jp_qda_PT[[7]]$ci[3])




######PR H Models######
predPS_qda_PH<-jackPredQDA(centPS,predictors = c("cohort","PageRank", "Hide"))
rocPS_qda_PH<-list()
rocPS_qda_PH[[1]]<-roc(predPS_qda_PH$pass,as.numeric(predPS_qda_PH$Week1),auc=T,ci=T)
rocPS_qda_PH[[2]]<-roc(predPS_qda_PH$pass,as.numeric(predPS_qda_PH$Week2),auc=T,ci=T)
rocPS_qda_PH[[3]]<-roc(predPS_qda_PH$pass,as.numeric(predPS_qda_PH$Week3),auc=T,ci=T)
rocPS_qda_PH[[4]]<-roc(predPS_qda_PH$pass,as.numeric(predPS_qda_PH$Week4),auc=T,ci=T)
rocPS_qda_PH[[5]]<-roc(predPS_qda_PH$pass,as.numeric(predPS_qda_PH$Week5),auc=T,ci=T)
rocPS_qda_PH[[6]]<-roc(predPS_qda_PH$pass,as.numeric(predPS_qda_PH$Week6),auc=T,ci=T)
rocPS_qda_PH[[7]]<-roc(predPS_qda_PH$pass,as.numeric(predPS_qda_PH$Week7),auc=T,ci=T)
PS_qda_PH_auc<-c(rocPS_qda_PH[[1]]$auc,rocPS_qda_PH[[2]]$auc,rocPS_qda_PH[[3]]$auc,rocPS_qda_PH[[4]]$auc,
                 rocPS_qda_PH[[5]]$auc,rocPS_qda_PH[[6]]$auc,rocPS_qda_PH[[7]]$auc)
PS_qda_PH_ciL<-c(rocPS_qda_PH[[1]]$ci[1],rocPS_qda_PH[[2]]$ci[1],rocPS_qda_PH[[3]]$ci[1],rocPS_qda_PH[[4]]$ci[1],
                 rocPS_qda_PH[[5]]$ci[1],rocPS_qda_PH[[6]]$ci[1],rocPS_qda_PH[[7]]$ci[1])
PS_qda_PH_ciH<-c(rocPS_qda_PH[[1]]$ci[3],rocPS_qda_PH[[2]]$ci[3],rocPS_qda_PH[[3]]$ci[3],rocPS_qda_PH[[4]]$ci[3],
                 rocPS_qda_PH[[5]]$ci[3],rocPS_qda_PH[[6]]$ci[3],rocPS_qda_PH[[7]]$ci[3])
##CD layer
predCD_qda_PH<-jackPredQDA(centCD,predictors = c("cohort","PageRank", "Hide"))
rocCD_qda_PH<-list()
rocCD_qda_PH[[1]]<-roc(predCD_qda_PH$pass,as.numeric(predCD_qda_PH$Week1),auc=T,ci=T)
rocCD_qda_PH[[2]]<-roc(predCD_qda_PH$pass,as.numeric(predCD_qda_PH$Week2),auc=T,ci=T)
rocCD_qda_PH[[3]]<-roc(predCD_qda_PH$pass,as.numeric(predCD_qda_PH$Week3),auc=T,ci=T)
rocCD_qda_PH[[4]]<-roc(predCD_qda_PH$pass,as.numeric(predCD_qda_PH$Week4),auc=T,ci=T)
rocCD_qda_PH[[5]]<-roc(predCD_qda_PH$pass,as.numeric(predCD_qda_PH$Week5),auc=T,ci=T)
rocCD_qda_PH[[6]]<-roc(predCD_qda_PH$pass,as.numeric(predCD_qda_PH$Week6),auc=T,ci=T)
rocCD_qda_PH[[7]]<-roc(predCD_qda_PH$pass,as.numeric(predCD_qda_PH$Week7),auc=T,ci=T)
CD_qda_PH_auc<-c(rocCD_qda_PH[[1]]$auc,rocCD_qda_PH[[2]]$auc,rocCD_qda_PH[[3]]$auc,rocCD_qda_PH[[4]]$auc,
                 rocCD_qda_PH[[5]]$auc,rocCD_qda_PH[[6]]$auc,rocCD_qda_PH[[7]]$auc)
CD_qda_PH_ciL<-c(rocCD_qda_PH[[1]]$ci[1],rocCD_qda_PH[[2]]$ci[1],rocCD_qda_PH[[3]]$ci[1],rocCD_qda_PH[[4]]$ci[1],
                 rocCD_qda_PH[[5]]$ci[1],rocCD_qda_PH[[6]]$ci[1],rocCD_qda_PH[[7]]$ci[1])
CD_qda_PH_ciH<-c(rocCD_qda_PH[[1]]$ci[3],rocCD_qda_PH[[2]]$ci[3],rocCD_qda_PH[[3]]$ci[3],rocCD_qda_PH[[4]]$ci[3],
                 rocCD_qda_PH[[5]]$ci[3],rocCD_qda_PH[[6]]$ci[3],rocCD_qda_PH[[7]]$ci[3])

#ICS layer
predICS_qda_PH<-jackPredQDA(centICS,predictors = c("cohort","PageRank", "Hide"))
rocICS_qda_PH<-list()
rocICS_qda_PH[[1]]<-roc(predICS_qda_PH$pass,as.numeric(predICS_qda_PH$Week1),auc=T,ci=T)
rocICS_qda_PH[[2]]<-roc(predICS_qda_PH$pass,as.numeric(predICS_qda_PH$Week2),auc=T,ci=T)
rocICS_qda_PH[[3]]<-roc(predICS_qda_PH$pass,as.numeric(predICS_qda_PH$Week3),auc=T,ci=T)
rocICS_qda_PH[[4]]<-roc(predICS_qda_PH$pass,as.numeric(predICS_qda_PH$Week4),auc=T,ci=T)
rocICS_qda_PH[[5]]<-roc(predICS_qda_PH$pass,as.numeric(predICS_qda_PH$Week5),auc=T,ci=T)
rocICS_qda_PH[[6]]<-roc(predICS_qda_PH$pass,as.numeric(predICS_qda_PH$Week6),auc=T,ci=T)
rocICS_qda_PH[[7]]<-roc(predICS_qda_PH$pass,as.numeric(predICS_qda_PH$Week7),auc=T,ci=T)
ICS_qda_PH_auc<-c(rocICS_qda_PH[[1]]$auc,rocICS_qda_PH[[2]]$auc,rocICS_qda_PH[[3]]$auc,rocICS_qda_PH[[4]]$auc,
                  rocICS_qda_PH[[5]]$auc,rocICS_qda_PH[[6]]$auc,rocICS_qda_PH[[7]]$auc)
ICS_qda_PH_ciL<-c(rocICS_qda_PH[[1]]$ci[1],rocICS_qda_PH[[2]]$ci[1],rocICS_qda_PH[[3]]$ci[1],rocICS_qda_PH[[4]]$ci[1],
                  rocICS_qda_PH[[5]]$ci[1],rocICS_qda_PH[[6]]$ci[1],rocICS_qda_PH[[7]]$ci[1])
ICS_qda_PH_ciH<-c(rocICS_qda_PH[[1]]$ci[3],rocICS_qda_PH[[2]]$ci[3],rocICS_qda_PH[[3]]$ci[3],rocICS_qda_PH[[4]]$ci[3],
                  rocICS_qda_PH[[5]]$ci[3],rocICS_qda_PH[[6]]$ci[3],rocICS_qda_PH[[7]]$ci[3])



###JUST PASS
##PS layer
predPS_jp_qda_PH<-jackPredQDA(centPS,outcome = "justpass",predictors = c("cohort","PageRank", "Hide"))
rocPS_jp_qda_PH<-list()
rocPS_jp_qda_PH[[1]]<-roc(predPS_jp_qda_PH$justpass,as.numeric(predPS_jp_qda_PH$Week1),auc=T,ci=T)
rocPS_jp_qda_PH[[2]]<-roc(predPS_jp_qda_PH$justpass,as.numeric(predPS_jp_qda_PH$Week2),auc=T,ci=T)
rocPS_jp_qda_PH[[3]]<-roc(predPS_jp_qda_PH$justpass,as.numeric(predPS_jp_qda_PH$Week3),auc=T,ci=T)
rocPS_jp_qda_PH[[4]]<-roc(predPS_jp_qda_PH$justpass,as.numeric(predPS_jp_qda_PH$Week4),auc=T,ci=T)
rocPS_jp_qda_PH[[5]]<-roc(predPS_jp_qda_PH$justpass,as.numeric(predPS_jp_qda_PH$Week5),auc=T,ci=T)
rocPS_jp_qda_PH[[6]]<-roc(predPS_jp_qda_PH$justpass,as.numeric(predPS_jp_qda_PH$Week6),auc=T,ci=T)
rocPS_jp_qda_PH[[7]]<-roc(predPS_jp_qda_PH$justpass,as.numeric(predPS_jp_qda_PH$Week7),auc=T,ci=T)

PS_jp_qda_PH_auc<-c(rocPS_jp_qda_PH[[1]]$auc,rocPS_jp_qda_PH[[2]]$auc,rocPS_jp_qda_PH[[3]]$auc,rocPS_jp_qda_PH[[4]]$auc,
                    rocPS_jp_qda_PH[[5]]$auc,rocPS_jp_qda_PH[[6]]$auc,rocPS_jp_qda_PH[[7]]$auc)
PS_jp_qda_PH_ciL<-c(rocPS_jp_qda_PH[[1]]$ci[1],rocPS_jp_qda_PH[[2]]$ci[1],rocPS_jp_qda_PH[[3]]$ci[1],rocPS_jp_qda_PH[[4]]$ci[1],
                    rocPS_jp_qda_PH[[5]]$ci[1],rocPS_jp_qda_PH[[6]]$ci[1],rocPS_jp_qda_PH[[7]]$ci[1])
PS_jp_qda_PH_ciH<-c(rocPS_jp_qda_PH[[1]]$ci[3],rocPS_jp_qda_PH[[2]]$ci[3],rocPS_jp_qda_PH[[3]]$ci[3],rocPS_jp_qda_PH[[4]]$ci[3],
                    rocPS_jp_qda_PH[[5]]$ci[3],rocPS_jp_qda_PH[[6]]$ci[3],rocPS_jp_qda_PH[[7]]$ci[3])
#CD Layer
predCD_jp_qda_PH<-jackPredQDA(centCD, outcome = "justpass",predictors = c("cohort","PageRank", "Hide"))
rocCD_jp_qda_PH<-list()
rocCD_jp_qda_PH[[1]]<-roc(predCD_jp_qda_PH$justpass,as.numeric(predCD_jp_qda_PH$Week1),auc=T,ci=T)
rocCD_jp_qda_PH[[2]]<-roc(predCD_jp_qda_PH$justpass,as.numeric(predCD_jp_qda_PH$Week2),auc=T,ci=T)
rocCD_jp_qda_PH[[3]]<-roc(predCD_jp_qda_PH$justpass,as.numeric(predCD_jp_qda_PH$Week3),auc=T,ci=T)
rocCD_jp_qda_PH[[4]]<-roc(predCD_jp_qda_PH$justpass,as.numeric(predCD_jp_qda_PH$Week4),auc=T,ci=T)
rocCD_jp_qda_PH[[5]]<-roc(predCD_jp_qda_PH$justpass,as.numeric(predCD_jp_qda_PH$Week5),auc=T,ci=T)
rocCD_jp_qda_PH[[6]]<-roc(predCD_jp_qda_PH$justpass,as.numeric(predCD_jp_qda_PH$Week6),auc=T,ci=T)
rocCD_jp_qda_PH[[7]]<-roc(predCD_jp_qda_PH$justpass,as.numeric(predCD_jp_qda_PH$Week7),auc=T,ci=T)
CD_jp_qda_PH_auc<-c(rocCD_jp_qda_PH[[1]]$auc,rocCD_jp_qda_PH[[2]]$auc,rocCD_jp_qda_PH[[3]]$auc,rocCD_jp_qda_PH[[4]]$auc,
                    rocCD_jp_qda_PH[[5]]$auc,rocCD_jp_qda_PH[[6]]$auc,rocCD_jp_qda_PH[[7]]$auc)
CD_jp_qda_PH_ciL<-c(rocCD_jp_qda_PH[[1]]$ci[1],rocCD_jp_qda_PH[[2]]$ci[1],rocCD_jp_qda_PH[[3]]$ci[1],rocCD_jp_qda_PH[[4]]$ci[1],
                    rocCD_jp_qda_PH[[5]]$ci[1],rocCD_jp_qda_PH[[6]]$ci[1],rocCD_jp_qda_PH[[7]]$ci[1])
CD_jp_qda_PH_ciH<-c(rocCD_jp_qda_PH[[1]]$ci[3],rocCD_jp_qda_PH[[2]]$ci[3],rocCD_jp_qda_PH[[3]]$ci[3],rocCD_jp_qda_PH[[4]]$ci[3],
                    rocCD_jp_qda_PH[[5]]$ci[3],rocCD_jp_qda_PH[[6]]$ci[3],rocCD_jp_qda_PH[[7]]$ci[3])

#ICS layer
predICS_jp_qda_PH<-jackPredQDA(centICS, outcome = "justpass",predictors = c("cohort","PageRank", "Hide"))
rocICS_jp_qda_PH<-list()
rocICS_jp_qda_PH[[1]]<-roc(predICS_jp_qda_PH$justpass,as.numeric(predICS_jp_qda_PH$Week1),auc=T,ci=T)
rocICS_jp_qda_PH[[2]]<-roc(predICS_jp_qda_PH$justpass,as.numeric(predICS_jp_qda_PH$Week2),auc=T,ci=T)
rocICS_jp_qda_PH[[3]]<-roc(predICS_jp_qda_PH$justpass,as.numeric(predICS_jp_qda_PH$Week3),auc=T,ci=T)
rocICS_jp_qda_PH[[4]]<-roc(predICS_jp_qda_PH$justpass,as.numeric(predICS_jp_qda_PH$Week4),auc=T,ci=T)
rocICS_jp_qda_PH[[5]]<-roc(predICS_jp_qda_PH$justpass,as.numeric(predICS_jp_qda_PH$Week5),auc=T,ci=T)
rocICS_jp_qda_PH[[6]]<-roc(predICS_jp_qda_PH$justpass,as.numeric(predICS_jp_qda_PH$Week6),auc=T,ci=T)
rocICS_jp_qda_PH[[7]]<-roc(predICS_jp_qda_PH$justpass,as.numeric(predICS_jp_qda_PH$Week7),auc=T,ci=T)
ICS_jp_qda_PH_auc<-c(rocICS_jp_qda_PH[[1]]$auc,rocICS_jp_qda_PH[[2]]$auc,rocICS_jp_qda_PH[[3]]$auc,rocICS_jp_qda_PH[[4]]$auc,
                     rocICS_jp_qda_PH[[5]]$auc,rocICS_jp_qda_PH[[6]]$auc,rocICS_jp_qda_PH[[7]]$auc)
ICS_jp_qda_PH_ciL<-c(rocICS_jp_qda_PH[[1]]$ci[1],rocICS_jp_qda_PH[[2]]$ci[1],rocICS_jp_qda_PH[[3]]$ci[1],rocICS_jp_qda_PH[[4]]$ci[1],
                     rocICS_jp_qda_PH[[5]]$ci[1],rocICS_jp_qda_PH[[6]]$ci[1],rocICS_jp_qda_PH[[7]]$ci[1])
ICS_jp_qda_PH_ciH<-c(rocICS_jp_qda_PH[[1]]$ci[3],rocICS_jp_qda_PH[[2]]$ci[3],rocICS_jp_qda_PH[[3]]$ci[3],rocICS_jp_qda_PH[[4]]$ci[3],
                     rocICS_jp_qda_PH[[5]]$ci[3],rocICS_jp_qda_PH[[6]]$ci[3],rocICS_jp_qda_PH[[7]]$ci[3])


######TE-H models######
#PS layer
predPS_qda_TH<-jackPredQDA(centPS,predictors = c("cohort","tarEnt", "Hide"))
rocPS_qda_TH<-list()
rocPS_qda_TH[[1]]<-roc(predPS_qda_TH$pass,as.numeric(predPS_qda_TH$Week1),auc=T,ci=T)
rocPS_qda_TH[[2]]<-roc(predPS_qda_TH$pass,as.numeric(predPS_qda_TH$Week2),auc=T,ci=T)
rocPS_qda_TH[[3]]<-roc(predPS_qda_TH$pass,as.numeric(predPS_qda_TH$Week3),auc=T,ci=T)
rocPS_qda_TH[[4]]<-roc(predPS_qda_TH$pass,as.numeric(predPS_qda_TH$Week4),auc=T,ci=T)
rocPS_qda_TH[[5]]<-roc(predPS_qda_TH$pass,as.numeric(predPS_qda_TH$Week5),auc=T,ci=T)
rocPS_qda_TH[[6]]<-roc(predPS_qda_TH$pass,as.numeric(predPS_qda_TH$Week6),auc=T,ci=T)
rocPS_qda_TH[[7]]<-roc(predPS_qda_TH$pass,as.numeric(predPS_qda_TH$Week7),auc=T,ci=T)
PS_qda_TH_auc<-c(rocPS_qda_TH[[1]]$auc,rocPS_qda_TH[[2]]$auc,rocPS_qda_TH[[3]]$auc,rocPS_qda_TH[[4]]$auc,
                 rocPS_qda_TH[[5]]$auc,rocPS_qda_TH[[6]]$auc,rocPS_qda_TH[[7]]$auc)
PS_qda_TH_ciL<-c(rocPS_qda_TH[[1]]$ci[1],rocPS_qda_TH[[2]]$ci[1],rocPS_qda_TH[[3]]$ci[1],rocPS_qda_TH[[4]]$ci[1],
                 rocPS_qda_TH[[5]]$ci[1],rocPS_qda_TH[[6]]$ci[1],rocPS_qda_TH[[7]]$ci[1])
PS_qda_TH_ciH<-c(rocPS_qda_TH[[1]]$ci[3],rocPS_qda_TH[[2]]$ci[3],rocPS_qda_TH[[3]]$ci[3],rocPS_qda_TH[[4]]$ci[3],
                 rocPS_qda_TH[[5]]$ci[3],rocPS_qda_TH[[6]]$ci[3],rocPS_qda_TH[[7]]$ci[3])
##CD layer
predCD_qda_TH<-jackPredQDA(centCD,predictors = c("cohort","tarEnt", "Hide"))
rocCD_qda_TH<-list()
rocCD_qda_TH[[1]]<-roc(predCD_qda_TH$pass,as.numeric(predCD_qda_TH$Week1),auc=T,ci=T)
rocCD_qda_TH[[2]]<-roc(predCD_qda_TH$pass,as.numeric(predCD_qda_TH$Week2),auc=T,ci=T)
rocCD_qda_TH[[3]]<-roc(predCD_qda_TH$pass,as.numeric(predCD_qda_TH$Week3),auc=T,ci=T)
rocCD_qda_TH[[4]]<-roc(predCD_qda_TH$pass,as.numeric(predCD_qda_TH$Week4),auc=T,ci=T)
rocCD_qda_TH[[5]]<-roc(predCD_qda_TH$pass,as.numeric(predCD_qda_TH$Week5),auc=T,ci=T)
rocCD_qda_TH[[6]]<-roc(predCD_qda_TH$pass,as.numeric(predCD_qda_TH$Week6),auc=T,ci=T)
rocCD_qda_TH[[7]]<-roc(predCD_qda_TH$pass,as.numeric(predCD_qda_TH$Week7),auc=T,ci=T)
CD_qda_TH_auc<-c(rocCD_qda_TH[[1]]$auc,rocCD_qda_TH[[2]]$auc,rocCD_qda_TH[[3]]$auc,rocCD_qda_TH[[4]]$auc,
                 rocCD_qda_TH[[5]]$auc,rocCD_qda_TH[[6]]$auc,rocCD_qda_TH[[7]]$auc)
CD_qda_TH_ciL<-c(rocCD_qda_TH[[1]]$ci[1],rocCD_qda_TH[[2]]$ci[1],rocCD_qda_TH[[3]]$ci[1],rocCD_qda_TH[[4]]$ci[1],
                 rocCD_qda_TH[[5]]$ci[1],rocCD_qda_TH[[6]]$ci[1],rocCD_qda_TH[[7]]$ci[1])
CD_qda_TH_ciH<-c(rocCD_qda_TH[[1]]$ci[3],rocCD_qda_TH[[2]]$ci[3],rocCD_qda_TH[[3]]$ci[3],rocCD_qda_TH[[4]]$ci[3],
                 rocCD_qda_TH[[5]]$ci[3],rocCD_qda_TH[[6]]$ci[3],rocCD_qda_TH[[7]]$ci[3])

#ICS layer
predICS_qda_TH<-jackPredQDA(centICS,predictors = c("cohort","tarEnt", "Hide"))
rocICS_qda_TH<-list()
rocICS_qda_TH[[1]]<-roc(predICS_qda_TH$pass,as.numeric(predICS_qda_TH$Week1),auc=T,ci=T)
rocICS_qda_TH[[2]]<-roc(predICS_qda_TH$pass,as.numeric(predICS_qda_TH$Week2),auc=T,ci=T)
rocICS_qda_TH[[3]]<-roc(predICS_qda_TH$pass,as.numeric(predICS_qda_TH$Week3),auc=T,ci=T)
rocICS_qda_TH[[4]]<-roc(predICS_qda_TH$pass,as.numeric(predICS_qda_TH$Week4),auc=T,ci=T)
rocICS_qda_TH[[5]]<-roc(predICS_qda_TH$pass,as.numeric(predICS_qda_TH$Week5),auc=T,ci=T)
rocICS_qda_TH[[6]]<-roc(predICS_qda_TH$pass,as.numeric(predICS_qda_TH$Week6),auc=T,ci=T)
rocICS_qda_TH[[7]]<-roc(predICS_qda_TH$pass,as.numeric(predICS_qda_TH$Week7),auc=T,ci=T)
ICS_qda_TH_auc<-c(rocICS_qda_TH[[1]]$auc,rocICS_qda_TH[[2]]$auc,rocICS_qda_TH[[3]]$auc,rocICS_qda_TH[[4]]$auc,
                  rocICS_qda_TH[[5]]$auc,rocICS_qda_TH[[6]]$auc,rocICS_qda_TH[[7]]$auc)
ICS_qda_TH_ciL<-c(rocICS_qda_TH[[1]]$ci[1],rocICS_qda_TH[[2]]$ci[1],rocICS_qda_TH[[3]]$ci[1],rocICS_qda_TH[[4]]$ci[1],
                  rocICS_qda_TH[[5]]$ci[1],rocICS_qda_TH[[6]]$ci[1],rocICS_qda_TH[[7]]$ci[1])
ICS_qda_TH_ciH<-c(rocICS_qda_TH[[1]]$ci[3],rocICS_qda_TH[[2]]$ci[3],rocICS_qda_TH[[3]]$ci[3],rocICS_qda_TH[[4]]$ci[3],
                  rocICS_qda_TH[[5]]$ci[3],rocICS_qda_TH[[6]]$ci[3],rocICS_qda_TH[[7]]$ci[3])


###JUST PASS
#PS Layer
predPS_jp_qda_TH<-jackPredQDA(centPS,outcome = "justpass",predictors = c("cohort","tarEnt", "Hide"))
rocPS_jp_qda_TH<-list()
rocPS_jp_qda_TH[[1]]<-roc(predPS_jp_qda_TH$justpass,as.numeric(predPS_jp_qda_TH$Week1),auc=T,ci=T)
rocPS_jp_qda_TH[[2]]<-roc(predPS_jp_qda_TH$justpass,as.numeric(predPS_jp_qda_TH$Week2),auc=T,ci=T)
rocPS_jp_qda_TH[[3]]<-roc(predPS_jp_qda_TH$justpass,as.numeric(predPS_jp_qda_TH$Week3),auc=T,ci=T)
rocPS_jp_qda_TH[[4]]<-roc(predPS_jp_qda_TH$justpass,as.numeric(predPS_jp_qda_TH$Week4),auc=T,ci=T)
rocPS_jp_qda_TH[[5]]<-roc(predPS_jp_qda_TH$justpass,as.numeric(predPS_jp_qda_TH$Week5),auc=T,ci=T)
rocPS_jp_qda_TH[[6]]<-roc(predPS_jp_qda_TH$justpass,as.numeric(predPS_jp_qda_TH$Week6),auc=T,ci=T)
rocPS_jp_qda_TH[[7]]<-roc(predPS_jp_qda_TH$justpass,as.numeric(predPS_jp_qda_TH$Week7),auc=T,ci=T)

PS_jp_qda_TH_auc<-c(rocPS_jp_qda_TH[[1]]$auc,rocPS_jp_qda_TH[[2]]$auc,rocPS_jp_qda_TH[[3]]$auc,rocPS_jp_qda_TH[[4]]$auc,
                    rocPS_jp_qda_TH[[5]]$auc,rocPS_jp_qda_TH[[6]]$auc,rocPS_jp_qda_TH[[7]]$auc)
PS_jp_qda_TH_ciL<-c(rocPS_jp_qda_TH[[1]]$ci[1],rocPS_jp_qda_TH[[2]]$ci[1],rocPS_jp_qda_TH[[3]]$ci[1],rocPS_jp_qda_TH[[4]]$ci[1],
                    rocPS_jp_qda_TH[[5]]$ci[1],rocPS_jp_qda_TH[[6]]$ci[1],rocPS_jp_qda_TH[[7]]$ci[1])
PS_jp_qda_TH_ciH<-c(rocPS_jp_qda_TH[[1]]$ci[3],rocPS_jp_qda_TH[[2]]$ci[3],rocPS_jp_qda_TH[[3]]$ci[3],rocPS_jp_qda_TH[[4]]$ci[3],
                    rocPS_jp_qda_TH[[5]]$ci[3],rocPS_jp_qda_TH[[6]]$ci[3],rocPS_jp_qda_TH[[7]]$ci[3])
####Rank def. problem####
#CD layer
#predCD_jp_qda_TH<-jackPredQDA(centCD, outcome = "justpass",predictors = c("cohort","tarEnt", "Hide"))
#rocCD_jp_qda_TH<-list()
#rocCD_jp_qda_TH[[1]]<-roc(predCD_jp_qda_TH$justpass,as.numeric(predCD_jp_qda_TH$Week1),auc=T,ci=T)
#rocCD_jp_qda_TH[[2]]<-roc(predCD_jp_qda_TH$justpass,as.numeric(predCD_jp_qda_TH$Week2),auc=T,ci=T)
#rocCD_jp_qda_TH[[3]]<-roc(predCD_jp_qda_TH$justpass,as.numeric(predCD_jp_qda_TH$Week3),auc=T,ci=T)
#rocCD_jp_qda_TH[[4]]<-roc(predCD_jp_qda_TH$justpass,as.numeric(predCD_jp_qda_TH$Week4),auc=T,ci=T)
#rocCD_jp_qda_TH[[5]]<-roc(predCD_jp_qda_TH$justpass,as.numeric(predCD_jp_qda_TH$Week5),auc=T,ci=T)
#rocCD_jp_qda_TH[[6]]<-roc(predCD_jp_qda_TH$justpass,as.numeric(predCD_jp_qda_TH$Week6),auc=T,ci=T)
#rocCD_jp_qda_TH[[7]]<-roc(predCD_jp_qda_TH$justpass,as.numeric(predCD_jp_qda_TH$Week7),auc=T,ci=T)
#CD_jp_qda_TH_auc<-c(rocCD_jp_qda_TH[[1]]$auc,rocCD_jp_qda_TH[[2]]$auc,rocCD_jp_qda_TH[[3]]$auc,rocCD_jp_qda_TH[[4]]$auc,
#                    rocCD_jp_qda_TH[[5]]$auc,rocCD_jp_qda_TH[[6]]$auc,rocCD_jp_qda_TH[[7]]$auc)
#CD_jp_qda_TH_ciL<-c(rocCD_jp_qda_TH[[1]]$ci[1],rocCD_jp_qda_TH[[2]]$ci[1],rocCD_jp_qda_TH[[3]]$ci[1],rocCD_jp_qda_TH[[4]]$ci[1],
#                   rocCD_jp_qda_TH[[5]]$ci[1],rocCD_jp_qda_TH[[6]]$ci[1],rocCD_jp_qda_TH[[7]]$ci[1])
#CD_jp_qda_TH_ciH<-c(rocCD_jp_qda_TH[[1]]$ci[3],rocCD_jp_qda_TH[[2]]$ci[3],rocCD_jp_qda_TH[[3]]$ci[3],rocCD_jp_qda_TH[[4]]$ci[3],
#                  rocCD_jp_qda_TH[[5]]$ci[3],rocCD_jp_qda_TH[[6]]$ci[3],rocCD_jp_qda_TH[[7]]$ci[3])

#ICS Layer
predICS_jp_qda_TH<-jackPredQDA(centICS, outcome = "justpass",predictors = c("cohort","tarEnt", "Hide"))
rocICS_jp_qda_TH<-list()
rocICS_jp_qda_TH[[1]]<-roc(predICS_jp_qda_TH$justpass,as.numeric(predICS_jp_qda_TH$Week1),auc=T,ci=T)
rocICS_jp_qda_TH[[2]]<-roc(predICS_jp_qda_TH$justpass,as.numeric(predICS_jp_qda_TH$Week2),auc=T,ci=T)
rocICS_jp_qda_TH[[3]]<-roc(predICS_jp_qda_TH$justpass,as.numeric(predICS_jp_qda_TH$Week3),auc=T,ci=T)
rocICS_jp_qda_TH[[4]]<-roc(predICS_jp_qda_TH$justpass,as.numeric(predICS_jp_qda_TH$Week4),auc=T,ci=T)
rocICS_jp_qda_TH[[5]]<-roc(predICS_jp_qda_TH$justpass,as.numeric(predICS_jp_qda_TH$Week5),auc=T,ci=T)
rocICS_jp_qda_TH[[6]]<-roc(predICS_jp_qda_TH$justpass,as.numeric(predICS_jp_qda_TH$Week6),auc=T,ci=T)
rocICS_jp_qda_TH[[7]]<-roc(predICS_jp_qda_TH$justpass,as.numeric(predICS_jp_qda_TH$Week7),auc=T,ci=T)
ICS_jp_qda_TH_auc<-c(rocICS_jp_qda_TH[[1]]$auc,rocICS_jp_qda_TH[[2]]$auc,rocICS_jp_qda_TH[[3]]$auc,rocICS_jp_qda_TH[[4]]$auc,
                     rocICS_jp_qda_TH[[5]]$auc,rocICS_jp_qda_TH[[6]]$auc,rocICS_jp_qda_TH[[7]]$auc)
ICS_jp_qda_TH_ciL<-c(rocICS_jp_qda_TH[[1]]$ci[1],rocICS_jp_qda_TH[[2]]$ci[1],rocICS_jp_qda_TH[[3]]$ci[1],rocICS_jp_qda_TH[[4]]$ci[1],
                     rocICS_jp_qda_TH[[5]]$ci[1],rocICS_jp_qda_TH[[6]]$ci[1],rocICS_jp_qda_TH[[7]]$ci[1])
ICS_jp_qda_TH_ciH<-c(rocICS_jp_qda_TH[[1]]$ci[3],rocICS_jp_qda_TH[[2]]$ci[3],rocICS_jp_qda_TH[[3]]$ci[3],rocICS_jp_qda_TH[[4]]$ci[3],
                     rocICS_jp_qda_TH[[5]]$ci[3],rocICS_jp_qda_TH[[6]]$ci[3],rocICS_jp_qda_TH[[7]]$ci[3])

######TE models######
#PS layer
predPS_qda_T<-jackPredQDA(centPS,predictors = c("cohort","tarEnt"))
rocPS_qda_T<-list()
rocPS_qda_T[[1]]<-roc(predPS_qda_T$pass,as.numeric(predPS_qda_T$Week1),auc=T,ci=T)
rocPS_qda_T[[2]]<-roc(predPS_qda_T$pass,as.numeric(predPS_qda_T$Week2),auc=T,ci=T)
rocPS_qda_T[[3]]<-roc(predPS_qda_T$pass,as.numeric(predPS_qda_T$Week3),auc=T,ci=T)
rocPS_qda_T[[4]]<-roc(predPS_qda_T$pass,as.numeric(predPS_qda_T$Week4),auc=T,ci=T)
rocPS_qda_T[[5]]<-roc(predPS_qda_T$pass,as.numeric(predPS_qda_T$Week5),auc=T,ci=T)
rocPS_qda_T[[6]]<-roc(predPS_qda_T$pass,as.numeric(predPS_qda_T$Week6),auc=T,ci=T)
rocPS_qda_T[[7]]<-roc(predPS_qda_T$pass,as.numeric(predPS_qda_T$Week7),auc=T,ci=T)
PS_qda_T_auc<-c(rocPS_qda_T[[1]]$auc,rocPS_qda_T[[2]]$auc,rocPS_qda_T[[3]]$auc,rocPS_qda_T[[4]]$auc,
                rocPS_qda_T[[5]]$auc,rocPS_qda_T[[6]]$auc,rocPS_qda_T[[7]]$auc)
PS_qda_T_ciL<-c(rocPS_qda_T[[1]]$ci[1],rocPS_qda_T[[2]]$ci[1],rocPS_qda_T[[3]]$ci[1],rocPS_qda_T[[4]]$ci[1],
                rocPS_qda_T[[5]]$ci[1],rocPS_qda_T[[6]]$ci[1],rocPS_qda_T[[7]]$ci[1])
PS_qda_T_ciH<-c(rocPS_qda_T[[1]]$ci[3],rocPS_qda_T[[2]]$ci[3],rocPS_qda_T[[3]]$ci[3],rocPS_qda_T[[4]]$ci[3],
                rocPS_qda_T[[5]]$ci[3],rocPS_qda_T[[6]]$ci[3],rocPS_qda_T[[7]]$ci[3])
##CD layer
predCD_qda_T<-jackPredQDA(centCD,predictors = c("cohort","tarEnt"))
rocCD_qda_T<-list()
rocCD_qda_T[[1]]<-roc(predCD_qda_T$pass,as.numeric(predCD_qda_T$Week1),auc=T,ci=T)
rocCD_qda_T[[2]]<-roc(predCD_qda_T$pass,as.numeric(predCD_qda_T$Week2),auc=T,ci=T)
rocCD_qda_T[[3]]<-roc(predCD_qda_T$pass,as.numeric(predCD_qda_T$Week3),auc=T,ci=T)
rocCD_qda_T[[4]]<-roc(predCD_qda_T$pass,as.numeric(predCD_qda_T$Week4),auc=T,ci=T)
rocCD_qda_T[[5]]<-roc(predCD_qda_T$pass,as.numeric(predCD_qda_T$Week5),auc=T,ci=T)
rocCD_qda_T[[6]]<-roc(predCD_qda_T$pass,as.numeric(predCD_qda_T$Week6),auc=T,ci=T)
rocCD_qda_T[[7]]<-roc(predCD_qda_T$pass,as.numeric(predCD_qda_T$Week7),auc=T,ci=T)
CD_qda_T_auc<-c(rocCD_qda_T[[1]]$auc,rocCD_qda_T[[2]]$auc,rocCD_qda_T[[3]]$auc,rocCD_qda_T[[4]]$auc,
                rocCD_qda_T[[5]]$auc,rocCD_qda_T[[6]]$auc,rocCD_qda_T[[7]]$auc)
CD_qda_T_ciL<-c(rocCD_qda_T[[1]]$ci[1],rocCD_qda_T[[2]]$ci[1],rocCD_qda_T[[3]]$ci[1],rocCD_qda_T[[4]]$ci[1],
                rocCD_qda_T[[5]]$ci[1],rocCD_qda_T[[6]]$ci[1],rocCD_qda_T[[7]]$ci[1])
CD_qda_T_ciH<-c(rocCD_qda_T[[1]]$ci[3],rocCD_qda_T[[2]]$ci[3],rocCD_qda_T[[3]]$ci[3],rocCD_qda_T[[4]]$ci[3],
                rocCD_qda_T[[5]]$ci[3],rocCD_qda_T[[6]]$ci[3],rocCD_qda_T[[7]]$ci[3])

#ICS layer
predICS_qda_T<-jackPredQDA(centICS,predictors = c("cohort","tarEnt"))
rocICS_qda_T<-list()
rocICS_qda_T[[1]]<-roc(predICS_qda_T$pass,as.numeric(predICS_qda_T$Week1),auc=T,ci=T)
rocICS_qda_T[[2]]<-roc(predICS_qda_T$pass,as.numeric(predICS_qda_T$Week2),auc=T,ci=T)
rocICS_qda_T[[3]]<-roc(predICS_qda_T$pass,as.numeric(predICS_qda_T$Week3),auc=T,ci=T)
rocICS_qda_T[[4]]<-roc(predICS_qda_T$pass,as.numeric(predICS_qda_T$Week4),auc=T,ci=T)
rocICS_qda_T[[5]]<-roc(predICS_qda_T$pass,as.numeric(predICS_qda_T$Week5),auc=T,ci=T)
rocICS_qda_T[[6]]<-roc(predICS_qda_T$pass,as.numeric(predICS_qda_T$Week6),auc=T,ci=T)
rocICS_qda_T[[7]]<-roc(predICS_qda_T$pass,as.numeric(predICS_qda_T$Week7),auc=T,ci=T)
ICS_qda_T_auc<-c(rocICS_qda_T[[1]]$auc,rocICS_qda_T[[2]]$auc,rocICS_qda_T[[3]]$auc,rocICS_qda_T[[4]]$auc,
                 rocICS_qda_T[[5]]$auc,rocICS_qda_T[[6]]$auc,rocICS_qda_T[[7]]$auc)
ICS_qda_T_ciL<-c(rocICS_qda_T[[1]]$ci[1],rocICS_qda_T[[2]]$ci[1],rocICS_qda_T[[3]]$ci[1],rocICS_qda_T[[4]]$ci[1],
                 rocICS_qda_T[[5]]$ci[1],rocICS_qda_T[[6]]$ci[1],rocICS_qda_T[[7]]$ci[1])
ICS_qda_T_ciH<-c(rocICS_qda_T[[1]]$ci[3],rocICS_qda_T[[2]]$ci[3],rocICS_qda_T[[3]]$ci[3],rocICS_qda_T[[4]]$ci[3],
                 rocICS_qda_T[[5]]$ci[3],rocICS_qda_T[[6]]$ci[3],rocICS_qda_T[[7]]$ci[3])

###JUST PASS
#PS layer
predPS_jp_qda_T<-jackPredQDA(centPS,outcome = "justpass",predictors = c("cohort","tarEnt"))
rocPS_jp_qda_T<-list()
rocPS_jp_qda_T[[1]]<-roc(predPS_jp_qda_T$justpass,as.numeric(predPS_jp_qda_T$Week1),auc=T,ci=T)
rocPS_jp_qda_T[[2]]<-roc(predPS_jp_qda_T$justpass,as.numeric(predPS_jp_qda_T$Week2),auc=T,ci=T)
rocPS_jp_qda_T[[3]]<-roc(predPS_jp_qda_T$justpass,as.numeric(predPS_jp_qda_T$Week3),auc=T,ci=T)
rocPS_jp_qda_T[[4]]<-roc(predPS_jp_qda_T$justpass,as.numeric(predPS_jp_qda_T$Week4),auc=T,ci=T)
rocPS_jp_qda_T[[5]]<-roc(predPS_jp_qda_T$justpass,as.numeric(predPS_jp_qda_T$Week5),auc=T,ci=T)
rocPS_jp_qda_T[[6]]<-roc(predPS_jp_qda_T$justpass,as.numeric(predPS_jp_qda_T$Week6),auc=T,ci=T)
rocPS_jp_qda_T[[7]]<-roc(predPS_jp_qda_T$justpass,as.numeric(predPS_jp_qda_T$Week7),auc=T,ci=T)

PS_jp_qda_T_auc<-c(rocPS_jp_qda_T[[1]]$auc,rocPS_jp_qda_T[[2]]$auc,rocPS_jp_qda_T[[3]]$auc,rocPS_jp_qda_T[[4]]$auc,
                   rocPS_jp_qda_T[[5]]$auc,rocPS_jp_qda_T[[6]]$auc,rocPS_jp_qda_T[[7]]$auc)
PS_jp_qda_T_ciL<-c(rocPS_jp_qda_T[[1]]$ci[1],rocPS_jp_qda_T[[2]]$ci[1],rocPS_jp_qda_T[[3]]$ci[1],rocPS_jp_qda_T[[4]]$ci[1],
                   rocPS_jp_qda_T[[5]]$ci[1],rocPS_jp_qda_T[[6]]$ci[1],rocPS_jp_qda_T[[7]]$ci[1])
PS_jp_qda_T_ciH<-c(rocPS_jp_qda_T[[1]]$ci[3],rocPS_jp_qda_T[[2]]$ci[3],rocPS_jp_qda_T[[3]]$ci[3],rocPS_jp_qda_T[[4]]$ci[3],
                   rocPS_jp_qda_T[[5]]$ci[3],rocPS_jp_qda_T[[6]]$ci[3],rocPS_jp_qda_T[[7]]$ci[3])
####Rank def. problem####
#CD layer
#predCD_jp_qda_T<-jackPredQDA(centCD, outcome = "justpass",predictors = c("cohort","tarEnt"))
#rocCD_jp_qda_T<-list()
#rocCD_jp_qda_T[[1]]<-roc(predCD_jp_qda_T$justpass,as.numeric(predCD_jp_qda_T$Week1),auc=T,ci=T)
#rocCD_jp_qda_T[[2]]<-roc(predCD_jp_qda_T$justpass,as.numeric(predCD_jp_qda_T$Week2),auc=T,ci=T)
#rocCD_jp_qda_T[[3]]<-roc(predCD_jp_qda_T$justpass,as.numeric(predCD_jp_qda_T$Week3),auc=T,ci=T)
#rocCD_jp_qda_T[[4]]<-roc(predCD_jp_qda_T$justpass,as.numeric(predCD_jp_qda_T$Week4),auc=T,ci=T)
#rocCD_jp_qda_T[[5]]<-roc(predCD_jp_qda_T$justpass,as.numeric(predCD_jp_qda_T$Week5),auc=T,ci=T)
#rocCD_jp_qda_T[[6]]<-roc(predCD_jp_qda_T$justpass,as.numeric(predCD_jp_qda_T$Week6),auc=T,ci=T)
#rocCD_jp_qda_T[[7]]<-roc(predCD_jp_qda_T$justpass,as.numeric(predCD_jp_qda_T$Week7),auc=T,ci=T)
#CD_jp_qda_T_auc<-c(rocCD_jp_qda_T[[1]]$auc,rocCD_jp_qda_T[[2]]$auc,rocCD_jp_qda_T[[3]]$auc,rocCD_jp_qda_T[[4]]$auc,
#                  rocCD_jp_qda_T[[5]]$auc,rocCD_jp_qda_T[[6]]$auc,rocCD_jp_qda_T[[7]]$auc)
#CD_jp_qda_T_ciL<-c(rocCD_jp_qda_T[[1]]$ci[1],rocCD_jp_qda_T[[2]]$ci[1],rocCD_jp_qda_T[[3]]$ci[1],rocCD_jp_qda_T[[4]]$ci[1],
#                   rocCD_jp_qda_T[[5]]$ci[1],rocCD_jp_qda_T[[6]]$ci[1],rocCD_jp_qda_T[[7]]$ci[1])
#CD_jp_qda_T_ciH<-c(rocCD_jp_qda_T[[1]]$ci[3],rocCD_jp_qda_T[[2]]$ci[3],rocCD_jp_qda_T[[3]]$ci[3],rocCD_jp_qda_T[[4]]$ci[3],
#                  rocCD_jp_qda_T[[5]]$ci[3],rocCD_jp_qda_T[[6]]$ci[3],rocCD_jp_qda_T[[7]]$ci[3])
#ICS layer
predICS_jp_qda_T<-jackPredQDA(centICS, outcome = "justpass",predictors = c("cohort","tarEnt"))
rocICS_jp_qda_T<-list()
rocICS_jp_qda_T[[1]]<-roc(predICS_jp_qda_T$justpass,as.numeric(predICS_jp_qda_T$Week1),auc=T,ci=T)
rocICS_jp_qda_T[[2]]<-roc(predICS_jp_qda_T$justpass,as.numeric(predICS_jp_qda_T$Week2),auc=T,ci=T)
rocICS_jp_qda_T[[3]]<-roc(predICS_jp_qda_T$justpass,as.numeric(predICS_jp_qda_T$Week3),auc=T,ci=T)
rocICS_jp_qda_T[[4]]<-roc(predICS_jp_qda_T$justpass,as.numeric(predICS_jp_qda_T$Week4),auc=T,ci=T)
rocICS_jp_qda_T[[5]]<-roc(predICS_jp_qda_T$justpass,as.numeric(predICS_jp_qda_T$Week5),auc=T,ci=T)
rocICS_jp_qda_T[[6]]<-roc(predICS_jp_qda_T$justpass,as.numeric(predICS_jp_qda_T$Week6),auc=T,ci=T)
rocICS_jp_qda_T[[7]]<-roc(predICS_jp_qda_T$justpass,as.numeric(predICS_jp_qda_T$Week7),auc=T,ci=T)
ICS_jp_qda_T_auc<-c(rocICS_jp_qda_T[[1]]$auc,rocICS_jp_qda_T[[2]]$auc,rocICS_jp_qda_T[[3]]$auc,rocICS_jp_qda_T[[4]]$auc,
                    rocICS_jp_qda_T[[5]]$auc,rocICS_jp_qda_T[[6]]$auc,rocICS_jp_qda_T[[7]]$auc)
ICS_jp_qda_T_ciL<-c(rocICS_jp_qda_T[[1]]$ci[1],rocICS_jp_qda_T[[2]]$ci[1],rocICS_jp_qda_T[[3]]$ci[1],rocICS_jp_qda_T[[4]]$ci[1],
                    rocICS_jp_qda_T[[5]]$ci[1],rocICS_jp_qda_T[[6]]$ci[1],rocICS_jp_qda_T[[7]]$ci[1])
ICS_jp_qda_T_ciH<-c(rocICS_jp_qda_T[[1]]$ci[3],rocICS_jp_qda_T[[2]]$ci[3],rocICS_jp_qda_T[[3]]$ci[3],rocICS_jp_qda_T[[4]]$ci[3],
                    rocICS_jp_qda_T[[5]]$ci[3],rocICS_jp_qda_T[[6]]$ci[3],rocICS_jp_qda_T[[7]]$ci[3])



######H models######
#PS layer
predPS_qda_H<-jackPredQDA(centPS,predictors = c("cohort", "Hide"))
rocPS_qda_H<-list()
rocPS_qda_H[[1]]<-roc(predPS_qda_H$pass,as.numeric(predPS_qda_H$Week1),auc=T,ci=T)
rocPS_qda_H[[2]]<-roc(predPS_qda_H$pass,as.numeric(predPS_qda_H$Week2),auc=T,ci=T)
rocPS_qda_H[[3]]<-roc(predPS_qda_H$pass,as.numeric(predPS_qda_H$Week3),auc=T,ci=T)
rocPS_qda_H[[4]]<-roc(predPS_qda_H$pass,as.numeric(predPS_qda_H$Week4),auc=T,ci=T)
rocPS_qda_H[[5]]<-roc(predPS_qda_H$pass,as.numeric(predPS_qda_H$Week5),auc=T,ci=T)
rocPS_qda_H[[6]]<-roc(predPS_qda_H$pass,as.numeric(predPS_qda_H$Week6),auc=T,ci=T)
rocPS_qda_H[[7]]<-roc(predPS_qda_H$pass,as.numeric(predPS_qda_H$Week7),auc=T,ci=T)
PS_qda_H_auc<-c(rocPS_qda_H[[1]]$auc,rocPS_qda_H[[2]]$auc,rocPS_qda_H[[3]]$auc,rocPS_qda_H[[4]]$auc,
                rocPS_qda_H[[5]]$auc,rocPS_qda_H[[6]]$auc,rocPS_qda_H[[7]]$auc)
PS_qda_H_ciL<-c(rocPS_qda_H[[1]]$ci[1],rocPS_qda_H[[2]]$ci[1],rocPS_qda_H[[3]]$ci[1],rocPS_qda_H[[4]]$ci[1],
                rocPS_qda_H[[5]]$ci[1],rocPS_qda_H[[6]]$ci[1],rocPS_qda_H[[7]]$ci[1])
PS_qda_H_ciH<-c(rocPS_qda_H[[1]]$ci[3],rocPS_qda_H[[2]]$ci[3],rocPS_qda_H[[3]]$ci[3],rocPS_qda_H[[4]]$ci[3],
                rocPS_qda_H[[5]]$ci[3],rocPS_qda_H[[6]]$ci[3],rocPS_qda_H[[7]]$ci[3])
##CD layer
predCD_qda_H<-jackPredQDA(centCD,predictors = c("cohort", "Hide"))
rocCD_qda_H<-list()
rocCD_qda_H[[1]]<-roc(predCD_qda_H$pass,as.numeric(predCD_qda_H$Week1),auc=T,ci=T)
rocCD_qda_H[[2]]<-roc(predCD_qda_H$pass,as.numeric(predCD_qda_H$Week2),auc=T,ci=T)
rocCD_qda_H[[3]]<-roc(predCD_qda_H$pass,as.numeric(predCD_qda_H$Week3),auc=T,ci=T)
rocCD_qda_H[[4]]<-roc(predCD_qda_H$pass,as.numeric(predCD_qda_H$Week4),auc=T,ci=T)
rocCD_qda_H[[5]]<-roc(predCD_qda_H$pass,as.numeric(predCD_qda_H$Week5),auc=T,ci=T)
rocCD_qda_H[[6]]<-roc(predCD_qda_H$pass,as.numeric(predCD_qda_H$Week6),auc=T,ci=T)
rocCD_qda_H[[7]]<-roc(predCD_qda_H$pass,as.numeric(predCD_qda_H$Week7),auc=T,ci=T)
CD_qda_H_auc<-c(rocCD_qda_H[[1]]$auc,rocCD_qda_H[[2]]$auc,rocCD_qda_H[[3]]$auc,rocCD_qda_H[[4]]$auc,
                rocCD_qda_H[[5]]$auc,rocCD_qda_H[[6]]$auc,rocCD_qda_H[[7]]$auc)
CD_qda_H_ciL<-c(rocCD_qda_H[[1]]$ci[1],rocCD_qda_H[[2]]$ci[1],rocCD_qda_H[[3]]$ci[1],rocCD_qda_H[[4]]$ci[1],
                rocCD_qda_H[[5]]$ci[1],rocCD_qda_H[[6]]$ci[1],rocCD_qda_H[[7]]$ci[1])
CD_qda_H_ciH<-c(rocCD_qda_H[[1]]$ci[3],rocCD_qda_H[[2]]$ci[3],rocCD_qda_H[[3]]$ci[3],rocCD_qda_H[[4]]$ci[3],
                rocCD_qda_H[[5]]$ci[3],rocCD_qda_H[[6]]$ci[3],rocCD_qda_H[[7]]$ci[3])

#ICS layer
predICS_qda_H<-jackPredQDA(centICS,predictors = c("cohort", "Hide"))
rocICS_qda_H<-list()
rocICS_qda_H[[1]]<-roc(predICS_qda_H$pass,as.numeric(predICS_qda_H$Week1),auc=T,ci=T)
rocICS_qda_H[[2]]<-roc(predICS_qda_H$pass,as.numeric(predICS_qda_H$Week2),auc=T,ci=T)
rocICS_qda_H[[3]]<-roc(predICS_qda_H$pass,as.numeric(predICS_qda_H$Week3),auc=T,ci=T)
rocICS_qda_H[[4]]<-roc(predICS_qda_H$pass,as.numeric(predICS_qda_H$Week4),auc=T,ci=T)
rocICS_qda_H[[5]]<-roc(predICS_qda_H$pass,as.numeric(predICS_qda_H$Week5),auc=T,ci=T)
rocICS_qda_H[[6]]<-roc(predICS_qda_H$pass,as.numeric(predICS_qda_H$Week6),auc=T,ci=T)
rocICS_qda_H[[7]]<-roc(predICS_qda_H$pass,as.numeric(predICS_qda_H$Week7),auc=T,ci=T)
ICS_qda_H_auc<-c(rocICS_qda_H[[1]]$auc,rocICS_qda_H[[2]]$auc,rocICS_qda_H[[3]]$auc,rocICS_qda_H[[4]]$auc,
                 rocICS_qda_H[[5]]$auc,rocICS_qda_H[[6]]$auc,rocICS_qda_H[[7]]$auc)
ICS_qda_H_ciL<-c(rocICS_qda_H[[1]]$ci[1],rocICS_qda_H[[2]]$ci[1],rocICS_qda_H[[3]]$ci[1],rocICS_qda_H[[4]]$ci[1],
                 rocICS_qda_H[[5]]$ci[1],rocICS_qda_H[[6]]$ci[1],rocICS_qda_H[[7]]$ci[1])
ICS_qda_H_ciH<-c(rocICS_qda_H[[1]]$ci[3],rocICS_qda_H[[2]]$ci[3],rocICS_qda_H[[3]]$ci[3],rocICS_qda_H[[4]]$ci[3],
                 rocICS_qda_H[[5]]$ci[3],rocICS_qda_H[[6]]$ci[3],rocICS_qda_H[[7]]$ci[3])


###JUST PASS
predPS_jp_qda_H<-jackPredQDA(centPS,outcome = "justpass",predictors = c("cohort", "Hide"))
rocPS_jp_qda_H<-list()
rocPS_jp_qda_H[[1]]<-roc(predPS_jp_qda_H$justpass,as.numeric(predPS_jp_qda_H$Week1),auc=T,ci=T)
rocPS_jp_qda_H[[2]]<-roc(predPS_jp_qda_H$justpass,as.numeric(predPS_jp_qda_H$Week2),auc=T,ci=T)
rocPS_jp_qda_H[[3]]<-roc(predPS_jp_qda_H$justpass,as.numeric(predPS_jp_qda_H$Week3),auc=T,ci=T)
rocPS_jp_qda_H[[4]]<-roc(predPS_jp_qda_H$justpass,as.numeric(predPS_jp_qda_H$Week4),auc=T,ci=T)
rocPS_jp_qda_H[[5]]<-roc(predPS_jp_qda_H$justpass,as.numeric(predPS_jp_qda_H$Week5),auc=T,ci=T)
rocPS_jp_qda_H[[6]]<-roc(predPS_jp_qda_H$justpass,as.numeric(predPS_jp_qda_H$Week6),auc=T,ci=T)
rocPS_jp_qda_H[[7]]<-roc(predPS_jp_qda_H$justpass,as.numeric(predPS_jp_qda_H$Week7),auc=T,ci=T)

PS_jp_qda_H_auc<-c(rocPS_jp_qda_H[[1]]$auc,rocPS_jp_qda_H[[2]]$auc,rocPS_jp_qda_H[[3]]$auc,rocPS_jp_qda_H[[4]]$auc,
                   rocPS_jp_qda_H[[5]]$auc,rocPS_jp_qda_H[[6]]$auc,rocPS_jp_qda_H[[7]]$auc)
PS_jp_qda_H_ciL<-c(rocPS_jp_qda_H[[1]]$ci[1],rocPS_jp_qda_H[[2]]$ci[1],rocPS_jp_qda_H[[3]]$ci[1],rocPS_jp_qda_H[[4]]$ci[1],
                   rocPS_jp_qda_H[[5]]$ci[1],rocPS_jp_qda_H[[6]]$ci[1],rocPS_jp_qda_H[[7]]$ci[1])
PS_jp_qda_H_ciH<-c(rocPS_jp_qda_H[[1]]$ci[3],rocPS_jp_qda_H[[2]]$ci[3],rocPS_jp_qda_H[[3]]$ci[3],rocPS_jp_qda_H[[4]]$ci[3],
                   rocPS_jp_qda_H[[5]]$ci[3],rocPS_jp_qda_H[[6]]$ci[3],rocPS_jp_qda_H[[7]]$ci[3])

predCD_jp_qda_H<-jackPredQDA(centCD, outcome = "justpass",predictors = c("cohort", "Hide"))
rocCD_jp_qda_H<-list()
rocCD_jp_qda_H[[1]]<-roc(predCD_jp_qda_H$justpass,as.numeric(predCD_jp_qda_H$Week1),auc=T,ci=T)
rocCD_jp_qda_H[[2]]<-roc(predCD_jp_qda_H$justpass,as.numeric(predCD_jp_qda_H$Week2),auc=T,ci=T)
rocCD_jp_qda_H[[3]]<-roc(predCD_jp_qda_H$justpass,as.numeric(predCD_jp_qda_H$Week3),auc=T,ci=T)
rocCD_jp_qda_H[[4]]<-roc(predCD_jp_qda_H$justpass,as.numeric(predCD_jp_qda_H$Week4),auc=T,ci=T)
rocCD_jp_qda_H[[5]]<-roc(predCD_jp_qda_H$justpass,as.numeric(predCD_jp_qda_H$Week5),auc=T,ci=T)
rocCD_jp_qda_H[[6]]<-roc(predCD_jp_qda_H$justpass,as.numeric(predCD_jp_qda_H$Week6),auc=T,ci=T)
rocCD_jp_qda_H[[7]]<-roc(predCD_jp_qda_H$justpass,as.numeric(predCD_jp_qda_H$Week7),auc=T,ci=T)
CD_jp_qda_H_auc<-c(rocCD_jp_qda_H[[1]]$auc,rocCD_jp_qda_H[[2]]$auc,rocCD_jp_qda_H[[3]]$auc,rocCD_jp_qda_H[[4]]$auc,
                   rocCD_jp_qda_H[[5]]$auc,rocCD_jp_qda_H[[6]]$auc,rocCD_jp_qda_H[[7]]$auc)
CD_jp_qda_H_ciL<-c(rocCD_jp_qda_H[[1]]$ci[1],rocCD_jp_qda_H[[2]]$ci[1],rocCD_jp_qda_H[[3]]$ci[1],rocCD_jp_qda_H[[4]]$ci[1],
                   rocCD_jp_qda_H[[5]]$ci[1],rocCD_jp_qda_H[[6]]$ci[1],rocCD_jp_qda_H[[7]]$ci[1])
CD_jp_qda_H_ciH<-c(rocCD_jp_qda_H[[1]]$ci[3],rocCD_jp_qda_H[[2]]$ci[3],rocCD_jp_qda_H[[3]]$ci[3],rocCD_jp_qda_H[[4]]$ci[3],
                   rocCD_jp_qda_H[[5]]$ci[3],rocCD_jp_qda_H[[6]]$ci[3],rocCD_jp_qda_H[[7]]$ci[3])


predICS_jp_qda_H<-jackPredQDA(centICS, outcome = "justpass",predictors = c("cohort", "Hide"))
rocICS_jp_qda_H<-list()
rocICS_jp_qda_H[[1]]<-roc(predICS_jp_qda_H$justpass,as.numeric(predICS_jp_qda_H$Week1),auc=T,ci=T)
rocICS_jp_qda_H[[2]]<-roc(predICS_jp_qda_H$justpass,as.numeric(predICS_jp_qda_H$Week2),auc=T,ci=T)
rocICS_jp_qda_H[[3]]<-roc(predICS_jp_qda_H$justpass,as.numeric(predICS_jp_qda_H$Week3),auc=T,ci=T)
rocICS_jp_qda_H[[4]]<-roc(predICS_jp_qda_H$justpass,as.numeric(predICS_jp_qda_H$Week4),auc=T,ci=T)
rocICS_jp_qda_H[[5]]<-roc(predICS_jp_qda_H$justpass,as.numeric(predICS_jp_qda_H$Week5),auc=T,ci=T)
rocICS_jp_qda_H[[6]]<-roc(predICS_jp_qda_H$justpass,as.numeric(predICS_jp_qda_H$Week6),auc=T,ci=T)
rocICS_jp_qda_H[[7]]<-roc(predICS_jp_qda_H$justpass,as.numeric(predICS_jp_qda_H$Week7),auc=T,ci=T)
ICS_jp_qda_H_auc<-c(rocICS_jp_qda_H[[1]]$auc,rocICS_jp_qda_H[[2]]$auc,rocICS_jp_qda_H[[3]]$auc,rocICS_jp_qda_H[[4]]$auc,
                    rocICS_jp_qda_H[[5]]$auc,rocICS_jp_qda_H[[6]]$auc,rocICS_jp_qda_H[[7]]$auc)
ICS_jp_qda_H_ciL<-c(rocICS_jp_qda_H[[1]]$ci[1],rocICS_jp_qda_H[[2]]$ci[1],rocICS_jp_qda_H[[3]]$ci[1],rocICS_jp_qda_H[[4]]$ci[1],
                    rocICS_jp_qda_H[[5]]$ci[1],rocICS_jp_qda_H[[6]]$ci[1],rocICS_jp_qda_H[[7]]$ci[1])
ICS_jp_qda_H_ciH<-c(rocICS_jp_qda_H[[1]]$ci[3],rocICS_jp_qda_H[[2]]$ci[3],rocICS_jp_qda_H[[3]]$ci[3],rocICS_jp_qda_H[[4]]$ci[3],
                    rocICS_jp_qda_H[[5]]$ci[3],rocICS_jp_qda_H[[6]]$ci[3],rocICS_jp_qda_H[[7]]$ci[3])


######PR models######
#PS layer
predPS_qda_P<-jackPredQDA(centPS,predictors = c("cohort","PageRank"))
rocPS_qda_P<-list()
rocPS_qda_P[[1]]<-roc(predPS_qda_P$pass,as.numeric(predPS_qda_P$Week1),auc=T,ci=T)
rocPS_qda_P[[2]]<-roc(predPS_qda_P$pass,as.numeric(predPS_qda_P$Week2),auc=T,ci=T)
rocPS_qda_P[[3]]<-roc(predPS_qda_P$pass,as.numeric(predPS_qda_P$Week3),auc=T,ci=T)
rocPS_qda_P[[4]]<-roc(predPS_qda_P$pass,as.numeric(predPS_qda_P$Week4),auc=T,ci=T)
rocPS_qda_P[[5]]<-roc(predPS_qda_P$pass,as.numeric(predPS_qda_P$Week5),auc=T,ci=T)
rocPS_qda_P[[6]]<-roc(predPS_qda_P$pass,as.numeric(predPS_qda_P$Week6),auc=T,ci=T)
rocPS_qda_P[[7]]<-roc(predPS_qda_P$pass,as.numeric(predPS_qda_P$Week7),auc=T,ci=T)
PS_qda_P_auc<-c(rocPS_qda_P[[1]]$auc,rocPS_qda_P[[2]]$auc,rocPS_qda_P[[3]]$auc,rocPS_qda_P[[4]]$auc,
                rocPS_qda_P[[5]]$auc,rocPS_qda_P[[6]]$auc,rocPS_qda_P[[7]]$auc)
PS_qda_P_ciL<-c(rocPS_qda_P[[1]]$ci[1],rocPS_qda_P[[2]]$ci[1],rocPS_qda_P[[3]]$ci[1],rocPS_qda_P[[4]]$ci[1],
                rocPS_qda_P[[5]]$ci[1],rocPS_qda_P[[6]]$ci[1],rocPS_qda_P[[7]]$ci[1])
PS_qda_P_ciH<-c(rocPS_qda_P[[1]]$ci[3],rocPS_qda_P[[2]]$ci[3],rocPS_qda_P[[3]]$ci[3],rocPS_qda_P[[4]]$ci[3],
                rocPS_qda_P[[5]]$ci[3],rocPS_qda_P[[6]]$ci[3],rocPS_qda_P[[7]]$ci[3])
##CD layer
predCD_qda_P<-jackPredQDA(centCD,predictors = c("cohort","PageRank"))
rocCD_qda_P<-list()
rocCD_qda_P[[1]]<-roc(predCD_qda_P$pass,as.numeric(predCD_qda_P$Week1),auc=T,ci=T)
rocCD_qda_P[[2]]<-roc(predCD_qda_P$pass,as.numeric(predCD_qda_P$Week2),auc=T,ci=T)
rocCD_qda_P[[3]]<-roc(predCD_qda_P$pass,as.numeric(predCD_qda_P$Week3),auc=T,ci=T)
rocCD_qda_P[[4]]<-roc(predCD_qda_P$pass,as.numeric(predCD_qda_P$Week4),auc=T,ci=T)
rocCD_qda_P[[5]]<-roc(predCD_qda_P$pass,as.numeric(predCD_qda_P$Week5),auc=T,ci=T)
rocCD_qda_P[[6]]<-roc(predCD_qda_P$pass,as.numeric(predCD_qda_P$Week6),auc=T,ci=T)
rocCD_qda_P[[7]]<-roc(predCD_qda_P$pass,as.numeric(predCD_qda_P$Week7),auc=T,ci=T)
CD_qda_P_auc<-c(rocCD_qda_P[[1]]$auc,rocCD_qda_P[[2]]$auc,rocCD_qda_P[[3]]$auc,rocCD_qda_P[[4]]$auc,
                rocCD_qda_P[[5]]$auc,rocCD_qda_P[[6]]$auc,rocCD_qda_P[[7]]$auc)
CD_qda_P_ciL<-c(rocCD_qda_P[[1]]$ci[1],rocCD_qda_P[[2]]$ci[1],rocCD_qda_P[[3]]$ci[1],rocCD_qda_P[[4]]$ci[1],
                rocCD_qda_P[[5]]$ci[1],rocCD_qda_P[[6]]$ci[1],rocCD_qda_P[[7]]$ci[1])
CD_qda_P_ciH<-c(rocCD_qda_P[[1]]$ci[3],rocCD_qda_P[[2]]$ci[3],rocCD_qda_P[[3]]$ci[3],rocCD_qda_P[[4]]$ci[3],
                rocCD_qda_P[[5]]$ci[3],rocCD_qda_P[[6]]$ci[3],rocCD_qda_P[[7]]$ci[3])

#ICS layer
predICS_qda_P<-jackPredQDA(centICS,predictors = c("cohort","PageRank"))
rocICS_qda_P<-list()
rocICS_qda_P[[1]]<-roc(predICS_qda_P$pass,as.numeric(predICS_qda_P$Week1),auc=T,ci=T)
rocICS_qda_P[[2]]<-roc(predICS_qda_P$pass,as.numeric(predICS_qda_P$Week2),auc=T,ci=T)
rocICS_qda_P[[3]]<-roc(predICS_qda_P$pass,as.numeric(predICS_qda_P$Week3),auc=T,ci=T)
rocICS_qda_P[[4]]<-roc(predICS_qda_P$pass,as.numeric(predICS_qda_P$Week4),auc=T,ci=T)
rocICS_qda_P[[5]]<-roc(predICS_qda_P$pass,as.numeric(predICS_qda_P$Week5),auc=T,ci=T)
rocICS_qda_P[[6]]<-roc(predICS_qda_P$pass,as.numeric(predICS_qda_P$Week6),auc=T,ci=T)
rocICS_qda_P[[7]]<-roc(predICS_qda_P$pass,as.numeric(predICS_qda_P$Week7),auc=T,ci=T)
ICS_qda_P_auc<-c(rocICS_qda_P[[1]]$auc,rocICS_qda_P[[2]]$auc,rocICS_qda_P[[3]]$auc,rocICS_qda_P[[4]]$auc,
                 rocICS_qda_P[[5]]$auc,rocICS_qda_P[[6]]$auc,rocICS_qda_P[[7]]$auc)
ICS_qda_P_ciL<-c(rocICS_qda_P[[1]]$ci[1],rocICS_qda_P[[2]]$ci[1],rocICS_qda_P[[3]]$ci[1],rocICS_qda_P[[4]]$ci[1],
                 rocICS_qda_P[[5]]$ci[1],rocICS_qda_P[[6]]$ci[1],rocICS_qda_P[[7]]$ci[1])
ICS_qda_P_ciH<-c(rocICS_qda_P[[1]]$ci[3],rocICS_qda_P[[2]]$ci[3],rocICS_qda_P[[3]]$ci[3],rocICS_qda_P[[4]]$ci[3],
                 rocICS_qda_P[[5]]$ci[3],rocICS_qda_P[[6]]$ci[3],rocICS_qda_P[[7]]$ci[3])

###JUST PASS
#PS layer
predPS_jp_qda_P<-jackPredQDA(centPS,outcome = "justpass",predictors = c("cohort","PageRank"))
rocPS_jp_qda_P<-list()
rocPS_jp_qda_P[[1]]<-roc(predPS_jp_qda_P$justpass,as.numeric(predPS_jp_qda_P$Week1),auc=T,ci=T)
rocPS_jp_qda_P[[2]]<-roc(predPS_jp_qda_P$justpass,as.numeric(predPS_jp_qda_P$Week2),auc=T,ci=T)
rocPS_jp_qda_P[[3]]<-roc(predPS_jp_qda_P$justpass,as.numeric(predPS_jp_qda_P$Week3),auc=T,ci=T)
rocPS_jp_qda_P[[4]]<-roc(predPS_jp_qda_P$justpass,as.numeric(predPS_jp_qda_P$Week4),auc=T,ci=T)
rocPS_jp_qda_P[[5]]<-roc(predPS_jp_qda_P$justpass,as.numeric(predPS_jp_qda_P$Week5),auc=T,ci=T)
rocPS_jp_qda_P[[6]]<-roc(predPS_jp_qda_P$justpass,as.numeric(predPS_jp_qda_P$Week6),auc=T,ci=T)
rocPS_jp_qda_P[[7]]<-roc(predPS_jp_qda_P$justpass,as.numeric(predPS_jp_qda_P$Week7),auc=T,ci=T)

PS_jp_qda_P_auc<-c(rocPS_jp_qda_P[[1]]$auc,rocPS_jp_qda_P[[2]]$auc,rocPS_jp_qda_P[[3]]$auc,rocPS_jp_qda_P[[4]]$auc,
                   rocPS_jp_qda_P[[5]]$auc,rocPS_jp_qda_P[[6]]$auc,rocPS_jp_qda_P[[7]]$auc)
PS_jp_qda_P_ciL<-c(rocPS_jp_qda_P[[1]]$ci[1],rocPS_jp_qda_P[[2]]$ci[1],rocPS_jp_qda_P[[3]]$ci[1],rocPS_jp_qda_P[[4]]$ci[1],
                   rocPS_jp_qda_P[[5]]$ci[1],rocPS_jp_qda_P[[6]]$ci[1],rocPS_jp_qda_P[[7]]$ci[1])
PS_jp_qda_P_ciH<-c(rocPS_jp_qda_P[[1]]$ci[3],rocPS_jp_qda_P[[2]]$ci[3],rocPS_jp_qda_P[[3]]$ci[3],rocPS_jp_qda_P[[4]]$ci[3],
                   rocPS_jp_qda_P[[5]]$ci[3],rocPS_jp_qda_P[[6]]$ci[3],rocPS_jp_qda_P[[7]]$ci[3])
#CD layer
predCD_jp_qda_P<-jackPredQDA(centCD, outcome = "justpass",predictors = c("cohort","PageRank"))
rocCD_jp_qda_P<-list()
rocCD_jp_qda_P[[1]]<-roc(predCD_jp_qda_P$justpass,as.numeric(predCD_jp_qda_P$Week1),auc=T,ci=T)
rocCD_jp_qda_P[[2]]<-roc(predCD_jp_qda_P$justpass,as.numeric(predCD_jp_qda_P$Week2),auc=T,ci=T)
rocCD_jp_qda_P[[3]]<-roc(predCD_jp_qda_P$justpass,as.numeric(predCD_jp_qda_P$Week3),auc=T,ci=T)
rocCD_jp_qda_P[[4]]<-roc(predCD_jp_qda_P$justpass,as.numeric(predCD_jp_qda_P$Week4),auc=T,ci=T)
rocCD_jp_qda_P[[5]]<-roc(predCD_jp_qda_P$justpass,as.numeric(predCD_jp_qda_P$Week5),auc=T,ci=T)
rocCD_jp_qda_P[[6]]<-roc(predCD_jp_qda_P$justpass,as.numeric(predCD_jp_qda_P$Week6),auc=T,ci=T)
rocCD_jp_qda_P[[7]]<-roc(predCD_jp_qda_P$justpass,as.numeric(predCD_jp_qda_P$Week7),auc=T,ci=T)
CD_jp_qda_P_auc<-c(rocCD_jp_qda_P[[1]]$auc,rocCD_jp_qda_P[[2]]$auc,rocCD_jp_qda_P[[3]]$auc,rocCD_jp_qda_P[[4]]$auc,
                   rocCD_jp_qda_P[[5]]$auc,rocCD_jp_qda_P[[6]]$auc,rocCD_jp_qda_P[[7]]$auc)
CD_jp_qda_P_ciL<-c(rocCD_jp_qda_P[[1]]$ci[1],rocCD_jp_qda_P[[2]]$ci[1],rocCD_jp_qda_P[[3]]$ci[1],rocCD_jp_qda_P[[4]]$ci[1],
                   rocCD_jp_qda_P[[5]]$ci[1],rocCD_jp_qda_P[[6]]$ci[1],rocCD_jp_qda_P[[7]]$ci[1])
CD_jp_qda_P_ciH<-c(rocCD_jp_qda_P[[1]]$ci[3],rocCD_jp_qda_P[[2]]$ci[3],rocCD_jp_qda_P[[3]]$ci[3],rocCD_jp_qda_P[[4]]$ci[3],
                   rocCD_jp_qda_P[[5]]$ci[3],rocCD_jp_qda_P[[6]]$ci[3],rocCD_jp_qda_P[[7]]$ci[3])

#ICS layer
predICS_jp_qda_P<-jackPredQDA(centICS, outcome = "justpass",predictors = c("cohort","PageRank"))
rocICS_jp_qda_P<-list()
rocICS_jp_qda_P[[1]]<-roc(predICS_jp_qda_P$justpass,as.numeric(predICS_jp_qda_P$Week1),auc=T,ci=T)
rocICS_jp_qda_P[[2]]<-roc(predICS_jp_qda_P$justpass,as.numeric(predICS_jp_qda_P$Week2),auc=T,ci=T)
rocICS_jp_qda_P[[3]]<-roc(predICS_jp_qda_P$justpass,as.numeric(predICS_jp_qda_P$Week3),auc=T,ci=T)
rocICS_jp_qda_P[[4]]<-roc(predICS_jp_qda_P$justpass,as.numeric(predICS_jp_qda_P$Week4),auc=T,ci=T)
rocICS_jp_qda_P[[5]]<-roc(predICS_jp_qda_P$justpass,as.numeric(predICS_jp_qda_P$Week5),auc=T,ci=T)
rocICS_jp_qda_P[[6]]<-roc(predICS_jp_qda_P$justpass,as.numeric(predICS_jp_qda_P$Week6),auc=T,ci=T)
rocICS_jp_qda_P[[7]]<-roc(predICS_jp_qda_P$justpass,as.numeric(predICS_jp_qda_P$Week7),auc=T,ci=T)
ICS_jp_qda_P_auc<-c(rocICS_jp_qda_P[[1]]$auc,rocICS_jp_qda_P[[2]]$auc,rocICS_jp_qda_P[[3]]$auc,rocICS_jp_qda_P[[4]]$auc,
                    rocICS_jp_qda_P[[5]]$auc,rocICS_jp_qda_P[[6]]$auc,rocICS_jp_qda_P[[7]]$auc)
ICS_jp_qda_P_ciL<-c(rocICS_jp_qda_P[[1]]$ci[1],rocICS_jp_qda_P[[2]]$ci[1],rocICS_jp_qda_P[[3]]$ci[1],rocICS_jp_qda_P[[4]]$ci[1],
                    rocICS_jp_qda_P[[5]]$ci[1],rocICS_jp_qda_P[[6]]$ci[1],rocICS_jp_qda_P[[7]]$ci[1])
ICS_jp_qda_P_ciH<-c(rocICS_jp_qda_P[[1]]$ci[3],rocICS_jp_qda_P[[2]]$ci[3],rocICS_jp_qda_P[[3]]$ci[3],rocICS_jp_qda_P[[4]]$ci[3],
                    rocICS_jp_qda_P[[5]]$ci[3],rocICS_jp_qda_P[[6]]$ci[3],rocICS_jp_qda_P[[7]]$ci[3])



####QDA PLOTS####
####Mean over all models####
##pass-fail##
PS_qda_RegAll<-data.frame(PS_qda_PTH_auc,PS_qda_PT_auc,PS_qda_TH_auc,PS_qda_PH_auc,PS_qda_P_auc,PS_qda_T_auc,PS_qda_H_auc)
PS_qda_RegAllciH<-data.frame(PS_qda_PTH_ciH,PS_qda_PT_ciH,PS_qda_TH_ciH,PS_qda_PH_ciH,PS_qda_P_ciH,PS_qda_T_ciH,PS_qda_H_ciH)
PS_qda_RegAllciL<-data.frame(PS_qda_PTH_ciL,PS_qda_PT_ciL,PS_qda_TH_ciL,PS_qda_PH_ciL,PS_qda_P_ciL,PS_qda_T_ciL,PS_qda_H_ciL)

PS_qda_RegSD<-sqrt(166)*(PS_qda_RegAllciH-PS_qda_RegAllciL)/3.92
PS_qda_RegWeigths<-1/PS_qda_RegSD^2
PS_qda_RegWM<-rowSums(PS_qda_RegAll*PS_qda_RegWeigths)/rowSums(PS_qda_RegWeigths)
PS_qda_RegSEWM<-sqrt(1/rowSums(PS_qda_RegWeigths))

CD_qda_RegAll<-data.frame(CD_qda_PTH_auc,CD_qda_PT_auc,CD_qda_TH_auc,CD_qda_PH_auc,CD_qda_P_auc,CD_qda_T_auc,CD_qda_H_auc)
CD_qda_RegAllciH<-data.frame(CD_qda_PTH_ciH,CD_qda_PT_ciH,CD_qda_TH_ciH,CD_qda_PH_ciH,CD_qda_P_ciH,CD_qda_T_ciH,CD_qda_H_ciH)
CD_qda_RegAllciL<-data.frame(CD_qda_PTH_ciL,CD_qda_PT_ciL,CD_qda_TH_ciL,CD_qda_PH_ciL,CD_qda_P_ciL,CD_qda_T_ciL,CD_qda_H_ciL)

CD_qda_RegSD<-sqrt(166)*(CD_qda_RegAllciH-CD_qda_RegAllciL)/3.92
CD_qda_RegWeigths<-1/CD_qda_RegSD^2
CD_qda_RegWM<-rowSums(CD_qda_RegAll*CD_qda_RegWeigths)/rowSums(CD_qda_RegWeigths)
CD_qda_RegSEWM<-sqrt(1/rowSums(CD_qda_RegWeigths))

ICS_qda_RegAll<-data.frame(ICS_qda_PTH_auc,ICS_qda_PT_auc,ICS_qda_TH_auc,ICS_qda_PH_auc,ICS_qda_P_auc,ICS_qda_T_auc,ICS_qda_H_auc)
ICS_qda_RegAllciH<-data.frame(ICS_qda_PTH_ciH,ICS_qda_PT_ciH,ICS_qda_TH_ciH,ICS_qda_PH_ciH,ICS_qda_P_ciH,ICS_qda_T_ciH,ICS_qda_H_ciH)
ICS_qda_RegAllciL<-data.frame(ICS_qda_PTH_ciL,ICS_qda_PT_ciL,ICS_qda_TH_ciL,ICS_qda_PH_ciL,ICS_qda_P_ciL,ICS_qda_T_ciL,ICS_qda_H_ciL)

ICS_qda_RegSD<-sqrt(166)*(ICS_qda_RegAllciH-ICS_qda_RegAllciL)/3.92
ICS_qda_RegWeigths<-1/ICS_qda_RegSD^2
ICS_qda_RegWM<-rowSums(ICS_qda_RegAll*ICS_qda_RegWeigths)/rowSums(ICS_qda_RegWeigths)
ICS_qda_RegSEWM<-sqrt(1/rowSums(ICS_qda_RegWeigths))

##just pass-fail##
PS_jp_qda_RegAll<-data.frame(PS_jp_qda_PTH_auc,PS_jp_qda_PT_auc,PS_jp_qda_TH_auc,PS_jp_qda_PH_auc,PS_jp_qda_P_auc,PS_jp_qda_T_auc,PS_jp_qda_H_auc)
PS_jp_qda_RegAllciH<-data.frame(PS_jp_qda_PTH_ciH,PS_jp_qda_PT_ciH,PS_jp_qda_TH_ciH,PS_jp_qda_PH_ciH,PS_jp_qda_P_ciH,PS_jp_qda_T_ciH,PS_jp_qda_H_ciH)
PS_jp_qda_RegAllciL<-data.frame(PS_jp_qda_PTH_ciL,PS_jp_qda_PT_ciL,PS_jp_qda_TH_ciL,PS_jp_qda_PH_ciL,PS_jp_qda_P_ciL,PS_jp_qda_T_ciL,PS_jp_qda_H_ciL)

PS_jp_qda_RegSD<-sqrt(67)*(PS_jp_qda_RegAllciH-PS_jp_qda_RegAllciL)/3.92
PS_jp_qda_RegWeigths<-1/PS_jp_qda_RegSD^2
PS_jp_qda_RegWM<-rowSums(PS_jp_qda_RegAll*PS_jp_qda_RegWeigths)/rowSums(PS_jp_qda_RegWeigths)
PS_jp_qda_RegSEWM<-sqrt(1/rowSums(PS_jp_qda_RegWeigths))

CD_jp_qda_RegAll<-data.frame(CD_jp_qda_PH_auc,CD_jp_qda_P_auc,CD_jp_qda_H_auc)
CD_jp_qda_RegAllciH<-data.frame(CD_jp_qda_PH_ciH,CD_jp_qda_P_ciH,CD_jp_qda_H_ciH)
CD_jp_qda_RegAllciL<-data.frame(CD_jp_qda_PH_ciL,CD_jp_qda_P_ciL,CD_jp_qda_H_ciL)

CD_jp_qda_RegSD<-sqrt(67)*(CD_jp_qda_RegAllciH-CD_jp_qda_RegAllciL)/3.92
CD_jp_qda_RegWeigths<-1/CD_jp_qda_RegSD^2 #Zero standard deviations become infinite here. They are probably a sign of an outlier.
CD_jp_qda_RegWeigths[!is.finite(as.matrix(CD_jp_qda_RegWeigths))]<-NA #Correction for zero standard deviations.
CD_jp_qda_RegWM<-rowSums(CD_jp_qda_RegAll*CD_jp_qda_RegWeigths,na.rm = T)/rowSums(CD_jp_qda_RegWeigths,na.rm = T)
CD_jp_qda_RegSEWM<-sqrt(1/rowSums(CD_jp_qda_RegWeigths,na.rm=T))

ICS_jp_qda_RegAll<-data.frame(ICS_jp_qda_PTH_auc,ICS_jp_qda_PT_auc,ICS_jp_qda_TH_auc,ICS_jp_qda_PH_auc,ICS_jp_qda_P_auc,ICS_jp_qda_T_auc,ICS_jp_qda_H_auc)
ICS_jp_qda_RegAllciH<-data.frame(ICS_jp_qda_PTH_ciH,ICS_jp_qda_PT_ciH,ICS_jp_qda_TH_ciH,ICS_jp_qda_PH_ciH,ICS_jp_qda_P_ciH,ICS_jp_qda_T_ciH,ICS_jp_qda_H_ciH)
ICS_jp_qda_RegAllciL<-data.frame(ICS_jp_qda_PTH_ciL,ICS_jp_qda_PT_ciL,ICS_jp_qda_TH_ciL,ICS_jp_qda_PH_ciL,ICS_jp_qda_P_ciL,ICS_jp_qda_T_ciL,ICS_jp_qda_H_ciL)

ICS_jp_qda_RegSD<-sqrt(67)*(ICS_jp_qda_RegAllciH-ICS_jp_qda_RegAllciL)/3.92
ICS_jp_qda_RegWeigths<-1/ICS_jp_qda_RegSD^2
ICS_jp_qda_RegWM<-rowSums(ICS_jp_qda_RegAll*ICS_jp_qda_RegWeigths)/rowSums(ICS_jp_qda_RegWeigths)
ICS_jp_qda_RegSEWM<-sqrt(1/rowSums(ICS_jp_qda_RegWeigths))
####PLOTTING####
pdf(file = "plots/ROC_AUC_plots/AUC04_NetworkMeasuresCohort_qda_weightedMeans.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,2))
x<-c(1:7)
plot(x, PS_qda_RegWM,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="QDA Pass-fail", sub="Weighted mean of all centrality models",type="b"
)
lines(x+0.1,CD_qda_RegWM,pch=1,type="b",col="darkred")
lines(x-0.1,ICS_qda_RegWM,pch=2,type="b",col="darkblue")
# hack: we draw arrows but with very special "arrowheads"

arrows(x, PS_qda_RegWM-1.96*PS_qda_RegSEWM/sqrt(166), x, PS_qda_RegWM+1.96*PS_qda_RegSEWM/sqrt(166), length=0.05, angle=90, code=3)
arrows(x+0.1, CD_qda_RegWM-1.96*CD_qda_RegSEWM/sqrt(166), x+0.1, CD_qda_RegWM+1.96*CD_qda_RegSEWM/sqrt(166), length=0.05, angle=90, code=3,col="darkred")
arrows(x-0.1, ICS_qda_RegWM-1.96*ICS_qda_RegSEWM/sqrt(166), x-0.1, ICS_qda_RegWM+1.96*ICS_qda_RegSEWM/sqrt(166), length=0.05, angle=90, code=3,col="darkblue")
legend(3, 0.3, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","darkred", "darkblue"), pch=c(19,1,2),lty=1, cex=0.8)
abline(h=lazy)

x<-c(1:7)
plot(x, PS_jp_qda_RegWM,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="QDA Just pass-fail", sub="Weighted mean of all centrality models",type="b"
)
lines(x+0.1,CD_jp_qda_RegWM,pch=1,type="b",col="darkred")
lines(x-0.1,ICS_jp_qda_RegWM,pch=2,type="b",col="darkblue")
# hack: we draw arrows but with very special "arrowheads"

arrows(x, PS_jp_qda_RegWM-1.96*PS_jp_qda_RegSEWM/sqrt(166), x, PS_jp_qda_RegWM+1.96*PS_jp_qda_RegSEWM/sqrt(166), length=0.05, angle=90, code=3)
arrows(x+0.1, CD_jp_qda_RegWM-1.96*CD_jp_qda_RegSEWM/sqrt(166), x+0.1, CD_jp_qda_RegWM+1.96*CD_jp_qda_RegSEWM/sqrt(166), length=0.05, angle=90, code=3,col="darkred")
arrows(x-0.1, ICS_jp_qda_RegWM-1.96*ICS_jp_qda_RegSEWM/sqrt(166), x-0.1, ICS_jp_qda_RegWM+1.96*ICS_jp_qda_RegSEWM/sqrt(166), length=0.05, angle=90, code=3,col="darkblue")
legend(3, 0.3, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","darkred", "darkblue"), pch=c(19,1,2),lty=1, cex=0.8)
abline(h=lazy_jp)
dev.off()
####ROC curves####
pdf(file = "plots/ROC_AUC_plots/ROC04_qda_pf.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 14) # The height of the plot in inches
par(mfrow=c(4,2))
plot(rocCD_qda_PTH[[1]], main="PTH")
lines(rocCD_qda_PTH[[2]],col="yellow")
lines(rocCD_qda_PTH[[3]],col="blue")
lines(rocCD_qda_PTH[[4]],col="magenta")
lines(rocCD_qda_PTH[[5]],col="red")
lines(rocCD_qda_PTH[[6]],col="green")
lines(rocCD_qda_PTH[[7]],col="purple")

plot(rocCD_qda_PT[[1]], main="PT")
lines(rocCD_qda_PT[[2]],col="yellow")
lines(rocCD_qda_PT[[3]],col="blue")
lines(rocCD_qda_PT[[4]],col="magenta")
lines(rocCD_qda_PT[[5]],col="red")
lines(rocCD_qda_PT[[6]],col="green")
lines(rocCD_qda_PT[[7]],col="purple")

plot(rocCD_qda_PH[[1]], main="PH")
lines(rocCD_qda_PH[[2]],col="yellow")
lines(rocCD_qda_PH[[3]],col="blue")
lines(rocCD_qda_PH[[4]],col="magenta")
lines(rocCD_qda_PH[[5]],col="red")
lines(rocCD_qda_PH[[6]],col="green")
lines(rocCD_qda_PH[[7]],col="purple")

plot(rocCD_qda_TH[[1]], main="TH")
lines(rocCD_qda_TH[[2]],col="yellow")
lines(rocCD_qda_TH[[3]],col="blue")
lines(rocCD_qda_TH[[4]],col="magenta")
lines(rocCD_qda_TH[[5]],col="red")
lines(rocCD_qda_TH[[6]],col="green")
lines(rocCD_qda_TH[[7]],col="purple")

plot(rocCD_qda_P[[1]], main="P")
lines(rocCD_qda_P[[2]],col="yellow")
lines(rocCD_qda_P[[3]],col="blue")
lines(rocCD_qda_P[[4]],col="magenta")
lines(rocCD_qda_P[[5]],col="red")
lines(rocCD_qda_P[[6]],col="green")
lines(rocCD_qda_P[[7]],col="purple")

plot(rocCD_qda_T[[1]], main="T")
lines(rocCD_qda_T[[2]],col="yellow")
lines(rocCD_qda_T[[3]],col="blue")
lines(rocCD_qda_T[[4]],col="magenta")
lines(rocCD_qda_T[[5]],col="red")
lines(rocCD_qda_T[[6]],col="green")
lines(rocCD_qda_T[[7]],col="purple")

plot(rocCD_qda_H[[1]], main="H")
lines(rocCD_qda_H[[2]],col="yellow")
lines(rocCD_qda_H[[3]],col="blue")
lines(rocCD_qda_H[[4]],col="magenta")
lines(rocCD_qda_H[[5]],col="red")
lines(rocCD_qda_H[[6]],col="green")
lines(rocCD_qda_H[[7]],col="purple")

plot.new()
legend(0,1,c("week 1","week 2","week 3","week 4"),
       col=c("black","yellow", "blue","magenta"),lty=1, cex=0.8)
legend(0.5,1,c("week 5","week 6","week 7"),
       col=c("red","green","purple"),lty=1, cex=0.8)

dev.off()
pdf(file = "plots/ROC_AUC_plots/ROC04_qda_jpf.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 14) # The height of the plot in inches
par(mfrow=c(4,2))
#plot(rocCD_jp_qda_PTH[[1]])
#lines(rocCD_jp_qda_PTH[[2]],col="yellow")
#lines(rocCD_jp_qda_PTH[[3]],col="blue")
#lines(rocCD_jp_qda_PTH[[4]],col="magenta")
#lines(rocCD_jp_qda_PTH[[5]],col="red")
#lines(rocCD_jp_qda_PTH[[6]],col="green")
#lines(rocCD_jp_qda_PTH[[7]],col="purple")

#plot(rocCD_jp_qda_PT[[1]])
#lines(rocCD_jp_qda_PT[[2]],col="yellow")
#lines(rocCD_jp_qda_PT[[3]],col="blue")
#lines(rocCD_jp_qda_PT[[4]],col="magenta")
#lines(rocCD_jp_qda_PT[[5]],col="red")
#lines(rocCD_jp_qda_PT[[6]],col="green")
#lines(rocCD_jp_qda_PT[[7]],col="purple")

plot(rocCD_jp_qda_PH[[1]])
lines(rocCD_jp_qda_PH[[2]],col="yellow")
lines(rocCD_jp_qda_PH[[3]],col="blue")
lines(rocCD_jp_qda_PH[[4]],col="magenta")
lines(rocCD_jp_qda_PH[[5]],col="red")
lines(rocCD_jp_qda_PH[[6]],col="green")
lines(rocCD_jp_qda_PH[[7]],col="purple")

#plot(rocCD_jp_qda_TH[[1]])
#lines(rocCD_jp_qda_TH[[2]],col="yellow")
#lines(rocCD_jp_qda_TH[[3]],col="blue")
#lines(rocCD_jp_qda_TH[[4]],col="magenta")
#lines(rocCD_jp_qda_TH[[5]],col="red")
#lines(rocCD_jp_qda_TH[[6]],col="green")
#lines(rocCD_jp_qda_TH[[7]],col="purple")

plot(rocCD_jp_qda_P[[1]])
lines(rocCD_jp_qda_P[[2]],col="yellow")
lines(rocCD_jp_qda_P[[3]],col="blue")
lines(rocCD_jp_qda_P[[4]],col="magenta")
lines(rocCD_jp_qda_P[[5]],col="red")
lines(rocCD_jp_qda_P[[6]],col="green")
lines(rocCD_jp_qda_P[[7]],col="purple")

#plot(rocCD_jp_qda_T[[1]])
#lines(rocCD_jp_qda_T[[2]],col="yellow")
#lines(rocCD_jp_qda_T[[3]],col="blue")
#lines(rocCD_jp_qda_T[[4]],col="magenta")
#lines(rocCD_jp_qda_T[[5]],col="red")
#lines(rocCD_jp_qda_T[[6]],col="green")
#lines(rocCD_jp_qda_T[[7]],col="purple")

plot(rocCD_jp_qda_H[[1]])
lines(rocCD_jp_qda_H[[2]],col="yellow")
lines(rocCD_jp_qda_H[[3]],col="blue")
lines(rocCD_jp_qda_H[[4]],col="magenta")
lines(rocCD_jp_qda_H[[5]],col="red")
lines(rocCD_jp_qda_H[[6]],col="green")
lines(rocCD_jp_qda_H[[7]],col="purple")

plot.new()
legend(0,1,c("week 1","week 2","week 3","week 4"),
       col=c("black","yellow", "blue","magenta"),lty=1, cex=0.8)
legend(0.5,1,c("week 5","week 6","week 7"),
       col=c("red","green","purple"),lty=1, cex=0.8)

dev.off()


#######################################
#########K NEAREST NEIGHBORS###########
#######################################

######Pagerank Target Entropy Hide models######
ROC_PS_knn_PTH<-list()
for(i in 1:20){
  predPS_knn_PTH<-jackPredKNN(centPS,predictors = c("cohort","PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_knn_PTH$allpred)
  ROC_PS_knn_PTH[[i]]<-ROC
}

SRs<-lapply(ROC_PS_knn_PTH,"[",8) #we keep this stupid code, even though the internet gave me a better option later. Sigh
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxPTH_PS_pf<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
PS_knn_PTH_means<-rowMeans(SR_m)
PS_knn_PTH_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_PTH_PS.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PTH PS")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)
x<-c(1:7)
plot(x,PS_knn_PTH_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN PTH PS")
arrows(y, PS_knn_PTH_means-1.96*PS_knn_PTH_sds/sqrt(6), y, PS_knn_PTH_means+1.96*PS_knn_PTH_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

plot(x,ROC_PS_knn_PTH[[maxPTH_PS_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week", main=paste("KNN, N=",maxPTH_PS_pf,"PTH PS",sep=" "),
     sub="Pass/fail (n=166), PR-TE-H",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_CD_knn_PTH<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("cohort","PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_PTH[[i]]<-ROC
}
SRs<-lapply(ROC_CD_knn_PTH,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
  
}
maxPTH_CD_pf<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
CD_knn_PTH_means<-rowMeans(SR_m)
CD_knn_PTH_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_PTH_CD.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PTH CD")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

x<-c(1:7)
plot(x,CD_knn_PTH_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN PTH PS")
arrows(y, CD_knn_PTH_means-1.96*CD_knn_PTH_sds/sqrt(6), y, CD_knn_PTH_means+1.96*CD_knn_PTH_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

plot(x,ROC_CD_knn_PTH[[maxPTH_CD_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week", main=paste("KNN, N=",maxPTH_CD_pf,"PTH CD",sep=" "),
     sub="Pass/fail (n=166), PR-TE-H",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_ICS_knn_PTH<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("cohort","PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_PTH[[i]]<-ROC
}

SRs<-lapply(ROC_ICS_knn_PTH,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxPTH_ICS_pf<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
ICS_knn_PTH_means<-rowMeans(SR_m)
ICS_knn_PTH_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_PTH_ICS.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PTH ICS")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

x<-c(1:7)
plot(x,ICS_knn_PTH_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN PTH PS")
arrows(y, ICS_knn_PTH_means-1.96*ICS_knn_PTH_sds/sqrt(6), y, ICS_knn_PTH_means+1.96*ICS_knn_PTH_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)
plot(x,ROC_ICS_knn_PTH[[maxPTH_ICS_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week",  main=paste("KNN, N=",maxPTH_ICS_pf,"PTH ICS",sep=" "),
     sub="Pass/fail (n=166), PR-TE-H",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_PS_justpass_knn_PTH<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("cohort","PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_PTH[[i]]<-ROC
}
SRs<-lapply(ROC_PS_justpass_knn_PTH,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxPTH_PS_jp<-which.max(means)
SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
PS_justpass_knn_PTH_means<-rowMeans(SR_m)
PS_justpass_knn_PTH_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_PTH_PS_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PTH PS",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,PS_justpass_knn_PTH_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN PTH PS")
arrows(y, PS_justpass_knn_PTH_means-1.96*PS_justpass_knn_PTH_sds/sqrt(6), y, PS_justpass_knn_PTH_means+1.96*PS_justpass_knn_PTH_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)
plot(x,ROC_PS_justpass_knn_PTH[[maxPTH_PS_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxPTH_PS_jp,"PTH PS",sep=" "),
     sub="Just Pass/fail (n=67), PR-TE-H",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()

ROC_CD_justpass_knn_PTH<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("cohort","PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_PTH[[i]]<-ROC
}
SRs<-lapply(ROC_CD_justpass_knn_PTH,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxPTH_CD_jp<-which.max(means)
SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
CD_justpass_knn_PTH_means<-rowMeans(SR_m)
CD_justpass_knn_PTH_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_PTH_CD_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PTH CD",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,CD_justpass_knn_PTH_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN PTH PS")
arrows(y, CD_justpass_knn_PTH_means-1.96*CD_justpass_knn_PTH_sds/sqrt(6), y, CD_justpass_knn_PTH_means+1.96*CD_justpass_knn_PTH_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

plot(x,ROC_CD_justpass_knn_PTH[[maxPTH_CD_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxPTH_PS_jp,"PTH CD",sep=" "),
     sub="Just Pass/fail (n=67), PR-TE-H",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()

ROC_ICS_justpass_knn_PTH<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("cohort","PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_PTH[[i]]<-ROC
}

SRs<-lapply(ROC_ICS_justpass_knn_PTH,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}

maxPTH_ICS_jp<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
ICS_justpass_knn_PTH_means<-rowMeans(SR_m)
ICS_justpass_knn_PTH_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_PTH_ICS_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PTH ICS",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,ICS_justpass_knn_PTH_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN PTH PS")
arrows(y, ICS_justpass_knn_PTH_means-1.96*ICS_justpass_knn_PTH_sds/sqrt(6), y, ICS_justpass_knn_PTH_means+1.96*ICS_justpass_knn_PTH_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

plot(x,ROC_ICS_justpass_knn_PTH[[maxPTH_ICS_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxPTH_ICS_jp,"PTH ICS",sep=" "),
     sub="Just Pass/fail (n=67), PR-TE-H",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()



######Pagerank Target Entropy models######
ROC_PS_knn_PT<-list()
for(i in 1:20){
  predPS_knn_PT<-jackPredKNN(centPS,predictors = c("cohort","PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predPS_knn_PT$allpred)
  ROC_PS_knn_PT[[i]]<-ROC
}

SRs<-lapply(ROC_PS_knn_PT,"[",8) #we keep this stupid code, even though the internet gave me a better option later. Sigh
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxPT_PS_pf<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
PS_knn_PT_means<-rowMeans(SR_m)
PS_knn_PT_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_PT_PS.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PT PS")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)
x<-c(1:7)
plot(x,PS_knn_PT_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN PT PS")
arrows(y, PS_knn_PT_means-1.96*PS_knn_PT_sds/sqrt(6), y, PS_knn_PT_means+1.96*PS_knn_PT_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

plot(x,ROC_PS_knn_PT[[maxPT_PS_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week", main=paste("KNN, N=",maxPT_PS_pf,"PT PS",sep=" "),
     sub="Pass/fail (n=166), PR-TE",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_CD_knn_PT<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("cohort","PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_PT[[i]]<-ROC
}
SRs<-lapply(ROC_CD_knn_PT,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
  
}
maxPT_CD_pf<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
CD_knn_PT_means<-rowMeans(SR_m)
CD_knn_PT_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_PT_CD.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PT CD")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

x<-c(1:7)
plot(x,CD_knn_PT_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN PT PS")
arrows(y, CD_knn_PT_means-1.96*CD_knn_PT_sds/sqrt(6), y, CD_knn_PT_means+1.96*CD_knn_PT_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

plot(x,ROC_CD_knn_PT[[maxPT_CD_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week", main=paste("KNN, N=",maxPT_CD_pf,"PT CD",sep=" "),
     sub="Pass/fail (n=166), PR-TE",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_ICS_knn_PT<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("cohort","PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_PT[[i]]<-ROC
}

SRs<-lapply(ROC_ICS_knn_PT,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxPT_ICS_pf<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
ICS_knn_PT_means<-rowMeans(SR_m)
ICS_knn_PT_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_PT_ICS.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PT ICS")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

x<-c(1:7)
plot(x,ICS_knn_PT_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN PT PS")
arrows(y, ICS_knn_PT_means-1.96*ICS_knn_PT_sds/sqrt(6), y, ICS_knn_PT_means+1.96*ICS_knn_PT_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)
plot(x,ROC_ICS_knn_PT[[maxPT_ICS_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week",  main=paste("KNN, N=",maxPT_ICS_pf,"PT ICS",sep=" "),
     sub="Pass/fail (n=166), PR-TE",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_PS_justpass_knn_PT<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("cohort","PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_PT[[i]]<-ROC
}
SRs<-lapply(ROC_PS_justpass_knn_PT,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxPT_PS_jp<-which.max(means)
SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
PS_justpass_knn_PT_means<-rowMeans(SR_m)
PS_justpass_knn_PT_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_PT_PS_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PT PS",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,PS_justpass_knn_PT_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN PT PS")
arrows(y, PS_justpass_knn_PT_means-1.96*PS_justpass_knn_PT_sds/sqrt(6), y, PS_justpass_knn_PT_means+1.96*PS_justpass_knn_PT_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)
plot(x,ROC_PS_justpass_knn_PT[[maxPT_PS_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxPT_PS_jp,"PT PS",sep=" "),
     sub="Just Pass/fail (n=67), PR-TE",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()

ROC_CD_justpass_knn_PT<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("cohort","PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_PT[[i]]<-ROC
}
SRs<-lapply(ROC_CD_justpass_knn_PT,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxPT_CD_jp<-which.max(means)
SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
CD_justpass_knn_PT_means<-rowMeans(SR_m)
CD_justpass_knn_PT_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_PT_CD_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PT CD",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,CD_justpass_knn_PT_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN PT PS")
arrows(y, CD_justpass_knn_PT_means-1.96*CD_justpass_knn_PT_sds/sqrt(6), y, CD_justpass_knn_PT_means+1.96*CD_justpass_knn_PT_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

plot(x,ROC_CD_justpass_knn_PT[[maxPT_CD_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxPT_PS_jp,"PT CD",sep=" "),
     sub="Just Pass/fail (n=67), PR-TE",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()

ROC_ICS_justpass_knn_PT<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("cohort","PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_PT[[i]]<-ROC
}

SRs<-lapply(ROC_ICS_justpass_knn_PT,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}

maxPT_ICS_jp<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
ICS_justpass_knn_PT_means<-rowMeans(SR_m)
ICS_justpass_knn_PT_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_PT_ICS_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PT ICS",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,ICS_justpass_knn_PT_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN PT PS")
arrows(y, ICS_justpass_knn_PT_means-1.96*ICS_justpass_knn_PT_sds/sqrt(6), y, ICS_justpass_knn_PT_means+1.96*ICS_justpass_knn_PT_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

plot(x,ROC_ICS_justpass_knn_PT[[maxPT_ICS_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxPT_ICS_jp,"PT ICS",sep=" "),
     sub="Just Pass/fail (n=67), PR-TE",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()




######Target Entropy Hide models######
ROC_PS_knn_TH<-list()
for(i in 1:20){
  predPS_knn_TH<-jackPredKNN(centPS,predictors = c("cohort","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_knn_TH$allpred)
  ROC_PS_knn_TH[[i]]<-ROC
}

SRs<-lapply(ROC_PS_knn_TH,"[",8) #we keep this stupid code, even though the internet gave me a better option later. Sigh
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxTH_PS_pf<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
PS_knn_TH_means<-rowMeans(SR_m)
PS_knn_TH_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_TH_PS.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN TH PS")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)
x<-c(1:7)
plot(x,PS_knn_TH_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN TH PS")
arrows(y, PS_knn_TH_means-1.96*PS_knn_TH_sds/sqrt(6), y, PS_knn_TH_means+1.96*PS_knn_TH_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

plot(x,ROC_PS_knn_TH[[maxTH_PS_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week", main=paste("KNN, N=",maxTH_PS_pf,"TH PS",sep=" "),
     sub="Pass/fail (n=166), TE-H",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_CD_knn_TH<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("cohort","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_TH[[i]]<-ROC
}
SRs<-lapply(ROC_CD_knn_TH,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
  
}
maxTH_CD_pf<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
CD_knn_TH_means<-rowMeans(SR_m)
CD_knn_TH_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_TH_CD.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN TH CD")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

x<-c(1:7)
plot(x,CD_knn_TH_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN TH PS")
arrows(y, CD_knn_TH_means-1.96*CD_knn_TH_sds/sqrt(6), y, CD_knn_TH_means+1.96*CD_knn_TH_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

plot(x,ROC_CD_knn_TH[[maxTH_CD_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week", main=paste("KNN, N=",maxTH_CD_pf,"TH CD",sep=" "),
     sub="Pass/fail (n=166), TE-H",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_ICS_knn_TH<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("cohort","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_TH[[i]]<-ROC
}

SRs<-lapply(ROC_ICS_knn_TH,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxTH_ICS_pf<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
ICS_knn_TH_means<-rowMeans(SR_m)
ICS_knn_TH_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_TH_ICS.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN TH ICS")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

x<-c(1:7)
plot(x,ICS_knn_TH_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN TH PS")
arrows(y, ICS_knn_TH_means-1.96*ICS_knn_TH_sds/sqrt(6), y, ICS_knn_TH_means+1.96*ICS_knn_TH_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)
plot(x,ROC_ICS_knn_TH[[maxTH_ICS_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week",  main=paste("KNN, N=",maxTH_ICS_pf,"TH ICS",sep=" "),
     sub="Pass/fail (n=166), TE-H",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_PS_justpass_knn_TH<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("cohort","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_TH[[i]]<-ROC
}
SRs<-lapply(ROC_PS_justpass_knn_TH,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxTH_PS_jp<-which.max(means)
SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
PS_justpass_knn_TH_means<-rowMeans(SR_m)
PS_justpass_knn_TH_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_TH_PS_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN TH PS",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,PS_justpass_knn_TH_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN TH PS")
arrows(y, PS_justpass_knn_TH_means-1.96*PS_justpass_knn_TH_sds/sqrt(6), y, PS_justpass_knn_TH_means+1.96*PS_justpass_knn_TH_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)
plot(x,ROC_PS_justpass_knn_TH[[maxTH_PS_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxTH_PS_jp,"TH PS",sep=" "),
     sub="Just Pass/fail (n=67), TE-H",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()

ROC_CD_justpass_knn_TH<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("cohort","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_TH[[i]]<-ROC
}
SRs<-lapply(ROC_CD_justpass_knn_TH,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxTH_CD_jp<-which.max(means)
SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
CD_justpass_knn_TH_means<-rowMeans(SR_m)
CD_justpass_knn_TH_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_TH_CD_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN TH CD",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,CD_justpass_knn_TH_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN TH PS")
arrows(y, CD_justpass_knn_TH_means-1.96*CD_justpass_knn_TH_sds/sqrt(6), y, CD_justpass_knn_TH_means+1.96*CD_justpass_knn_TH_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

plot(x,ROC_CD_justpass_knn_TH[[maxTH_CD_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxTH_PS_jp,"TH CD",sep=" "),
     sub="Just Pass/fail (n=67), TE-H",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()

ROC_ICS_justpass_knn_TH<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("cohort","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_TH[[i]]<-ROC
}

SRs<-lapply(ROC_ICS_justpass_knn_TH,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}

maxTH_ICS_jp<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
ICS_justpass_knn_TH_means<-rowMeans(SR_m)
ICS_justpass_knn_TH_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_TH_ICS_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN TH ICS",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,ICS_justpass_knn_TH_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN TH PS")
arrows(y, ICS_justpass_knn_TH_means-1.96*ICS_justpass_knn_TH_sds/sqrt(6), y, ICS_justpass_knn_TH_means+1.96*ICS_justpass_knn_TH_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

plot(x,ROC_ICS_justpass_knn_TH[[maxTH_ICS_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxTH_ICS_jp,"TH ICS",sep=" "),
     sub="Just Pass/fail (n=67), TE-H",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()



######Pagerank Hide models######
ROC_PS_knn_PH<-list()
for(i in 1:20){
  predPS_knn_PH<-jackPredKNN(centPS,predictors = c("cohort","PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_knn_PH$allpred)
  ROC_PS_knn_PH[[i]]<-ROC
}

SRs<-lapply(ROC_PS_knn_PH,"[",8) #we keep this stupid code, even though the internet gave me a better option later. Sigh
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxPH_PS_pf<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
PS_knn_PH_means<-rowMeans(SR_m)
PS_knn_PH_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_PH_PS.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PH PS")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)
x<-c(1:7)
plot(x,PS_knn_PH_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN PH PS")
arrows(y, PS_knn_PH_means-1.96*PS_knn_PH_sds/sqrt(6), y, PS_knn_PH_means+1.96*PS_knn_PH_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

plot(x,ROC_PS_knn_PH[[maxPH_PS_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week", main=paste("KNN, N=",maxPH_PS_pf,"PH PS",sep=" "),
     sub="Pass/fail (n=166), PR-H",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_CD_knn_PH<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("cohort","PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_PH[[i]]<-ROC
}
SRs<-lapply(ROC_CD_knn_PH,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
  
}
maxPH_CD_pf<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
CD_knn_PH_means<-rowMeans(SR_m)
CD_knn_PH_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_PH_CD.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PH CD")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

x<-c(1:7)
plot(x,CD_knn_PH_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN PH PS")
arrows(y, CD_knn_PH_means-1.96*CD_knn_PH_sds/sqrt(6), y, CD_knn_PH_means+1.96*CD_knn_PH_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

plot(x,ROC_CD_knn_PH[[maxPH_CD_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week", main=paste("KNN, N=",maxPH_CD_pf,"PH CD",sep=" "),
     sub="Pass/fail (n=166), PR-H",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_ICS_knn_PH<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("cohort","PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_PH[[i]]<-ROC
}

SRs<-lapply(ROC_ICS_knn_PH,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxPH_ICS_pf<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
ICS_knn_PH_means<-rowMeans(SR_m)
ICS_knn_PH_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_PH_ICS.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PH ICS")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

x<-c(1:7)
plot(x,ICS_knn_PH_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN PH PS")
arrows(y, ICS_knn_PH_means-1.96*ICS_knn_PH_sds/sqrt(6), y, ICS_knn_PH_means+1.96*ICS_knn_PH_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)
plot(x,ROC_ICS_knn_PH[[maxPH_ICS_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week",  main=paste("KNN, N=",maxPH_ICS_pf,"PH ICS",sep=" "),
     sub="Pass/fail (n=166), PR-H",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_PS_justpass_knn_PH<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("cohort","PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_PH[[i]]<-ROC
}
SRs<-lapply(ROC_PS_justpass_knn_PH,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxPH_PS_jp<-which.max(means)
SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
PS_justpass_knn_PH_means<-rowMeans(SR_m)
PS_justpass_knn_PH_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_PH_PS_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PH PS",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,PS_justpass_knn_PH_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN PH PS")
arrows(y, PS_justpass_knn_PH_means-1.96*PS_justpass_knn_PH_sds/sqrt(6), y, PS_justpass_knn_PH_means+1.96*PS_justpass_knn_PH_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)
plot(x,ROC_PS_justpass_knn_PH[[maxPH_PS_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxPH_PS_jp,"PH PS",sep=" "),
     sub="Just Pass/fail (n=67), PR-H",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()

ROC_CD_justpass_knn_PH<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("cohort","PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_PH[[i]]<-ROC
}
SRs<-lapply(ROC_CD_justpass_knn_PH,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxPH_CD_jp<-which.max(means)
SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
CD_justpass_knn_PH_means<-rowMeans(SR_m)
CD_justpass_knn_PH_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_PH_CD_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PH CD",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,CD_justpass_knn_PH_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN PH PS")
arrows(y, CD_justpass_knn_PH_means-1.96*CD_justpass_knn_PH_sds/sqrt(6), y, CD_justpass_knn_PH_means+1.96*CD_justpass_knn_PH_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

plot(x,ROC_CD_justpass_knn_PH[[maxPH_CD_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxPH_PS_jp,"PH CD",sep=" "),
     sub="Just Pass/fail (n=67), PR-H",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()

ROC_ICS_justpass_knn_PH<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("cohort","PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_PH[[i]]<-ROC
}

SRs<-lapply(ROC_ICS_justpass_knn_PH,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}

maxPH_ICS_jp<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
ICS_justpass_knn_PH_means<-rowMeans(SR_m)
ICS_justpass_knn_PH_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_PH_ICS_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PH ICS",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,ICS_justpass_knn_PH_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN PH PS")
arrows(y, ICS_justpass_knn_PH_means-1.96*ICS_justpass_knn_PH_sds/sqrt(6), y, ICS_justpass_knn_PH_means+1.96*ICS_justpass_knn_PH_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

plot(x,ROC_ICS_justpass_knn_PH[[maxPH_ICS_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxPH_ICS_jp,"PH ICS",sep=" "),
     sub="Just Pass/fail (n=67), PR-H",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()


######Pagerank models######
ROC_PS_knn_P<-list()
for(i in 1:20){
  predPS_knn_P<-jackPredKNN(centPS,predictors = c("cohort","PageRank"),nK = i)
  ROC<-ROCplusWeeks(predPS_knn_P$allpred)
  ROC_PS_knn_P[[i]]<-ROC
}

SRs<-lapply(ROC_PS_knn_P,"[",8) #we keep this stupid code, even though the internet gave me a better option later. Sigh
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxP_PS_pf<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
PS_knn_P_means<-rowMeans(SR_m)
PS_knn_P_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_P_PS.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN P PS")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)
x<-c(1:7)
plot(x,PS_knn_P_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN P PS")
arrows(y, PS_knn_P_means-1.96*PS_knn_P_sds/sqrt(6), y, PS_knn_P_means+1.96*PS_knn_P_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

plot(x,ROC_PS_knn_P[[maxP_PS_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week", main=paste("KNN, N=",maxP_PS_pf,"P PS",sep=" "),
     sub="Pass/fail (n=166), PR",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_CD_knn_P<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("cohort","PageRank"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_P[[i]]<-ROC
}
SRs<-lapply(ROC_CD_knn_P,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
  
}
maxP_CD_pf<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
CD_knn_P_means<-rowMeans(SR_m)
CD_knn_P_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_P_CD.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN P CD")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

x<-c(1:7)
plot(x,CD_knn_P_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN P PS")
arrows(y, CD_knn_P_means-1.96*CD_knn_P_sds/sqrt(6), y, CD_knn_P_means+1.96*CD_knn_P_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

plot(x,ROC_CD_knn_P[[maxP_CD_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week", main=paste("KNN, N=",maxP_CD_pf,"P CD",sep=" "),
     sub="Pass/fail (n=166), PR",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_ICS_knn_P<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("cohort","PageRank"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_P[[i]]<-ROC
}

SRs<-lapply(ROC_ICS_knn_P,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxP_ICS_pf<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
ICS_knn_P_means<-rowMeans(SR_m)
ICS_knn_P_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_P_ICS.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN P ICS")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

x<-c(1:7)
plot(x,ICS_knn_P_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN P PS")
arrows(y, ICS_knn_P_means-1.96*ICS_knn_P_sds/sqrt(6), y, ICS_knn_P_means+1.96*ICS_knn_P_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)
plot(x,ROC_ICS_knn_P[[maxP_ICS_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week",  main=paste("KNN, N=",maxP_ICS_pf,"P ICS",sep=" "),
     sub="Pass/fail (n=166), PR",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_PS_justpass_knn_P<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("cohort","PageRank"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_P[[i]]<-ROC
}
SRs<-lapply(ROC_PS_justpass_knn_P,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxP_PS_jp<-which.max(means)
SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
PS_justpass_knn_P_means<-rowMeans(SR_m)
PS_justpass_knn_P_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_P_PS_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN P PS",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,PS_justpass_knn_P_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN P PS")
arrows(y, PS_justpass_knn_P_means-1.96*PS_justpass_knn_P_sds/sqrt(6), y, PS_justpass_knn_P_means+1.96*PS_justpass_knn_P_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)
plot(x,ROC_PS_justpass_knn_P[[maxP_PS_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxP_PS_jp,"P PS",sep=" "),
     sub="Just Pass/fail (n=67), PR",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()

ROC_CD_justpass_knn_P<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("cohort","PageRank"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_P[[i]]<-ROC
}
SRs<-lapply(ROC_CD_justpass_knn_P,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxP_CD_jp<-which.max(means)
SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
CD_justpass_knn_P_means<-rowMeans(SR_m)
CD_justpass_knn_P_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_P_CD_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN P CD",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,CD_justpass_knn_P_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN P PS")
arrows(y, CD_justpass_knn_P_means-1.96*CD_justpass_knn_P_sds/sqrt(6), y, CD_justpass_knn_P_means+1.96*CD_justpass_knn_P_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

plot(x,ROC_CD_justpass_knn_P[[maxP_CD_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxP_PS_jp,"P CD",sep=" "),
     sub="Just Pass/fail (n=67), PR",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()

ROC_ICS_justpass_knn_P<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("cohort","PageRank"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_P[[i]]<-ROC
}

SRs<-lapply(ROC_ICS_justpass_knn_P,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}

maxP_ICS_jp<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
ICS_justpass_knn_P_means<-rowMeans(SR_m)
ICS_justpass_knn_P_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_P_ICS_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN P ICS",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,ICS_justpass_knn_P_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN P PS")
arrows(y, ICS_justpass_knn_P_means-1.96*ICS_justpass_knn_P_sds/sqrt(6), y, ICS_justpass_knn_P_means+1.96*ICS_justpass_knn_P_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

plot(x,ROC_ICS_justpass_knn_P[[maxP_ICS_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxP_ICS_jp,"P ICS",sep=" "),
     sub="Just Pass/fail (n=67), PR",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()


######Target Entropy models######
ROC_PS_knn_T<-list()
for(i in 1:20){
  predPS_knn_T<-jackPredKNN(centPS,predictors = c("cohort","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predPS_knn_T$allpred)
  ROC_PS_knn_T[[i]]<-ROC
}

SRs<-lapply(ROC_PS_knn_T,"[",8) #we keep this stupid code, even though the internet gave me a better option later. Sigh
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxT_PS_pf<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
PS_knn_T_means<-rowMeans(SR_m)
PS_knn_T_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_T_PS.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN T PS")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)
x<-c(1:7)
plot(x,PS_knn_T_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN T PS")
arrows(y, PS_knn_T_means-1.96*PS_knn_T_sds/sqrt(6), y, PS_knn_T_means+1.96*PS_knn_T_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

plot(x,ROC_PS_knn_T[[maxT_PS_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week", main=paste("KNN, N=",maxT_PS_pf,"T PS",sep=" "),
     sub="Pass/fail (n=166), TE",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_CD_knn_T<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("cohort","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_T[[i]]<-ROC
}
SRs<-lapply(ROC_CD_knn_T,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
  
}
maxT_CD_pf<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
CD_knn_T_means<-rowMeans(SR_m)
CD_knn_T_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_T_CD.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN T CD")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

x<-c(1:7)
plot(x,CD_knn_T_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN T PS")
arrows(y, CD_knn_T_means-1.96*CD_knn_T_sds/sqrt(6), y, CD_knn_T_means+1.96*CD_knn_T_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

plot(x,ROC_CD_knn_T[[maxT_CD_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week", main=paste("KNN, N=",maxT_CD_pf,"T CD",sep=" "),
     sub="Pass/fail (n=166), TE",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_ICS_knn_T<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("cohort","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_T[[i]]<-ROC
}

SRs<-lapply(ROC_ICS_knn_T,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxT_ICS_pf<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
ICS_knn_T_means<-rowMeans(SR_m)
ICS_knn_T_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_T_ICS.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN T ICS")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

x<-c(1:7)
plot(x,ICS_knn_T_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN T PS")
arrows(y, ICS_knn_T_means-1.96*ICS_knn_T_sds/sqrt(6), y, ICS_knn_T_means+1.96*ICS_knn_T_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)
plot(x,ROC_ICS_knn_T[[maxT_ICS_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week",  main=paste("KNN, N=",maxT_ICS_pf,"T ICS",sep=" "),
     sub="Pass/fail (n=166), TE",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_PS_justpass_knn_T<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("cohort","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_T[[i]]<-ROC
}
SRs<-lapply(ROC_PS_justpass_knn_T,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxT_PS_jp<-which.max(means)
SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
PS_justpass_knn_T_means<-rowMeans(SR_m)
PS_justpass_knn_T_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_T_PS_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN T PS",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,PS_justpass_knn_T_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN T PS")
arrows(y, PS_justpass_knn_T_means-1.96*PS_justpass_knn_T_sds/sqrt(6), y, PS_justpass_knn_T_means+1.96*PS_justpass_knn_T_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)
plot(x,ROC_PS_justpass_knn_T[[maxT_PS_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxT_PS_jp,"T PS",sep=" "),
     sub="Just Pass/fail (n=67), TE",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()

ROC_CD_justpass_knn_T<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("cohort","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_T[[i]]<-ROC
}
SRs<-lapply(ROC_CD_justpass_knn_T,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxT_CD_jp<-which.max(means)
SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
CD_justpass_knn_T_means<-rowMeans(SR_m)
CD_justpass_knn_T_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_T_CD_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN T CD",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,CD_justpass_knn_T_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN T PS")
arrows(y, CD_justpass_knn_T_means-1.96*CD_justpass_knn_T_sds/sqrt(6), y, CD_justpass_knn_T_means+1.96*CD_justpass_knn_T_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

plot(x,ROC_CD_justpass_knn_T[[maxT_CD_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxT_PS_jp,"T CD",sep=" "),
     sub="Just Pass/fail (n=67), TE",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()

ROC_ICS_justpass_knn_T<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("cohort","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_T[[i]]<-ROC
}

SRs<-lapply(ROC_ICS_justpass_knn_T,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}

maxT_ICS_jp<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
ICS_justpass_knn_T_means<-rowMeans(SR_m)
ICS_justpass_knn_T_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_T_ICS_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN T ICS",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,ICS_justpass_knn_T_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN T PS")
arrows(y, ICS_justpass_knn_T_means-1.96*ICS_justpass_knn_T_sds/sqrt(6), y, ICS_justpass_knn_T_means+1.96*ICS_justpass_knn_T_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

plot(x,ROC_ICS_justpass_knn_T[[maxT_ICS_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxT_ICS_jp,"T ICS",sep=" "),
     sub="Just Pass/fail (n=67), TE",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()


######Hide models######
ROC_PS_knn_H<-list()
for(i in 1:20){
  predPS_knn_H<-jackPredKNN(centPS,predictors = c("cohort","Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_knn_H$allpred)
  ROC_PS_knn_H[[i]]<-ROC
}

SRs<-lapply(ROC_PS_knn_H,"[",8) #we keep this stupid code, even though the internet gave me a better option later. Sigh
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxH_PS_pf<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
PS_knn_H_means<-rowMeans(SR_m)
PS_knn_H_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_H_PS.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN H PS")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)
x<-c(1:7)
plot(x,PS_knn_H_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN H PS")
arrows(y, PS_knn_H_means-1.96*PS_knn_H_sds/sqrt(6), y, PS_knn_H_means+1.96*PS_knn_H_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

plot(x,ROC_PS_knn_H[[maxH_PS_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week", main=paste("KNN, N=",maxH_PS_pf,"H PS",sep=" "),
     sub="Pass/fail (n=166), H",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_CD_knn_H<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("cohort","Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_H[[i]]<-ROC
}
SRs<-lapply(ROC_CD_knn_H,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
  
}
maxH_CD_pf<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
CD_knn_H_means<-rowMeans(SR_m)
CD_knn_H_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_H_CD.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN H CD")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

x<-c(1:7)
plot(x,CD_knn_H_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN H PS")
arrows(y, CD_knn_H_means-1.96*CD_knn_H_sds/sqrt(6), y, CD_knn_H_means+1.96*CD_knn_H_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

plot(x,ROC_CD_knn_H[[maxH_CD_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week", main=paste("KNN, N=",maxH_CD_pf,"H CD",sep=" "),
     sub="Pass/fail (n=166), H",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_ICS_knn_H<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("cohort","Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_H[[i]]<-ROC
}

SRs<-lapply(ROC_ICS_knn_H,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxH_ICS_pf<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
ICS_knn_H_means<-rowMeans(SR_m)
ICS_knn_H_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_H_ICS.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN H ICS")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

x<-c(1:7)
plot(x,ICS_knn_H_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN H PS")
arrows(y, ICS_knn_H_means-1.96*ICS_knn_H_sds/sqrt(6), y, ICS_knn_H_means+1.96*ICS_knn_H_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)
plot(x,ROC_ICS_knn_H[[maxH_ICS_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week",  main=paste("KNN, N=",maxH_ICS_pf,"H ICS",sep=" "),
     sub="Pass/fail (n=166), H",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_PS_justpass_knn_H<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("cohort","Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_H[[i]]<-ROC
}
SRs<-lapply(ROC_PS_justpass_knn_H,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxH_PS_jp<-which.max(means)
SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
PS_justpass_knn_H_means<-rowMeans(SR_m)
PS_justpass_knn_H_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_H_PS_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN H PS",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,PS_justpass_knn_H_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN H PS")
arrows(y, PS_justpass_knn_H_means-1.96*PS_justpass_knn_H_sds/sqrt(6), y, PS_justpass_knn_H_means+1.96*PS_justpass_knn_H_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)
plot(x,ROC_PS_justpass_knn_H[[maxH_PS_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxH_PS_jp,"H PS",sep=" "),
     sub="Just Pass/fail (n=67), H",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()

ROC_CD_justpass_knn_H<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("cohort","Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_H[[i]]<-ROC
}
SRs<-lapply(ROC_CD_justpass_knn_H,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxH_CD_jp<-which.max(means)
SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
CD_justpass_knn_H_means<-rowMeans(SR_m)
CD_justpass_knn_H_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_H_CD_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN H CD",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,CD_justpass_knn_H_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN H PS")
arrows(y, CD_justpass_knn_H_means-1.96*CD_justpass_knn_H_sds/sqrt(6), y, CD_justpass_knn_H_means+1.96*CD_justpass_knn_H_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

plot(x,ROC_CD_justpass_knn_H[[maxH_CD_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxH_PS_jp,"H CD",sep=" "),
     sub="Just Pass/fail (n=67), H",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()

ROC_ICS_justpass_knn_H<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("cohort","Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_H[[i]]<-ROC
}

SRs<-lapply(ROC_ICS_justpass_knn_H,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}

maxH_ICS_jp<-which.max(means)

SR_m<-matrix(unlist(SRs),ncol=20,byrow=T) #The better option - the above code could be nicely modified with this...
ICS_justpass_knn_H_means<-rowMeans(SR_m)
ICS_justpass_knn_H_sds<-apply(SR_m,1,sd)

pdf(file = "plots/KNN_plots/SR04_NetworkMeasures_Cohort_knn_H_ICS_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,3))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN H ICS",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,ICS_justpass_knn_H_means,type="b",ylim=c(0,1),ylab="mean successrate per week",xlab="Course Week",main="KNN H PS")
arrows(y, ICS_justpass_knn_H_means-1.96*ICS_justpass_knn_H_sds/sqrt(6), y, ICS_justpass_knn_H_means+1.96*ICS_justpass_knn_H_sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

plot(x,ROC_ICS_justpass_knn_H[[maxH_ICS_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxH_ICS_jp,"H ICS",sep=" "),
     sub="Just Pass/fail (n=67), H",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()



####Mean over all models####
PS_knn_All<-data.frame(PS_knn_PTH_means,PS_knn_PT_means, PS_knn_TH_means, PS_knn_PH_means, PS_knn_P_means, PS_knn_T_means, PS_knn_H_means)
CD_knn_All<-data.frame(CD_knn_PTH_means,CD_knn_PT_means, CD_knn_TH_means, CD_knn_PH_means, CD_knn_P_means, CD_knn_T_means, CD_knn_H_means)
ICS_knn_All<-data.frame(ICS_knn_PTH_means,ICS_knn_PT_means, ICS_knn_TH_means, ICS_knn_PH_means, ICS_knn_P_means, ICS_knn_T_means, ICS_knn_H_means)
PS_jp_knn_All<-data.frame(PS_justpass_knn_PTH_means,PS_justpass_knn_PT_means, PS_justpass_knn_TH_means, PS_justpass_knn_PH_means, PS_justpass_knn_P_means, PS_justpass_knn_T_means, PS_justpass_knn_H_means)
CD_jp_knn_All<-data.frame(CD_justpass_knn_PTH_means,CD_justpass_knn_PT_means, CD_justpass_knn_TH_means, CD_justpass_knn_PH_means, CD_justpass_knn_P_means, CD_justpass_knn_T_means, CD_justpass_knn_H_means)
ICS_jp_knn_All<-data.frame(ICS_justpass_knn_PTH_means,ICS_justpass_knn_PT_means, ICS_justpass_knn_TH_means, ICS_justpass_knn_PH_means, ICS_justpass_knn_P_means, ICS_justpass_knn_T_means, ICS_justpass_knn_H_means)

PS_knn_All_sd<-data.frame(PS_knn_PTH_sds,PS_knn_PT_sds, PS_knn_TH_sds, PS_knn_PH_sds, PS_knn_P_sds, PS_knn_T_sds, PS_knn_H_sds)
CD_knn_All_sd<-data.frame(CD_knn_PTH_sds,CD_knn_PT_sds, CD_knn_TH_sds, CD_knn_PH_sds, CD_knn_P_sds, CD_knn_T_sds, CD_knn_H_sds)
ICS_knn_All_sd<-data.frame(ICS_knn_PTH_sds,ICS_knn_PT_sds, ICS_knn_TH_sds, ICS_knn_PH_sds, ICS_knn_P_sds, ICS_knn_T_sds, ICS_knn_H_sds)
PS_jp_knn_All_sd<-data.frame(PS_justpass_knn_PTH_sds,PS_justpass_knn_PT_sds, PS_justpass_knn_TH_sds, PS_justpass_knn_PH_sds, PS_justpass_knn_P_sds, PS_justpass_knn_T_sds, PS_justpass_knn_H_sds)
CD_jp_knn_All_sd<-data.frame(CD_justpass_knn_PTH_sds,CD_justpass_knn_PT_sds, CD_justpass_knn_TH_sds, CD_justpass_knn_PH_sds, CD_justpass_knn_P_sds, CD_justpass_knn_T_sds, CD_justpass_knn_H_sds)
ICS_jp_knn_All_sd<-data.frame(ICS_justpass_knn_PTH_sds,ICS_justpass_knn_PT_sds, ICS_justpass_knn_TH_sds, ICS_justpass_knn_PH_sds, ICS_justpass_knn_P_sds, ICS_justpass_knn_T_sds, ICS_justpass_knn_H_sds)


PS_knn_Weigths<-1/PS_knn_All_sd^2
PS_knn_WM<-rowSums(PS_knn_All*PS_knn_Weigths)/rowSums(PS_knn_Weigths)
PS_knn_SEWM<-sqrt(1/rowSums(PS_knn_Weigths))

CD_knn_Weigths<-1/CD_knn_All_sd^2
CD_knn_Weigths[!is.finite(as.matrix(CD_knn_Weigths))]<-NA
CD_knn_WM<-rowSums(CD_knn_All*CD_knn_Weigths,na.rm = T)/rowSums(CD_knn_Weigths,na.rm=T)

CD_knn_SEWM<-sqrt(1/rowSums(CD_knn_Weigths))

ICS_knn_Weigths<-1/ICS_knn_All_sd^2
ICS_knn_WM<-rowSums(ICS_knn_All*ICS_knn_Weigths)/rowSums(ICS_knn_Weigths)
ICS_knn_SEWM<-sqrt(1/rowSums(ICS_knn_Weigths))

PS_jp_knn_Weigths<-1/PS_jp_knn_All_sd^2
PS_jp_knn_WM<-rowSums(PS_jp_knn_All*PS_jp_knn_Weigths)/rowSums(PS_jp_knn_Weigths)
PS_jp_knn_SEWM<-sqrt(1/rowSums(PS_jp_knn_Weigths))

CD_jp_knn_Weigths<-1/CD_jp_knn_All_sd^2
CD_jp_knn_WM<-rowSums(CD_jp_knn_All*CD_jp_knn_Weigths)/rowSums(CD_jp_knn_Weigths)
CD_jp_knn_SEWM<-sqrt(1/rowSums(CD_jp_knn_Weigths))

ICS_jp_knn_Weigths<-1/ICS_jp_knn_All_sd^2
ICS_jp_knn_WM<-rowSums(ICS_jp_knn_All*ICS_jp_knn_Weigths)/rowSums(ICS_jp_knn_Weigths)
ICS_jp_knn_SEWM<-sqrt(1/rowSums(ICS_jp_knn_Weigths))

####PLOTTING####
pdf(file = "plots/ROC_AUC_plots/SR04_NetworkMeasuresCohort_knn_weightedMeans.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,2))
x<-c(1:7)
plot(x, PS_knn_WM,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="KNN Pass-fail", sub="Weighted mean of all centrality models",type="b"
)
lines(x+0.1,CD_knn_WM,pch=1,type="b",col="darkred")
lines(x-0.1,ICS_knn_WM,pch=2,type="b",col="darkblue")
# hack: we draw arrows but with very special "arrowheads"

arrows(x, PS_knn_WM-1.96*PS_knn_SEWM/sqrt(20), x, PS_knn_WM+1.96*PS_knn_SEWM/sqrt(20), length=0.05, angle=90, code=3)
arrows(x+0.1, CD_knn_WM-1.96*CD_knn_SEWM/sqrt(20), x+0.1, CD_knn_WM+1.96*CD_knn_SEWM/sqrt(20), length=0.05, angle=90, code=3,col="darkred")
arrows(x-0.1, ICS_knn_WM-1.96*ICS_knn_SEWM/sqrt(20), x-0.1, ICS_knn_WM+1.96*ICS_knn_SEWM/sqrt(20), length=0.05, angle=90, code=3,col="darkblue")
legend(3, 0.3, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","darkred", "darkblue"), pch=c(19,1,2),lty=1, cex=0.8)
abline(h=lazy)

x<-c(1:7)
plot(x, PS_jp_knn_WM,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="<SR> and CI",
     main="KNN Just pass-fail", sub="Weighted mean of all centrality models",type="b"
)
lines(x+0.1,CD_jp_knn_WM,pch=1,type="b",col="darkred")
lines(x-0.1,ICS_jp_knn_WM,pch=2,type="b",col="darkblue")
# hack: we draw arrows but with very special "arrowheads"

arrows(x, PS_jp_knn_WM-1.96*PS_jp_knn_SEWM/sqrt(20), x, PS_jp_knn_WM+1.96*PS_jp_knn_SEWM/sqrt(20), length=0.05, angle=90, code=3)
arrows(x+0.1, CD_jp_knn_WM-1.96*CD_jp_knn_SEWM/sqrt(20), x+0.1, CD_jp_knn_WM+1.96*CD_jp_knn_SEWM/sqrt(20), length=0.05, angle=90, code=3,col="darkred")
arrows(x-0.1, ICS_jp_knn_WM-1.96*ICS_jp_knn_SEWM/sqrt(20), x-0.1, ICS_jp_knn_WM+1.96*ICS_jp_knn_SEWM/sqrt(20), length=0.05, angle=90, code=3,col="darkblue")
legend(3, 0.3, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","darkred", "darkblue"), pch=c(19,1,2),lty=1, cex=0.8)
abline(h=lazy_jp)
dev.off()

t2<-Sys.time()
t2-t1
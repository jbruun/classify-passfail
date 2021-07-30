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
predPS_log_PTH<-jackPredLog(centPS,predictors = c("PageRank","tarEnt", "Hide"))
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

predCD_log_PTH<-jackPredLog(centCD,predictors = c("PageRank","tarEnt", "Hide"))
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



predICS_log_PTH<-jackPredLog(centICS,predictors = c("PageRank","tarEnt", "Hide"))
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
predPS_jp_log_PTH<-jackPredLog(centPS,outcome = "justpass",predictors = c("PageRank","tarEnt", "Hide"))
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

predCD_jp_log_PTH<-jackPredLog(centCD, outcome = "justpass",predictors = c("PageRank","tarEnt", "Hide"))
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


predICS_jp_log_PTH<-jackPredLog(centICS, outcome = "justpass",predictors = c("PageRank","tarEnt", "Hide"))
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
predPS_log_PT<-jackPredLog(centPS,predictors = c("PageRank","tarEnt"))
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
predCD_log_PT<-jackPredLog(centCD,predictors = c("PageRank","tarEnt"))
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
predICS_log_PT<-jackPredLog(centICS,predictors = c("PageRank","tarEnt"))
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
predPS_jp_log_PT<-jackPredLog(centPS,outcome = "justpass",predictors = c("PageRank","tarEnt"))
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

predCD_jp_log_PT<-jackPredLog(centCD, outcome = "justpass",predictors = c("PageRank","tarEnt"))
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


predICS_jp_log_PT<-jackPredLog(centICS, outcome = "justpass",predictors = c("PageRank","tarEnt"))
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
predPS_log_PH<-jackPredLog(centPS,predictors = c("PageRank", "Hide"))
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
predCD_log_PH<-jackPredLog(centCD,predictors = c("PageRank", "Hide"))
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
predICS_log_PH<-jackPredLog(centICS,predictors = c("PageRank", "Hide"))
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
predPS_jp_log_PH<-jackPredLog(centPS,outcome = "justpass",predictors = c("PageRank", "Hide"))
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
predCD_jp_log_PH<-jackPredLog(centCD, outcome = "justpass",predictors = c("PageRank", "Hide"))
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
predICS_jp_log_PH<-jackPredLog(centICS, outcome = "justpass",predictors = c("PageRank", "Hide"))
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
predPS_log_TH<-jackPredLog(centPS,predictors = c("tarEnt", "Hide"))
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
predCD_log_TH<-jackPredLog(centCD,predictors = c("tarEnt", "Hide"))
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
predICS_log_TH<-jackPredLog(centICS,predictors = c("tarEnt", "Hide"))
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
predPS_jp_log_TH<-jackPredLog(centPS,outcome = "justpass",predictors = c("tarEnt", "Hide"))
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
predCD_jp_log_TH<-jackPredLog(centCD, outcome = "justpass",predictors = c("tarEnt", "Hide"))
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
predICS_jp_log_TH<-jackPredLog(centICS, outcome = "justpass",predictors = c("tarEnt", "Hide"))
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
predPS_log_T<-jackPredLog(centPS,predictors = c("tarEnt"))
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
predCD_log_T<-jackPredLog(centCD,predictors = c("tarEnt"))
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
predICS_log_T<-jackPredLog(centICS,predictors = c("tarEnt"))
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
predPS_jp_log_T<-jackPredLog(centPS,outcome = "justpass",predictors = c("tarEnt"))
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
predCD_jp_log_T<-jackPredLog(centCD, outcome = "justpass",predictors = c("tarEnt"))
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
predICS_jp_log_T<-jackPredLog(centICS, outcome = "justpass",predictors = c("tarEnt"))
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
predPS_log_H<-jackPredLog(centPS,predictors = c( "Hide"))
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
predCD_log_H<-jackPredLog(centCD,predictors = c( "Hide"))
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
predICS_log_H<-jackPredLog(centICS,predictors = c( "Hide"))
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
predPS_jp_log_H<-jackPredLog(centPS,outcome = "justpass",predictors = c( "Hide"))
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

predCD_jp_log_H<-jackPredLog(centCD, outcome = "justpass",predictors = c( "Hide"))
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


predICS_jp_log_H<-jackPredLog(centICS, outcome = "justpass",predictors = c( "Hide"))
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
predPS_log_P<-jackPredLog(centPS,predictors = c("PageRank"))
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
predCD_log_P<-jackPredLog(centCD,predictors = c("PageRank"))
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
predICS_log_P<-jackPredLog(centICS,predictors = c("PageRank"))
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
predPS_jp_log_P<-jackPredLog(centPS,outcome = "justpass",predictors = c("PageRank"))
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
predCD_jp_log_P<-jackPredLog(centCD, outcome = "justpass",predictors = c("PageRank"))
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
predICS_jp_log_P<-jackPredLog(centICS, outcome = "justpass",predictors = c("PageRank"))
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

CD_jp_log_RegAll<-data.frame(CD_jp_log_PTH_auc,CD_jp_log_PT_auc,CD_jp_log_TH_auc,CD_jp_log_P_auc,CD_jp_log_T_auc,CD_jp_log_H_auc)
CD_jp_log_RegAllciH<-data.frame(CD_jp_log_PTH_ciH,CD_jp_log_PT_ciH,CD_jp_log_TH_ciH,CD_jp_log_P_ciH,CD_jp_log_T_ciH,CD_jp_log_H_ciH)
CD_jp_log_RegAllciL<-data.frame(CD_jp_log_PTH_ciL,CD_jp_log_PT_ciL,CD_jp_log_TH_ciL,CD_jp_log_P_ciL,CD_jp_log_T_ciL,CD_jp_log_H_ciL)

CD_jp_log_RegSD<-sqrt(67)*(CD_jp_log_RegAllciH-CD_jp_log_RegAllciL)/3.92
CD_jp_log_RegWeigths<-1/CD_jp_log_RegSD^2 #Zero standard deviations become infinite here. They are probably a sign of an outlier.
CD_jp_log_RegWeigths[!is.finite(as.matrix(CD_jp_log_RegWeigths))]<-NA #Correction for zero standard deviations.
CD_jp_log_RegWM<-rowSums(CD_jp_log_RegAll*CD_jp_log_RegWeigths,na.rm = T)/rowSums(CD_jp_log_RegWeigths,na.rm = T)
CD_jp_log_RegSEWM<-sqrt(1/rowSums(CD_jp_log_RegWeigths,na.rm=T))

ICS_jp_log_RegAll<-data.frame(ICS_jp_log_PTH_auc,ICS_jp_log_PT_auc,ICS_jp_log_TH_auc,ICS_jp_log_P_auc,ICS_jp_log_T_auc,ICS_jp_log_H_auc)
ICS_jp_log_RegAllciH<-data.frame(ICS_jp_log_PTH_ciH,ICS_jp_log_PT_ciH,ICS_jp_log_TH_ciH,ICS_jp_log_P_ciH,ICS_jp_log_T_ciH,ICS_jp_log_H_ciH)
ICS_jp_log_RegAllciL<-data.frame(ICS_jp_log_PTH_ciL,ICS_jp_log_PT_ciL,ICS_jp_log_TH_ciL,ICS_jp_log_P_ciL,ICS_jp_log_T_ciL,ICS_jp_log_H_ciL)

ICS_jp_log_RegSD<-sqrt(67)*(ICS_jp_logRegAllciH-ICS_jp_logRegAllciL)/3.92
ICS_jp_log_RegWeigths<-1/ICS_jp_log_RegSD^2
ICS_jp_log_RegWM<-rowSums(ICS_jp_log_RegAll*ICS_jp_log_RegWeigths)/rowSums(ICS_jp_log_RegWeigths)
ICS_jp_log_RegSEWM<-sqrt(1/rowSums(ICS_jp_log_RegWeigths))

###PLOTTING###
pdf(file = "/Users/rmn845/GitHub/classify-passfail/plots/ROC_AUC_plots/AUC01_onlyNetworkMeasures_log_weightedMeans.pdf",   # The directory you want to save the file in
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
pdf(file = "/Users/rmn845/GitHub/classify-passfail/plots/ROC_AUC_plots/ROC01_pf.pdf",   # The directory you want to save the file in
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
pdf(file = "/Users/rmn845/GitHub/classify-passfail/plots/ROC_AUC_plots/ROC01_jpf.pdf",   # The directory you want to save the file in
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
predPS_lda_PTH<-jackPredLDA(centPS,predictors = c("PageRank","tarEnt", "Hide"))
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

predCD_lda_PTH<-jackPredLDA(centCD,predictors = c("PageRank","tarEnt", "Hide"))
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



predICS_lda_PTH<-jackPredLDA(centICS,predictors = c("PageRank","tarEnt", "Hide"))
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
predPS_jp_lda_PTH<-jackPredLDA(centPS,outcome = "justpass",predictors = c("PageRank","tarEnt", "Hide"))
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

predCD_jp_lda_PTH<-jackPredLDA(centCD, outcome = "justpass",predictors = c("PageRank","tarEnt", "Hide"))
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


predICS_jp_lda_PTH<-jackPredLDA(centICS, outcome = "justpass",predictors = c("PageRank","tarEnt", "Hide"))
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
predPS_lda_PT<-jackPredLDA(centPS,predictors = c("PageRank","tarEnt"))
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
predCD_lda_PT<-jackPredLDA(centCD,predictors = c("PageRank","tarEnt"))
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
predICS_lda_PT<-jackPredLDA(centICS,predictors = c("PageRank","tarEnt"))
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
predPS_jp_lda_PT<-jackPredLDA(centPS,outcome = "justpass",predictors = c("PageRank","tarEnt"))
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

predCD_jp_lda_PT<-jackPredLDA(centCD, outcome = "justpass",predictors = c("PageRank","tarEnt"))
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


predICS_jp_lda_PT<-jackPredLDA(centICS, outcome = "justpass",predictors = c("PageRank","tarEnt"))
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
predPS_lda_PH<-jackPredLDA(centPS,predictors = c("PageRank", "Hide"))
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
predCD_lda_PH<-jackPredLDA(centCD,predictors = c("PageRank", "Hide"))
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
predICS_lda_PH<-jackPredLDA(centICS,predictors = c("PageRank", "Hide"))
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
predPS_jp_lda_PH<-jackPredLDA(centPS,outcome = "justpass",predictors = c("PageRank", "Hide"))
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
predCD_jp_lda_PH<-jackPredLDA(centCD, outcome = "justpass",predictors = c("PageRank", "Hide"))
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
predICS_jp_lda_PH<-jackPredLDA(centICS, outcome = "justpass",predictors = c("PageRank", "Hide"))
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
predPS_lda_TH<-jackPredLDA(centPS,predictors = c("tarEnt", "Hide"))
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
predCD_lda_TH<-jackPredLDA(centCD,predictors = c("tarEnt", "Hide"))
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
predICS_lda_TH<-jackPredLDA(centICS,predictors = c("tarEnt", "Hide"))
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
predPS_jp_lda_TH<-jackPredLDA(centPS,outcome = "justpass",predictors = c("tarEnt", "Hide"))
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
predCD_jp_lda_TH<-jackPredLDA(centCD, outcome = "justpass",predictors = c("tarEnt", "Hide"))
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
predICS_jp_lda_TH<-jackPredLDA(centICS, outcome = "justpass",predictors = c("tarEnt", "Hide"))
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
predPS_lda_T<-jackPredLDA(centPS,predictors = c("tarEnt"))
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
predCD_lda_T<-jackPredLDA(centCD,predictors = c("tarEnt"))
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
predICS_lda_T<-jackPredLDA(centICS,predictors = c("tarEnt"))
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
predPS_jp_lda_T<-jackPredLDA(centPS,outcome = "justpass",predictors = c("tarEnt"))
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
predCD_jp_lda_T<-jackPredLDA(centCD, outcome = "justpass",predictors = c("tarEnt"))
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
predICS_jp_lda_T<-jackPredLDA(centICS, outcome = "justpass",predictors = c("tarEnt"))
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
predPS_lda_H<-jackPredLDA(centPS,predictors = c( "Hide"))
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
predCD_lda_H<-jackPredLDA(centCD,predictors = c( "Hide"))
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
predICS_lda_H<-jackPredLDA(centICS,predictors = c( "Hide"))
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
predPS_jp_lda_H<-jackPredLDA(centPS,outcome = "justpass",predictors = c( "Hide"))
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

predCD_jp_lda_H<-jackPredLDA(centCD, outcome = "justpass",predictors = c( "Hide"))
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


predICS_jp_lda_H<-jackPredLDA(centICS, outcome = "justpass",predictors = c( "Hide"))
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
predPS_lda_P<-jackPredLDA(centPS,predictors = c("PageRank"))
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
predCD_lda_P<-jackPredLDA(centCD,predictors = c("PageRank"))
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
predICS_lda_P<-jackPredLDA(centICS,predictors = c("PageRank"))
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
predPS_jp_lda_P<-jackPredLDA(centPS,outcome = "justpass",predictors = c("PageRank"))
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
predCD_jp_lda_P<-jackPredLDA(centCD, outcome = "justpass",predictors = c("PageRank"))
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
predICS_jp_lda_P<-jackPredLDA(centICS, outcome = "justpass",predictors = c("PageRank"))
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
PS_lda_RegAll<-data.frame(PS_lda_PTH_auc,PS_lda_PT_auc,PS_lda_TH_auc,PS_lda_P_auc,PS_lda_T_auc,PS_lda_H_auc)
PS_lda_RegAllciH<-data.frame(PS_lda_PTH_ciH,PS_lda_PT_ciH,PS_lda_TH_ciH,PS_lda_P_ciH,PS_lda_T_ciH,PS_lda_H_ciH)
PS_lda_RegAllciL<-data.frame(PS_lda_PTH_ciL,PS_lda_PT_ciL,PS_lda_TH_ciL,PS_lda_P_ciL,PS_lda_T_ciL,PS_lda_H_ciL)

PS_lda_RegSD<-sqrt(166)*(PS_lda_RegAllciH-PS_lda_RegAllciL)/3.92
PS_lda_RegWeigths<-1/PS_lda_RegSD^2
PS_lda_RegWM<-rowSums(PS_lda_RegAll*PS_lda_RegWeigths)/rowSums(PS_lda_RegWeigths)
PS_lda_RegSEWM<-sqrt(1/rowSums(PS_lda_RegWeigths))

CD_lda_RegAll<-data.frame(CD_lda_PTH_auc,CD_lda_PT_auc,CD_lda_TH_auc,CD_lda_P_auc,CD_lda_T_auc,CD_lda_H_auc)
CD_lda_RegAllciH<-data.frame(CD_lda_PTH_ciH,CD_lda_PT_ciH,CD_lda_TH_ciH,CD_lda_P_ciH,CD_lda_T_ciH,CD_lda_H_ciH)
CD_lda_RegAllciL<-data.frame(CD_lda_PTH_ciL,CD_lda_PT_ciL,CD_lda_TH_ciL,CD_lda_P_ciL,CD_lda_T_ciL,CD_lda_H_ciL)

CD_lda_RegSD<-sqrt(166)*(CD_lda_RegAllciH-CD_lda_RegAllciL)/3.92
CD_lda_RegWeigths<-1/CD_lda_RegSD^2
CD_lda_RegWM<-rowSums(CD_lda_RegAll*CD_lda_RegWeigths)/rowSums(CD_lda_RegWeigths)
CD_lda_RegSEWM<-sqrt(1/rowSums(CD_lda_RegWeigths))

ICS_lda_RegAll<-data.frame(ICS_lda_PTH_auc,ICS_lda_PT_auc,ICS_lda_TH_auc,ICS_lda_P_auc,ICS_lda_T_auc,ICS_lda_H_auc)
ICS_lda_RegAllciH<-data.frame(ICS_lda_PTH_ciH,ICS_lda_PT_ciH,ICS_lda_TH_ciH,ICS_lda_P_ciH,ICS_lda_T_ciH,ICS_lda_H_ciH)
ICS_lda_RegAllciL<-data.frame(ICS_lda_PTH_ciL,ICS_lda_PT_ciL,ICS_lda_TH_ciL,ICS_lda_P_ciL,ICS_lda_T_ciL,ICS_lda_H_ciL)

ICS_lda_RegSD<-sqrt(166)*(ICS_lda_RegAllciH-ICS_lda_RegAllciL)/3.92
ICS_lda_RegWeigths<-1/ICS_lda_RegSD^2
ICS_lda_RegWM<-rowSums(ICS_lda_RegAll*ICS_lda_RegWeigths)/rowSums(ICS_lda_RegWeigths)
ICS_lda_RegSEWM<-sqrt(1/rowSums(ICS_lda_RegWeigths))

##just pass-fail##
PS_jp_lda_RegAll<-data.frame(PS_jp_lda_PTH_auc,PS_jp_lda_PT_auc,PS_jp_lda_TH_auc,PS_jp_lda_P_auc,PS_jp_lda_T_auc,PS_jp_lda_H_auc)
PS_jp_lda_RegAllciH<-data.frame(PS_jp_lda_PTH_ciH,PS_jp_lda_PT_ciH,PS_jp_lda_TH_ciH,PS_jp_lda_P_ciH,PS_jp_lda_T_ciH,PS_jp_lda_H_ciH)
PS_jp_lda_RegAllciL<-data.frame(PS_jp_lda_PTH_ciL,PS_jp_lda_PT_ciL,PS_jp_lda_TH_ciL,PS_jp_lda_P_ciL,PS_jp_lda_T_ciL,PS_jp_lda_H_ciL)

PS_jp_lda_RegSD<-sqrt(67)*(PS_jp_lda_RegAllciH-PS_jp_lda_RegAllciL)/3.92
PS_jp_lda_RegWeigths<-1/PS_jp_lda_RegSD^2
PS_jp_lda_RegWM<-rowSums(PS_jp_lda_RegAll*PS_jp_lda_RegWeigths)/rowSums(PS_jp_lda_RegWeigths)
PS_jp_lda_RegSEWM<-sqrt(1/rowSums(PS_jp_lda_RegWeigths))

CD_jp_lda_RegAll<-data.frame(CD_jp_lda_PTH_auc,CD_jp_lda_PT_auc,CD_jp_lda_TH_auc,CD_jp_lda_P_auc,CD_jp_lda_T_auc,CD_jp_lda_H_auc)
CD_jp_lda_RegAllciH<-data.frame(CD_jp_lda_PTH_ciH,CD_jp_lda_PT_ciH,CD_jp_lda_TH_ciH,CD_jp_lda_P_ciH,CD_jp_lda_T_ciH,CD_jp_lda_H_ciH)
CD_jp_lda_RegAllciL<-data.frame(CD_jp_lda_PTH_ciL,CD_jp_lda_PT_ciL,CD_jp_lda_TH_ciL,CD_jp_lda_P_ciL,CD_jp_lda_T_ciL,CD_jp_lda_H_ciL)

CD_jp_lda_RegSD<-sqrt(67)*(CD_jp_lda_RegAllciH-CD_jp_lda_RegAllciL)/3.92
CD_jp_lda_RegWeigths<-1/CD_jp_lda_RegSD^2 #Zero standard deviations become infinite here. They are probably a sign of an outlier.
CD_jp_lda_RegWeigths[!is.finite(as.matrix(CD_jp_lda_RegWeigths))]<-NA #Correction for zero standard deviations.
CD_jp_lda_RegWM<-rowSums(CD_jp_lda_RegAll*CD_jp_lda_RegWeigths,na.rm = T)/rowSums(CD_jp_lda_RegWeigths,na.rm = T)
CD_jp_lda_RegSEWM<-sqrt(1/rowSums(CD_jp_lda_RegWeigths,na.rm=T))

ICS_jp_lda_RegAll<-data.frame(ICS_jp_lda_PTH_auc,ICS_jp_lda_PT_auc,ICS_jp_lda_TH_auc,ICS_jp_lda_P_auc,ICS_jp_lda_T_auc,ICS_jp_lda_H_auc)
ICS_jp_lda_RegAllciH<-data.frame(ICS_jp_lda_PTH_ciH,ICS_jp_lda_PT_ciH,ICS_jp_lda_TH_ciH,ICS_jp_lda_P_ciH,ICS_jp_lda_T_ciH,ICS_jp_lda_H_ciH)
ICS_jp_lda_RegAllciL<-data.frame(ICS_jp_lda_PTH_ciL,ICS_jp_lda_PT_ciL,ICS_jp_lda_TH_ciL,ICS_jp_lda_P_ciL,ICS_jp_lda_T_ciL,ICS_jp_lda_H_ciL)

ICS_jp_lda_RegSD<-sqrt(67)*(ICS_jp_logRegAllciH-ICS_jp_logRegAllciL)/3.92
ICS_jp_lda_RegWeigths<-1/ICS_jp_lda_RegSD^2
ICS_jp_lda_RegWM<-rowSums(ICS_jp_lda_RegAll*ICS_jp_lda_RegWeigths)/rowSums(ICS_jp_lda_RegWeigths)
ICS_jp_lda_RegSEWM<-sqrt(1/rowSums(ICS_jp_lda_RegWeigths))
###PLOTS###
pdf(file = "/Users/rmn845/GitHub/classify-passfail/plots/ROC_AUC_plots/AUC01_onlyNetworkMeasures_lda_weightedMeans.pdf",   # The directory you want to save the file in
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
pdf(file = "/Users/rmn845/GitHub/classify-passfail/plots/ROC_AUC_plots/ROC01_pf.pdf",   # The directory you want to save the file in
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
pdf(file = "/Users/rmn845/GitHub/classify-passfail/plots/ROC_AUC_plots/ROC01_jpf.pdf",   # The directory you want to save the file in
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



#######################################
#########K NEAREST NEIGHBORS###########
#######################################

######Pagerank Target Entropy Hide models######
t1<-Sys.time()
ROC_PS_knn<-list()
for(i in 1:20){
  predPS_knn_PTH<-jackPredKNN(centPS,predictors = c("PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_knn_PTH$allpred)
  ROC_PS_knn[[i]]<-ROC
}

SRs<-lapply(ROC_PS_knn,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxPTH_PS_pf<-which.max(means+sds)
pdf(file = "/Users/rmn845/GitHub/classify-passfail/plots/KNN_plots/SR01_onlyNetworkMeasures_knn_PTH_PS.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,2))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PTH PS")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

x<-c(1:7)
plot(x,ROC_PS_knn[[max]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week", main=paste("KNN, N=",maxPTH_PS_pf,"PTH PS",sep=" "),
     sub="Pass/fail (n=166), PR-TE-H",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_CD_knn<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn[[i]]<-ROC
}
SRs<-lapply(ROC_CD_knn,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
  
}
maxPTH_CD_pf<-which.max(means+sds)
pdf(file = "/Users/rmn845/GitHub/classify-passfail/plots/KNN_plots/SR01_onlyNetworkMeasures_knn_PTH_CD.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,2))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PTH CD")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

x<-c(1:7)
plot(x,ROC_CD_knn[[maxPTH_CD_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week", main=paste("KNN, N=",maxPTH_CD_pf,"PTH CD",sep=" "),
     sub="Pass/fail (n=166), PR-TE-H",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_ICS_knn<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn[[i]]<-ROC
}

SRs<-lapply(ROC_ICS_knn,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxPTH_ICS_pf<-which.max(means+sds)
pdf(file = "/Users/rmn845/GitHub/classify-passfail/plots/KNN_plots/SR01_onlyNetworkMeasures_knn_PTH_ICS.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,2))
y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PTH ICS")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy)

x<-c(1:7)
plot(x,ROC_ICS_knn[[maxPTH_ICS_pf]]$SR,type="b",ylim=c(0.6,0.8),xlab="Course Week",  main=paste("KNN, N=",maxPTH_ICS_pf,"PTH ICS",sep=" "),
     sub="Pass/fail (n=166), PR-TE-H",ylab="Succes Rate")
abline(h=lazy)
dev.off()

ROC_PS_justpass_knn<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn[[i]]<-ROC
}
SRs<-lapply(ROC_PS_justpass_knn,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxPTH_PS_jp<-which.max(means+sds)
pdf(file = "/Users/rmn845/GitHub/classify-passfail/plots/KNN_plots/SR01_onlyNetworkMeasures_knn_PTH_PS_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,2))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PTH PS",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,ROC_PS_justpass_knn[[maxPTH_PS_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxPTH_PS_jp,"PTH PS",sep=" "),
     sub="Just Pass/fail (n=67), PR-TE-H",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()

ROC_CD_justpass_knn<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn[[i]]<-ROC
}
SRs<-lapply(ROC_CD_justpass_knn,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}
maxPTH_CD_jp<-which.max(means+sds)
pdf(file = "/Users/rmn845/GitHub/classify-passfail/plots/KNN_plots/SR01_onlyNetworkMeasures_knn_PTH_CD_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,2))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PTH CD",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,ROC_CD_justpass_knn[[maxPTH_CD_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxPTH_PS_jp,"PTH CD",sep=" "),
     sub="Just Pass/fail (n=67), PR-TE-H",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()

ROC_ICS_justpass_knn<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn[[i]]<-ROC
}

SRs<-lapply(ROC_ICS_justpass_knn,"[",8)
means<-lapply(SRs,colMeans)
means<-unlist(means)
sds<-vector()
for(i in 1:20){
  sds[i]<-sd(unlist(SRs[[i]]))
}

maxPTH_ICS_jp<-which.max(means+sds)
pdf(file = "/Users/rmn845/GitHub/classify-passfail/plots/KNN_plots/SR01_onlyNetworkMeasures_knn_PTH_ICS_jp.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches
par(mfrow=c(1,2))

y<-c(1:20)
plot(y,means,type="b",ylim=c(0,1),ylab="mean successrate",xlab="N nearest neighbors",main="KNN PTH ICS",sub="just pass/fail")
arrows(y, means-1.96*sds/sqrt(6), y, means+1.96*sds/sqrt(6), length=0.05, angle=90, code=3)
abline(h=lazy_jp)

x<-c(1:7)
plot(x,ROC_ICS_justpass_knn[[maxPTH_ICS_jp]]$SR,type="b",ylim=c(0,1),xlab="Course Week", main=paste("KNN, N=",maxPTH_ICS_jp,"PTH ICS",sep=" "),
     sub="Just Pass/fail (n=67), PR-TE-H",ylab="Succes Rate")
abline(h=lazy_jp)
dev.off()

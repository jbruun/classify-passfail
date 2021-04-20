## Leave one out cross-validation
##Made ready for ROC curves
##Models without background measures


rm(list = ls())
t1<-Sys.time()

library(igraph)
library(pROC)
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

source("code/jackknife_functionsAlternate.R")
source("code/ROC_functions.R")

#########LOGISTIC REGRESSION###########
#########LOGISTIC REGRESSION###########
#########LOGISTIC REGRESSION###########
#########LOGISTIC REGRESSION###########

######ALL NETWORK PREDICTORS######
lazy<-table(predPS_log_PTH$pass)[2]/sum(table(predICS_jp_log_PTH$pass))

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

lazy<-table(predPS_log_PTH$pass)[2]/sum(table(predPS_log_PTH$pass))

x<-c(1:7)
plot(x, PS_log_PTH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Weeks", ylab="AUC and CI",
     main="AUC with Confidence Intervals",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_log_PTH_ciL, x, PS_log_PTH_ciH, length=0.05, angle=90, code=3)
abline(h = lazy)

plot(rocPS_log_PTH[[1]])
lines(rocPS_log_PTH[[2]],col="yellow")
lines(rocPS_log_PTH[[3]],col="blue")
lines(rocPS_log_PTH[[4]],col="magenta")
lines(rocPS_log_PTH[[5]],col="red")
lines(rocPS_log_PTH[[6]],col="green")
lines(rocPS_log_PTH[[7]],col="purple")


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

lines(x, CD_log_PTH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Weeks", ylab="AUC and CI",
     main="AUC with Confidence Intervals",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, CD_log_PTH_ciL, x, CD_log_PTH_ciH, length=0.05, angle=90, code=3)

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

lines(x, ICS_log_PTH_auc,
      ylim=range(c(0, 1)),
      pch=19, xlab="Weeks", ylab="AUC and CI",
      main="AUC with Confidence Intervals",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, ICS_log_PTH_ciL, x, ICS_log_PTH_ciH, length=0.05, angle=90, code=3)
##JUSTPASSED
lazy<-table(predICS_jp_log_PTH$justpass)[2]/sum(table(predICS_jp_log_PTH$justpass))

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

lazyjp<-table(predPS_jp_log_PTH$justpass)[2]/sum(table(predPS_jp_log_PTH$justpass))
x<-c(1:7)
plot(x, PS_jp_log_PTH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Weeks", ylab="AUC and CI",
     main="AUC with Confidence Intervals",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_log_PTH_ciL, x, PS_jp_log_PTH_ciH, length=0.05, angle=90, code=3)
abline(h=lazyjp)


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

x<-c(1:7)
lines(x, CD_jp_log_PTH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Weeks", ylab="AUC and CI",
     main="AUC with Confidence Intervals",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, CD_jp_log_PTH_ciL, x, CD_jp_log_PTH_ciH, length=0.05, angle=90, code=3)

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

x<-c(1:7)
lines(x, ICS_jp_log_PTH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Weeks", ylab="AUC and CI",
     main="AUC with Confidence Intervals",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, ICS_jp_log_PTH_ciL, x, ICS_jp_log_PTH_ciH, length=0.05, angle=90, code=3)

######PAGERANK TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_log_PRTE<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("PageRank","tarEnt"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_PRTE[[i]]<-ROC
}

ROC_CD_log_PRTE<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("PageRank","tarEnt"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_PRTE[[i]]<-ROC
}

ROC_ICS_log_PRTE<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("PageRank","tarEnt"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_PRTE[[i]]<-ROC
}

ROC_PS_justpass_log_PRTE<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("PageRank","tarEnt"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_PRTE[[i]]<-ROC
}

ROC_CD_justpass_log_PRTE<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("PageRank","tarEnt"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_PRTE[[i]]<-ROC
}

ROC_ICS_justpass_log_PRTE<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("PageRank","tarEnt"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log_PRTE[[i]]<-ROC
}

######PAGERANK HIDE NETWORK PREDICTORS######

ROC_PS_log_PRH<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("PageRank","Hide"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_PRH[[i]]<-ROC
}

ROC_CD_log_PRH<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("PageRank","Hide"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_PRH[[i]]<-ROC
}

ROC_ICS_log_PRH<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("PageRank","Hide"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_PRH[[i]]<-ROC
}

ROC_PS_justpass_log_PRH<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("PageRank","Hide"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_PRH[[i]]<-ROC
}

ROC_CD_justpass_log_PRH<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("PageRank","Hide"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_PRH[[i]]<-ROC
}

ROC_ICS_justpass_log_PRH<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("PageRank","Hide"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log_PRH[[i]]<-ROC
}

######TARGET ENTROPY HIDE NETWORK PREDICTORS######

ROC_PS_log_TEH<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("tarEnt","Hide"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_TEH[[i]]<-ROC
}

ROC_CD_log_TEH<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("tarEnt","Hide"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_TEH[[i]]<-ROC
}

ROC_ICS_log_TEH<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("tarEnt","Hide"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_TEH[[i]]<-ROC
}

ROC_PS_justpass_log_TEH<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("tarEnt","Hide"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_TEH[[i]]<-ROC
}

ROC_CD_justpass_log_TEH<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("tarEnt","Hide"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_TEH[[i]]<-ROC
}

ROC_ICS_justpass_log_TEH<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("tarEnt","Hide"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log_TEH[[i]]<-ROC
}

######TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_log_TE<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("tarEnt"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_TE[[i]]<-ROC
}

ROC_CD_log_TE<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("tarEnt"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_TE[[i]]<-ROC
}

ROC_ICS_log_TE<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("tarEnt"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_TE[[i]]<-ROC
}

ROC_PS_justpass_log_TE<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("tarEnt"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_TE[[i]]<-ROC
}

ROC_CD_justpass_log_TE<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("tarEnt"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_TE[[i]]<-ROC
}

ROC_ICS_justpass_log_TE<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("tarEnt"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log_TE[[i]]<-ROC
}

######HIDE NETWORK PREDICTORS######

ROC_PS_log_H<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("Hide"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_H[[i]]<-ROC
}

ROC_CD_log_H<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("Hide"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_H[[i]]<-ROC
}

ROC_ICS_log_H<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("Hide"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_H[[i]]<-ROC
}

ROC_PS_justpass_log_H<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("Hide"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_H[[i]]<-ROC
}

ROC_CD_justpass_log_H<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("Hide"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_H[[i]]<-ROC
}

ROC_ICS_justpass_log_H<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("Hide"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log_H[[i]]<-ROC
}

######PAGERANK NETWORK PREDICTORS######

ROC_PS_log_PR<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("PageRank"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log_PR[[i]]<-ROC
}

ROC_CD_log_PR<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("PageRank"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log_PR[[i]]<-ROC
}

ROC_ICS_log_PR<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("PageRank"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log_PR[[i]]<-ROC
}

ROC_PS_justpass_log_PR<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("PageRank"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log_PR[[i]]<-ROC
}

ROC_CD_justpass_log_PR<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("PageRank"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log_PR[[i]]<-ROC
}

ROC_ICS_justpass_log_PR<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("PageRank"))
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
     file="data/ROC01_NB_log.Rdata")


#########LINEAR DISCRIMINANT ANALYSIS###########
#########LINEAR DISCRIMINANT ANALYSIS###########
#########LINEAR DISCRIMINANT ANALYSIS ###########
#########LINEAR DISCRIMINANT ANALYSIS ###########

######ALL NETWORK PREDICTORS######

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

lazy<-table(predPS_lda_PTH$pass)[2]/sum(table(predPS_lda_PTH$pass))

x<-c(1:7)
plot(x, PS_lda_PTH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Weeks", ylab="AUC and CI",
     main="AUC with Confidence Intervals",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_lda_PTH_ciL, x, PS_lda_PTH_ciH, length=0.05, angle=90, code=3)
abline(h = lazy)

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

lines(x, CD_lda_PTH_auc,
      ylim=range(c(0, 1)),
      pch=19, xlab="Weeks", ylab="AUC and CI",
      main="AUC with Confidence Intervals",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, CD_lda_PTH_ciL, x, CD_lda_PTH_ciH, length=0.05, angle=90, code=3)

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

lines(x, ICS_lda_PTH_auc,
      ylim=range(c(0, 1)),
      pch=19, xlab="Weeks", ylab="AUC and CI",
      main="AUC with Confidence Intervals",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, ICS_lda_PTH_ciL, x, ICS_lda_PTH_ciH, length=0.05, angle=90, code=3)
##JUSTPASSED
lazy<-table(predICS_jp_lda_PTH$justpass)[2]/sum(table(predICS_jp_lda_PTH$justpass))

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

lazyjp<-table(predPS_jp_lda_PTH$justpass)[2]/sum(table(predPS_jp_lda_PTH$justpass))
x<-c(1:7)
plot(x, PS_jp_lda_PTH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Weeks", ylab="AUC and CI",
     main="AUC with Confidence Intervals",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_lda_PTH_ciL, x, PS_jp_lda_PTH_ciH, length=0.05, angle=90, code=3)
abline(h=lazyjp)


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

x<-c(1:7)
lines(x, CD_jp_lda_PTH_auc,
      ylim=range(c(0, 1)),
      pch=19, xlab="Weeks", ylab="AUC and CI",
      main="AUC with Confidence Intervals",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, CD_jp_lda_PTH_ciL, x, CD_jp_lda_PTH_ciH, length=0.05, angle=90, code=3)

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

x<-c(1:7)
lines(x, ICS_jp_lda_PTH_auc,
      ylim=range(c(0, 1)),
      pch=19, xlab="Weeks", ylab="AUC and CI",
      main="AUC with Confidence Intervals",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, ICS_jp_lda_PTH_ciL, x, ICS_jp_lda_PTH_ciH, length=0.05, angle=90, code=3)
######PAGERANK TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_lda_PRTE<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("PageRank","tarEnt"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_lda_PRTE[[i]]<-ROC
}

ROC_CD_lda_PRTE<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD,predictors = c("PageRank","tarEnt"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_lda_PRTE[[i]]<-ROC
}

ROC_ICS_lda_PRTE<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS,predictors = c("PageRank","tarEnt"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_lda_PRTE[[i]]<-ROC
}

ROC_PS_justpass_lda_PRTE<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,outcome = "justpass",predictors = c("PageRank","tarEnt"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_lda_PRTE[[i]]<-ROC
}

ROC_CD_justpass_lda_PRTE<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD, outcome = "justpass",predictors = c("PageRank","tarEnt"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_lda_PRTE[[i]]<-ROC
}

ROC_ICS_justpass_lda_PRTE<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS, outcome = "justpass",predictors = c("PageRank","tarEnt"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_lda_PRTE[[i]]<-ROC
}

######PAGERANK HIDE NETWORK PREDICTORS######

ROC_PS_lda_PRH<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("PageRank","Hide"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_lda_PRH[[i]]<-ROC
}

ROC_CD_lda_PRH<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD,predictors = c("PageRank","Hide"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_lda_PRH[[i]]<-ROC
}

ROC_ICS_lda_PRH<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS,predictors = c("PageRank","Hide"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_lda_PRH[[i]]<-ROC
}

ROC_PS_justpass_lda_PRH<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,outcome = "justpass",predictors = c("PageRank","Hide"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_lda_PRH[[i]]<-ROC
}

ROC_CD_justpass_lda_PRH<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD, outcome = "justpass",predictors = c("PageRank","Hide"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_lda_PRH[[i]]<-ROC
}

ROC_ICS_justpass_lda_PRH<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS, outcome = "justpass",predictors = c("PageRank","Hide"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_lda_PRH[[i]]<-ROC
}

######TARGET ENTROPY HIDE NETWORK PREDICTORS######

ROC_PS_lda_TEH<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("tarEnt","Hide"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_lda_TEH[[i]]<-ROC
}

ROC_CD_lda_TEH<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD,predictors = c("tarEnt","Hide"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_lda_TEH[[i]]<-ROC
}

ROC_ICS_lda_TEH<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS,predictors = c("tarEnt","Hide"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_lda_TEH[[i]]<-ROC
}

ROC_PS_justpass_lda_TEH<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,outcome = "justpass",predictors = c("tarEnt","Hide"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_lda_TEH[[i]]<-ROC
}

ROC_CD_justpass_lda_TEH<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD, outcome = "justpass",predictors = c("tarEnt","Hide"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_lda_TEH[[i]]<-ROC
}

ROC_ICS_justpass_lda_TEH<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS, outcome = "justpass",predictors = c("tarEnt","Hide"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_lda_TEH[[i]]<-ROC
}

######TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_lda_TE<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("tarEnt"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_lda_TE[[i]]<-ROC
}

ROC_CD_lda_TE<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD,predictors = c("tarEnt"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_lda_TE[[i]]<-ROC
}

ROC_ICS_lda_TE<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS,predictors = c("tarEnt"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_lda_TE[[i]]<-ROC
}

ROC_PS_justpass_lda_TE<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,outcome = "justpass",predictors = c("tarEnt"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_lda_TE[[i]]<-ROC
}

ROC_CD_justpass_lda_TE<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD, outcome = "justpass",predictors = c("tarEnt"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_lda_TE[[i]]<-ROC
}

ROC_ICS_justpass_lda_TE<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS, outcome = "justpass",predictors = c("tarEnt"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_lda_TE[[i]]<-ROC
}

######HIDE NETWORK PREDICTORS######

ROC_PS_lda_H<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("Hide"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_lda_H[[i]]<-ROC
}

ROC_CD_lda_H<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD,predictors = c("Hide"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_lda_H[[i]]<-ROC
}

ROC_ICS_lda_H<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS,predictors = c("Hide"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_lda_H[[i]]<-ROC
}

ROC_PS_justpass_lda_H<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,outcome = "justpass",predictors = c("Hide"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_lda_H[[i]]<-ROC
}

ROC_CD_justpass_lda_H<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD, outcome = "justpass",predictors = c("Hide"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_lda_H[[i]]<-ROC
}

ROC_ICS_justpass_lda_H<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS, outcome = "justpass",predictors = c("Hide"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_lda_H[[i]]<-ROC
}

######TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_lda_PR<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,predictors = c("PageRank"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_lda_PR[[i]]<-ROC
}

ROC_CD_lda_PR<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD,predictors = c("PageRank"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_lda_PR[[i]]<-ROC
}

ROC_ICS_lda_PR<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS,predictors = c("PageRank"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_lda_PR[[i]]<-ROC
}

ROC_PS_justpass_lda_PR<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,outcome = "justpass",predictors = c("PageRank"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_lda_PR[[i]]<-ROC
}

ROC_CD_justpass_lda_PR<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD, outcome = "justpass",predictors = c("PageRank"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_lda_PR[[i]]<-ROC
}

ROC_ICS_justpass_lda_PR<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS, outcome = "justpass",predictors = c("PageRank"))
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
     file="data/ROC01_NB_lda.Rdata")



#########QUADRATIC DISCRIMINANT ANALYSIS###########
#########QUADRATIC DISCRIMINANT ANALYSIS###########
#########QUADRATIC DISCRIMINANT ANALYSIS ###########
#########QUADRATIC DISCRIMINANT ANALYSIS ###########

######ALL NETWORK PREDICTORS######
predPS_qda_PTH<-jackPredQDA(centPS,predictors = c("PageRank","tarEnt", "Hide"))
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

lazy<-table(predPS_qda_PTH$pass)[2]/sum(table(predPS_qda_PTH$pass))

x<-c(1:7)
plot(x, PS_qda_PTH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Weeks", ylab="AUC and CI",
     main="AUC with Confidence Intervals",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_qda_PTH_ciL, x, PS_qda_PTH_ciH, length=0.05, angle=90, code=3)
abline(h = lazy)

predCD_qda_PTH<-jackPredQDA(centCD,predictors = c("PageRank","tarEnt", "Hide"))
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

lines(x, CD_qda_PTH_auc,
      ylim=range(c(0, 1)),
      pch=19, xlab="Weeks", ylab="AUC and CI",
      main="AUC with Confidence Intervals",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, CD_qda_PTH_ciL, x, CD_qda_PTH_ciH, length=0.05, angle=90, code=3)

predICS_qda_PTH<-jackPredQDA(centICS,predictors = c("PageRank","tarEnt", "Hide"))
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

lines(x, ICS_qda_PTH_auc,
      ylim=range(c(0, 1)),
      pch=19, xlab="Weeks", ylab="AUC and CI",
      main="AUC with Confidence Intervals",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, ICS_qda_PTH_ciL, x, ICS_qda_PTH_ciH, length=0.05, angle=90, code=3)
##JUSTPASSED
lazy<-table(predICS_jp_qda_PTH$justpass)[2]/sum(table(predICS_jp_qda_PTH$justpass))

predPS_jp_qda_PTH<-jackPredQDA(centPS,outcome = "justpass",predictors = c("PageRank","tarEnt", "Hide"))
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

lazyjp<-table(predPS_jp_qda_PTH$justpass)[2]/sum(table(predPS_jp_qda_PTH$justpass))
x<-c(1:7)
plot(x, PS_jp_qda_PTH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Weeks", ylab="AUC and CI",
     main="AUC with Confidence Intervals",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_qda_PTH_ciL, x, PS_jp_qda_PTH_ciH, length=0.05, angle=90, code=3)
abline(h=lazyjp)


predCD_jp_qda_PTH<-jackPredQDA(centCD, outcome = "justpass",predictors = c("PageRank","tarEnt", "Hide")) #produces error: Error in qda.default(x, grouping, ...) : rank deficiency in group 1 
rocCD_jp_qda_PTH<-list()
rocCD_jp_qda_PTH[[1]]<-roc(predCD_jp_qda_PTH$justpass,as.numeric(predCD_jp_qda_PTH$Week1),auc=T,ci=T)
rocCD_jp_qda_PTH[[2]]<-roc(predCD_jp_qda_PTH$justpass,as.numeric(predCD_jp_qda_PTH$Week2),auc=T,ci=T)
rocCD_jp_qda_PTH[[3]]<-roc(predCD_jp_qda_PTH$justpass,as.numeric(predCD_jp_qda_PTH$Week3),auc=T,ci=T)
rocCD_jp_qda_PTH[[4]]<-roc(predCD_jp_qda_PTH$justpass,as.numeric(predCD_jp_qda_PTH$Week4),auc=T,ci=T)
rocCD_jp_qda_PTH[[5]]<-roc(predCD_jp_qda_PTH$justpass,as.numeric(predCD_jp_qda_PTH$Week5),auc=T,ci=T)
rocCD_jp_qda_PTH[[6]]<-roc(predCD_jp_qda_PTH$justpass,as.numeric(predCD_jp_qda_PTH$Week6),auc=T,ci=T)
rocCD_jp_qda_PTH[[7]]<-roc(predCD_jp_qda_PTH$justpass,as.numeric(predCD_jp_qda_PTH$Week7),auc=T,ci=T)

CD_jp_qda_PTH_auc<-c(rocCD_jp_qda_PTH[[1]]$auc,rocCD_jp_qda_PTH[[2]]$auc,rocCD_jp_qda_PTH[[3]]$auc,rocCD_jp_qda_PTH[[4]]$auc,
                     rocCD_jp_qda_PTH[[5]]$auc,rocCD_jp_qda_PTH[[6]]$auc,rocCD_jp_qda_PTH[[7]]$auc)
CD_jp_qda_PTH_ciL<-c(rocCD_jp_qda_PTH[[1]]$ci[1],rocCD_jp_qda_PTH[[2]]$ci[1],rocCD_jp_qda_PTH[[3]]$ci[1],rocCD_jp_qda_PTH[[4]]$ci[1],
                     rocCD_jp_qda_PTH[[5]]$ci[1],rocCD_jp_qda_PTH[[6]]$ci[1],rocCD_jp_qda_PTH[[7]]$ci[1])
CD_jp_qda_PTH_ciH<-c(rocCD_jp_qda_PTH[[1]]$ci[3],rocCD_jp_qda_PTH[[2]]$ci[3],rocCD_jp_qda_PTH[[3]]$ci[3],rocCD_jp_qda_PTH[[4]]$ci[3],
                     rocCD_jp_qda_PTH[[5]]$ci[3],rocCD_jp_qda_PTH[[6]]$ci[3],rocCD_jp_qda_PTH[[7]]$ci[3])

x<-c(1:7)
lines(x, CD_jp_qda_PTH_auc,
      ylim=range(c(0, 1)),
      pch=19, xlab="Weeks", ylab="AUC and CI",
      main="AUC with Confidence Intervals",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, CD_jp_qda_PTH_ciL, x, CD_jp_qda_PTH_ciH, length=0.05, angle=90, code=3)

predICS_jp_qda_PTH<-jackPredQDA(centICS, outcome = "justpass",predictors = c("PageRank","tarEnt", "Hide"))
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

x<-c(1:7)
lines(x, ICS_jp_qda_PTH_auc,
      ylim=range(c(0, 1)),
      pch=19, xlab="Weeks", ylab="AUC and CI",
      main="AUC with Confidence Intervals",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, ICS_jp_qda_PTH_ciL, x, ICS_jp_qda_PTH_ciH, length=0.05, angle=90, code=3)

######PAGERANK TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_qda_PRTE<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("PageRank","tarEnt"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_qda_PRTE[[i]]<-ROC
}

ROC_CD_qda_PRTE<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD,predictors = c("PageRank","tarEnt"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_qda_PRTE[[i]]<-ROC
}

ROC_ICS_qda_PRTE<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS,predictors = c("PageRank","tarEnt"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_qda_PRTE[[i]]<-ROC
}

ROC_PS_justpass_qda_PRTE<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,outcome = "justpass",predictors = c("PageRank","tarEnt"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_qda_PRTE[[i]]<-ROC
}

ROC_CD_justpass_qda_PRTE<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD, outcome = "justpass",predictors = c("PageRank","tarEnt"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_qda_PRTE[[i]]<-ROC
}

ROC_ICS_justpass_qda_PRTE<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS, outcome = "justpass",predictors = c("PageRank","tarEnt"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_qda_PRTE[[i]]<-ROC
}

######PAGERANK HIDE NETWORK PREDICTORS######

ROC_PS_qda_PRH<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("PageRank","Hide"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_qda_PRH[[i]]<-ROC
}

ROC_CD_qda_PRH<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD,predictors = c("PageRank","Hide"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_qda_PRH[[i]]<-ROC
}

ROC_ICS_qda_PRH<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS,predictors = c("PageRank","Hide"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_qda_PRH[[i]]<-ROC
}

ROC_PS_justpass_qda_PRH<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,outcome = "justpass",predictors = c("PageRank","Hide"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_qda_PRH[[i]]<-ROC
}

ROC_CD_justpass_qda_PRH<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD, outcome = "justpass",predictors = c("PageRank","Hide"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_qda_PRH[[i]]<-ROC
}

ROC_ICS_justpass_qda_PRH<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS, outcome = "justpass",predictors = c("PageRank","Hide"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_qda_PRH[[i]]<-ROC
}

######TARGET ENTROPY HIDE NETWORK PREDICTORS######

ROC_PS_qda_TEH<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("tarEnt","Hide"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_qda_TEH[[i]]<-ROC
}

ROC_CD_qda_TEH<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD,predictors = c("tarEnt","Hide"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_qda_TEH[[i]]<-ROC
}

ROC_ICS_qda_TEH<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS,predictors = c("tarEnt","Hide"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_qda_TEH[[i]]<-ROC
}

ROC_PS_justpass_qda_TEH<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,outcome = "justpass",predictors = c("tarEnt","Hide"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_qda_TEH[[i]]<-ROC
}

ROC_CD_justpass_qda_TEH<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD, outcome = "justpass",predictors = c("tarEnt","Hide"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_qda_TEH[[i]]<-ROC
}

ROC_ICS_justpass_qda_TEH<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS, outcome = "justpass",predictors = c("tarEnt","Hide"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_qda_TEH[[i]]<-ROC
}

######TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_qda_TE<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("tarEnt"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_qda_TE[[i]]<-ROC
}

ROC_CD_qda_TE<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD,predictors = c("tarEnt"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_qda_TE[[i]]<-ROC
}

ROC_ICS_qda_TE<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS,predictors = c("tarEnt"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_qda_TE[[i]]<-ROC
}

ROC_PS_justpass_qda_TE<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,outcome = "justpass",predictors = c("tarEnt"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_qda_TE[[i]]<-ROC
}

ROC_CD_justpass_qda_TE<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD, outcome = "justpass",predictors = c("tarEnt"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_qda_TE[[i]]<-ROC
}

ROC_ICS_justpass_qda_TE<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS, outcome = "justpass",predictors = c("tarEnt"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_qda_TE[[i]]<-ROC
}

######HIDE NETWORK PREDICTORS######

ROC_PS_qda_H<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("Hide"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_qda_H[[i]]<-ROC
}

ROC_CD_qda_H<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD,predictors = c("Hide"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_qda_H[[i]]<-ROC
}

ROC_ICS_qda_H<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS,predictors = c("Hide"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_qda_H[[i]]<-ROC
}

ROC_PS_justpass_qda_H<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,outcome = "justpass",predictors = c("Hide"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_qda_H[[i]]<-ROC
}

ROC_CD_justpass_qda_H<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD, outcome = "justpass",predictors = c("Hide"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_qda_H[[i]]<-ROC
}

ROC_ICS_justpass_qda_H<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS, outcome = "justpass",predictors = c("Hide"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_qda_H[[i]]<-ROC
}

######TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_qda_PR<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,predictors = c("PageRank"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_qda_PR[[i]]<-ROC
}

ROC_CD_qda_PR<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD,predictors = c("PageRank"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_qda_PR[[i]]<-ROC
}

ROC_ICS_qda_PR<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS,predictors = c("PageRank"))
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_qda_PR[[i]]<-ROC
}

ROC_PS_justpass_qda_PR<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,outcome = "justpass",predictors = c("PageRank"))
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_qda_PR[[i]]<-ROC
}

ROC_CD_justpass_qda_PR<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD, outcome = "justpass",predictors = c("PageRank"))
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_qda_PR[[i]]<-ROC
}

ROC_ICS_justpass_qda_PR<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS, outcome = "justpass",predictors = c("PageRank"))
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
     file="data/ROC01_NB_qda.Rdata")



#########K NEAREST NEIGHBORS###########
#########K NEAREST NEIGHBORS###########
#########K NEAREST NEIGHBORS###########
#########K NEAREST NEIGHBORS###########

######ALL NETWORK PREDICTORS######
t1<-Sys.time()
ROC_PS_knn<-list()
for(i in 1:20){
  predPS_knn_PTH<-jackPredKNN(centPS,predictors = c("PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_knn_PTH$allpred)
  ROC_PS_knn[[i]]<-ROC
}

x<-c(1:7)
plot(x,ROC_PS_knn[[1]]$SR,type="b",ylim=c(0.6,0.8))
lines(x,ROC_PS_knn[[2]]$SR,type="b",pch=2)
lines(x,ROC_PS_knn[[3]]$SR,type="b",pch=3)
lines(x,ROC_PS_knn[[4]]$SR,type="b",pch=4)
lines(x,ROC_PS_knn[[5]]$SR,type="b",pch=5)
lines(x,ROC_PS_knn[[6]]$SR,type="b",pch=6)
lines(x,ROC_PS_knn[[7]]$SR,type="b",pch=7)
lines(x,ROC_PS_knn[[8]]$SR,type="b",pch=8)
lines(x,ROC_PS_knn[[9]]$SR,type="b",pch=9)
lines(x,ROC_PS_knn[[10]]$SR,type="b",pch=10)
lines(x,ROC_PS_knn[[11]]$SR,type="b",pch=11)
lines(x,ROC_PS_knn[[12]]$SR,type="b",pch=12)
lines(x,ROC_PS_knn[[13]]$SR,type="b",pch=13)
lines(x,ROC_PS_knn[[14]]$SR,type="b",pch=14)
lines(x,ROC_PS_knn[[15]]$SR,type="b",pch=15)
lines(x,ROC_PS_knn[[16]]$SR,type="b",pch=16)
lines(x,ROC_PS_knn[[17]]$SR,type="b",pch=17)
lines(x,ROC_PS_knn[[18]]$SR,type="b",pch=18)
lines(x,ROC_PS_knn[[19]]$SR,type="b",pch=19)
lines(x,ROC_PS_knn[[20]]$SR,type="b",pch=20)
abline(h=0.77)




ROC_CD_knn<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn[[i]]<-ROC
}

x<-c(1:7)
plot(x,ROC_CD_knn[[1]]$SR,type="b",ylim=c(0.6,0.8))
lines(x,ROC_CD_knn[[2]]$SR,type="b",pch=2)
lines(x,ROC_CD_knn[[3]]$SR,type="b",pch=3)
lines(x,ROC_CD_knn[[4]]$SR,type="b",pch=4)
lines(x,ROC_CD_knn[[5]]$SR,type="b",pch=5)
lines(x,ROC_CD_knn[[6]]$SR,type="b",pch=6)
lines(x,ROC_CD_knn[[7]]$SR,type="b",pch=7)
lines(x,ROC_CD_knn[[8]]$SR,type="b",pch=8)
lines(x,ROC_CD_knn[[9]]$SR,type="b",pch=9)
lines(x,ROC_CD_knn[[10]]$SR,type="b",pch=10)
lines(x,ROC_CD_knn[[11]]$SR,type="b",pch=11)
lines(x,ROC_CD_knn[[12]]$SR,type="b",pch=12)
lines(x,ROC_CD_knn[[13]]$SR,type="b",pch=13)
lines(x,ROC_CD_knn[[14]]$SR,type="b",pch=14)
lines(x,ROC_CD_knn[[15]]$SR,type="b",pch=15)
lines(x,ROC_CD_knn[[16]]$SR,type="b",pch=16)
lines(x,ROC_CD_knn[[17]]$SR,type="b",pch=17)
lines(x,ROC_CD_knn[[18]]$SR,type="b",pch=18)
lines(x,ROC_CD_knn[[19]]$SR,type="b",pch=19)
lines(x,ROC_CD_knn[[20]]$SR,type="b",pch=20)
abline(h=0.77)

ROC_ICS_knn<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn[[i]]<-ROC
}

x<-c(1:7)
plot(x,ROC_ICS_knn[[1]]$SR,type="b",ylim=c(0.6,0.8))
lines(x,ROC_ICS_knn[[2]]$SR,type="b",pch=2)
lines(x,ROC_ICS_knn[[3]]$SR,type="b",pch=3)
lines(x,ROC_ICS_knn[[4]]$SR,type="b",pch=4)
lines(x,ROC_ICS_knn[[5]]$SR,type="b",pch=5)
lines(x,ROC_ICS_knn[[6]]$SR,type="b",pch=6)
lines(x,ROC_ICS_knn[[7]]$SR,type="b",pch=7)
lines(x,ROC_ICS_knn[[8]]$SR,type="b",pch=8)
lines(x,ROC_ICS_knn[[9]]$SR,type="b",pch=9)
lines(x,ROC_ICS_knn[[10]]$SR,type="b",pch=10)
lines(x,ROC_ICS_knn[[11]]$SR,type="b",pch=11)
lines(x,ROC_ICS_knn[[12]]$SR,type="b",pch=12)
lines(x,ROC_ICS_knn[[13]]$SR,type="b",pch=13)
lines(x,ROC_ICS_knn[[14]]$SR,type="b",pch=14)
lines(x,ROC_ICS_knn[[15]]$SR,type="b",pch=15)
lines(x,ROC_ICS_knn[[16]]$SR,type="b",pch=16)
lines(x,ROC_ICS_knn[[17]]$SR,type="b",pch=17)
lines(x,ROC_ICS_knn[[18]]$SR,type="b",pch=18)
lines(x,ROC_ICS_knn[[19]]$SR,type="b",pch=19)
lines(x,ROC_ICS_knn[[20]]$SR,type="b",pch=20)
abline(h=0.77)

ROC_PS_justpass_knn<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn[[i]]<-ROC
}
x<-c(1:7)
plot(x,ROC_PS_justpass_knn[[1]]$SR,type="b",ylim=c(0.6,0.8))
lines(x,ROC_PS_justpass_knn[[2]]$SR,type="b",pch=2)
lines(x,ROC_PS_justpass_knn[[3]]$SR,type="b",pch=3)
lines(x,ROC_PS_justpass_knn[[4]]$SR,type="b",pch=4)
lines(x,ROC_PS_justpass_knn[[5]]$SR,type="b",pch=5)
lines(x,ROC_PS_justpass_knn[[6]]$SR,type="b",pch=6)
lines(x,ROC_PS_justpass_knn[[7]]$SR,type="b",pch=7)
lines(x,ROC_PS_justpass_knn[[8]]$SR,type="b",pch=8)
lines(x,ROC_PS_justpass_knn[[9]]$SR,type="b",pch=9)
lines(x,ROC_PS_justpass_knn[[10]]$SR,type="b",pch=10)
lines(x,ROC_PS_justpass_knn[[11]]$SR,type="b",pch=11)
lines(x,ROC_PS_justpass_knn[[12]]$SR,type="b",pch=12)
lines(x,ROC_PS_justpass_knn[[13]]$SR,type="b",pch=13)
lines(x,ROC_PS_justpass_knn[[14]]$SR,type="b",pch=14)
lines(x,ROC_PS_justpass_knn[[15]]$SR,type="b",pch=15)
lines(x,ROC_PS_justpass_knn[[16]]$SR,type="b",pch=16)
lines(x,ROC_PS_justpass_knn[[17]]$SR,type="b",pch=17)
lines(x,ROC_PS_justpass_knn[[18]]$SR,type="b",pch=18)
lines(x,ROC_PS_justpass_knn[[19]]$SR,type="b",pch=19)
lines(x,ROC_PS_justpass_knn[[20]]$SR,type="b",pch=20)
abline(h=0.58)

ROC_CD_justpass_knn<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn[[i]]<-ROC
}

x<-c(1:7)
plot(x,ROC_CD_justpass_knn[[1]]$SR,type="b",ylim=c(0.6,0.8))
lines(x,ROC_CD_justpass_knn[[2]]$SR,type="b",pch=2)
lines(x,ROC_CD_justpass_knn[[3]]$SR,type="b",pch=3)
lines(x,ROC_CD_justpass_knn[[4]]$SR,type="b",pch=4)
lines(x,ROC_CD_justpass_knn[[5]]$SR,type="b",pch=5)
lines(x,ROC_CD_justpass_knn[[6]]$SR,type="b",pch=6)
lines(x,ROC_CD_justpass_knn[[7]]$SR,type="b",pch=7)
lines(x,ROC_CD_justpass_knn[[8]]$SR,type="b",pch=8)
lines(x,ROC_CD_justpass_knn[[9]]$SR,type="b",pch=9)
lines(x,ROC_CD_justpass_knn[[10]]$SR,type="b",pch=10)
lines(x,ROC_CD_justpass_knn[[11]]$SR,type="b",pch=11)
lines(x,ROC_CD_justpass_knn[[12]]$SR,type="b",pch=12)
lines(x,ROC_CD_justpass_knn[[13]]$SR,type="b",pch=13)
lines(x,ROC_CD_justpass_knn[[14]]$SR,type="b",pch=14)
lines(x,ROC_CD_justpass_knn[[15]]$SR,type="b",pch=15)
lines(x,ROC_CD_justpass_knn[[16]]$SR,type="b",pch=16)
lines(x,ROC_CD_justpass_knn[[17]]$SR,type="b",pch=17)
lines(x,ROC_CD_justpass_knn[[18]]$SR,type="b",pch=18)
lines(x,ROC_CD_justpass_knn[[19]]$SR,type="b",pch=19)
lines(x,ROC_CD_justpass_knn[[20]]$SR,type="b",pch=20)
abline(h=0.58)

ROC_ICS_justpass_knn<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn[[i]]<-ROC
}

x<-c(1:7)
plot(x,ROC_ICS_justpass_knn[[1]]$SR,type="b",ylim=c(0.6,0.8))
lines(x,ROC_ICS_justpass_knn[[2]]$SR,type="b",pch=2)
lines(x,ROC_ICS_justpass_knn[[3]]$SR,type="b",pch=3)
lines(x,ROC_ICS_justpass_knn[[4]]$SR,type="b",pch=4)
lines(x,ROC_ICS_justpass_knn[[5]]$SR,type="b",pch=5)
lines(x,ROC_ICS_justpass_knn[[6]]$SR,type="b",pch=6)
lines(x,ROC_ICS_justpass_knn[[7]]$SR,type="b",pch=7)
lines(x,ROC_ICS_justpass_knn[[8]]$SR,type="b",pch=8)
lines(x,ROC_ICS_justpass_knn[[9]]$SR,type="b",pch=9)
lines(x,ROC_ICS_justpass_knn[[10]]$SR,type="b",pch=10)
lines(x,ROC_ICS_justpass_knn[[11]]$SR,type="b",pch=11)
lines(x,ROC_ICS_justpass_knn[[12]]$SR,type="b",pch=12)
lines(x,ROC_ICS_justpass_knn[[13]]$SR,type="b",pch=13)
lines(x,ROC_ICS_justpass_knn[[14]]$SR,type="b",pch=14)
lines(x,ROC_ICS_justpass_knn[[15]]$SR,type="b",pch=15)
lines(x,ROC_ICS_justpass_knn[[16]]$SR,type="b",pch=16)
lines(x,ROC_ICS_justpass_knn[[17]]$SR,type="b",pch=17)
lines(x,ROC_ICS_justpass_knn[[18]]$SR,type="b",pch=18)
lines(x,ROC_ICS_justpass_knn[[19]]$SR,type="b",pch=19)
lines(x,ROC_ICS_justpass_knn[[20]]$SR,type="b",pch=20)
abline(h=0.77)

######PAGERANK TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_knn_PRTE<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,predictors = c("PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_knn_PRTE[[i]]<-ROC
}

ROC_CD_knn_PRTE<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_PRTE[[i]]<-ROC
}

ROC_ICS_knn_PRTE<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_PRTE[[i]]<-ROC
}

ROC_PS_justpass_knn_PRTE<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_PRTE[[i]]<-ROC
}

ROC_CD_justpass_knn_PRTE<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_PRTE[[i]]<-ROC
}

ROC_ICS_justpass_knn_PRTE<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("PageRank","tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_PRTE[[i]]<-ROC
}

######PAGERANK HIDE NETWORK PREDICTORS######

ROC_PS_knn_PRH<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,predictors = c("PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_knn_PRH[[i]]<-ROC
}

ROC_CD_knn_PRH<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_PRH[[i]]<-ROC
}

ROC_ICS_knn_PRH<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_PRH[[i]]<-ROC
}

ROC_PS_justpass_knn_PRH<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_PRH[[i]]<-ROC
}

ROC_CD_justpass_knn_PRH<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_PRH[[i]]<-ROC
}

ROC_ICS_justpass_knn_PRH<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("PageRank","Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_PRH[[i]]<-ROC
}

######TARGET ENTROPY HIDE NETWORK PREDICTORS######

ROC_PS_knn_TEH<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,predictors = c("tarEnt","Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_knn_TEH[[i]]<-ROC
}

ROC_CD_knn_TEH<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("tarEnt","Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_TEH[[i]]<-ROC
}

ROC_ICS_knn_TEH<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("tarEnt","Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_TEH[[i]]<-ROC
}

ROC_PS_justpass_knn_TEH<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("tarEnt","Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_TEH[[i]]<-ROC
}

ROC_CD_justpass_knn_TEH<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("tarEnt","Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_TEH[[i]]<-ROC
}

ROC_ICS_justpass_knn_TEH<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("tarEnt","Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_TEH[[i]]<-ROC
}

######TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_knn_TE<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,predictors = c("tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_knn_TE[[i]]<-ROC
}

ROC_CD_knn_TE<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_TE[[i]]<-ROC
}

ROC_ICS_knn_TE<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_TE[[i]]<-ROC
}

ROC_PS_justpass_knn_TE<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_TE[[i]]<-ROC
}

ROC_CD_justpass_knn_TE<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_TE[[i]]<-ROC
}

ROC_ICS_justpass_knn_TE<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("tarEnt"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_TE[[i]]<-ROC
}

######HIDE NETWORK PREDICTORS######

ROC_PS_knn_H<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,predictors = c("Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_knn_H[[i]]<-ROC
}

ROC_CD_knn_H<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_H[[i]]<-ROC
}

ROC_ICS_knn_H<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_H[[i]]<-ROC
}

ROC_PS_justpass_knn_H<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_H[[i]]<-ROC
}

ROC_CD_justpass_knn_H<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_H[[i]]<-ROC
}

ROC_ICS_justpass_knn_H<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn_H[[i]]<-ROC
}

######TARGET ENTROPY NETWORK PREDICTORS######

ROC_PS_knn_PR<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,predictors = c("PageRank"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_knn_PR[[i]]<-ROC
}

ROC_CD_knn_PR<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("PageRank"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn_PR[[i]]<-ROC
}

ROC_ICS_knn_PR<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("PageRank"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn_PR[[i]]<-ROC
}

ROC_PS_justpass_knn_PR<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("PageRank"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn_PR[[i]]<-ROC
}

ROC_CD_justpass_knn_PR<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("PageRank"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn_PR[[i]]<-ROC
}

ROC_ICS_justpass_knn_PR<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("PageRank"),nK = i)
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
     file="data/ROC01_NB_knn.Rdata")

t2<-Sys.time()
t2-t1

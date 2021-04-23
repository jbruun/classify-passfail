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

lazy<-table(centPS[[1]]$pass)[2]/sum(table(centPS[[1]]$pass))
lazy_jp<-table(centPS[[1]]$justpass)[2]/sum(table(centPS[[1]]$justpass))
pass<-centPS[[1]]$pass
justpass<-centPS[[1]]$justpass

p_rate<-function(d,i) table(d)[2]/sum(table(d))
boot(pass,p_rate,R=1000)

bs<-function(d,m,R){
  p_r<-vector()
  for(i in 1:R){
  x<-sample(d,m)
  p_r[i]<-table(x)[2]/sum(table(x))
  }
  m<-mean(p_r)
  sdev<-sd(p_r)
  return(data.frame(m,sdev))
}

#########LOGISTIC REGRESSION###########
#########LOGISTIC REGRESSION###########
#########LOGISTIC REGRESSION###########
#########LOGISTIC REGRESSION###########

######ALL NETWORK PREDICTORS######


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
##CD layer
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

#ICS layer
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_log_PTH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Logistic Regression Model", sub="Pass/fail (n=166), PR-TE-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_log_PTH_ciL, x, PS_log_PTH_ciH, length=0.05, angle=90, code=3)
lines(x,CD_log_PTH_auc,pch=17,type="b",col="red")
arrows(x, CD_log_PTH_ciL, x, CD_log_PTH_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_log_PTH_auc,pch=17,type="b",col="blue")
arrows(x, ICS_log_PTH_ciL, x, ICS_log_PTH_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy)

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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_jp_log_PTH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Logistic Regression Model", sub="Just pass/Just fail (n=67), PR-TE-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_log_PTH_ciL, x, PS_jp_log_PTH_ciH, length=0.05, angle=90, code=3)
lines(x,CD_jp_log_PTH_auc,pch=17,type="b",col="red")
arrows(x, CD_jp_log_PTH_ciL, x, CD_jp_log_PTH_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_jp_log_PTH_auc,pch=17,type="b",col="blue")
arrows(x, ICS_jp_log_PTH_ciL, x, ICS_jp_log_PTH_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy_jp)

######PAGERANK TARGET ENTROPY NETWORK PREDICTORS######


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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_log_PT_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Logistic Regression Model", sub="Pass/fail (n=166), PR-TE",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_log_PT_ciL, x, PS_log_PT_ciH, length=0.05, angle=90, code=3)
lines(x,CD_log_PT_auc,pch=17,type="b",col="red")
arrows(x, CD_log_PT_ciL, x, CD_log_PT_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_log_PT_auc,pch=17,type="b",col="blue")
arrows(x, ICS_log_PT_ciL, x, ICS_log_PT_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy)

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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_jp_log_PT_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Logistic Regression Model", sub="Just pass/Just fail (n=67), PR-TE",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_log_PT_ciL, x, PS_jp_log_PT_ciH, length=0.05, angle=90, code=3)
lines(x,CD_jp_log_PT_auc,pch=17,type="b",col="red")
arrows(x, CD_jp_log_PT_ciL, x, CD_jp_log_PT_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_jp_log_PT_auc,pch=17,type="b",col="blue")
arrows(x, ICS_jp_log_PT_ciL, x, ICS_jp_log_PT_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy_jp)


######PAGERANK HIDE NETWORK PREDICTORS######


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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_log_PH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Logistic Regression Model", sub="Pass/fail (n=166), PR-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_log_PH_ciL, x, PS_log_PH_ciH, length=0.05, angle=90, code=3)
lines(x,CD_log_PH_auc,pch=17,type="b",col="red")
arrows(x, CD_log_PH_ciL, x, CD_log_PH_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_log_PH_auc,pch=17,type="b",col="blue")
arrows(x, ICS_log_PH_ciL, x, ICS_log_PH_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy)

###JUST PASS
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_jp_log_PH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Logistic Regression Model", sub="Just pass/just fail (n=67), PR-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_log_PH_ciL, x, PS_jp_log_PH_ciH, length=0.05, angle=90, code=3)
lines(x,CD_jp_log_PH_auc,pch=17,type="b",col="red")
arrows(x, CD_jp_log_PH_ciL, x, CD_jp_log_PH_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_jp_log_PH_auc,pch=17,type="b",col="blue")
arrows(x, ICS_jp_log_PH_ciL, x, ICS_jp_log_PH_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy_jp)
######TARGET ENTROPY HIDE NETWORK PREDICTORS######


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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_log_TH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Logistic Regression Model", sub="Pass/fail (n=166), TE-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_log_TH_ciL, x, PS_log_TH_ciH, length=0.05, angle=90, code=3)
lines(x,CD_log_TH_auc,pch=17,type="b",col="red")
arrows(x, CD_log_TH_ciL, x, CD_log_TH_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_log_TH_auc,pch=17,type="b",col="blue")
arrows(x, ICS_log_TH_ciL, x, ICS_log_TH_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy)

###JUST PASS
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_jp_log_TH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Logistic Regression Model", sub="Just pass/Just fail (n=67), TE-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_log_TH_ciL, x, PS_jp_log_TH_ciH, length=0.05, angle=90, code=3)
lines(x,CD_jp_log_TH_auc,pch=17,type="b",col="red")
arrows(x, CD_jp_log_TH_ciL, x, CD_jp_log_TH_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_jp_log_TH_auc,pch=17,type="b",col="blue")
arrows(x, ICS_jp_log_TH_ciL, x, ICS_jp_log_TH_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy_jp)

######TARGET ENTROPY NETWORK PREDICTORS######

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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_log_T_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Logistic Regression Model", sub="Pass/fail (n=166), TE",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_log_T_ciL, x, PS_log_T_ciH, length=0.05, angle=90, code=3)
lines(x,CD_log_T_auc,pch=17,type="b",col="red")
arrows(x, CD_log_T_ciL, x, CD_log_T_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_log_T_auc,pch=17,type="b",col="blue")
arrows(x, ICS_log_T_ciL, x, ICS_log_T_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy)

###JUST PASS
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_jp_log_T_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Logistic Regression Model", sub="Just pass/Just fail (n=67), TE",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_log_T_ciL, x, PS_jp_log_T_ciH, length=0.05, angle=90, code=3)
lines(x,CD_jp_log_T_auc,pch=17,type="b",col="red")
arrows(x, CD_jp_log_T_ciL, x, CD_jp_log_T_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_jp_log_T_auc,pch=17,type="b",col="blue")
arrows(x, ICS_jp_log_T_ciL, x, ICS_jp_log_T_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy_jp)

######HIDE NETWORK PREDICTORS######

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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_log_H_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Logistic Regression Model", sub="Pass/fail (n=166), TE-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_log_H_ciL, x, PS_log_H_ciH, length=0.05, angle=90, code=3)
lines(x,CD_log_H_auc,pch=17,type="b",col="red")
arrows(x, CD_log_H_ciL, x, CD_log_H_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_log_H_auc,pch=17,type="b",col="blue")
arrows(x, ICS_log_H_ciL, x, ICS_log_H_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy)

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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_jp_log_H_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Logistic Regression Model", sub="Just pass/Just fail (n=67), TE-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_log_H_ciL, x, PS_jp_log_H_ciH, length=0.05, angle=90, code=3)
lines(x,CD_jp_log_H_auc,pch=17,type="b",col="red")
arrows(x, CD_jp_log_H_ciL, x, CD_jp_log_H_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_jp_log_H_auc,pch=17,type="b",col="blue")
arrows(x, ICS_jp_log_H_ciL, x, ICS_jp_log_H_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy_jp)

######PAGERANK NETWORK PREDICTORS######

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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_log_P_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Logistic Regression Model", sub="Pass/fail (n=166), PR",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_log_P_ciL, x, PS_log_P_ciH, length=0.05, angle=90, code=3)
lines(x,CD_log_P_auc,pch=17,type="b",col="red")
arrows(x, CD_log_P_ciL, x, CD_log_P_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_log_P_auc,pch=17,type="b",col="blue")
arrows(x, ICS_log_P_ciL, x, ICS_log_P_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy)

###JUST PASS
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_jp_log_P_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Logistic Regression Model", sub="Just pass/Just fail (n=67), PR",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_log_P_ciL, x, PS_jp_log_P_ciH, length=0.05, angle=90, code=3)
lines(x,CD_jp_log_P_auc,pch=17,type="b",col="red")
arrows(x, CD_jp_log_P_ciL, x, CD_jp_log_P_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_jp_log_P_auc,pch=17,type="b",col="blue")
arrows(x, ICS_jp_log_P_ciL, x, ICS_jp_log_P_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy_jp)


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
##CD layer
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

#ICS layer
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_lda_PTH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Linear Discriminant Analysis Model", sub="Pass/fail (n=166), PR-TE-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_lda_PTH_ciL, x, PS_lda_PTH_ciH, length=0.05, angle=90, code=3)
lines(x,CD_lda_PTH_auc,pch=17,type="b",col="red")
arrows(x, CD_lda_PTH_ciL, x, CD_lda_PTH_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_lda_PTH_auc,pch=17,type="b",col="blue")
arrows(x, ICS_lda_PTH_ciL, x, ICS_lda_PTH_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy)

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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_jp_lda_PTH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Linear Discriminant Analysis Model", sub="Just pass/Just fail (n=67), PR-TE-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_lda_PTH_ciL, x, PS_jp_lda_PTH_ciH, length=0.05, angle=90, code=3)
lines(x,CD_jp_lda_PTH_auc,pch=17,type="b",col="red")
arrows(x, CD_jp_lda_PTH_ciL, x, CD_jp_lda_PTH_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_jp_lda_PTH_auc,pch=17,type="b",col="blue")
arrows(x, ICS_jp_lda_PTH_ciL, x, ICS_jp_lda_PTH_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy_jp)

######PAGERANK TARGET ENTROPY NETWORK PREDICTORS######


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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_lda_PT_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Linear Discriminant Analysis Model", sub="Pass/fail (n=166), PR-TE",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_lda_PT_ciL, x, PS_lda_PT_ciH, length=0.05, angle=90, code=3)
lines(x,CD_lda_PT_auc,pch=17,type="b",col="red")
arrows(x, CD_lda_PT_ciL, x, CD_lda_PT_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_lda_PT_auc,pch=17,type="b",col="blue")
arrows(x, ICS_lda_PT_ciL, x, ICS_lda_PT_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy)

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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_jp_lda_PT_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Linear Discriminant Analysis Model", sub="Just pass/Just fail (n=67), PR-TE",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_lda_PT_ciL, x, PS_jp_lda_PT_ciH, length=0.05, angle=90, code=3)
lines(x,CD_jp_lda_PT_auc,pch=17,type="b",col="red")
arrows(x, CD_jp_lda_PT_ciL, x, CD_jp_lda_PT_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_jp_lda_PT_auc,pch=17,type="b",col="blue")
arrows(x, ICS_jp_lda_PT_ciL, x, ICS_jp_lda_PT_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy_jp)


######PAGERANK HIDE NETWORK PREDICTORS######


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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_lda_PH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Linear Discriminant Analysis Model", sub="Pass/fail (n=166), PR-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_lda_PH_ciL, x, PS_lda_PH_ciH, length=0.05, angle=90, code=3)
lines(x,CD_lda_PH_auc,pch=17,type="b",col="red")
arrows(x, CD_lda_PH_ciL, x, CD_lda_PH_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_lda_PH_auc,pch=17,type="b",col="blue")
arrows(x, ICS_lda_PH_ciL, x, ICS_lda_PH_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy)

###JUST PASS
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_jp_lda_PH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Linear Discriminant Analysis Model", sub="Just pass/just fail (n=67), PR-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_lda_PH_ciL, x, PS_jp_lda_PH_ciH, length=0.05, angle=90, code=3)
lines(x,CD_jp_lda_PH_auc,pch=17,type="b",col="red")
arrows(x, CD_jp_lda_PH_ciL, x, CD_jp_lda_PH_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_jp_lda_PH_auc,pch=17,type="b",col="blue")
arrows(x, ICS_jp_lda_PH_ciL, x, ICS_jp_lda_PH_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy_jp)
######TARGET ENTROPY HIDE NETWORK PREDICTORS######


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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_lda_TH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Linear Discriminant Analysis Model", sub="Pass/fail (n=166), TE-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_lda_TH_ciL, x, PS_lda_TH_ciH, length=0.05, angle=90, code=3)
lines(x,CD_lda_TH_auc,pch=17,type="b",col="red")
arrows(x, CD_lda_TH_ciL, x, CD_lda_TH_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_lda_TH_auc,pch=17,type="b",col="blue")
arrows(x, ICS_lda_TH_ciL, x, ICS_lda_TH_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy)

###JUST PASS
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_jp_lda_TH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Linear Discriminant Analysis Model", sub="Just pass/Just fail (n=67), TE-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_lda_TH_ciL, x, PS_jp_lda_TH_ciH, length=0.05, angle=90, code=3)
lines(x,CD_jp_lda_TH_auc,pch=17,type="b",col="red")
arrows(x, CD_jp_lda_TH_ciL, x, CD_jp_lda_TH_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_jp_lda_TH_auc,pch=17,type="b",col="blue")
arrows(x, ICS_jp_lda_TH_ciL, x, ICS_jp_lda_TH_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy_jp)

######TARGET ENTROPY NETWORK PREDICTORS######

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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_lda_T_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Linear Discriminant Analysis Model", sub="Pass/fail (n=166), TE",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_lda_T_ciL, x, PS_lda_T_ciH, length=0.05, angle=90, code=3)
lines(x,CD_lda_T_auc,pch=17,type="b",col="red")
arrows(x, CD_lda_T_ciL, x, CD_lda_T_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_lda_T_auc,pch=17,type="b",col="blue")
arrows(x, ICS_lda_T_ciL, x, ICS_lda_T_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy)

###JUST PASS
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_jp_lda_T_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Linear Discriminant Analysis Model", sub="Just pass/Just fail (n=67), TE",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_lda_T_ciL, x, PS_jp_lda_T_ciH, length=0.05, angle=90, code=3)
lines(x,CD_jp_lda_T_auc,pch=17,type="b",col="red")
arrows(x, CD_jp_lda_T_ciL, x, CD_jp_lda_T_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_jp_lda_T_auc,pch=17,type="b",col="blue")
arrows(x, ICS_jp_lda_T_ciL, x, ICS_jp_lda_T_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy_jp)

######HIDE NETWORK PREDICTORS######

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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_lda_H_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Linear Discriminant Analysis Model", sub="Pass/fail (n=166), TE-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_lda_H_ciL, x, PS_lda_H_ciH, length=0.05, angle=90, code=3)
lines(x,CD_lda_H_auc,pch=17,type="b",col="red")
arrows(x, CD_lda_H_ciL, x, CD_lda_H_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_lda_H_auc,pch=17,type="b",col="blue")
arrows(x, ICS_lda_H_ciL, x, ICS_lda_H_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy)

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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_jp_lda_H_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Linear Discriminant Analysis Model", sub="Just pass/Just fail (n=67), TE-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_lda_H_ciL, x, PS_jp_lda_H_ciH, length=0.05, angle=90, code=3)
lines(x,CD_jp_lda_H_auc,pch=17,type="b",col="red")
arrows(x, CD_jp_lda_H_ciL, x, CD_jp_lda_H_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_jp_lda_H_auc,pch=17,type="b",col="blue")
arrows(x, ICS_jp_lda_H_ciL, x, ICS_jp_lda_H_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy_jp)

######PAGERANK NETWORK PREDICTORS######

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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_lda_P_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Linear Discriminant Analysis Model", sub="Pass/fail (n=166), PR",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_lda_P_ciL, x, PS_lda_P_ciH, length=0.05, angle=90, code=3)
lines(x,CD_lda_P_auc,pch=17,type="b",col="red")
arrows(x, CD_lda_P_ciL, x, CD_lda_P_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_lda_P_auc,pch=17,type="b",col="blue")
arrows(x, ICS_lda_P_ciL, x, ICS_lda_P_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy)

###JUST PASS
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_jp_lda_P_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Linear Discriminant Analysis Model", sub="Just pass/Just fail (n=67), PR",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_lda_P_ciL, x, PS_jp_lda_P_ciH, length=0.05, angle=90, code=3)
lines(x,CD_jp_lda_P_auc,pch=17,type="b",col="red")
arrows(x, CD_jp_lda_P_ciL, x, CD_jp_lda_P_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_jp_lda_P_auc,pch=17,type="b",col="blue")
arrows(x, ICS_jp_lda_P_ciL, x, ICS_jp_lda_P_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy_jp)

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
##CD layer
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

#ICS layer
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_qda_PTH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Quadratic Discriminant Analysis Model", sub="Pass/fail (n=166), PR-TE-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_qda_PTH_ciL, x, PS_qda_PTH_ciH, length=0.05, angle=90, code=3)
lines(x,CD_qda_PTH_auc,pch=17,type="b",col="red")
arrows(x, CD_qda_PTH_ciL, x, CD_qda_PTH_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_qda_PTH_auc,pch=17,type="b",col="blue")
arrows(x, ICS_qda_PTH_ciL, x, ICS_qda_PTH_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy)

###JUST PASS
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

predCD_jp_qda_PTH<-jackPredQDA(centCD, outcome = "justpass",predictors = c("PageRank","tarEnt", "Hide"))
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_jp_qda_PTH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Quadratic Discriminant Analysis Model", sub="Just pass/Just fail (n=67), PR-TE-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_qda_PTH_ciL, x, PS_jp_qda_PTH_ciH, length=0.05, angle=90, code=3)
lines(x,CD_jp_qda_PTH_auc,pch=17,type="b",col="red")
arrows(x, CD_jp_qda_PTH_ciL, x, CD_jp_qda_PTH_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_jp_qda_PTH_auc,pch=17,type="b",col="blue")
arrows(x, ICS_jp_qda_PTH_ciL, x, ICS_jp_qda_PTH_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy_jp)

######PAGERANK TARGET ENTROPY NETWORK PREDICTORS######


predPS_qda_PT<-jackPredQDA(centPS,predictors = c("PageRank","tarEnt"))
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
predCD_qda_PT<-jackPredQDA(centCD,predictors = c("PageRank","tarEnt"))
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
predICS_qda_PT<-jackPredQDA(centICS,predictors = c("PageRank","tarEnt"))
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_qda_PT_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Quadratic Discriminant Analysis Model", sub="Pass/fail (n=166), PR-TE",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_qda_PT_ciL, x, PS_qda_PT_ciH, length=0.05, angle=90, code=3)
lines(x,CD_qda_PT_auc,pch=17,type="b",col="red")
arrows(x, CD_qda_PT_ciL, x, CD_qda_PT_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_qda_PT_auc,pch=17,type="b",col="blue")
arrows(x, ICS_qda_PT_ciL, x, ICS_qda_PT_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy)

###JUST PASS
predPS_jp_qda_PT<-jackPredQDA(centPS,outcome = "justpass",predictors = c("PageRank","tarEnt"))
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

predCD_jp_qda_PT<-jackPredQDA(centCD, outcome = "justpass",predictors = c("PageRank","tarEnt"))
rocCD_jp_qda_PT<-list()
rocCD_jp_qda_PT[[1]]<-roc(predCD_jp_qda_PT$justpass,as.numeric(predCD_jp_qda_PT$Week1),auc=T,ci=T)
rocCD_jp_qda_PT[[2]]<-roc(predCD_jp_qda_PT$justpass,as.numeric(predCD_jp_qda_PT$Week2),auc=T,ci=T)
rocCD_jp_qda_PT[[3]]<-roc(predCD_jp_qda_PT$justpass,as.numeric(predCD_jp_qda_PT$Week3),auc=T,ci=T)
rocCD_jp_qda_PT[[4]]<-roc(predCD_jp_qda_PT$justpass,as.numeric(predCD_jp_qda_PT$Week4),auc=T,ci=T)
rocCD_jp_qda_PT[[5]]<-roc(predCD_jp_qda_PT$justpass,as.numeric(predCD_jp_qda_PT$Week5),auc=T,ci=T)
rocCD_jp_qda_PT[[6]]<-roc(predCD_jp_qda_PT$justpass,as.numeric(predCD_jp_qda_PT$Week6),auc=T,ci=T)
rocCD_jp_qda_PT[[7]]<-roc(predCD_jp_qda_PT$justpass,as.numeric(predCD_jp_qda_PT$Week7),auc=T,ci=T)
CD_jp_qda_PT_auc<-c(rocCD_jp_qda_PT[[1]]$auc,rocCD_jp_qda_PT[[2]]$auc,rocCD_jp_qda_PT[[3]]$auc,rocCD_jp_qda_PT[[4]]$auc,
                    rocCD_jp_qda_PT[[5]]$auc,rocCD_jp_qda_PT[[6]]$auc,rocCD_jp_qda_PT[[7]]$auc)
CD_jp_qda_PT_ciL<-c(rocCD_jp_qda_PT[[1]]$ci[1],rocCD_jp_qda_PT[[2]]$ci[1],rocCD_jp_qda_PT[[3]]$ci[1],rocCD_jp_qda_PT[[4]]$ci[1],
                    rocCD_jp_qda_PT[[5]]$ci[1],rocCD_jp_qda_PT[[6]]$ci[1],rocCD_jp_qda_PT[[7]]$ci[1])
CD_jp_qda_PT_ciH<-c(rocCD_jp_qda_PT[[1]]$ci[3],rocCD_jp_qda_PT[[2]]$ci[3],rocCD_jp_qda_PT[[3]]$ci[3],rocCD_jp_qda_PT[[4]]$ci[3],
                    rocCD_jp_qda_PT[[5]]$ci[3],rocCD_jp_qda_PT[[6]]$ci[3],rocCD_jp_qda_PT[[7]]$ci[3])


predICS_jp_qda_PT<-jackPredQDA(centICS, outcome = "justpass",predictors = c("PageRank","tarEnt"))
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_jp_qda_PT_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Quadratic Discriminant Analysis Model", sub="Just pass/Just fail (n=67), PR-TE",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_qda_PT_ciL, x, PS_jp_qda_PT_ciH, length=0.05, angle=90, code=3)
lines(x,CD_jp_qda_PT_auc,pch=17,type="b",col="red")
arrows(x, CD_jp_qda_PT_ciL, x, CD_jp_qda_PT_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_jp_qda_PT_auc,pch=17,type="b",col="blue")
arrows(x, ICS_jp_qda_PT_ciL, x, ICS_jp_qda_PT_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy_jp)


######PAGERANK HIDE NETWORK PREDICTORS######


predPS_qda_PH<-jackPredQDA(centPS,predictors = c("PageRank", "Hide"))
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
predCD_qda_PH<-jackPredQDA(centCD,predictors = c("PageRank", "Hide"))
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
predICS_qda_PH<-jackPredQDA(centICS,predictors = c("PageRank", "Hide"))
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_qda_PH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Quadratic Discriminant Analysis Model", sub="Pass/fail (n=166), PR-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_qda_PH_ciL, x, PS_qda_PH_ciH, length=0.05, angle=90, code=3)
lines(x,CD_qda_PH_auc,pch=17,type="b",col="red")
arrows(x, CD_qda_PH_ciL, x, CD_qda_PH_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_qda_PH_auc,pch=17,type="b",col="blue")
arrows(x, ICS_qda_PH_ciL, x, ICS_qda_PH_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy)

###JUST PASS
predPS_jp_qda_PH<-jackPredQDA(centPS,outcome = "justpass",predictors = c("PageRank", "Hide"))
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

predCD_jp_qda_PH<-jackPredQDA(centCD, outcome = "justpass",predictors = c("PageRank", "Hide"))
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


predICS_jp_qda_PH<-jackPredQDA(centICS, outcome = "justpass",predictors = c("PageRank", "Hide"))
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_jp_qda_PH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Quadratic Discriminant Analysis Model", sub="Just pass/just fail (n=67), PR-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_qda_PH_ciL, x, PS_jp_qda_PH_ciH, length=0.05, angle=90, code=3)
lines(x,CD_jp_qda_PH_auc,pch=17,type="b",col="red")
arrows(x, CD_jp_qda_PH_ciL, x, CD_jp_qda_PH_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_jp_qda_PH_auc,pch=17,type="b",col="blue")
arrows(x, ICS_jp_qda_PH_ciL, x, ICS_jp_qda_PH_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy_jp)
######TARGET ENTROPY HIDE NETWORK PREDICTORS######


predPS_qda_TH<-jackPredQDA(centPS,predictors = c("tarEnt", "Hide"))
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
predCD_qda_TH<-jackPredQDA(centCD,predictors = c("tarEnt", "Hide"))
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
predICS_qda_TH<-jackPredQDA(centICS,predictors = c("tarEnt", "Hide"))
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_qda_TH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Quadratic Discriminant Analysis Model", sub="Pass/fail (n=166), TE-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_qda_TH_ciL, x, PS_qda_TH_ciH, length=0.05, angle=90, code=3)
lines(x,CD_qda_TH_auc,pch=17,type="b",col="red")
arrows(x, CD_qda_TH_ciL, x, CD_qda_TH_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_qda_TH_auc,pch=17,type="b",col="blue")
arrows(x, ICS_qda_TH_ciL, x, ICS_qda_TH_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy)

###JUST PASS
predPS_jp_qda_TH<-jackPredQDA(centPS,outcome = "justpass",predictors = c("tarEnt", "Hide"))
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

predCD_jp_qda_TH<-jackPredQDA(centCD, outcome = "justpass",predictors = c("tarEnt", "Hide"))
rocCD_jp_qda_TH<-list()
rocCD_jp_qda_TH[[1]]<-roc(predCD_jp_qda_TH$justpass,as.numeric(predCD_jp_qda_TH$Week1),auc=T,ci=T)
rocCD_jp_qda_TH[[2]]<-roc(predCD_jp_qda_TH$justpass,as.numeric(predCD_jp_qda_TH$Week2),auc=T,ci=T)
rocCD_jp_qda_TH[[3]]<-roc(predCD_jp_qda_TH$justpass,as.numeric(predCD_jp_qda_TH$Week3),auc=T,ci=T)
rocCD_jp_qda_TH[[4]]<-roc(predCD_jp_qda_TH$justpass,as.numeric(predCD_jp_qda_TH$Week4),auc=T,ci=T)
rocCD_jp_qda_TH[[5]]<-roc(predCD_jp_qda_TH$justpass,as.numeric(predCD_jp_qda_TH$Week5),auc=T,ci=T)
rocCD_jp_qda_TH[[6]]<-roc(predCD_jp_qda_TH$justpass,as.numeric(predCD_jp_qda_TH$Week6),auc=T,ci=T)
rocCD_jp_qda_TH[[7]]<-roc(predCD_jp_qda_TH$justpass,as.numeric(predCD_jp_qda_TH$Week7),auc=T,ci=T)
CD_jp_qda_TH_auc<-c(rocCD_jp_qda_TH[[1]]$auc,rocCD_jp_qda_TH[[2]]$auc,rocCD_jp_qda_TH[[3]]$auc,rocCD_jp_qda_TH[[4]]$auc,
                    rocCD_jp_qda_TH[[5]]$auc,rocCD_jp_qda_TH[[6]]$auc,rocCD_jp_qda_TH[[7]]$auc)
CD_jp_qda_TH_ciL<-c(rocCD_jp_qda_TH[[1]]$ci[1],rocCD_jp_qda_TH[[2]]$ci[1],rocCD_jp_qda_TH[[3]]$ci[1],rocCD_jp_qda_TH[[4]]$ci[1],
                    rocCD_jp_qda_TH[[5]]$ci[1],rocCD_jp_qda_TH[[6]]$ci[1],rocCD_jp_qda_TH[[7]]$ci[1])
CD_jp_qda_TH_ciH<-c(rocCD_jp_qda_TH[[1]]$ci[3],rocCD_jp_qda_TH[[2]]$ci[3],rocCD_jp_qda_TH[[3]]$ci[3],rocCD_jp_qda_TH[[4]]$ci[3],
                    rocCD_jp_qda_TH[[5]]$ci[3],rocCD_jp_qda_TH[[6]]$ci[3],rocCD_jp_qda_TH[[7]]$ci[3])


predICS_jp_qda_TH<-jackPredQDA(centICS, outcome = "justpass",predictors = c("tarEnt", "Hide"))
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_jp_qda_TH_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Quadratic Discriminant Analysis Model", sub="Just pass/Just fail (n=67), TE-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_qda_TH_ciL, x, PS_jp_qda_TH_ciH, length=0.05, angle=90, code=3)
lines(x,CD_jp_qda_TH_auc,pch=17,type="b",col="red")
arrows(x, CD_jp_qda_TH_ciL, x, CD_jp_qda_TH_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_jp_qda_TH_auc,pch=17,type="b",col="blue")
arrows(x, ICS_jp_qda_TH_ciL, x, ICS_jp_qda_TH_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy_jp)

######TARGET ENTROPY NETWORK PREDICTORS######

predPS_qda_T<-jackPredQDA(centPS,predictors = c("tarEnt"))
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
predCD_qda_T<-jackPredQDA(centCD,predictors = c("tarEnt"))
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
predICS_qda_T<-jackPredQDA(centICS,predictors = c("tarEnt"))
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_qda_T_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Quadratic Discriminant Analysis Model", sub="Pass/fail (n=166), TE",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_qda_T_ciL, x, PS_qda_T_ciH, length=0.05, angle=90, code=3)
lines(x,CD_qda_T_auc,pch=17,type="b",col="red")
arrows(x, CD_qda_T_ciL, x, CD_qda_T_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_qda_T_auc,pch=17,type="b",col="blue")
arrows(x, ICS_qda_T_ciL, x, ICS_qda_T_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy)

###JUST PASS
predPS_jp_qda_T<-jackPredQDA(centPS,outcome = "justpass",predictors = c("tarEnt"))
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

predCD_jp_qda_T<-jackPredQDA(centCD, outcome = "justpass",predictors = c("tarEnt"))
rocCD_jp_qda_T<-list()
rocCD_jp_qda_T[[1]]<-roc(predCD_jp_qda_T$justpass,as.numeric(predCD_jp_qda_T$Week1),auc=T,ci=T)
rocCD_jp_qda_T[[2]]<-roc(predCD_jp_qda_T$justpass,as.numeric(predCD_jp_qda_T$Week2),auc=T,ci=T)
rocCD_jp_qda_T[[3]]<-roc(predCD_jp_qda_T$justpass,as.numeric(predCD_jp_qda_T$Week3),auc=T,ci=T)
rocCD_jp_qda_T[[4]]<-roc(predCD_jp_qda_T$justpass,as.numeric(predCD_jp_qda_T$Week4),auc=T,ci=T)
rocCD_jp_qda_T[[5]]<-roc(predCD_jp_qda_T$justpass,as.numeric(predCD_jp_qda_T$Week5),auc=T,ci=T)
rocCD_jp_qda_T[[6]]<-roc(predCD_jp_qda_T$justpass,as.numeric(predCD_jp_qda_T$Week6),auc=T,ci=T)
rocCD_jp_qda_T[[7]]<-roc(predCD_jp_qda_T$justpass,as.numeric(predCD_jp_qda_T$Week7),auc=T,ci=T)
CD_jp_qda_T_auc<-c(rocCD_jp_qda_T[[1]]$auc,rocCD_jp_qda_T[[2]]$auc,rocCD_jp_qda_T[[3]]$auc,rocCD_jp_qda_T[[4]]$auc,
                   rocCD_jp_qda_T[[5]]$auc,rocCD_jp_qda_T[[6]]$auc,rocCD_jp_qda_T[[7]]$auc)
CD_jp_qda_T_ciL<-c(rocCD_jp_qda_T[[1]]$ci[1],rocCD_jp_qda_T[[2]]$ci[1],rocCD_jp_qda_T[[3]]$ci[1],rocCD_jp_qda_T[[4]]$ci[1],
                   rocCD_jp_qda_T[[5]]$ci[1],rocCD_jp_qda_T[[6]]$ci[1],rocCD_jp_qda_T[[7]]$ci[1])
CD_jp_qda_T_ciH<-c(rocCD_jp_qda_T[[1]]$ci[3],rocCD_jp_qda_T[[2]]$ci[3],rocCD_jp_qda_T[[3]]$ci[3],rocCD_jp_qda_T[[4]]$ci[3],
                   rocCD_jp_qda_T[[5]]$ci[3],rocCD_jp_qda_T[[6]]$ci[3],rocCD_jp_qda_T[[7]]$ci[3])


predICS_jp_qda_T<-jackPredQDA(centICS, outcome = "justpass",predictors = c("tarEnt"))
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_jp_qda_T_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Quadratic Discriminant Analysis Model", sub="Just pass/Just fail (n=67), TE",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_qda_T_ciL, x, PS_jp_qda_T_ciH, length=0.05, angle=90, code=3)
lines(x,CD_jp_qda_T_auc,pch=17,type="b",col="red")
arrows(x, CD_jp_qda_T_ciL, x, CD_jp_qda_T_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_jp_qda_T_auc,pch=17,type="b",col="blue")
arrows(x, ICS_jp_qda_T_ciL, x, ICS_jp_qda_T_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy_jp)

######HIDE NETWORK PREDICTORS######

predPS_qda_H<-jackPredQDA(centPS,predictors = c( "Hide"))
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
predCD_qda_H<-jackPredQDA(centCD,predictors = c( "Hide"))
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
predICS_qda_H<-jackPredQDA(centICS,predictors = c( "Hide"))
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_qda_H_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Quadratic Discriminant Analysis Model", sub="Pass/fail (n=166), TE-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_qda_H_ciL, x, PS_qda_H_ciH, length=0.05, angle=90, code=3)
lines(x,CD_qda_H_auc,pch=17,type="b",col="red")
arrows(x, CD_qda_H_ciL, x, CD_qda_H_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_qda_H_auc,pch=17,type="b",col="blue")
arrows(x, ICS_qda_H_ciL, x, ICS_qda_H_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy)

###JUST PASS
predPS_jp_qda_H<-jackPredQDA(centPS,outcome = "justpass",predictors = c( "Hide"))
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

predCD_jp_qda_H<-jackPredQDA(centCD, outcome = "justpass",predictors = c( "Hide"))
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


predICS_jp_qda_H<-jackPredQDA(centICS, outcome = "justpass",predictors = c( "Hide"))
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_jp_qda_H_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Quadratic Discriminant Analysis Model", sub="Just pass/Just fail (n=67), TE-H",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_qda_H_ciL, x, PS_jp_qda_H_ciH, length=0.05, angle=90, code=3)
lines(x,CD_jp_qda_H_auc,pch=17,type="b",col="red")
arrows(x, CD_jp_qda_H_ciL, x, CD_jp_qda_H_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_jp_qda_H_auc,pch=17,type="b",col="blue")
arrows(x, ICS_jp_qda_H_ciL, x, ICS_jp_qda_H_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy_jp)

######PAGERANK NETWORK PREDICTORS######

predPS_qda_P<-jackPredQDA(centPS,predictors = c("PageRank"))
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
predCD_qda_P<-jackPredQDA(centCD,predictors = c("PageRank"))
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
predICS_qda_P<-jackPredQDA(centICS,predictors = c("PageRank"))
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_qda_P_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Quadratic Discriminant Analysis Model", sub="Pass/fail (n=166), PR",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_qda_P_ciL, x, PS_qda_P_ciH, length=0.05, angle=90, code=3)
lines(x,CD_qda_P_auc,pch=17,type="b",col="red")
arrows(x, CD_qda_P_ciL, x, CD_qda_P_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_qda_P_auc,pch=17,type="b",col="blue")
arrows(x, ICS_qda_P_ciL, x, ICS_qda_P_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy)

###JUST PASS
predPS_jp_qda_P<-jackPredQDA(centPS,outcome = "justpass",predictors = c("PageRank"))
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

predCD_jp_qda_P<-jackPredQDA(centCD, outcome = "justpass",predictors = c("PageRank"))
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


predICS_jp_qda_P<-jackPredQDA(centICS, outcome = "justpass",predictors = c("PageRank"))
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

#Plot AUC for three layers.
x<-c(1:7)
plot(x, PS_jp_qda_P_auc,
     ylim=range(c(0, 1)),
     pch=19, xlab="Course Week", ylab="AUC and CI",
     main="Quadratic Discriminant Analysis Model", sub="Just pass/Just fail (n=67), PR",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, PS_jp_qda_P_ciL, x, PS_jp_qda_P_ciH, length=0.05, angle=90, code=3)
lines(x,CD_jp_qda_P_auc,pch=17,type="b",col="red")
arrows(x, CD_jp_qda_P_ciL, x, CD_jp_qda_P_ciH, length=0.05, angle=90, code=3,col="red")
lines(x,ICS_jp_qda_P_auc,pch=17,type="b",col="blue")
arrows(x, ICS_jp_qda_P_ciL, x, ICS_jp_qda_P_ciH, length=0.05, angle=90, code=3,col="blue")
legend(1, 1, legend=c("Problem Solving", "Concept Discussion","In-Class Social"),
       col=c("black","red", "blue"), lty=1, cex=0.8)
abline(h=lazy_jp)

#########K NEAREST NEIGHBORS###########
#########K NEAREST NEIGHBORS###########
#########K NEAREST NEIGHBORS###########
#########K NEAREST NEIGHBORS###########

######ALL NETWORK PREDICTORS######
t1<-Sys.time()
ROC_PS_knn<-list()
for(i in 1:20){
  predPS_knn_PTH<-jackPredKNN(centPS,predictors = c("PageRank","tarEnt", "Hide"),nK = 3)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_knn[[i]]<-ROC
}

ROC_CD_knn<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD,predictors = c("PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn[[i]]<-ROC
}

ROC_ICS_knn<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS,predictors = c("PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn[[i]]<-ROC
}

ROC_PS_justpass_knn<-list()
for(i in 1:20){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn[[i]]<-ROC
}

ROC_CD_justpass_knn<-list()
for(i in 1:20){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn[[i]]<-ROC
}

ROC_ICS_justpass_knn<-list()
for(i in 1:20){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("PageRank","tarEnt", "Hide"),nK = i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn[[i]]<-ROC
}

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

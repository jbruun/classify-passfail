library(xtable)
library(igraph)
library(dplyr)
library(class)   # for knn
library(tidyr)
library(ggplot2)  # for plotting success rates
library(MASS)

source("code/ROC_functions.R")
#Lazy classifiers assume that everybody passes. Here we calculate their success rate
lazyPass<-as.numeric(table(centPS[[1]]$pass)[2]/sum(table(centPS[[1]]$pass))) #p=0.77
lazyJustPass<-as.numeric(table(centPS[[1]]$justpass)[2]/sum(table(centPS[[1]]$justpass))) #p=0.58

#Classifiers that only rely on background variables
load("data/centrality_data_frames.Rdata")
load("data/ROC_AUC/AUC_SR_BackgroundOnly_fullSet.Rdata")

#source("code/AUC_OnlyBackground_fci_c.R")
#logRegP<-c(GCF_AUC_log,GF_AUC_log,GC_AUC_log,CF_AUC_log,F_AUC_log,G_AUC_log,C_AUC_log)
#ldaP<-c(GCF_AUC_lda,GF_AUC_lda,GC_AUC_lda,CF_AUC_lda,F_AUC_lda,G_AUC_lda,C_AUC_lda)
#qdaP<-c(GCF_AUC_qda,GF_AUC_qda,GC_AUC_qda,CF_AUC_qda,F_AUC_qda,G_AUC_qda,C_AUC_qda)
#KNNP<-c(max(GCF_SR_knn),max(GF_SR_knn),max(GC_SR_knn),max(CF_SR_knn),max(F_SR_knn),max(G_SR_knn),max(C_SR_knn))
#OnlyBackground<-data.frame(logistic=logRegP, LDA=ldaP,QDA=qdaP,KNN=KNNP)
#rownames(OnlyBackground)<-c("GCF","GF","GC","CF","F","G","C")

xtable(OnlyBackground_fci_pre_c)

#logRegP<-c(GCF_AUC_justpass_log,GF_AUC_justpass_log,GC_AUC_justpass_log,CF_AUC_justpass_log,F_AUC_justpass_log,G_AUC_justpass_log,C_AUC_justpass_log)
#ldaP<-c(GCF_AUC_justpass_lda,GF_AUC_justpass_lda,GC_AUC_justpass_lda,CF_AUC_justpass_lda,F_AUC_justpass_lda,G_AUC_justpass_lda,C_AUC_justpass_lda)
#qdaP<-c(GCF_AUC_justpass_qda,GF_AUC_justpass_qda,GC_AUC_justpass_qda,CF_AUC_justpass_qda,F_AUC_justpass_qda,G_AUC_justpass_qda,C_AUC_justpass_qda)
#KNNP<-c(max(GCF_SR_knn_JP),max(GF_SR_knn_JP),max(GC_SR_knn_JP),max(CF_SR_knn_JP),max(F_SR_knn_JP),max(G_SR_knn_JP),max(C_SR_knn_JP))
#OnlyBackground_JP<-data.frame(logistic=logRegP, LDA=ldaP,QDA=qdaP,KNN=KNNP)
#rownames(OnlyBackground_JP)<-c("GCF","GF","GC","CF","F","G","C")

xtable(OnlyBackground_JP_fci_pre_c)



###PLOTS ONLY NETWORK VARIABLES#####
source("code/AUC/AUC_OnlyNetwork.R")
pdf(file="plots/AUC_logreg_OnlyNetworkMeasures.pdf",paper = "a4r")
plot(PS_AUC_NB_log_all, ylab="AUC", xlab="week", ylim=c(0,1),
     main="Logistic Regression Only Network Measures",type="o", lty=1)
abline(h=lazyPass)
lines(CD_AUC_NB_log_all,type="o",pch=2, lty=2)
lines(ICS_AUC_NB_log_all,type="o",pch=3, lty=3)
lines(PS_AUC_NB_log_PRTE,type="o",pch=4, lty=1,col="blue")
lines(CD_AUC_NB_log_PRTE,type="o",pch=5, lty=2,col="blue")
lines(ICS_AUC_NB_log_PRTE,type="o",pch=6, lty=3,col="blue")
lines(PS_AUC_NB_log_PRH,type="o",pch=7, lty=1,col="red")
lines(CD_AUC_NB_log_PRH,type="o",pch=8, lty=2,col="red")
lines(ICS_AUC_NB_log_PRH,type="o",pch=9, lty=3,col="red")
lines(PS_AUC_NB_log_TEH,type="o",pch=10, lty=1,col="green")
lines(CD_AUC_NB_log_TEH,type="o",pch=11, lty=2,col="green")
lines(ICS_AUC_NB_log_TEH,type="o",pch=12, lty=3,col="green")
lines(PS_AUC_NB_log_PR,type="o",pch=13, lty=1,col="magenta")
lines(CD_AUC_NB_log_PR,type="o",pch=14, lty=2,col="magenta")
lines(ICS_AUC_NB_log_PR,type="o",pch=15, lty=3,col="magenta")
lines(PS_AUC_NB_log_TE,type="o",pch=16, lty=1,col="cyan")
lines(CD_AUC_NB_log_TE,type="o",pch=17, lty=2,col="cyan")
lines(ICS_AUC_NB_log_TE,type="o",pch=18, lty=3,col="cyan")
lines(PS_AUC_NB_log_H,type="o",pch=19, lty=1,col="grey")
lines(CD_AUC_NB_log_H,type="o",pch=20, lty=2,col="grey")
lines(ICS_AUC_NB_log_H,type="o",pch=21, lty=3,col="grey")
legend("bottomright", cex=0.8, legend=c("PS","CD","ICS"),lty=c(1,2,3))
legend("bottomleft", cex=0.8, legend=c("All","PR+TE","PR+H","TE+H","PR","TE","H"),col=c("black","blue","red","green","magenta","cyan","grey"),lty=1)
dev.off()

pdf(file="plots/AUC_lda_OnlyNetworkMeasures.pdf",paper = "a4r")
plot(PS_AUC_NB_lda_all, ylab="AUC", xlab="week", ylim=c(0,1),
     main="Linear Discriminant Analysis Only Network Measures",type="o", lty=1)
abline(h=lazyPass)
lines(CD_AUC_NB_lda_all,type="o",pch=2, lty=2)
lines(ICS_AUC_NB_lda_all,type="o",pch=3, lty=3)
lines(PS_AUC_NB_lda_PRTE,type="o",pch=4, lty=1,col="blue")
lines(CD_AUC_NB_lda_PRTE,type="o",pch=5, lty=2,col="blue")
lines(ICS_AUC_NB_lda_PRTE,type="o",pch=6, lty=3,col="blue")
lines(PS_AUC_NB_lda_PRH,type="o",pch=7, lty=1,col="red")
lines(CD_AUC_NB_lda_PRH,type="o",pch=8, lty=2,col="red")
lines(ICS_AUC_NB_lda_PRH,type="o",pch=9, lty=3,col="red")
lines(PS_AUC_NB_lda_TEH,type="o",pch=10, lty=1,col="green")
lines(CD_AUC_NB_lda_TEH,type="o",pch=11, lty=2,col="green")
lines(ICS_AUC_NB_lda_TEH,type="o",pch=12, lty=3,col="green")
lines(PS_AUC_NB_lda_PR,type="o",pch=13, lty=1,col="magenta")
lines(CD_AUC_NB_lda_PR,type="o",pch=14, lty=2,col="magenta")
lines(ICS_AUC_NB_lda_PR,type="o",pch=15, lty=3,col="magenta")
lines(PS_AUC_NB_lda_TE,type="o",pch=16, lty=1,col="cyan")
lines(CD_AUC_NB_lda_TE,type="o",pch=17, lty=2,col="cyan")
lines(ICS_AUC_NB_lda_TE,type="o",pch=18, lty=3,col="cyan")
lines(PS_AUC_NB_lda_H,type="o",pch=19, lty=1,col="yellow")
lines(CD_AUC_NB_lda_H,type="o",pch=20, lty=2,col="yellow")
lines(ICS_AUC_NB_lda_H,type="o",pch=21, lty=3,col="yellow")
legend("bottomright", cex=0.8, legend=c("PS","CD","ICS"),lty=c(1,2,3))
legend("bottomleft", cex=0.8, legend=c("All","PR+TE","PR+H","TE+H","PR","TE","H"),col=c("black","blue","red","green","magenta","cyan","yellow"),lty=1)
dev.off()

pdf(file="plots/AUC_qda_OnlyNetworkMeasures.pdf",paper = "a4r")
plot(PS_AUC_NB_qda_all, ylab="AUC", xlab="week", ylim=c(0,1),
     main="Quadratic Discriminant Analysis Only Network Measures",type="o", lty=1)
abline(h=lazyPass)
lines(CD_AUC_NB_qda_all,type="o",pch=2, lty=2)
lines(ICS_AUC_NB_qda_all,type="o",pch=3, lty=3)
lines(PS_AUC_NB_qda_PRTE,type="o",pch=4, lty=1,col="blue")
lines(CD_AUC_NB_qda_PRTE,type="o",pch=5, lty=2,col="blue")
lines(ICS_AUC_NB_qda_PRTE,type="o",pch=6, lty=3,col="blue")
lines(PS_AUC_NB_qda_PRH,type="o",pch=7, lty=1,col="red")
lines(CD_AUC_NB_qda_PRH,type="o",pch=8, lty=2,col="red")
lines(ICS_AUC_NB_qda_PRH,type="o",pch=9, lty=3,col="red")
lines(PS_AUC_NB_qda_TEH,type="o",pch=10, lty=1,col="green")
lines(CD_AUC_NB_qda_TEH,type="o",pch=11, lty=2,col="green")
lines(ICS_AUC_NB_qda_TEH,type="o",pch=12, lty=3,col="green")
lines(PS_AUC_NB_qda_PR,type="o",pch=13, lty=1,col="magenta")
lines(CD_AUC_NB_qda_PR,type="o",pch=14, lty=2,col="magenta")
lines(ICS_AUC_NB_qda_PR,type="o",pch=15, lty=3,col="magenta")
lines(PS_AUC_NB_qda_TE,type="o",pch=16, lty=1,col="cyan")
lines(CD_AUC_NB_qda_TE,type="o",pch=17, lty=2,col="cyan")
lines(ICS_AUC_NB_qda_TE,type="o",pch=18, lty=3,col="cyan")
lines(PS_AUC_NB_qda_H,type="o",pch=19, lty=1,col="yellow")
lines(CD_AUC_NB_qda_H,type="o",pch=20, lty=2,col="yellow")
lines(ICS_AUC_NB_qda_H,type="o",pch=21, lty=3,col="yellow")
legend("bottomright", cex=0.8, legend=c("PS","CD","ICS"),lty=c(1,2,3))
legend("bottomleft", cex=0.8, legend=c("All","PR+TE","PR+H","TE+H","PR","TE","H"),col=c("black","blue","red","green","magenta","cyan","yellow"),lty=1)
dev.off()

pdf(file="plots/mSR_knn_OnlyNetworkMeasures.pdf",paper = "a4r")
plot(PS_mSR_NB_knn_all, ylab="AUC", xlab="week", ylim=c(0,1),
     main="K-nearest neighbours Only Network Measures",type="o", lty=1)
abline(h=lazyPass)
lines(CD_mSR_NB_knn_all,type="o",pch=2, lty=2)
lines(ICS_mSR_NB_knn_all,type="o",pch=3, lty=3)
lines(PS_mSR_NB_knn_PRTE,type="o",pch=4, lty=1,col="blue")
lines(CD_mSR_NB_knn_PRTE,type="o",pch=5, lty=2,col="blue")
lines(ICS_mSR_NB_knn_PRTE,type="o",pch=6, lty=3,col="blue")
lines(PS_mSR_NB_knn_PRH,type="o",pch=7, lty=1,col="red")
lines(CD_mSR_NB_knn_PRH,type="o",pch=8, lty=2,col="red")
lines(ICS_mSR_NB_knn_PRH,type="o",pch=9, lty=3,col="red")
lines(PS_mSR_NB_knn_TEH,type="o",pch=10, lty=1,col="green")
lines(CD_mSR_NB_knn_TEH,type="o",pch=11, lty=2,col="green")
lines(ICS_mSR_NB_knn_TEH,type="o",pch=12, lty=3,col="green")
lines(PS_mSR_NB_knn_PR,type="o",pch=13, lty=1,col="magenta")
lines(CD_mSR_NB_knn_PR,type="o",pch=14, lty=2,col="magenta")
lines(ICS_mSR_NB_knn_PR,type="o",pch=15, lty=3,col="magenta")
lines(PS_mSR_NB_knn_TE,type="o",pch=16, lty=1,col="cyan")
lines(CD_mSR_NB_knn_TE,type="o",pch=17, lty=2,col="cyan")
lines(ICS_mSR_NB_knn_TE,type="o",pch=18, lty=3,col="cyan")
#lines(PS_mSR_NB_knn_H,type="o",pch=19, lty=1,col="yellow")
lines(CD_mSR_NB_knn_H,type="o",pch=20, lty=2,col="yellow")
lines(ICS_mSR_NB_knn_H,type="o",pch=21, lty=3,col="yellow")
legend("bottomright", cex=0.8, legend=c("PS","CD","ICS"),lty=c(1,2,3))
legend("bottomleft", cex=0.8, legend=c("All","PR+TE","PR+H","TE+H","PR","TE","H"),col=c("black","blue","red","green","magenta","cyan","yellow"),lty=1)
dev.off()



###PLOTS NO BACKGROUND JUST PASSED#####
pdf(file="plots/AUC_logreg_justpass_OnlyNetworkMeasures.pdf",paper = "a4r")
plot(PS_AUC_NB_justpass_log_all, ylab="AUC", xlab="week", ylim=c(0,1),
     main="Logistic Regression Only Network Measures",type="o", lty=1)
abline(h=lazyJustPass)
lines(CD_AUC_NB_justpass_log_all,type="o",pch=2, lty=2)
lines(ICS_AUC_NB_justpass_log_all,type="o",pch=3, lty=3)
lines(PS_AUC_NB_justpass_log_PRTE,type="o",pch=4, lty=1,col="blue")
lines(CD_AUC_NB_justpass_log_PRTE,type="o",pch=5, lty=2,col="blue")
lines(ICS_AUC_NB_justpass_log_PRTE,type="o",pch=6, lty=3,col="blue")
lines(PS_AUC_NB_justpass_log_PRH,type="o",pch=7, lty=1,col="red")
lines(CD_AUC_NB_justpass_log_PRH,type="o",pch=8, lty=2,col="red")
lines(ICS_AUC_NB_justpass_log_PRH,type="o",pch=9, lty=3,col="red")
lines(PS_AUC_NB_justpass_log_TEH,type="o",pch=10, lty=1,col="green")
lines(CD_AUC_NB_justpass_log_TEH,type="o",pch=11, lty=2,col="green")
lines(ICS_AUC_NB_justpass_log_TEH,type="o",pch=12, lty=3,col="green")
lines(PS_AUC_NB_justpass_log_PR,type="o",pch=13, lty=1,col="magenta")
lines(CD_AUC_NB_justpass_log_PR,type="o",pch=14, lty=2,col="magenta")
lines(ICS_AUC_NB_justpass_log_PR,type="o",pch=15, lty=3,col="magenta")
lines(PS_AUC_NB_justpass_log_TE,type="o",pch=16, lty=1,col="cyan")
lines(CD_AUC_NB_justpass_log_TE,type="o",pch=17, lty=2,col="cyan")
lines(ICS_AUC_NB_justpass_log_TE,type="o",pch=18, lty=3,col="cyan")
lines(PS_AUC_NB_justpass_log_H,type="o",pch=19, lty=1,col="yellow")
lines(CD_AUC_NB_justpass_log_H,type="o",pch=20, lty=2,col="yellow")
lines(ICS_AUC_NB_justpass_log_H,type="o",pch=21, lty=3,col="yellow")
legend("bottomright", cex=0.8, legend=c("PS","CD","ICS"),lty=c(1,2,3))
legend("bottomleft", cex=0.8, legend=c("All","PR+TE","PR+H","TE+H","PR","TE","H"),col=c("black","blue","red","green","magenta","cyan","yellow"),lty=1)
dev.off()

pdf(file="plots/AUC_lda_justpass_OnlyNetworkMeasures.pdf",paper = "a4r")
plot(PS_AUC_NB_justpass_lda_all, ylab="AUC", xlab="week", ylim=c(0,1),
     main="Linear Discriminant Analysis Only Network Measures",type="o", lty=1)
abline(h=lazyJustPass)
lines(CD_AUC_NB_justpass_lda_all,type="o",pch=2, lty=2)
lines(ICS_AUC_NB_justpass_lda_all,type="o",pch=3, lty=3)
lines(PS_AUC_NB_justpass_lda_PRTE,type="o",pch=4, lty=1,col="blue")
lines(CD_AUC_NB_justpass_lda_PRTE,type="o",pch=5, lty=2,col="blue")
lines(ICS_AUC_NB_justpass_lda_PRTE,type="o",pch=6, lty=3,col="blue")
lines(PS_AUC_NB_justpass_lda_PRH,type="o",pch=7, lty=1,col="red")
lines(CD_AUC_NB_justpass_lda_PRH,type="o",pch=8, lty=2,col="red")
lines(ICS_AUC_NB_justpass_lda_PRH,type="o",pch=9, lty=3,col="red")
lines(PS_AUC_NB_justpass_lda_TEH,type="o",pch=10, lty=1,col="green")
lines(CD_AUC_NB_justpass_lda_TEH,type="o",pch=11, lty=2,col="green")
lines(ICS_AUC_NB_justpass_lda_TEH,type="o",pch=12, lty=3,col="green")
lines(PS_AUC_NB_justpass_lda_PR,type="o",pch=13, lty=1,col="magenta")
lines(CD_AUC_NB_justpass_lda_PR,type="o",pch=14, lty=2,col="magenta")
lines(ICS_AUC_NB_justpass_lda_PR,type="o",pch=15, lty=3,col="magenta")
lines(PS_AUC_NB_justpass_lda_TE,type="o",pch=16, lty=1,col="cyan")
lines(CD_AUC_NB_justpass_lda_TE,type="o",pch=17, lty=2,col="cyan")
lines(ICS_AUC_NB_justpass_lda_TE,type="o",pch=18, lty=3,col="cyan")
lines(PS_AUC_NB_justpass_lda_H,type="o",pch=19, lty=1,col="yellow")
lines(CD_AUC_NB_justpass_lda_H,type="o",pch=20, lty=2,col="yellow")
lines(ICS_AUC_NB_justpass_lda_H,type="o",pch=21, lty=3,col="yellow")
legend("bottomright", cex=0.8, legend=c("PS","CD","ICS"),lty=c(1,2,3))
legend("bottomleft", cex=0.8, legend=c("All","PR+TE","PR+H","TE+H","PR","TE","H"),col=c("black","blue","red","green","magenta","cyan","yellow"),lty=1)
dev.off()

pdf(file="plots/AUC_qda_justpass_OnlyNetworkMeasures.pdf",paper = "a4r")
plot(PS_AUC_NB_justpass_qda_all, ylab="AUC", xlab="week", ylim=c(0,1),
     main="Quadratic Discriminant Analysis Only Network Measures",type="o", lty=1)
abline(h=lazyJustPass)
lines(CD_AUC_NB_justpass_qda_all,type="o",pch=2, lty=2)
lines(ICS_AUC_NB_justpass_qda_all,type="o",pch=3, lty=3)
lines(PS_AUC_NB_justpass_qda_PRTE,type="o",pch=4, lty=1,col="blue")
lines(CD_AUC_NB_justpass_qda_PRTE,type="o",pch=5, lty=2,col="blue")
lines(ICS_AUC_NB_justpass_qda_PRTE,type="o",pch=6, lty=3,col="blue")
lines(PS_AUC_NB_justpass_qda_PRH,type="o",pch=7, lty=1,col="red")
lines(CD_AUC_NB_justpass_qda_PRH,type="o",pch=8, lty=2,col="red")
lines(ICS_AUC_NB_justpass_qda_PRH,type="o",pch=9, lty=3,col="red")
lines(PS_AUC_NB_justpass_qda_TEH,type="o",pch=10, lty=1,col="green")
lines(CD_AUC_NB_justpass_qda_TEH,type="o",pch=11, lty=2,col="green")
lines(ICS_AUC_NB_justpass_qda_TEH,type="o",pch=12, lty=3,col="green")
lines(PS_AUC_NB_justpass_qda_PR,type="o",pch=13, lty=1,col="magenta")
lines(CD_AUC_NB_justpass_qda_PR,type="o",pch=14, lty=2,col="magenta")
lines(ICS_AUC_NB_justpass_qda_PR,type="o",pch=15, lty=3,col="magenta")
lines(PS_AUC_NB_justpass_qda_TE,type="o",pch=16, lty=1,col="cyan")
lines(CD_AUC_NB_justpass_qda_TE,type="o",pch=17, lty=2,col="cyan")
lines(ICS_AUC_NB_justpass_qda_TE,type="o",pch=18, lty=3,col="cyan")
lines(PS_AUC_NB_justpass_qda_H,type="o",pch=19, lty=1,col="yellow")
lines(CD_AUC_NB_justpass_qda_H,type="o",pch=20, lty=2,col="yellow")
lines(ICS_AUC_NB_justpass_qda_H,type="o",pch=21, lty=3,col="yellow")
legend("bottomright", cex=0.8, legend=c("PS","CD","ICS"),lty=c(1,2,3))
legend("bottomleft", cex=0.8, legend=c("All","PR+TE","PR+H","TE+H","PR","TE","H"),col=c("black","blue","red","green","magenta","cyan","yellow"),lty=1)
dev.off()

pdf(file="plots/mSR_knn_justpass_OnlyNetworkMeasures.pdf",paper = "a4r")
plot(PS_mSR_NB_justpass_knn_all, ylab="AUC", xlab="week", ylim=c(0,1),
     main="K-nearest neighbours Only Network Measures",type="o", lty=1)
abline(h=lazyJustPass)
lines(CD_mSR_NB_justpass_knn_all,type="o",pch=2, lty=2)
lines(ICS_mSR_NB_justpass_knn_all,type="o",pch=3, lty=3)
lines(PS_mSR_NB_justpass_knn_PRTE,type="o",pch=4, lty=1,col="blue")
lines(CD_mSR_NB_justpass_knn_PRTE,type="o",pch=5, lty=2,col="blue")
lines(ICS_mSR_NB_justpass_knn_PRTE,type="o",pch=6, lty=3,col="blue")
lines(PS_mSR_NB_justpass_knn_PRH,type="o",pch=7, lty=1,col="red")
lines(CD_mSR_NB_justpass_knn_PRH,type="o",pch=8, lty=2,col="red")
lines(ICS_mSR_NB_justpass_knn_PRH,type="o",pch=9, lty=3,col="red")
lines(PS_mSR_NB_justpass_knn_TEH,type="o",pch=10, lty=1,col="green")
lines(CD_mSR_NB_justpass_knn_TEH,type="o",pch=11, lty=2,col="green")
lines(ICS_mSR_NB_justpass_knn_TEH,type="o",pch=12, lty=3,col="green")
lines(PS_mSR_NB_justpass_knn_PR,type="o",pch=13, lty=1,col="magenta")
lines(CD_mSR_NB_justpass_knn_PR,type="o",pch=14, lty=2,col="magenta")
lines(ICS_mSR_NB_justpass_knn_PR,type="o",pch=15, lty=3,col="magenta")
lines(PS_mSR_NB_justpass_knn_TE,type="o",pch=16, lty=1,col="cyan")
lines(CD_mSR_NB_justpass_knn_TE,type="o",pch=17, lty=2,col="cyan")
lines(ICS_mSR_NB_justpass_knn_TE,type="o",pch=18, lty=3,col="cyan")
#lines(PS_mSR_NB_justpass_knn_H,type="o",pch=19, lty=1,col="yellow")
lines(CD_mSR_NB_justpass_knn_H,type="o",pch=20, lty=2,col="yellow")
lines(ICS_mSR_NB_justpass_knn_H,type="o",pch=21, lty=3,col="yellow")
legend("bottomright", cex=0.8, legend=c("PS","CD","ICS"),lty=c(1,2,3))
legend("bottomleft", cex=0.8, legend=c("All","PR+TE","PR+H","TE+H","PR","TE","H"),col=c("black","blue","red","green","magenta","cyan","yellow"),lty=1)
dev.off()



#WITH FCI_C PRE
source("code/AUC/AUC_NetworkMeasuresFCI_C.R")

pdf(file="plots/AUC_logreg_NetworkMeasuresFCI_C.pdf",paper = "a4r")
plot(PS_AUC_NetworkMeasuresFCI_C_log_all, ylab="AUC", xlab="week", ylim=c(0,1),
     main="Logistic Regression Network Measures + FCI C",type="o", lty=1)
abline(h=lazyPass)
lines(CD_AUC_NetworkMeasuresFCI_C_log_all,type="o",pch=2, lty=2)
lines(ICS_AUC_NetworkMeasuresFCI_C_log_all,type="o",pch=3, lty=3)
lines(PS_AUC_NetworkMeasuresFCI_C_log_PRTE,type="o",pch=4, lty=1,col="blue")
lines(CD_AUC_NetworkMeasuresFCI_C_log_PRTE,type="o",pch=5, lty=2,col="blue")
lines(ICS_AUC_NetworkMeasuresFCI_C_log_PRTE,type="o",pch=6, lty=3,col="blue")
lines(PS_AUC_NetworkMeasuresFCI_C_log_PRH,type="o",pch=7, lty=1,col="red")
lines(CD_AUC_NetworkMeasuresFCI_C_log_PRH,type="o",pch=8, lty=2,col="red")
lines(ICS_AUC_NetworkMeasuresFCI_C_log_PRH,type="o",pch=9, lty=3,col="red")
lines(PS_AUC_NetworkMeasuresFCI_C_log_TEH,type="o",pch=10, lty=1,col="green")
lines(CD_AUC_NetworkMeasuresFCI_C_log_TEH,type="o",pch=11, lty=2,col="green")
lines(ICS_AUC_NetworkMeasuresFCI_C_log_TEH,type="o",pch=12, lty=3,col="green")
lines(PS_AUC_NetworkMeasuresFCI_C_log_PR,type="o",pch=13, lty=1,col="magenta")
lines(CD_AUC_NetworkMeasuresFCI_C_log_PR,type="o",pch=14, lty=2,col="magenta")
lines(ICS_AUC_NetworkMeasuresFCI_C_log_PR,type="o",pch=15, lty=3,col="magenta")
lines(PS_AUC_NetworkMeasuresFCI_C_log_TE,type="o",pch=16, lty=1,col="cyan")
lines(CD_AUC_NetworkMeasuresFCI_C_log_TE,type="o",pch=17, lty=2,col="cyan")
lines(ICS_AUC_NetworkMeasuresFCI_C_log_TE,type="o",pch=18, lty=3,col="cyan")
lines(PS_AUC_NetworkMeasuresFCI_C_log_H,type="o",pch=19, lty=1,col="grey")
lines(CD_AUC_NetworkMeasuresFCI_C_log_H,type="o",pch=20, lty=2,col="grey")
lines(ICS_AUC_NetworkMeasuresFCI_C_log_H,type="o",pch=21, lty=3,col="grey")
legend("bottomright", cex=0.8, legend=c("PS","CD","ICS"),lty=c(1,2,3))
legend("bottomleft", cex=0.8, legend=c("All","PR+TE","PR+H","TE+H","PR","TE","H"),col=c("black","blue","red","green","magenta","cyan","grey"),lty=1)
dev.off()

pdf(file="plots/AUC_lda_NetworkMeasuresFCI_C.pdf",paper = "a4r")
plot(PS_AUC_NetworkMeasuresFCI_C_lda_all, ylab="AUC", xlab="week", ylim=c(0,1),
     main="Linear Discriminant Analysis Network Measures + FCI C",type="o", lty=1)
abline(h=lazyPass)
lines(CD_AUC_NetworkMeasuresFCI_C_lda_all,type="o",pch=2, lty=2)
lines(ICS_AUC_NetworkMeasuresFCI_C_lda_all,type="o",pch=3, lty=3)
lines(PS_AUC_NetworkMeasuresFCI_C_lda_PRTE,type="o",pch=4, lty=1,col="blue")
lines(CD_AUC_NetworkMeasuresFCI_C_lda_PRTE,type="o",pch=5, lty=2,col="blue")
lines(ICS_AUC_NetworkMeasuresFCI_C_lda_PRTE,type="o",pch=6, lty=3,col="blue")
lines(PS_AUC_NetworkMeasuresFCI_C_lda_PRH,type="o",pch=7, lty=1,col="red")
lines(CD_AUC_NetworkMeasuresFCI_C_lda_PRH,type="o",pch=8, lty=2,col="red")
lines(ICS_AUC_NetworkMeasuresFCI_C_lda_PRH,type="o",pch=9, lty=3,col="red")
lines(PS_AUC_NetworkMeasuresFCI_C_lda_TEH,type="o",pch=10, lty=1,col="green")
lines(CD_AUC_NetworkMeasuresFCI_C_lda_TEH,type="o",pch=11, lty=2,col="green")
lines(ICS_AUC_NetworkMeasuresFCI_C_lda_TEH,type="o",pch=12, lty=3,col="green")
lines(PS_AUC_NetworkMeasuresFCI_C_lda_PR,type="o",pch=13, lty=1,col="magenta")
lines(CD_AUC_NetworkMeasuresFCI_C_lda_PR,type="o",pch=14, lty=2,col="magenta")
lines(ICS_AUC_NetworkMeasuresFCI_C_lda_PR,type="o",pch=15, lty=3,col="magenta")
lines(PS_AUC_NetworkMeasuresFCI_C_lda_TE,type="o",pch=16, lty=1,col="cyan")
lines(CD_AUC_NetworkMeasuresFCI_C_lda_TE,type="o",pch=17, lty=2,col="cyan")
lines(ICS_AUC_NetworkMeasuresFCI_C_lda_TE,type="o",pch=18, lty=3,col="cyan")
lines(PS_AUC_NetworkMeasuresFCI_C_lda_H,type="o",pch=19, lty=1,col="yellow")
lines(CD_AUC_NetworkMeasuresFCI_C_lda_H,type="o",pch=20, lty=2,col="yellow")
lines(ICS_AUC_NetworkMeasuresFCI_C_lda_H,type="o",pch=21, lty=3,col="yellow")
legend("bottomright", cex=0.8, legend=c("PS","CD","ICS"),lty=c(1,2,3))
legend("bottomleft", cex=0.8, legend=c("All","PR+TE","PR+H","TE+H","PR","TE","H"),col=c("black","blue","red","green","magenta","cyan","yellow"),lty=1)
dev.off()

pdf(file="plots/AUC_qda_NetworkMeasuresFCI_C.pdf",paper = "a4r")
plot(PS_AUC_NetworkMeasuresFCI_C_qda_all, ylab="AUC", xlab="week", ylim=c(0,1),
     main="Quadratic Discriminant Analysis Network Measures + FCI C",type="o", lty=1)
abline(h=lazyPass)
lines(CD_AUC_NetworkMeasuresFCI_C_qda_all,type="o",pch=2, lty=2)
lines(ICS_AUC_NetworkMeasuresFCI_C_qda_all,type="o",pch=3, lty=3)
lines(PS_AUC_NetworkMeasuresFCI_C_qda_PRTE,type="o",pch=4, lty=1,col="blue")
lines(CD_AUC_NetworkMeasuresFCI_C_qda_PRTE,type="o",pch=5, lty=2,col="blue")
lines(ICS_AUC_NetworkMeasuresFCI_C_qda_PRTE,type="o",pch=6, lty=3,col="blue")
lines(PS_AUC_NetworkMeasuresFCI_C_qda_PRH,type="o",pch=7, lty=1,col="red")
lines(CD_AUC_NetworkMeasuresFCI_C_qda_PRH,type="o",pch=8, lty=2,col="red")
lines(ICS_AUC_NetworkMeasuresFCI_C_qda_PRH,type="o",pch=9, lty=3,col="red")
lines(PS_AUC_NetworkMeasuresFCI_C_qda_TEH,type="o",pch=10, lty=1,col="green")
lines(CD_AUC_NetworkMeasuresFCI_C_qda_TEH,type="o",pch=11, lty=2,col="green")
lines(ICS_AUC_NetworkMeasuresFCI_C_qda_TEH,type="o",pch=12, lty=3,col="green")
lines(PS_AUC_NetworkMeasuresFCI_C_qda_PR,type="o",pch=13, lty=1,col="magenta")
lines(CD_AUC_NetworkMeasuresFCI_C_qda_PR,type="o",pch=14, lty=2,col="magenta")
lines(ICS_AUC_NetworkMeasuresFCI_C_qda_PR,type="o",pch=15, lty=3,col="magenta")
lines(PS_AUC_NetworkMeasuresFCI_C_qda_TE,type="o",pch=16, lty=1,col="cyan")
lines(CD_AUC_NetworkMeasuresFCI_C_qda_TE,type="o",pch=17, lty=2,col="cyan")
lines(ICS_AUC_NetworkMeasuresFCI_C_qda_TE,type="o",pch=18, lty=3,col="cyan")
lines(PS_AUC_NetworkMeasuresFCI_C_qda_H,type="o",pch=19, lty=1,col="yellow")
lines(CD_AUC_NetworkMeasuresFCI_C_qda_H,type="o",pch=20, lty=2,col="yellow")
lines(ICS_AUC_NetworkMeasuresFCI_C_qda_H,type="o",pch=21, lty=3,col="yellow")
legend("bottomright", cex=0.8, legend=c("PS","CD","ICS"),lty=c(1,2,3))
legend("bottomleft", cex=0.8, legend=c("All","PR+TE","PR+H","TE+H","PR","TE","H"),col=c("black","blue","red","green","magenta","cyan","yellow"),lty=1)
dev.off()

pdf(file="plots/mSR_knn_NetworkMeasuresFCI_C.pdf",paper = "a4r")
plot(PS_mSR_NetworkMeasuresFCI_C_knn_all, ylab="AUC", xlab="week", ylim=c(0,1),
     main="K-nearest neighbours Network Measures + FCI C",type="o", lty=1)
abline(h=lazyPass)
lines(CD_mSR_NetworkMeasuresFCI_C_knn_all,type="o",pch=2, lty=2)
lines(ICS_mSR_NetworkMeasuresFCI_C_knn_all,type="o",pch=3, lty=3)
lines(PS_mSR_NetworkMeasuresFCI_C_knn_PRTE,type="o",pch=4, lty=1,col="blue")
lines(CD_mSR_NetworkMeasuresFCI_C_knn_PRTE,type="o",pch=5, lty=2,col="blue")
lines(ICS_mSR_NetworkMeasuresFCI_C_knn_PRTE,type="o",pch=6, lty=3,col="blue")
lines(PS_mSR_NetworkMeasuresFCI_C_knn_PRH,type="o",pch=7, lty=1,col="red")
lines(CD_mSR_NetworkMeasuresFCI_C_knn_PRH,type="o",pch=8, lty=2,col="red")
lines(ICS_mSR_NetworkMeasuresFCI_C_knn_PRH,type="o",pch=9, lty=3,col="red")
lines(PS_mSR_NetworkMeasuresFCI_C_knn_TEH,type="o",pch=10, lty=1,col="green")
lines(CD_mSR_NetworkMeasuresFCI_C_knn_TEH,type="o",pch=11, lty=2,col="green")
lines(ICS_mSR_NetworkMeasuresFCI_C_knn_TEH,type="o",pch=12, lty=3,col="green")
lines(PS_mSR_NetworkMeasuresFCI_C_knn_PR,type="o",pch=13, lty=1,col="magenta")
lines(CD_mSR_NetworkMeasuresFCI_C_knn_PR,type="o",pch=14, lty=2,col="magenta")
lines(ICS_mSR_NetworkMeasuresFCI_C_knn_PR,type="o",pch=15, lty=3,col="magenta")
lines(PS_mSR_NetworkMeasuresFCI_C_knn_TE,type="o",pch=16, lty=1,col="cyan")
lines(CD_mSR_NetworkMeasuresFCI_C_knn_TE,type="o",pch=17, lty=2,col="cyan")
lines(ICS_mSR_NetworkMeasuresFCI_C_knn_TE,type="o",pch=18, lty=3,col="cyan")
#lines(PS_mSR_NetworkMeasuresFCI_C_knn_H,type="o",pch=19, lty=1,col="yellow")
lines(CD_mSR_NetworkMeasuresFCI_C_knn_H,type="o",pch=20, lty=2,col="yellow")
lines(ICS_mSR_NetworkMeasuresFCI_C_knn_H,type="o",pch=21, lty=3,col="yellow")
legend("bottomright", cex=0.8, legend=c("PS","CD","ICS"),lty=c(1,2,3))
legend("bottomleft", cex=0.8, legend=c("All","PR+TE","PR+H","TE+H","PR","TE","H"),col=c("black","blue","red","green","magenta","cyan","yellow"),lty=1)
dev.off()



###PLOTS NO BACKGROUND JUST PASSED#####
pdf(file="plots/AUC_logreg_justpass_NetworkMeasuresFCI_C.pdf",paper = "a4r")
plot(PS_AUC_NetworkMeasuresFCI_C_justpass_log_all, ylab="AUC", xlab="week", ylim=c(0,1),
     main="Logistic Regression Network Measures + FCI C",type="o", lty=1)
abline(h=lazyJustPass)
lines(CD_AUC_NetworkMeasuresFCI_C_justpass_log_all,type="o",pch=2, lty=2)
lines(ICS_AUC_NetworkMeasuresFCI_C_justpass_log_all,type="o",pch=3, lty=3)
lines(PS_AUC_NetworkMeasuresFCI_C_justpass_log_PRTE,type="o",pch=4, lty=1,col="blue")
lines(CD_AUC_NetworkMeasuresFCI_C_justpass_log_PRTE,type="o",pch=5, lty=2,col="blue")
lines(ICS_AUC_NetworkMeasuresFCI_C_justpass_log_PRTE,type="o",pch=6, lty=3,col="blue")
lines(PS_AUC_NetworkMeasuresFCI_C_justpass_log_PRH,type="o",pch=7, lty=1,col="red")
lines(CD_AUC_NetworkMeasuresFCI_C_justpass_log_PRH,type="o",pch=8, lty=2,col="red")
lines(ICS_AUC_NetworkMeasuresFCI_C_justpass_log_PRH,type="o",pch=9, lty=3,col="red")
lines(PS_AUC_NetworkMeasuresFCI_C_justpass_log_TEH,type="o",pch=10, lty=1,col="green")
lines(CD_AUC_NetworkMeasuresFCI_C_justpass_log_TEH,type="o",pch=11, lty=2,col="green")
lines(ICS_AUC_NetworkMeasuresFCI_C_justpass_log_TEH,type="o",pch=12, lty=3,col="green")
lines(PS_AUC_NetworkMeasuresFCI_C_justpass_log_PR,type="o",pch=13, lty=1,col="magenta")
lines(CD_AUC_NetworkMeasuresFCI_C_justpass_log_PR,type="o",pch=14, lty=2,col="magenta")
lines(ICS_AUC_NetworkMeasuresFCI_C_justpass_log_PR,type="o",pch=15, lty=3,col="magenta")
lines(PS_AUC_NetworkMeasuresFCI_C_justpass_log_TE,type="o",pch=16, lty=1,col="cyan")
lines(CD_AUC_NetworkMeasuresFCI_C_justpass_log_TE,type="o",pch=17, lty=2,col="cyan")
lines(ICS_AUC_NetworkMeasuresFCI_C_justpass_log_TE,type="o",pch=18, lty=3,col="cyan")
lines(PS_AUC_NetworkMeasuresFCI_C_justpass_log_H,type="o",pch=19, lty=1,col="yellow")
lines(CD_AUC_NetworkMeasuresFCI_C_justpass_log_H,type="o",pch=20, lty=2,col="yellow")
lines(ICS_AUC_NetworkMeasuresFCI_C_justpass_log_H,type="o",pch=21, lty=3,col="yellow")
legend("bottomright", cex=0.8, legend=c("PS","CD","ICS"),lty=c(1,2,3))
legend("bottomleft", cex=0.8, legend=c("All","PR+TE","PR+H","TE+H","PR","TE","H"),col=c("black","blue","red","green","magenta","cyan","yellow"),lty=1)
dev.off()

pdf(file="plots/AUC_lda_justpass_NetworkMeasuresFCI_C.pdf",paper = "a4r")
plot(PS_AUC_NetworkMeasuresFCI_C_justpass_lda_all, ylab="AUC", xlab="week", ylim=c(0,1),
     main="Linear Discriminant Analysis Network Measures + FCI C",type="o", lty=1)
abline(h=lazyJustPass)
lines(CD_AUC_NetworkMeasuresFCI_C_justpass_lda_all,type="o",pch=2, lty=2)
lines(ICS_AUC_NetworkMeasuresFCI_C_justpass_lda_all,type="o",pch=3, lty=3)
lines(PS_AUC_NetworkMeasuresFCI_C_justpass_lda_PRTE,type="o",pch=4, lty=1,col="blue")
lines(CD_AUC_NetworkMeasuresFCI_C_justpass_lda_PRTE,type="o",pch=5, lty=2,col="blue")
lines(ICS_AUC_NetworkMeasuresFCI_C_justpass_lda_PRTE,type="o",pch=6, lty=3,col="blue")
lines(PS_AUC_NetworkMeasuresFCI_C_justpass_lda_PRH,type="o",pch=7, lty=1,col="red")
lines(CD_AUC_NetworkMeasuresFCI_C_justpass_lda_PRH,type="o",pch=8, lty=2,col="red")
lines(ICS_AUC_NetworkMeasuresFCI_C_justpass_lda_PRH,type="o",pch=9, lty=3,col="red")
lines(PS_AUC_NetworkMeasuresFCI_C_justpass_lda_TEH,type="o",pch=10, lty=1,col="green")
lines(CD_AUC_NetworkMeasuresFCI_C_justpass_lda_TEH,type="o",pch=11, lty=2,col="green")
lines(ICS_AUC_NetworkMeasuresFCI_C_justpass_lda_TEH,type="o",pch=12, lty=3,col="green")
lines(PS_AUC_NetworkMeasuresFCI_C_justpass_lda_PR,type="o",pch=13, lty=1,col="magenta")
lines(CD_AUC_NetworkMeasuresFCI_C_justpass_lda_PR,type="o",pch=14, lty=2,col="magenta")
lines(ICS_AUC_NetworkMeasuresFCI_C_justpass_lda_PR,type="o",pch=15, lty=3,col="magenta")
lines(PS_AUC_NetworkMeasuresFCI_C_justpass_lda_TE,type="o",pch=16, lty=1,col="cyan")
lines(CD_AUC_NetworkMeasuresFCI_C_justpass_lda_TE,type="o",pch=17, lty=2,col="cyan")
lines(ICS_AUC_NetworkMeasuresFCI_C_justpass_lda_TE,type="o",pch=18, lty=3,col="cyan")
lines(PS_AUC_NetworkMeasuresFCI_C_justpass_lda_H,type="o",pch=19, lty=1,col="yellow")
lines(CD_AUC_NetworkMeasuresFCI_C_justpass_lda_H,type="o",pch=20, lty=2,col="yellow")
lines(ICS_AUC_NetworkMeasuresFCI_C_justpass_lda_H,type="o",pch=21, lty=3,col="yellow")
legend("bottomright", cex=0.8, legend=c("PS","CD","ICS"),lty=c(1,2,3))
legend("bottomleft", cex=0.8, legend=c("All","PR+TE","PR+H","TE+H","PR","TE","H"),col=c("black","blue","red","green","magenta","cyan","yellow"),lty=1)
dev.off()

pdf(file="plots/AUC_qda_justpass_NetworkMeasuresFCI_C.pdf",paper = "a4r")
plot(PS_AUC_NetworkMeasuresFCI_C_justpass_qda_all, ylab="AUC", xlab="week", ylim=c(0,1),
     main="Quadratic Discriminant Analysis Network Measures + FCI C",type="o", lty=1)
abline(h=lazyJustPass)
lines(CD_AUC_NetworkMeasuresFCI_C_justpass_qda_all,type="o",pch=2, lty=2)
lines(ICS_AUC_NetworkMeasuresFCI_C_justpass_qda_all,type="o",pch=3, lty=3)
lines(PS_AUC_NetworkMeasuresFCI_C_justpass_qda_PRTE,type="o",pch=4, lty=1,col="blue")
lines(CD_AUC_NetworkMeasuresFCI_C_justpass_qda_PRTE,type="o",pch=5, lty=2,col="blue")
lines(ICS_AUC_NetworkMeasuresFCI_C_justpass_qda_PRTE,type="o",pch=6, lty=3,col="blue")
lines(PS_AUC_NetworkMeasuresFCI_C_justpass_qda_PRH,type="o",pch=7, lty=1,col="red")
lines(CD_AUC_NetworkMeasuresFCI_C_justpass_qda_PRH,type="o",pch=8, lty=2,col="red")
lines(ICS_AUC_NetworkMeasuresFCI_C_justpass_qda_PRH,type="o",pch=9, lty=3,col="red")
lines(PS_AUC_NetworkMeasuresFCI_C_justpass_qda_TEH,type="o",pch=10, lty=1,col="green")
lines(CD_AUC_NetworkMeasuresFCI_C_justpass_qda_TEH,type="o",pch=11, lty=2,col="green")
lines(ICS_AUC_NetworkMeasuresFCI_C_justpass_qda_TEH,type="o",pch=12, lty=3,col="green")
lines(PS_AUC_NetworkMeasuresFCI_C_justpass_qda_PR,type="o",pch=13, lty=1,col="magenta")
lines(CD_AUC_NetworkMeasuresFCI_C_justpass_qda_PR,type="o",pch=14, lty=2,col="magenta")
lines(ICS_AUC_NetworkMeasuresFCI_C_justpass_qda_PR,type="o",pch=15, lty=3,col="magenta")
lines(PS_AUC_NetworkMeasuresFCI_C_justpass_qda_TE,type="o",pch=16, lty=1,col="cyan")
lines(CD_AUC_NetworkMeasuresFCI_C_justpass_qda_TE,type="o",pch=17, lty=2,col="cyan")
lines(ICS_AUC_NetworkMeasuresFCI_C_justpass_qda_TE,type="o",pch=18, lty=3,col="cyan")
lines(PS_AUC_NetworkMeasuresFCI_C_justpass_qda_H,type="o",pch=19, lty=1,col="yellow")
lines(CD_AUC_NetworkMeasuresFCI_C_justpass_qda_H,type="o",pch=20, lty=2,col="yellow")
lines(ICS_AUC_NetworkMeasuresFCI_C_justpass_qda_H,type="o",pch=21, lty=3,col="yellow")
legend("bottomright", cex=0.8, legend=c("PS","CD","ICS"),lty=c(1,2,3))
legend("bottomleft", cex=0.8, legend=c("All","PR+TE","PR+H","TE+H","PR","TE","H"),col=c("black","blue","red","green","magenta","cyan","yellow"),lty=1)
dev.off()

pdf(file="plots/mSR_knn_justpass_NetworkMeasuresFCI_C.pdf",paper = "a4r")
plot(PS_mSR_NetworkMeasuresFCI_C_justpass_knn_all, ylab="AUC", xlab="week", ylim=c(0,1),
     main="K-nearest neighbours Network Measures + FCI C",type="o", lty=1)
abline(h=lazyJustPass)
lines(CD_mSR_NetworkMeasuresFCI_C_justpass_knn_all,type="o",pch=2, lty=2)
lines(ICS_mSR_NetworkMeasuresFCI_C_justpass_knn_all,type="o",pch=3, lty=3)
lines(PS_mSR_NetworkMeasuresFCI_C_justpass_knn_PRTE,type="o",pch=4, lty=1,col="blue")
lines(CD_mSR_NetworkMeasuresFCI_C_justpass_knn_PRTE,type="o",pch=5, lty=2,col="blue")
lines(ICS_mSR_NetworkMeasuresFCI_C_justpass_knn_PRTE,type="o",pch=6, lty=3,col="blue")
lines(PS_mSR_NetworkMeasuresFCI_C_justpass_knn_PRH,type="o",pch=7, lty=1,col="red")
lines(CD_mSR_NetworkMeasuresFCI_C_justpass_knn_PRH,type="o",pch=8, lty=2,col="red")
lines(ICS_mSR_NetworkMeasuresFCI_C_justpass_knn_PRH,type="o",pch=9, lty=3,col="red")
lines(PS_mSR_NetworkMeasuresFCI_C_justpass_knn_TEH,type="o",pch=10, lty=1,col="green")
lines(CD_mSR_NetworkMeasuresFCI_C_justpass_knn_TEH,type="o",pch=11, lty=2,col="green")
lines(ICS_mSR_NetworkMeasuresFCI_C_justpass_knn_TEH,type="o",pch=12, lty=3,col="green")
lines(PS_mSR_NetworkMeasuresFCI_C_justpass_knn_PR,type="o",pch=13, lty=1,col="magenta")
lines(CD_mSR_NetworkMeasuresFCI_C_justpass_knn_PR,type="o",pch=14, lty=2,col="magenta")
lines(ICS_mSR_NetworkMeasuresFCI_C_justpass_knn_PR,type="o",pch=15, lty=3,col="magenta")
lines(PS_mSR_NetworkMeasuresFCI_C_justpass_knn_TE,type="o",pch=16, lty=1,col="cyan")
lines(CD_mSR_NetworkMeasuresFCI_C_justpass_knn_TE,type="o",pch=17, lty=2,col="cyan")
lines(ICS_mSR_NetworkMeasuresFCI_C_justpass_knn_TE,type="o",pch=18, lty=3,col="cyan")
#lines(PS_mSR_NetworkMeasuresFCI_C_justpass_knn_H,type="o",pch=19, lty=1,col="yellow")
lines(CD_mSR_NetworkMeasuresFCI_C_justpass_knn_H,type="o",pch=20, lty=2,col="yellow")
lines(ICS_mSR_NetworkMeasuresFCI_C_justpass_knn_H,type="o",pch=21, lty=3,col="yellow")
legend("bottomright", cex=0.8, legend=c("PS","CD","ICS"),lty=c(1,2,3))
legend("bottomleft", cex=0.8, legend=c("All","PR+TE","PR+H","TE+H","PR","TE","H"),col=c("black","blue","red","green","magenta","cyan","yellow"),lty=1)
dev.off()



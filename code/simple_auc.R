simple_auc <- function(TPR, FPR){
  x<-data.frame(TPR,FPR)
  x<- x[order(x[, 1],decreasing = F),]
  x<- x[order(x[, 2],decreasing = F),]
  TPR<-x[,1]
  FPR<-x[,2]
  dFPR <- abs(c(diff(FPR), 0))
  dTPR <- abs(c(diff(TPR), 0))
  sum(TPR * dFPR) + sum(dTPR * dFPR)/2
}
PS_AUC<-vector()
for (i in 1:7){
  PS_AUC[i]<-simple_auc(ROC_PS_TPR[,i],ROC_PS_FPR[,i])
}

CD_AUC<-vector()
for (i in 1:7){
  CD_AUC[i]<-simple_auc(ROC_CD_TPR[,i],ROC_CD_FPR[,i])
}

ICS_AUC<-vector()
for (i in 1:7){
  ICS_AUC[i]<-simple_auc(ROC_ICS_TPR[,i],ROC_ICS_FPR[,i])
}

PS_AUC_JP<-vector()
for (i in 1:7){
  PS_AUC_JP[i]<-simple_auc(ROC_PS_justpass_TPR[,i],ROC_PS_justpass_FPR[,i])
}

CD_AUC_JP<-vector()
for (i in 1:7){
  CD_AUC_JP[i]<-simple_auc(ROC_CD_justpass_TPR[,i],ROC_CD_justpass_FPR[,i])
}

ICS_AUC_JP<-vector()
for (i in 1:7){
  ICS_AUC_JP[i]<-simple_auc(ROC_ICS_justpass_TPR[,i],ROC_ICS_justpass_FPR[,i])
}
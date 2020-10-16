ROCplus<-function(predFrame,w){
  if(length(unique(predFrame[,w+2]))!=1){
    confusionMatrix<-table(truth=predFrame[,2],prediction=predFrame[,w+2])
  }
  else if (predFrame[1,w+2]==1){
    confusionMatrix<-matrix(0,2,2)
    confusionMatrix[,2]<-table(truth=predFrame[,2],prediction=predFrame[,w+2])
  }
  else{
    confusionMatrix<-matrix(0,2,2)
    confusionMatrix[,1]<-table(truth=predFrame[,2],prediction=predFrame[,w+2])
  }
  FP<-confusionMatrix[1,2]
  FN<-confusionMatrix[2,1]
  TN<-confusionMatrix[1,1]
  TP<-confusionMatrix[2,2]
  FPR<-FP/(FP+TN) #false positive rate, Type I erro, 1-specificity
  TPR<-TP/(FN+TP) #true positive rate, 1- Type II error, power, sensitivity, recall
  PPV<-TP/(FP+TP) #precision, 1-false discovery proportion
  SR<-(TP+TN)/(FP+FN+TN+TP)
  N<-(FP+FN+TN+TP)#success-rate
  res<-c(FP,FN,TN,TP,FPR,TPR,PPV,SR) 
  return(res)
}

ROCplusWeeks<-function(predFrame){
  res<-matrix(0,ncol=8,nrow=7)
  rownames(res)<-c("w1","w2","w3","w4","w5","w6","w7")
  colnames(res)<-c("FP","FN","TN","TP","FPR","TPR","PPV","SR")
  for(i in 1:7){
    res[i,]<-ROCplus(predFrame,i)
  }
  res<-as.data.frame(res)
  return(res)
}

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
jackPred <- function(layer, outcome = "pass", 
                     predictors = c("gender", "cohort", "fci_pre", "PageRank", 
                                    "tarEnt", "Hide"),p=0.5) {
  if (outcome == "pass" | outcome == "justpass") {
    choices <- c("0", "1")
  } else {
    stop("Not a valid outcome variable.")
  }
  # remove incomplete rows
  userows <- complete.cases(layer[[length(layer)]][, c(outcome,predictors)])  
  
  allprob <- matrix(nrow = sum(userows), ncol = length(layer))
  fitStr <- paste(predictors, collapse = " + ")
  fitForm <- paste0(outcome, " ~ ", fitStr)
  for(j in 1:length(layer)) {
    # data is complete cases
    data <- layer[[j]][userows, c(outcome, predictors)]
    # Loop through all nodes
    for(i in 1:dim(data)[1]) {
      # training set is data minus observation i
      train <- data[-i, ]
      glm.fit <- glm(fitForm, family = binomial, data = train)
      allprob[i, j] <- predict(glm.fit, newdata = data[i, ], type = "response")
    }
  }
  allpred <- allprob
  allpred[allprob < p] <- choices[1]    # 0
  allpred[allprob >= p] <- choices[2]   # 1
  
  # To return: node name, actual outcome, predicted outcome columns
  alldata <- data.frame(layer[[1]][userows, "name"], data[, outcome], as.data.frame(allpred))
  
  # Turn outcomes into factor
  for(i in seq_along(layer)) {
    alldata[, i+2] <- as.factor(alldata[, i+2])
  }
  
  names(alldata) <- c("name", outcome, paste0("Week", c(1:length(layer))))
  print(paste0("Fit: ", fitForm, ", complete N = ", dim(alldata)[1]))
  return(alldata)
}

# Predict pass/fail
predPS <- jackPred(centPS,p=0.2)
predCD <- jackPred(centCD,p=0.2)
predICS <- jackPred(centICS,p=0.2)

# Predict just-pass/just-fail (2/0)
predJustPS <- jackPred(centPS, outcome = "justpass",p=0.2)
predJustCD <- jackPred(centCD, outcome = "justpass",p=0.2)
predJustICS <- jackPred(centICS, outcome = "justpass",p=0.2)
###ROC curves and such###
ROCplus<-function(predFrame,w){
  if(length(unique(predFrame[,w+2]))!=1){
  confusionMatrix<-table(truth=predFrame[,2],prediction=predFrame[,w+2])
  }
  else if (predFrame[1,w+2]==1){
    confusionMatrix<-matrix(0,2,2)
    confusionMatrix[,2]<-table(truth=predPS[,2],prediction=predPS[,w+2])
  }
  else{
    confusionMatrix<-matrix(0,2,2)
    confusionMatrix[,1]<-table(truth=predPS[,2],prediction=predPS[,w+2])
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

ROCplusWeeks<-function(predPS){
  res<-matrix(0,ncol=8,nrow=7)
  rownames(res)<-c("w1","w2","w3","w4","w5","w6","w7")
  colnames(res)<-c("FP","FN","TN","TP","FPR","TPR","PPV","SR")
  for(i in 1:7){
  res[i,]<-ROCplus(predPS,i)
  }
  return(res)
}
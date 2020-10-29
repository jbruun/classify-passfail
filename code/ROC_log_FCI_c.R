
rm(list = ls())

library(igraph)
library(dplyr)



# Import pass/fail centrality data
#loadvars <- load("data/centPassFail.Rdata")
load("data/centrality_data_frames.Rdata")
load("data/ROC_logreg.Rdata")

## Run jackknife logistic regression 

# Turn long data frame into list of weekly frames
centPS <- dfPS %>% group_split(Week)
centCD <- dfCD %>% group_split(Week)
centICS <- dfICS %>% group_split(Week)

jackPredLog <- function(layer, outcome = "pass", 
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

jackPredLDA <- function(layer, outcome = "pass", 
                        predictors = c("gender", "cohort", "fci_pre", 
                                       "PageRank", "tarEnt", "Hide"),p=0.5) {
  # Check for valid input
  if (outcome == "pass" | outcome == "justpass") {
    choices <- c("0", "1")
  } else {
    stop("Not a valid outcome variable.")
  }
  # Remove incomplete rows
  userows <- complete.cases(layer[[length(layer)]][, c(outcome,predictors)])  
  
  allprob <- matrix(nrow = sum(userows), ncol = length(layer))
  cases <- c(1:sum(userows))
  
  # Build fitting input string using predictor names
  fitStr <- paste(predictors, collapse = " + ")
  fitForm <- paste0(outcome, " ~ ", fitStr)
  
  # Loop through all weeks
  for(j in 1:length(layer)) {
    # Data is complete cases
    data <- data.frame(layer[[j]][userows, c(outcome, predictors)])
    # Loop through all nodes
    for(i in 1:dim(data)[1]) {
      # Training set is data minus observation i
      train <- !cases==i
      lda.fit <- lda(data[,-1], grouping=data[,1], subset = train)
      allprob[i, j] <- predict(lda.fit, newdata = data[i,-1])$posterior[2]
    }
  }
  allpred <- allprob
  allpred[allprob < p] <- choices[1]    # 0
  allpred[allprob >= p] <- choices[2]   # 1
  
  # To return: node name, actual outcome, predicted outcome columns
  alldata <- data.frame(layer[[1]][userows, "name"], data[, outcome], 
                        as.data.frame(allpred))
  
  # Turn outcomes into factor
  for(i in seq_along(layer)) {
    alldata[, i+2] <- as.factor(alldata[, i+2])
  }
  
  names(alldata) <- c("name", outcome, paste0("Week", c(1:length(layer))))
  print(paste0("Fit: ", fitForm, ", complete N = ", dim(alldata)[1]))
  return(alldata)
}

###ROC curves and such###
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

#RUN
ROC_PS_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,predictors = c("fci_pre_0", "PageRank","age" 
                                            ),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_log[[i]]<-ROC
}

ROC_CD_log<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD,predictors = c("fci_pre_c", "PageRank", 
                                              "tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_log[[i]]<-ROC
}

ROC_ICS_log<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS,predictors = c("gender", "cohort", "fci_pre_c", "PageRank", 
                                                "tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_log[[i]]<-ROC
}

ROC_PS_justpass_log<-list()
for(i in 1:100){
  predPS_x<-jackPredLog(centPS,outcome = "justpass",predictors = c("gender", "cohort", "fci_pre_c", "PageRank", 
                                                                   "tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_log[[i]]<-ROC
}

ROC_CD_justpass_log<-list()
for(i in 1:100){
  predCD_x<-jackPredLog(centCD, outcome = "justpass",predictors = c("gender", "cohort", "fci_pre_c", "PageRank", 
                                                                    "tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_log[[i]]<-ROC
}

ROC_ICS_justpass_log<-list()
for(i in 1:100){
  predICS_x<-jackPredLog(centICS, outcome = "justpass",predictors = c("gender", "cohort", "fci_pre_c", "PageRank", 
                                                                      "tarEnt", "Hide"),p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_log[[i]]<-ROC
}


save(ROC_PS_log,ROC_CD_log,ROC_ICS_log, ROC_PS_justpass_log,ROC_CD_justpass_log,ROC_ICS_justpass_log,file="data/ROC_logreg.Rdata")



ROC_PS_TPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_FPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_PPV<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_SR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_PS_TPR[i,1:7]<-ROC_PS_log[[i]]$TPR[1:7]
  ROC_PS_FPR[i,1:7]<-ROC_PS_log[[i]]$FPR[1:7]
  ROC_PS_PPV[i,1:7]<-ROC_PS_log[[i]]$PPV[1:7]
  ROC_PS_SR[i,1:7]<-ROC_PS_log[[i]]$SR[1:7]
}

PS_AUC<-vector()
for (i in 1:7){
  PS_AUC[i]<-simple_auc(ROC_PS_TPR[,i],ROC_PS_FPR[,i])
}

ROC_CD_TPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_FPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_PPV<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_SR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_CD_TPR[i,1:7]<-ROC_CD_log[[i]]$TPR[1:7]
  ROC_CD_FPR[i,1:7]<-ROC_CD_log[[i]]$FPR[1:7]
  ROC_CD_PPV[i,1:7]<-ROC_CD_log[[i]]$PPV[1:7]
  ROC_CD_SR[i,1:7]<-ROC_CD_log[[i]]$SR[1:7]
}

ROC_ICS_TPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_FPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_PPV<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_SR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_ICS_TPR[i,1:7]<-ROC_ICS_log[[i]]$TPR[1:7]
  ROC_ICS_FPR[i,1:7]<-ROC_ICS_log[[i]]$FPR[1:7]
  ROC_ICS_PPV[i,1:7]<-ROC_ICS_log[[i]]$PPV[1:7]
  ROC_ICS_SR[i,1:7]<-ROC_ICS_log[[i]]$SR[1:7]
}

ROC_PS_justpass_TPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_justpass_FPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_justpass_PPV<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_justpass_SR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_PS_justpass_TPR[i,1:7]<-ROC_PS_justpass_log[[i]]$TPR[1:7]
  ROC_PS_justpass_FPR[i,1:7]<-ROC_PS_justpass_log[[i]]$FPR[1:7]
  ROC_PS_justpass_PPV[i,1:7]<-ROC_PS_justpass_log[[i]]$PPV[1:7]
  ROC_PS_justpass_SR[i,1:7]<-ROC_PS_justpass_log[[i]]$SR[1:7]
}

ROC_CD_justpass_TPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_justpass_FPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_justpass_PPV<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_justpass_SR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_CD_justpass_TPR[i,1:7]<-ROC_CD_justpass_log[[i]]$TPR[1:7]
  ROC_CD_justpass_FPR[i,1:7]<-ROC_CD_justpass_log[[i]]$FPR[1:7]
  ROC_CD_justpass_PPV[i,1:7]<-ROC_CD_justpass_log[[i]]$PPV[1:7]
  ROC_CD_justpass_SR[i,1:7]<-ROC_CD_justpass_log[[i]]$SR[1:7]
}

ROC_ICS_justpass_TPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_justpass_FPR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_justpass_PPV<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_justpass_SR<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_ICS_justpass_TPR[i,1:7]<-ROC_ICS_justpass_log[[i]]$TPR[1:7]
  ROC_ICS_justpass_FPR[i,1:7]<-ROC_ICS_justpass_log[[i]]$FPR[1:7]
  ROC_ICS_justpass_PPV[i,1:7]<-ROC_ICS_justpass_log[[i]]$PPV[1:7]
  ROC_ICS_justpass_SR[i,1:7]<-ROC_ICS_justpass_log[[i]]$SR[1:7]
}
###PLOTTING###
plot(ROC_PS_FPR$w1,ROC_PS_TPR$w1,main="Pass/fail logistic regression jack-knife PS", xlab = "False Postive Rate",ylab="True Positive Rate",type="l",col="black")
points(ROC_PS_FPR$w2,ROC_PS_TPR$w2,type="l",col="blue")
points(ROC_PS_FPR$w3,ROC_PS_TPR$w3,type="l",col="red")
points(ROC_PS_FPR$w4,ROC_PS_TPR$w4,type="l",col="yellow")
points(ROC_PS_FPR$w5,ROC_PS_TPR$w5,type="l",col="grey")
points(ROC_PS_FPR$w6,ROC_PS_TPR$w6,type="l",col="purple")
points(ROC_PS_FPR$w7,ROC_PS_TPR$w7,type="l",col="green")

plot(ROC_CD_FPR$w1,ROC_CD_TPR$w1,main="Pass/fail logistic regression jack-knife CD", xlab = "False Postive Rate",ylab="True Positive Rate",type="l",col="black")
points(ROC_CD_FPR$w2,ROC_CD_TPR$w2,type="l",col="blue")
points(ROC_CD_FPR$w3,ROC_CD_TPR$w3,type="l",col="red")
points(ROC_CD_FPR$w4,ROC_CD_TPR$w4,type="l",col="yellow")
points(ROC_CD_FPR$w5,ROC_CD_TPR$w5,type="l",col="grey")
points(ROC_CD_FPR$w6,ROC_CD_TPR$w6,type="l",col="purple")
points(ROC_CD_FPR$w7,ROC_CD_TPR$w7,type="l",col="green")

plot(ROC_ICS_FPR$w1,ROC_ICS_TPR$w1,main="Pass/fail logistic regression jack-knife ICS", xlab = "False Postive Rate",ylab="True Positive Rate",type="l",col="black")
points(ROC_ICS_FPR$w2,ROC_ICS_TPR$w2,type="l",col="blue")
points(ROC_ICS_FPR$w3,ROC_ICS_TPR$w3,type="l",col="red")
points(ROC_ICS_FPR$w4,ROC_ICS_TPR$w4,type="l",col="yellow")
points(ROC_ICS_FPR$w5,ROC_ICS_TPR$w5,type="l",col="grey")
points(ROC_ICS_FPR$w6,ROC_ICS_TPR$w6,type="l",col="purple")
points(ROC_ICS_FPR$w7,ROC_ICS_TPR$w7,type="l",col="green")

plot(ROC_PS_justpass_FPR$w1,ROC_PS_justpass_TPR$w1,main="Just pass/fail logistic regression jack-knife PS", xlab = "False Postive Rate",ylab="True Positive Rate",type="l",col="black")
points(ROC_PS_justpass_FPR$w2,ROC_PS_justpass_TPR$w2,type="l",col="blue")
points(ROC_PS_justpass_FPR$w3,ROC_PS_justpass_TPR$w3,type="l",col="red")
points(ROC_PS_justpass_FPR$w4,ROC_PS_justpass_TPR$w4,type="l",col="yellow")
points(ROC_PS_justpass_FPR$w5,ROC_PS_justpass_TPR$w5,type="l",col="grey")
points(ROC_PS_justpass_FPR$w6,ROC_PS_justpass_TPR$w6,type="l",col="purple")
points(ROC_PS_justpass_FPR$w7,ROC_PS_justpass_TPR$w7,type="l",col="green")

plot(ROC_CD_justpass_FPR$w1,ROC_CD_justpass_TPR$w1,main="Just pass/fail logistic regression jack-knife CD", xlab = "False Postive Rate",ylab="True Positive Rate",type="l",col="black")
points(ROC_CD_justpass_FPR$w2,ROC_CD_justpass_TPR$w2,type="l",col="blue")
points(ROC_CD_justpass_FPR$w3,ROC_CD_justpass_TPR$w3,type="l",col="red")
points(ROC_CD_justpass_FPR$w4,ROC_CD_justpass_TPR$w4,type="l",col="yellow")
points(ROC_CD_justpass_FPR$w5,ROC_CD_justpass_TPR$w5,type="l",col="grey")
points(ROC_CD_justpass_FPR$w6,ROC_CD_justpass_TPR$w6,type="l",col="purple")
points(ROC_CD_justpass_FPR$w7,ROC_CD_justpass_TPR$w7,type="l",col="green")

plot(ROC_ICS_justpass_FPR$w1,ROC_ICS_justpass_TPR$w1,main="Just pass/fail logistic regression jack-knife ICS", xlab = "False Postive Rate",ylab="True Positive Rate",type="l",col="black")
points(ROC_ICS_justpass_FPR$w2,ROC_ICS_justpass_TPR$w2,type="l",col="blue")
points(ROC_ICS_justpass_FPR$w3,ROC_ICS_justpass_TPR$w3,type="l",col="red")
points(ROC_ICS_justpass_FPR$w4,ROC_ICS_justpass_TPR$w4,type="l",col="yellow")
points(ROC_ICS_justpass_FPR$w5,ROC_ICS_justpass_TPR$w5,type="l",col="grey")
points(ROC_ICS_justpass_FPR$w6,ROC_ICS_justpass_TPR$w6,type="l",col="purple")
points(ROC_ICS_justpass_FPR$w7,ROC_ICS_justpass_TPR$w7,type="l",col="green")

plot(c(1:100),ROC_PS_PPV$w1,xlab = "Threshold probability",ylab="Positive Prediction Value",type="l",col="black")
points(c(1:100),ROC_PS_PPV$w2,type="l",col="blue")
points(c(1:100),ROC_PS_PPV$w3,type="l",col="red")
points(c(1:100),ROC_PS_PPV$w4,type="l",col="yellow")
points(c(1:100),ROC_PS_PPV$w5,type="l",col="grey")
points(c(1:100),ROC_PS_PPV$w6,type="l",col="purple")
points(c(1:100),ROC_PS_PPV$w7,type="l",col="green")

plot(c(1:100),ROC_PS_SR$w1,ylim=c(0,1), xlab = "Threshold probability",ylab="Success Rate",type="l",col="black")
points(c(1:100),ROC_PS_SR$w2,type="l",col="blue")
points(c(1:100),ROC_PS_SR$w3,type="l",col="red")
points(c(1:100),ROC_PS_SR$w4,type="l",col="yellow")
points(c(1:100),ROC_PS_SR$w5,type="l",col="grey")
points(c(1:100),ROC_PS_SR$w6,type="l",col="purple")
points(c(1:100),ROC_PS_SR$w7,type="l",col="green")

plot(c(1:100),ROC_PS_FPR$w1,ylim=c(0,1), xlab = "Threshold probability",ylab="False Positive Rate",type="l",col="black")
points(c(1:100),ROC_PS_FPR$w2,type="l",col="blue")
points(c(1:100),ROC_PS_FPR$w3,type="l",col="red")
points(c(1:100),ROC_PS_FPR$w4,type="l",col="yellow")
points(c(1:100),ROC_PS_FPR$w5,type="l",col="grey")
points(c(1:100),ROC_PS_FPR$w6,type="l",col="purple")
points(c(1:100),ROC_PS_FPR$w7,type="l",col="green")

plot(c(1:100),ROC_PS_TPR$w1,ylim=c(0,1), xlab = "Threshold probability",ylab="True Positive Rate",type="l",col="black")
points(c(1:100),ROC_PS_TPR$w2,type="l",col="blue")
points(c(1:100),ROC_PS_TPR$w3,type="l",col="red")
points(c(1:100),ROC_PS_TPR$w4,type="l",col="yellow")
points(c(1:100),ROC_PS_TPR$w5,type="l",col="grey")
points(c(1:100),ROC_PS_TPR$w6,type="l",col="purple")
points(c(1:100),ROC_PS_TPR$w7,type="l",col="green")

save(ROC_PS_log,ROC_CD_log,ROC_ICS_log, file="data/ROC_logreg.Rdata")

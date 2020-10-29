# K nearest neighbors version
# Input: List of weekly data frames, optional outcome (pass/justpass), optional 
#  subset of predictors to use
# Output: List of prediction vectors for that weekly aggregate network; each 
#  node in the vector is predicted using all the other nodes
jackPredKNN <- function(layer, nK = 1, outcome = "pass", 
                        predictors = c("gender", "cohort", "fci_pre", 
                                       "PageRank", "tarEnt", "Hide")) {
  # Check for valid input
  if (outcome == "pass" | outcome == "justpass") {
    choices <- c("0", "1")
  } else {
    stop("Not a valid outcome variable.")
  }
  
  # Input data (complete cases) and empty frame to store predictions 
  Nlayer <- length(layer)
  userows <- complete.cases(layer[[Nlayer]][, c(outcome, predictors)])  
  allpred <- matrix(nrow = sum(userows), ncol = Nlayer)
  
  # Build fitting input string using predictor names
  fitStr <- paste(predictors, collapse = " + ")
  fitForm <- paste0(outcome, " ~ ", fitStr)
  
  # Loop through all weeks
  for(j in seq(Nlayer)) {
    data <- layer[[j]][userows, c(outcome, predictors)] # data is complete cases
    
    # Loop through all nodes
    for(i in 1:dim(data)[1]) {
      # Training set is data minus observation i
      # Predictor matrices for training and test data
      train <- as.matrix(data[-i, predictors])
      test <- as.matrix(data[i, predictors])
      
      # Outcome vectors for training and test data
      # Can't just do data[-i, outcome], for explanation see
      # https://stackoverflow.com/questions/51063381/vector-from-tibble-has-length-0
      trOutcome <- pull(data[-i, ], var = outcome)
      teOutcome <- pull(data[i, ], var = outcome)
      
      # Run KNN
      set.seed(2)
      # Logistic regression makes probabilities, which you translate into 
      # prediction; KNN does it all in one step
      allpred[i, j] <- knn(train, test, trOutcome, k = nK)
    }
  }
  
  # Assemble data frame: Translate factor to labels, add node names and outcomes
  allpred[allpred == 1] <- choices[1] # 0
  allpred[allpred == 2] <- choices[2] # 1
  allpred <- data.frame(layer[[1]][userows, "name"],  # node names
                        data[, outcome],            # real outcome
                        as.data.frame(allpred))     # predicted outcome
  names(allpred) <- c("name", outcome, paste0("Week", c(1:length(layer))))
  
  # Print info string and return predictions
  print(paste0("Fit: ", fitForm, ", #neighbors = ", nK, 
               ", complete N = ", dim(allpred)[1]))
  return(list(nK = nK, allpred = allpred))
}

ROC_PS_knn<-list()
for(i in 1:10){
  predPS_x<-jackPredKNN(centPS,nK=i,predictors = c("gender", "fci_pre_c", 
                                                   "PageRank", "tarEnt", "Hide"))
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_knn[[i]]<-ROC
}

ROC_CD_knn<-list()
for(i in 1:10){
  predCD_x<-jackPredKNN(centCD,predictors = c("gender", "fci_pre_c", 
                                              "PageRank", "tarEnt", "Hide"),nK=i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_knn[[i]]<-ROC
}

ROC_ICS_knn<-list()
for(i in 1:10){
  predICS_x<-jackPredKNN(centICS,predictors = c("gender", "fci_pre_c", 
                                                "PageRank", "tarEnt", "Hide"),nK=i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_knn[[i]]<-ROC
}

ROC_PS_justpass_knn<-list()
for(i in 1:10){
  predPS_x<-jackPredKNN(centPS,outcome = "justpass",predictors = c("gender", "fci_pre_c", 
                                                                   "PageRank", "tarEnt", "Hide"),nK=i)
  ROC<-ROCplusWeeks(predPS_x$allpred)
  ROC_PS_justpass_knn[[i]]<-ROC
}

ROC_CD_justpass_knn<-list()
for(i in 1:10){
  predCD_x<-jackPredKNN(centCD, outcome = "justpass",predictors = c("gender", "fci_pre_c", 
                                                                    "PageRank", "tarEnt", "Hide"),nK=i)
  ROC<-ROCplusWeeks(predCD_x$allpred)
  ROC_CD_justpass_knn[[i]]<-ROC
}

ROC_ICS_justpass_knn<-list()
for(i in 1:10){
  predICS_x<-jackPredKNN(centICS, outcome = "justpass",predictors = c("gender", "fci_pre_c", 
                                                                      "PageRank", "tarEnt", "Hide"),nK=i)
  ROC<-ROCplusWeeks(predICS_x$allpred)
  ROC_ICS_justpass_knn[[i]]<-ROC
}

#
ROC_PS_TPR_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_FPR_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_PPV_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_SR_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:10){
  ROC_PS_TPR_knn[i,1:7]<-ROC_PS_knn[[i]]$TPR[1:7]
  ROC_PS_FPR_knn[i,1:7]<-ROC_PS_knn[[i]]$FPR[1:7]
  ROC_PS_PPV_knn[i,1:7]<-ROC_PS_knn[[i]]$PPV[1:7]
  ROC_PS_SR_knn[i,1:7]<-ROC_PS_knn[[i]]$SR[1:7]
}

ROC_CD_TPR_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_FPR_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_PPV_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_SR_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:10){
  ROC_CD_TPR_knn[i,1:7]<-ROC_CD_knn[[i]]$TPR[1:7]
  ROC_CD_FPR_knn[i,1:7]<-ROC_CD_knn[[i]]$FPR[1:7]
  ROC_CD_PPV_knn[i,1:7]<-ROC_CD_knn[[i]]$PPV[1:7]
  ROC_CD_SR_knn[i,1:7]<-ROC_CD_knn[[i]]$SR[1:7]
}

ROC_ICS_TPR_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_FPR_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_PPV_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_SR_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:10){
  ROC_ICS_TPR_knn[i,1:7]<-ROC_ICS_knn[[i]]$TPR[1:7]
  ROC_ICS_FPR_knn[i,1:7]<-ROC_ICS_knn[[i]]$FPR[1:7]
  ROC_ICS_PPV_knn[i,1:7]<-ROC_ICS_knn[[i]]$PPV[1:7]
  ROC_ICS_SR_knn[i,1:7]<-ROC_ICS_knn[[i]]$SR[1:7]
}

ROC_PS_justpass_TPR_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_justpass_FPR_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_justpass_PPV_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_justpass_SR_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:10){
  ROC_PS_justpass_TPR_knn[i,1:7]<-ROC_PS_justpass_knn[[i]]$TPR[1:7]
  ROC_PS_justpass_FPR_knn[i,1:7]<-ROC_PS_justpass_knn[[i]]$FPR[1:7]
  ROC_PS_justpass_PPV_knn[i,1:7]<-ROC_PS_justpass_knn[[i]]$PPV[1:7]
  ROC_PS_justpass_SR_knn[i,1:7]<-ROC_PS_justpass_knn[[i]]$SR[1:7]
}
ROC_CD_justpass_TPR_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_justpass_FPR_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_justpass_PPV_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_justpass_SR_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:10){
  ROC_CD_justpass_TPR_knn[i,1:7]<-ROC_CD_justpass_knn[[i]]$TPR[1:7]
  ROC_CD_justpass_FPR_knn[i,1:7]<-ROC_CD_justpass_knn[[i]]$FPR[1:7]
  ROC_CD_justpass_PPV_knn[i,1:7]<-ROC_CD_justpass_knn[[i]]$PPV[1:7]
  ROC_CD_justpass_SR_knn[i,1:7]<-ROC_CD_justpass_knn[[i]]$SR[1:7]
}

ROC_ICS_justpass_TPR_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_justpass_FPR_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_justpass_PPV_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_justpass_SR_knn<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:10){
  ROC_ICS_justpass_TPR_knn[i,1:7]<-ROC_ICS_justpass_knn[[i]]$TPR[1:7]
  ROC_ICS_justpass_FPR_knn[i,1:7]<-ROC_ICS_justpass_knn[[i]]$FPR[1:7]
  ROC_ICS_justpass_PPV_knn[i,1:7]<-ROC_ICS_justpass_knn[[i]]$PPV[1:7]
  ROC_ICS_justpass_SR_knn[i,1:7]<-ROC_ICS_justpass_knn[[i]]$SR[1:7]
}


#
PS_AUC_knn<-vector()
for (i in 1:7){
  PS_AUC_knn[i]<-simple_auc(ROC_PS_TPR_knn[,i],ROC_PS_FPR_knn[,i])
}
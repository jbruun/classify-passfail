# K-fold cross-validation loops for different classifiers. Lots of duplicate 
# code, should probably combine functions, but haven't yet.

# Split rows of a data frame into (approximately) equal-sized chunks
# See https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks
chunk <- function(df, n) {
  x <- seq(nrow(df))
  # split(df, cut(x, n, labels = FALSE))  # for now, only split row numbers
  split(x, cut(x, n, labels = FALSE))
}


# Logistic regression version
# Input: List of weekly data frames, outcome (pass/justpass), number of chunks 
#  to divide data into, predictors to use, probability threshold for outcome
# Output: Data frame of prediction vectors for that weekly aggregate network; 
#  each chunk in the vector is predicted using the other k-1 chunks
kfoldLog <- function(layer, outcome = "pass", k = 5, 
                        predictors = c("gender", "cohort", "fci_pre", 
                                       "PageRank", "tarEnt", "Hide"), p=0.5) {
  # Check for valid input
  if (outcome == "pass" | outcome == "justpass") {
    choices <- c("0", "1")
  } else {
    stop("Not a valid outcome variable.")
  }
  # Remove incomplete rows
  userows <- complete.cases(layer[[length(layer)]][, c(outcome,predictors)])  
  
  allprob <- matrix(nrow = sum(userows), ncol = length(layer))
  fitStr <- paste(predictors, collapse = " + ")
  fitForm <- paste0(outcome, " ~ ", fitStr)
  
  # Loop through all weekly data frames
  for(j in 1:length(layer)) {
    # Data is complete cases
    data <- layer[[j]][userows, c(outcome, predictors)]
    chunkrows <- chunk(data, n = k)
  
    # Loop through all chunks
    for(i in 1:k) {
      # Training set is data minus chunk i
      train <- data[-chunkrows[[i]], ]
      test <- data[chunkrows[[i]], ]
      
      glm.fit <- glm(fitForm, family = binomial, data = train)
      allprob[chunkrows[[i]], j] <- predict(glm.fit, newdata = test, 
                                            type = "response")
    }
  }
  allpred <- allprob
  allpred[allprob < p] <- choices[1]    # 0
  allpred[allprob >= p] <- choices[2]   # 1
  
  # To return: node name, actual outcome, predicted outcome columns
  alldata <- data.frame(layer[[1]][userows, "name"], data[, outcome], 
                        as.data.frame(allpred))
  
  # Turn outcomes into factor, looping through each week's column
  for(i in seq_along(layer)) {
    alldata[, i+2] <- as.factor(alldata[, i+2])
  }
  
  names(alldata) <- c("name", outcome, paste0("Week", c(1:length(layer))))
  print(paste0("Fit: ", fitForm, ", complete N = ", dim(alldata)[1], ", k = ", k))
  return(alldata)
}


# Linear discriminant analysis (LDA) version
# Input: List of weekly data frames, outcome (pass/justpass), number of chunks 
#  to divide data into, predictors to use, probability threshold for outcome
# Output: Data frame of prediction vectors for that weekly aggregate network; 
#  each chunk in the vector is predicted using the other k-1 chunks
kfoldLDA <- function(layer, outcome = "pass", k = 5, 
                     predictors = c("gender", "cohort", "fci_pre", 
                                    "PageRank", "tarEnt", "Hide"), p = 0.5) {
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
    # Data is complete cases (outcome + all predictors)
    data <- data.frame(layer[[j]][userows, c(outcome, predictors)])
    chunkrows <- chunk(data, n = k)
    
    # Loop through all chunks
    for(i in 1:k) {
      # Training set is data minus chunk i
      #trainrows <- cases[-chunkrows[[i]]]
      train <- data[-chunkrows[[i]], ]
      test <- data[chunkrows[[i]], ]
      
      #lda.fit <- lda(data[, -1], grouping = data[, 1], subset = trainrows)
      lda.fit <- lda(formula = as.formula(fitForm), data = train)
      lda.pred <- predict(lda.fit, newdata = test)$posterior
      allprob[chunkrows[[i]], j] <- lda.pred[, 2]  # NOT the same as predict(...)$posterior[2]
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
  
  names(alldata) <- c("name", outcome, paste0("Week", seq(layer)))
  print(paste0("Fit: ", fitForm, ", complete N = ", dim(alldata)[1], ", k = ", k))
  return(alldata)
}

# Quadratic discriminant analysis (QDA) version
# Input: List of weekly data frames, outcome (pass/justpass), number of chunks 
#  to divide data into, predictors to use, probability threshold for outcome
# Output: Data frame of prediction vectors for that weekly aggregate network; 
#  each chunk in the vector is predicted using the other k-1 chunks
kfoldQDA <- function(layer, outcome = "pass", k = 5, 
                     predictors = c("gender", "cohort", "fci_pre", 
                                    "PageRank", "tarEnt", "Hide"), p = 0.5) {
  if (outcome == "pass" | outcome == "justpass") {
    choices <- c("0", "1")
  } else {
    stop("Not a valid outcome variable.")
  }
  # remove incomplete rows
  userows <- complete.cases(layer[[length(layer)]][, c(outcome,predictors)])  
  
  allprob <- matrix(nrow = sum(userows), ncol = length(layer))
  cases <- c(1:sum(userows))
  
  # Build fitting input string using predictor names
  fitStr <- paste(predictors, collapse = " + ")
  fitForm <- paste0(outcome, " ~ ", fitStr)
  
  # Loop through all weeks
  for(j in 1:length(layer)) {
    # Data is complete cases (outcome + all predictors)
    data <- data.frame(layer[[j]][userows, c(outcome, predictors)])
    chunkrows <- chunk(data, n = k)
    
    # Loop through all chunks
    for(i in 1:k) {
      # Training set is data minus chunk i
      train <- data[-chunkrows[[i]], ]
      test <- data[chunkrows[[i]], ]
      
      qda.fit <- qda(formula = as.formula(fitForm), data = train)
      qda.pred <- predict(qda.fit, newdata = test)$posterior
      allprob[chunkrows[[i]], j] <- qda.pred[, 2]
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

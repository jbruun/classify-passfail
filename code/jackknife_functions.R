# Jackknife prediction loops for different classifiers. There's a lot of 
# duplicate code, so they should probably be combined into one ur-function, 
# but that's a maybe-later thing. 

# Logistic regression version
# Input: List of weekly data frames, optional outcome (pass/justpass), optional 
#  subset of predictors to use
# Output: List of prediction vectors for that weekly aggregate network; each 
#  node in the vector is predicted using all the other nodes
jackPredLog <- function(layer, outcome = "pass", 
                        predictors = c("gender", "cohort", "fci_pre", 
                                       "PageRank", "tarEnt", "Hide")) {
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
  
    # Loop through all nodes
    for(i in 1:dim(data)[1]) {
      # Training set is data minus observation i
      train <- data[-i, ]
      glm.fit <- glm(fitForm, family = binomial, data = train)
      allprob[i, j] <- predict(glm.fit, newdata = data[i, ], type = "response")
    }
  }
  allpred <- allprob
  allpred[allprob < 0.5] <- choices[1]    # 0
  allpred[allprob >= 0.5] <- choices[2]   # 1
  
  # To return: node name, actual outcome, predicted outcome columns
  alldata <- data.frame(layer[[1]][userows, "name"], data[, outcome], 
                        as.data.frame(allpred))
  
  # Turn outcomes into factor, looping through each week's column
  for(i in seq_along(layer)) {
    alldata[, i+2] <- as.factor(alldata[, i+2])
  }
  
  names(alldata) <- c("name", outcome, paste0("Week", c(1:length(layer))))
  print(paste0("Fit: ", fitForm, ", complete N = ", dim(alldata)[1]))
  return(alldata)
}



# K nearest neighbors on pass/fail centrality by removing single observations.
# Last modified: 8/18/20 (function-ize success rate calculations, add no-FCI)
# 
# Status: Works. 

rm(list = ls())

library(igraph)
library(dplyr)
library(class)   # for knn
library(tidyr)
library(ggplot2)  # for plotting success rates

# Import pass/fail centrality data
#loadvars <- load("data/centPassFail.Rdata")  # old data
load("data/centrality_data_frames.Rdata")


## Run jackknife K nearest neighbors

# Turn long data frame into list of weekly frames
centPS <- dfPS %>% group_split(Week)
centCD <- dfCD %>% group_split(Week)
centICS <- dfICS %>% group_split(Week)

# Input: List of weekly data frames, optional outcome (pass/justpass), optional 
#  subset of predictors to use
# Output: List of prediction vectors for that weekly aggregate network; each node in the 
#  vector is predicted using all the other nodes
jackPred <- function(layer, nK = 1, outcome = "pass", 
                     predictors = c("gender", "cohort", "fci_pre", "PageRank", 
                                    "tarEnt", "Hide")) {
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
      # training set is data minus observation i
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
      # Logistic regression makes probabilities, which you translate into prediction; 
      # KNN does it all in one step
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

# Predict pass/fail
predPS <- jackPred(centPS, nK = 2)
predCD <- jackPred(centCD, nK = 2)
predICS <- jackPred(centICS, nK = 2)

# Predict just-pass/just-fail (2/0)
predJustPS <- jackPred(centPS, nK = 2, outcome = "justpass")
predJustCD <- jackPred(centCD, nK = 2, outcome = "justpass")
predJustICS <- jackPred(centICS, nK = 2, outcome = "justpass")

# Save pass/fail predictions
allnK <- c(predPS$nK, predCD$nK, predICS$nK, predJustPS$nK, 
           predJustCD$nK, predJustICS$nK)
if (max(allnK) == min(allnK)) {
  outfile <- paste0("data/jackknife_knn", as.character(predPS$nK), 
                    "_predictions.Rda")
} else {
  cat("Warning: Not all neighbor #s are the same\n")
  outfile <- "data/jackknife_knnX_predictions.Rda"
}
save(predPS, predCD, predICS, predJustPS, predJustCD, predJustICS,
     file = outfile)


## Collect success rates and compare with guessing everyone passes

# Success rate for each week (prediction == outcome)

# Input: Three prediction data frames, outcome variable (pass/justpass)
# Output: Success rate data frame with layer name, # of neighbors (nK),
#  # of nodes (N), weekly prediction success rates, and guessing success rate
getSucc <- function(pPS, pCD, pICS, outcome = "pass") {
  
  # Calculate one row of success rate table:
  # Pull weekly columns and compare them with outcome (pass/justpass)
  succRow <- function(p) sapply(p[[2]][, 3:9], 
                                function(x) mean(x == p[[2]][[outcome]]))
  
  compareSucc <- rbind(succRow(pPS), succRow(pCD), succRow(pICS))

  # Number of nodes: pull from prediction data frame
  Nnodes <- c(dim(pPS[[2]])[1], dim(pCD[[2]])[1], dim(pICS[[2]])[1])
  
  # How successful is it to just guess that everyone passes?
  Guessing  <- c(mean(pPS[[2]][[outcome]] == "1"), 
                 mean(pCD[[2]][[outcome]] == "1"),
                 mean(pICS[[2]][[outcome]] == "1"))
    
  # Package all these calculations in a data frame
  succRate <- data.frame(Layer = c("PS","CD","ICS"), 
                         nK = c(pPS[[1]], pCD[[1]], pICS[[1]]),
                         N = Nnodes, 
                         compareSucc, 
                         Guessing)
  return(succRate)
}

succRate <- getSucc(predPS, predCD, predICS)

write.csv(succRate,"succRate_knn.csv", row.names = FALSE)

# Success rate for predictions on the pass/fail boundary
succRateJust <- getSucc(predJustPS, predJustCD, predJustICS, "justpass")

write.csv(succRateJust,"succRateJust_knn.csv", row.names = FALSE)


## Plotting success rates
dflong <- succRate %>% 
  pivot_longer(Week1:Week7, names_to = "Week", values_to = "Rate")

plotlab <- paste0("Success rate: KNN, ", median(succRate$nK), " neighbor(s)")

ggplot(data = dflong, mapping = aes(x = Week, y = Rate)) + 
  geom_point(mapping = aes(color = Layer, shape = Layer)) + 
  geom_hline(aes(yintercept = Guessing)) + 
  ylim(0, 1) + 
  labs(title = plotlab)
  


## No-FCI version

# What if we ignore FCIpre?
predict_noFCI <- c("gender", "cohort", "PageRank", "tarEnt", "Hide")

# All pass/fail
predPSnoFCI <-jackPred(centPS, nK = 2, predictors = predict_noFCI)
predCDnoFCI <-jackPred(centCD, nK = 2, predictors = predict_noFCI)
predICSnoFCI <-jackPred(centICS, nK = 2, predictors = predict_noFCI)

# Just-pass/just-fail (2/0)
predJustPSnoFCI <- jackPred(centPS, nK = 2, outcome = "justpass", 
                            predictors = predict_noFCI)
predJustCDnoFCI <- jackPred(centCD, nK = 2, outcome = "justpass", 
                            predictors = predict_noFCI)
predJustICSnoFCI <- jackPred(centICS, nK = 2, outcome = "justpass", 
                             predictors = predict_noFCI)

# Save pass/fail predictions
allnKnoFCI <- c(predPSnoFCI$nK, predCDnoFCI$nK, predICSnoFCI$nK, 
                predJustPSnoFCI$nK, predJustCDnoFCI$nK, predJustICSnoFCI$nK)
if (max(allnKnoFCI) == min(allnKnoFCI)) {
  outfile <- paste0("data/jackknife_knn", as.character(predPS$nK), 
                    "_noFCI_predictions.Rda")
} else {
  cat("Warning: Not all neighbor #s are the same\n")
  outfile <- "data/jackknife_knnX_noFCI_predictions.Rda"
}
save(predPSnoFCI, predCDnoFCI, predICSnoFCI, predJustPSnoFCI, 
     predJustCDnoFCI, predJustICSnoFCI, file = outfile)


# Success rate tables

succRatenoFCI <- getSucc(predPSnoFCI, predCDnoFCI, predICSnoFCI, "pass")
succRateJustnoFCI <- getSucc(predJustPSnoFCI, predJustCDnoFCI, predJustICSnoFCI, 
                             "justpass")

write.csv(succRatenoFCI,"succRate_knn_noFCI.csv", row.names = FALSE)
write.csv(succRateJustnoFCI,"succRateJust_knn_noFCI.csv", row.names = FALSE)


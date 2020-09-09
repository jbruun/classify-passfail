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

source("code/jackknife_functions.R")

# Predict pass/fail
predPS <- jackPredKNN(centPS, nK = 2)
predCD <- jackPredKNN(centCD, nK = 2)
predICS <- jackPredKNN(centICS, nK = 2)

# Predict just-pass/just-fail (2/0)
predJustPS <- jackPredKNN(centPS, nK = 2, outcome = "justpass")
predJustCD <- jackPredKNN(centCD, nK = 2, outcome = "justpass")
predJustICS <- jackPredKNN(centICS, nK = 2, outcome = "justpass")

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
predPSnoFCI <-jackPredKNN(centPS, nK = 2, predictors = predict_noFCI)
predCDnoFCI <-jackPredKNN(centCD, nK = 2, predictors = predict_noFCI)
predICSnoFCI <-jackPredKNN(centICS, nK = 2, predictors = predict_noFCI)

# Just-pass/just-fail (2/0)
predJustPSnoFCI <- jackPredKNN(centPS, nK = 2, outcome = "justpass", 
                            predictors = predict_noFCI)
predJustCDnoFCI <- jackPredKNN(centCD, nK = 2, outcome = "justpass", 
                            predictors = predict_noFCI)
predJustICSnoFCI <- jackPredKNN(centICS, nK = 2, outcome = "justpass", 
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


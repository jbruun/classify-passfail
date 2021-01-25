# K nearest neighbors on pass/fail centrality with k-fold cross-validation.
# Last modified: 1/25/21 (fixed plotting code (column numbering issue))
# 
# Status: Works.

rm(list = ls())

library(igraph)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(class)   # for knn

# Import pass/fail centrality data
#loadvars <- load("data/centPassFail.Rdata")
load("data/centrality_data_frames.Rdata")


## Run k-fold linear discriminant analysis

# Turn long data frame into list of weekly frames
centPS <- dfPS %>% group_split(Week)
centCD <- dfCD %>% group_split(Week)
centICS <- dfICS %>% group_split(Week)

source("code/k-fold_functions.R")

# Predict pass/fail
kf <- 10
preds <- c("gender", "cohort", "fci_pre_c", "PageRank", "tarEnt", "Hide")

predPS <- kfoldKNN(centPS, k = kf, nK = 2, predictors = preds)
predCD <- kfoldKNN(centCD, k = kf, nK = 2, predictors = preds)
predICS <- kfoldKNN(centICS, k = kf, nK = 2, predictors = preds)

# Predict just-pass/just-fail (2/0)
predJustPS <- kfoldKNN(centPS, outcome = "justpass", k = kf, nK = 2, predictors = preds)
predJustCD <- kfoldKNN(centCD, outcome = "justpass", k = kf, nK = 2, predictors = preds)
predJustICS <- kfoldKNN(centICS, outcome = "justpass", k = kf, nK = 2, predictors = preds)

# Save pass/fail predictions
outfile <- paste0("data/kfold", as.character(kf), "_KNN_predictions.Rdata")
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

# Success rate for all predictions 
succRate <- getSucc(predPS, predCD, predICS)
SRfile <- paste0("results/succRate_knn_kfold", as.character(kf), ".csv")
write.csv(succRate, SRfile, row.names = FALSE)

# Success rate for predictions on the pass/fail boundary
succRateJust <- getSucc(predJustPS, predJustCD, predJustICS, "justpass")
SRfile <- paste0("results/succRateJust_knn_kfold", as.character(kf), ".csv")
write.csv(succRateJust, SRfile, row.names = FALSE)


## Plotting success rates 

# Change next three lines to toggle pass/justpass
df <- succRate
toplabel <- paste0("Pass outcome, k = ", as.character(kf))
plotfile <- paste0("plots/succRate_knn_kfold", as.character(kf), ".png")

longRate <- df %>% 
  select(Layer, Week1:Week7) %>% 
  gather(Week, SuccRate, -Layer) %>% 
  mutate(Week = parse_number(Week))
ggsucc <- ggplot(longRate, aes(x = Week, y = SuccRate, color = Layer))
plotcolors <- unique(ggplot_build(ggsucc)$data[[1]][,1])

p1 <- ggsucc + geom_line() + scale_y_continuous(limits = c(0.5, 1))
p1 + geom_hline(yintercept = df[1, "Guessing"], linetype = "dashed", color = "black") +
  ggtitle(toplabel)

ggsave(plotfile, width = 5, height = 4, units = "in", dpi = 150)


# Linear discriminant analysis on pass/fail centrality by removing single observations.
# Last modified: 1/25/21 (standardize CSV file names)
# 
# Status: Works.

rm(list = ls())

library(igraph)
library(dplyr)
library(MASS)

# Import pass/fail centrality data
#loadvars <- load("data/centPassFail.Rdata")
load("data/centrality_data_frames.Rdata")


## Run jackknife logistic regression 

# Turn long data frame into list of weekly frames
centPS <- dfPS %>% group_split(Week)
centCD <- dfCD %>% group_split(Week)
centICS <- dfICS %>% group_split(Week)

source("code/jackknife_functions.R")

# Predict pass/fail
preds <- c("gender", "cohort", "fci_pre_c", "PageRank", "tarEnt", "Hide")

predPS <- jackPredLDA(centPS, predictors = preds)
predCD <- jackPredLDA(centCD, predictors = preds)
predICS <- jackPredLDA(centICS, predictors = preds)

# Predict just-pass/just-fail (2/0)
predJustPS <- jackPredLDA(centPS, outcome = "justpass", predictors = preds)
predJustCD <- jackPredLDA(centCD, outcome = "justpass", predictors = preds)
predJustICS <- jackPredLDA(centICS, outcome = "justpass", predictors = preds)

# Save pass/fail predictions
save(predPS, predCD, predICS, predJustPS, predJustCD, predJustICS,
     file = "data/jackknife_lda_predictions.Rdata")


## Collect success rates and compare with guessing everyone passes

# Success rate for each week (prediction == outcome)
compareSucc <- rbind(sapply(predPS[, 3:9], function(x) mean(x == predPS$pass)),
                     sapply(predCD[, 3:9], function(x) mean(x == predCD$pass)),
                     sapply(predICS[, 3:9], function(x) mean(x == predICS$pass)))
succRate <- data.frame(Layer = c("PS","CD","ICS"), 
                       N = c(dim(predPS)[1], dim(predCD)[1], dim(predICS)[1]),
                       compareSucc, 
                       Guessing = c(mean(predPS$pass == "1"), mean(predCD$pass == "1"),
                                    mean(predICS$pass == "1")))

write.csv(succRate,"results/succRate_lda.csv", row.names = FALSE)

# Success rate for predictions on the pass/fail boundary
compareJust <- rbind(sapply(predJustPS[, 3:9], function(x) mean(x == predJustPS$justpass)),
                     sapply(predJustCD[, 3:9], function(x) mean(x == predJustCD$justpass)),
                     sapply(predJustICS[, 3:9], function(x) mean(x == predJustICS$justpass)))
succRateJust <- data.frame(Layer = c("PS", "CD", "ICS"),
                           N = c(dim(predJustPS)[1], dim(predJustCD)[1], dim(predJustICS)[1]),
                           compareJust,
                           Guessing = c(mean(predJustPS$justpass == "1"),
                                        mean(predJustCD$justpass == "1"),
                                        mean(predJustICS$justpass == "1")))

write.csv(succRateJust,"results/succRateJust_lda.csv", row.names = FALSE)

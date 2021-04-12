# Quadratic Discriminant Analysis on pass/fail centrality by removing single observations.
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

predPS <- jackPredQDA(centPS, predictors = preds)
predCD <- jackPredQDA(centCD, predictors = preds)
predICS <- jackPredQDA(centICS, predictors = preds)

# Predict just-pass/just-fail (2/0)
predJustPS <- jackPredQDA(centPS, outcome = "justpass", predictors = preds)
predJustCD <- jackPredQDA(centCD, outcome = "justpass", predictors = preds)
predJustICS <- jackPredQDA(centICS, outcome = "justpass", predictors = preds)

# Save pass/fail predictions
save(predPS, predCD, predICS, predJustPS, #predJustCD, 
     predJustICS,
     file = "data/jackknife_qda_predictions.Rdata")


## Collect success rates and compare with guessing everyone passes

# Success rate for each week (prediction == outcome)
compareSucc <- rbind(sapply(predPS[, 3:9], function(x) mean(x == predPS$pass)),
                     sapply(predCD[, 3:9], function(x) mean(x == predCD$pass)),
                     sapply(predICS[, 3:9], function(x) mean(x == predICS$pass)))
succRateQDA <- data.frame(Layer = c("PS","CD","ICS"), 
                       N = c(dim(predPS)[1], dim(predCD)[1], dim(predICS)[1]),
                       compareSucc, 
                       Guessing = c(mean(predPS$pass == "1"), mean(predCD$pass == "1"),
                                    mean(predICS$pass == "1")))

write.csv(succRateQDA,"results/succRate_qda.csv", row.names = FALSE)

# Success rate for predictions on the pass/fail boundary
compareJust <- rbind(sapply(predJustPS[, 3:9], function(x) mean(x == predJustPS$justpass)),
                     #sapply(predJustCD[, 3:9], function(x) mean(x == predJustCD$justpass)),
                     NA, 
                     sapply(predJustICS[, 3:9], function(x) mean(x == predJustICS$justpass)))
succRateJustQDA <- data.frame(Layer = c("PS", "CD", "ICS"),
                           N = c(dim(predJustPS)[1], #dim(predJustCD)[1], 
                                 NA, 
                                 dim(predJustICS)[1]),
                           compareJust,
                           Guessing = c(mean(predJustPS$justpass == "1"),
                                        #mean(predJustCD$justpass == "1"),
                                        NA, 
                                        mean(predJustICS$justpass == "1")))

write.csv(succRateJustQDA,"results/succRateJust_qda.csv", row.names = FALSE)



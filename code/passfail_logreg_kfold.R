# Logstic regression on pass/fail centrality with k-fold cross-validation.
# Last modified: 12/3/20 (run and save k=5 results, check plot)
# 
# Status: Works. 

rm(list = ls())

library(igraph)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

# Import pass/fail centrality data
#loadvars <- load("data/centPassFail.Rdata")
load("data/centrality_data_frames.Rdata")


## Run k-fold logistic regression 

# Turn long data frame into list of weekly frames
centPS <- dfPS %>% group_split(Week)
centCD <- dfCD %>% group_split(Week)
centICS <- dfICS %>% group_split(Week)

source("code/k-fold_functions.R")

# Predict pass/fail
predictors <- c("gender", "cohort", "fci_pre_c", "PageRank", "tarEnt", "Hide")

predPS <- kfoldLog(centPS, k = 5, predictors = predictors)
predCD <- kfoldLog(centCD, k = 5, predictors = predictors)
predICS <- kfoldLog(centICS, k = 5, predictors = predictors)

# Predict just-pass/just-fail (2/0)
predJustPS <- kfoldLog(centPS, outcome = "justpass", k = 5, 
                       predictors = predictors)
predJustCD <- kfoldLog(centCD, outcome = "justpass", k = 5, 
                       predictors = predictors)
predJustICS <- kfoldLog(centICS, outcome = "justpass", k = 5, 
                        predictors = predictors)

# Save pass/fail predictions
save(predPS, predCD, predICS, predJustPS, predJustCD, predJustICS,
     file = "data/kfold5_logistic_predictions.Rdata")


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

write.csv(succRate,"succRate_logReg_kfold5.csv", row.names = FALSE)

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

write.csv(succRateJust,"succRateJust_logReg_kfold5.csv", row.names = FALSE)



# Plot it up
longRate <- succRate[,c(1,3:9)] %>% 
  gather(Week, SuccRate, -Layer) %>% 
  mutate(Week = parse_number(Week))
ggsucc <- ggplot(longRate, aes(x = Week, y = SuccRate, color = Layer))
plotcolors <- unique(ggplot_build(ggsucc)$data[[1]][,1])

p1 <- ggsucc + geom_line() + scale_y_continuous(limits = c(0.5, 1))
p1 + geom_hline(yintercept = succRate[1, 10], linetype = "dashed", color = "black")  

#p1 + geom_hline(yintercept = succRate[1, 10], linetype = "dashed", color = plotcolors[1]) + 
#  geom_hline(yintercept = succRate[2, 10], linetype = "dashed", color = plotcolors[2]) +
#  geom_hline(yintercept = succRate[3, 10], linetype = "dashed", color = plotcolors[3])
#ggsave("../figures/succRate_jackknife.png",width=5,height=4,units="in",dpi=150)

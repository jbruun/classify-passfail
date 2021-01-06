# Linear discriminant analysis on pass/fail centrality with k-fold cross-validation.
# Last modified: 1/7/21 (re-ran after fixing bug in k-fold_functions.R)
# 
# Status: Works.

rm(list = ls())

library(igraph)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(MASS)

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
kf <- 5
preds <- c("gender", "cohort", "fci_pre_c", "PageRank", "tarEnt", "Hide")

predPS <- kfoldLDA(centPS, k = kf, predictors = preds)
predCD <- kfoldLDA(centCD, k = kf, predictors = preds)
predICS <- kfoldLDA(centICS, k = kf, predictors = preds)

# Predict just-pass/just-fail (2/0)
predJustPS <- kfoldLDA(centPS, outcome = "justpass", k = kf, predictors = preds)
predJustCD <- kfoldLDA(centCD, outcome = "justpass", k = kf, predictors = preds)
predJustICS <- kfoldLDA(centICS, outcome = "justpass", k = kf, predictors = preds)

# Save pass/fail predictions
outfile <- paste0("data/kfold", as.character(kf), "_LDA_predictions.Rdata")
save(predPS, predCD, predICS, predJustPS, predJustCD, predJustICS,
     file = outfile)


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

SRfile <- paste0("succRate_lda_kfold", as.character(kf), ".csv")
write.csv(succRate, SRfile, row.names = FALSE)

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

SRfile <- paste0("succRateJust_lda_kfold", as.character(kf), ".csv")
write.csv(succRateJust, SRfile, row.names = FALSE)


# Plot it up
# Can change next three rows for pass/justpass
df <- succRateJust
toplabel <- paste0("JustPass outcome, k = ", as.character(kf))
plotfile <- paste0("figures/succRateJust_lda_kfold", as.character(kf), ".png")

longRate <- df[,c(1,3:9)] %>% 
  gather(Week, SuccRate, -Layer) %>% 
  mutate(Week = parse_number(Week))
ggsucc <- ggplot(longRate, aes(x = Week, y = SuccRate, color = Layer))
plotcolors <- unique(ggplot_build(ggsucc)$data[[1]][,1])

p1 <- ggsucc + geom_line() + scale_y_continuous(limits = c(0.5, 1))
p1 + geom_hline(yintercept = df[1, 10], linetype = "dashed", color = "black") +
  ggtitle(toplabel)

ggsave(plotfile, width = 5, height = 4, units = "in", dpi = 150)

#p1 + geom_hline(yintercept=succRate[1,10],linetype="dashed",color=plotcolors[1]) + 
#  geom_hline(yintercept=succRate[2,10],linetype="dashed",color=plotcolors[2]) +
#  geom_hline(yintercept=succRate[3,10],linetype="dashed",color=plotcolors[3])
#ggsave("../figures/succRate_jackknife.png",width=5,height=4,units="in",dpi=150)

# Logstic regression on pass/fail centrality with k-fold cross-validation.
# Last modified: 11/6/20 (forked from passfail_logreg_jackknife.R)
# 
# Status: 

rm(list = ls())

library(igraph)
library(dplyr)

# Import pass/fail centrality data
#loadvars <- load("data/centPassFail.Rdata")
load("data/centrality_data_frames.Rdata")


## Run jackknife logistic regression 

# Turn long data frame into list of weekly frames
centPS <- dfPS %>% group_split(Week)
centCD <- dfCD %>% group_split(Week)
centICS <- dfICS %>% group_split(Week)

source("code/k-fold_functions.R")

# Predict pass/fail
predPS <- kfoldLog(centPS, k = 5)
predCD <- kfoldLog(centCD, k = 5)
predICS <- kfoldLog(centICS, k = 5)

# Predict just-pass/just-fail (2/0)
predJustPS <- kfoldLog(centPS, outcome = "justpass", k = 5)
predJustCD <- kfoldLog(centCD, outcome = "justpass", k = 5)
predJustICS <- kfoldLog(centICS, outcome = "justpass", k = 5)

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

write.csv(succRate,"succRate.csv", row.names = FALSE)

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

write.csv(succRateJust,"succRateJust.csv", row.names = FALSE)


## OLD, NOT UPDATED YET 

# What if we ignore FCIpre?
predPSnoFCI <-jackPredLog(centPS,predictors=c("Gender","Section","PageRank","tarEnt","Hide"))
predCDnoFCI <-jackPredLog(centCD,predictors=c("Gender","Section","PageRank","tarEnt","Hide"))
predICSnoFCI <-jackPredLog(centICS,predictors=c("Gender","Section","PageRank","tarEnt","Hide"))
succRatenoFCI <- rbind(sapply(predPSnoFCI[,3:9],function(x) mean(x==predPSnoFCI$Pass)),
                       sapply(predCDnoFCI[,3:9],function(x) mean(x==predCDnoFCI$Pass)),
                       sapply(predICSnoFCI[,3:9],function(x) mean(x==predICSnoFCI$Pass)))
succRatenoFCI <- data.frame(Layer=c("PS","CD","ICS"),N=c(dim(predPSnoFCI)[1],dim(predCDnoFCI)[1],dim(predICSnoFCI)[1]),
                            succRatenoFCI,
                            Guessing=c(mean(predPSnoFCI$Pass=="Pass"),mean(predCDnoFCI$Pass=="Pass"),
                                       mean(predICSnoFCI$Pass=="Pass")))

predPSnoFCI02 <-jackPredLog(centPS,outcome="JustPass",predictors=c("Gender","Section","PageRank","tarEnt","Hide"))
predCDnoFCI02 <-jackPredLog(centCD,outcome="JustPass",predictors=c("Gender","Section","PageRank","tarEnt","Hide"))
predICSnoFCI02 <-jackPredLog(centICS,outcome="JustPass",predictors=c("Gender","Section","PageRank","tarEnt","Hide"))
succRatenoFCI02 <- rbind(sapply(predPSnoFCI02[,3:9],function(x) mean(x==predPSnoFCI02$JustPass)),
                       sapply(predCDnoFCI02[,3:9],function(x) mean(x==predCDnoFCI02$JustPass)),
                       sapply(predICSnoFCI02[,3:9],function(x) mean(x==predICSnoFCI02$JustPass)))
succRatenoFCI02 <- data.frame(Layer=c("PS","CD","ICS"),N=c(dim(predPSnoFCI02)[1],dim(predCDnoFCI02)[1],dim(predICSnoFCI02)[1]),
                              succRatenoFCI02,
                              Guessing=c(mean(predPSnoFCI02$JustPass=="Pass2"),mean(predCDnoFCI02$JustPass=="Pass2"),
                                       mean(predICSnoFCI02$JustPass=="Pass2")))

write.csv(succRatenoFCI,"../succRatenoFCI.csv",row.names=FALSE)
write.csv(succRatenoFCI02,"../succRatenoFCI02.csv",row.names=FALSE)

# Save no-FCI weekly predictions
save(predPSnoFCI,predCDnoFCI,predICSnoFCI,predPSnoFCI02,predCDnoFCI02,predICSnoFCI02,
     file="../data/jackknife_predictions_noFCI.Rdata")



# Plot it up
longRate <- succRate[,c(1,3:9)] %>% gather(Week,SuccRate,-Layer) %>% mutate(Week=parse_number(Week))
ggsucc <- ggplot(longRate,aes(x=Week,y=SuccRate,color=Layer))
plotcolors <- unique(ggplot_build(ggsucc)$data[[1]][,1])
p1 <- ggsucc + geom_line() + scale_y_continuous(limits=c(0.5,1))
p1 + geom_hline(yintercept=succRate[1,10],linetype="dashed",color=plotcolors[1]) + 
  geom_hline(yintercept=succRate[2,10],linetype="dashed",color=plotcolors[2]) +
  geom_hline(yintercept=succRate[3,10],linetype="dashed",color=plotcolors[3])
ggsave("../figures/succRate_jackknife.png",width=5,height=4,units="in",dpi=150)

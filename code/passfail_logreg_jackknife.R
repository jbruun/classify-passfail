# Logstic regression on pass/fail centrality by removing single observations.
# Last modified:  8/22/18 (update jackPred to record Pass2/Fail0 when checking that outcome, check no-FCI results)
# 
# Status: Works.

rm(list = ls())

library(igraph)
library(dplyr)

# Import pass/fail centrality data
#loadvars <- load("data/centPassFail.Rdata")
load("data/centrality_data_frames.Rdata")


## Run jackknife logistic regression 

# Turn long data frame into list of weekly frames
centPS <- dfPS %>% group_split(Week)
centCD <- dfPS %>% group_split(Week)
centICS <- dfPS %>% group_split(Week)

# Input: List of weekly data frames, optional outcome (Pass/JustPass), optional 
#  subset of predictors to use
# Output: List of prediction vectors for that weekly aggregate network; each node in the 
#  vector is predicted using all the other nodes
jackPred <- function(layer, outcome = "Pass", 
                     predictors = c("Gender", "Section", "FCIPre", "PageRank", 
                                    "tarEnt", "Hide")) {
  if (outcome == "Pass") {
    choices <- c("Fail", "Pass")
  } else if (outcome == "JustPass" | outcome == "Pass2") {
    choices <- c("Fail0", "Pass2")
  } else {
    stop("Not a valid outcome variable.")
  }
  # remove incomplete rows
  userows <- complete.cases(layer[[length(layer)]][, c(outcome,predictors)])  

  allprob <- matrix(nrow = sum(userows), ncol = length(layer))
  fitStr <- paste(predictors, collapse = "+")
  fitForm <- paste0(outcome, "~", fitStr)
  for(j in 1:length(layer)) {
    # data is complete cases
    data <- layer[[j]][userows, c(outcome, predictors)]
    # Loop through all nodes
    for(i in 1:dim(data)[1]) {
      # training set is data minus observation i
      train <- data[-i, ]
      glm.fit <- glm(fitForm, family = binomial, data = train)
      allprob[i, j] <- predict(glm.fit, newdata = data[i, ], type = "response")
    }
  }
  allpred <- allprob
  allpred[allprob < 0.5] <- choices[1] #"Fail"
  allpred[allprob >= 0.5] <- choices[2] #"Pass"
  allpred <- data.frame(layer[[1]][userows, "id"], data[, outcome], as.data.frame(allpred))
  names(allpred) <- c("id", outcome, paste0("Week", c(1:length(layer))))
  print(paste0("Fit: ", fitForm, ", complete N = ", dim(allpred)[1]))
  return(allpred)
}

# Predict pass/fail
predPS <- jackPred(centPS)
predCD <- jackPred(centCD)
predICS <- jackPred(centICS)

# Predict just-pass/just-fail (2/0)
predJustPS <- jackPred(centPS, outcome = "JustPass")
predJustCD <- jackPred(centCD, outcome = "JustPass")
predJustICS <- jackPred(centICS, outcome = "JustPass")

# Save weekly predictions
save(predPS, predCD, predICS, predJustPS, predJustCD, predJustICS,
     file = "data/jackknife_predictions.Rdata")


## Collect success rates and compare with guessing everyone passes

# Success rate for each week (prediction == outcome)
succRate <- rbind(sapply(predPS[, 3:9], function(x) mean(x == predPS$Pass)),
                  sapply(predCD[, 3:9], function(x) mean(x == predCD$Pass)),
                  sapply(predICS[, 3:9], function(x) mean(x == predICS$Pass)))
succRate <- data.frame(Layer = c("PS","CD","ICS"), 
                       N = c(dim(predPS)[1], dim(predCD)[1], dim(predICS)[1]),
                       succRate, 
                       Guessing = c(mean(predPS$Pass == "Pass"), mean(predCD$Pass == "Pass"),
                                    mean(predICS$Pass == "Pass")))

write.csv(succRate,"succRate.csv", row.names = FALSE)

# Success rate for predictions on the pass/fail boundary
succRate02 <- rbind(sapply(predJustPS[, 3:9], function(x) mean(x == predJustPS$JustPass)),
                    sapply(predJustCD[, 3:9], function(x) mean(x == predJustCD$JustPass)),
                    sapply(predJustICS[, 3:9], function(x) mean(x == predJustICS$JustPass)))
succRate02 <- data.frame(Layer = c("PS", "CD", "ICS"),
                         N = c(dim(predJustPS)[1], dim(predJustCD)[1], dim(predJustICS)[1]),
                         succRate02,
                         Guessing = c(mean(predJustPS$JustPass == "Pass2"),
                                      mean(predJustCD$JustPass == "Pass2"),
                                      mean(predJustICS$JustPass == "Pass2")))

write.csv(succRate02,"succRate02.csv", row.names = FALSE)


## OLD, NOT UPDATED YET 

# What if we ignore FCIpre?
predPSnoFCI <-jackPred(centPS,predictors=c("Gender","Section","PageRank","tarEnt","Hide"))
predCDnoFCI <-jackPred(centCD,predictors=c("Gender","Section","PageRank","tarEnt","Hide"))
predICSnoFCI <-jackPred(centICS,predictors=c("Gender","Section","PageRank","tarEnt","Hide"))
succRatenoFCI <- rbind(sapply(predPSnoFCI[,3:9],function(x) mean(x==predPSnoFCI$Pass)),
                       sapply(predCDnoFCI[,3:9],function(x) mean(x==predCDnoFCI$Pass)),
                       sapply(predICSnoFCI[,3:9],function(x) mean(x==predICSnoFCI$Pass)))
succRatenoFCI <- data.frame(Layer=c("PS","CD","ICS"),N=c(dim(predPSnoFCI)[1],dim(predCDnoFCI)[1],dim(predICSnoFCI)[1]),
                            succRatenoFCI,
                            Guessing=c(mean(predPSnoFCI$Pass=="Pass"),mean(predCDnoFCI$Pass=="Pass"),
                                       mean(predICSnoFCI$Pass=="Pass")))

predPSnoFCI02 <-jackPred(centPS,outcome="JustPass",predictors=c("Gender","Section","PageRank","tarEnt","Hide"))
predCDnoFCI02 <-jackPred(centCD,outcome="JustPass",predictors=c("Gender","Section","PageRank","tarEnt","Hide"))
predICSnoFCI02 <-jackPred(centICS,outcome="JustPass",predictors=c("Gender","Section","PageRank","tarEnt","Hide"))
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


## Old code

# # Not sure I'm still using this...
# aggPS <- centPS[[7]][complete.cases(centPS[[7]][,c(2:4,7:10)]),]
# aggCD <- centCD[[7]]
# aggICS <- centICS[[7]]
# 
# # Toy run: Use entire last week of PS data to predict itself (ISLR 4.6.2)
# glm.PS <- glm(Pass~PageRank+tarEnt+Hide+Gender+Section+FCIPre,family=binomial,data=aggPS)
# PS.probs <- predict(glm.PS,type="response")
# predN <- sum(complete.cases(aggPS[,c(2:4,7:10)]))
# PS.pred <- rep("Fail",predN)
# PS.pred[PS.probs>0.5] <- "Pass"
# table(PS.pred,aggPS$Pass)
# mean(PS.pred==aggPS$Pass)
# mean(aggPS$Pass=="Pass")
# 
# # Remove Section and redo
# glm.PS2 <- glm(Pass~PageRank+FCIPre,family=binomial,data=aggPS)
# PS.probs2 <- predict(glm.PS2,type="response")
# PS.pred2 <- rep("Fail",predN)
# PS.pred2[PS.probs2>0.5] <- "Pass"
# table(PS.pred2,aggPS$Pass)
# mean(PS.pred2==aggPS$Pass)
# mean(aggPS$Pass=="Pass")
# 

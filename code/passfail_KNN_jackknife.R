# K nearest neighbors on pass/fail centrality by removing single observations.
# Last modified: 6/24/20 (created)
# 
# Status: 

rm(list = ls())

library(igraph)
library(dplyr)
library(class)   # for knn

# Import pass/fail centrality data
#loadvars <- load("data/centPassFail.Rdata")
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
jackPred <- function(layer, outcome = "pass", 
                     predictors = c("gender", "cohort", "fci_pre", "PageRank", 
                                    "tarEnt", "Hide")) {
  if (outcome == "pass" | outcome == "justpass") {
    choices <- c("0", "1")
  } else {
    stop("Not a valid outcome variable.")
  }
  
  # Input data (complete cases) and empty frame to store predictions 
  Nlayer <- length(layer)
  userows <- complete.cases(layer[[Nlayer]][, c(outcome,predictors)])  
  allprob <- matrix(nrow = sum(userows), ncol = Nlayer)
  
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
      trOutcome <- data[-i, outcome]
      teOutcome <- data[i, outcome]
      
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
  allpred <- data.frame(layer[[1]][userows, "id"],  # node names
                        data[, outcome],            # real outcome
                        as.data.frame(allpred))     # predicted outcome
  names(allpred) <- c("id", outcome, paste0("Week", c(1:length(layer))))
  
  # Print info string and return predictions
  print(paste0("Fit: ", fitForm, ", #neighbors = ", nK, ", complete N = ", dim(allpred)[1]))
  return(allpred)
}

## NOT TESTED YET

# Predict pass/fail
predPS <- jackPred(centPS)
predCD <- jackPred(centCD)
predICS <- jackPred(centICS)

# Predict just-pass/just-fail (2/0)
predJustPS <- jackPred(centPS, outcome = "justpass")
predJustCD <- jackPred(centCD, outcome = "justpass")
predJustICS <- jackPred(centICS, outcome = "justpass")

# Save pass/fail predictions
save(predPS, predCD, predICS, predJustPS, predJustCD, predJustICS,
     file = "data/jackknife_logistic_predictions.Rdata")


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

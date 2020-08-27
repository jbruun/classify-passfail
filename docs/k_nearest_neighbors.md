---
title: "K nearest neighbors"
author: "Jesper Bruun & Adrienne Traxler"
date: "8/18/2020--"
output: 
  html_document: 
    keep_md: yes
---



Goal for this document is to use K nearest neighbors to predict passing and just-passing in the (single-layer) PS, CD, and ICS weekly networks. Calculate success rates for each week and save.

**Update 8/18:** Starting this file to document calculations I worked out in `passfail_KNN_jackknife.R`. 



## Import data

At this point, the following Rmd files have already been run: `loadAllNetworks`, `calculatePR_TE_H`, and `make_node_data_frames`. Importing the results of that gives me three long data frames. Each has all the node info and centrality values, by week, for a single layer. 


```r
(load("../data/centrality_data_frames.Rdata"))
```

```
## [1] "dfPS"      "dfCD"      "dfICS"     "nPass"     "nJustPass"
```


## Jackknife K nearest neighbors

Because of the data set size, we'll use a "jackknife" approach, where each observation is removed, the remaining observations are used to predict the missing one, and then we repeat until we've checked them all. 

Turn the long data frames into lists of single (weekly) data frames: 


```r
centPS <- dfPS %>% group_split(Week)
centCD <- dfCD %>% group_split(Week)
centICS <- dfICS %>% group_split(Week)

centPS[[1]]
```

```
## # A tibble: 166 x 17
##    Week  name  grade gender   age cohort   sog fci_pre fci_pre_0 fci_pre_s
##    <fct> <chr> <int>  <int> <int>  <int> <dbl>   <dbl>     <dbl>     <dbl>
##  1 1     Pers~     2      1    19      3     6      17        17        17
##  2 1     Pers~    -3      1    20     10    NA      NA         0         6
##  3 1     Pers~     0      1    22     10     9      NA         0        15
##  4 1     Pers~     4      1    19      6    20      28        28        28
##  5 1     Pers~     2      1    27      1     7      26        26        26
##  6 1     Pers~     4      1    20      2     0      18        18        18
##  7 1     Pers~     0      1    19     10    11      NA         0        15
##  8 1     Pers~     7      1    20      2    NA      15        15        15
##  9 1     Pers~    12      1   100      4    NA      26        26        26
## 10 1     Pers~     7      1    20      3    11      13        13        13
## # ... with 156 more rows, and 7 more variables: fci_pre_c <dbl>, pass <fct>,
## #   justpass <fct>, Week.1 <int>, PageRank <dbl>, tarEnt <dbl>, Hide <dbl>
```

### Jackknife loop

Next, the function that actually does the calculations. 

* Input: A list of weekly data frames, optional outcome (pass/justpass, defaults to "pass"), and an optional subset of predictors to use (defaults gender, cohort, FCI pre, and centrality)
* Output: A list with two items. The first is the number of neighbors used (`nK`); the second is a data frame with node names, pass/fail outcome, and prediction vectors for that weekly aggregate network. Each node is predicted using all the other nodes in its week.


```r
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
```

It takes a few seconds to run the loop, so I'll import the results rather than executing it here. I did this for each centrality data frame (PS, CD, and ICS), using demographics (Gender/Section), FCI pretest score, and all three centrality measures as predictors. The function predicts passing if P > 0.5. 


```r
# Predict pass/fail
#predPS <- jackPred(centPS, nK = 2)
#predCD <- jackPred(centCD, nK = 2)
#predICS <- jackPred(centICS, nK = 2)

# Predict just-pass/just-fail (2/0)
#predJustPS <- jackPred(centPS, nK = 2, outcome = "justpass")
#predJustCD <- jackPred(centCD, nK = 2, outcome = "justpass")
#predJustICS <- jackPred(centICS, nK = 2, outcome = "justpass")

load("../data/jackknife_knn1_predictions.Rda")
```


### Success rates

We're interested in a couple of success rate comparisons: how predictions compared with reality for each week, and how well it would have worked to just guess that everyone passed.

I got tired of repasting code lines (which got clunkier with the list return format in KNN), so I wrote a function to compile this. 

* Input: Three prediction data frames, outcome variable (pass/justpass)
* Output: Success rate data frame with layer name, # of neighbors (`nK`), # of nodes (`N`), weekly prediction success rates, guessing success rate


```r
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
succRate
```

```
##   Layer nK   N     Week1     Week2     Week3     Week4     Week5     Week6
## 1    PS  1 142 0.8028169 0.7042254 0.7605634 0.7394366 0.7253521 0.7183099
## 2    CD  1 142 0.8239437 0.7394366 0.7676056 0.7816901 0.7676056 0.7253521
## 3   ICS  1 142 0.8450704 0.8028169 0.7887324 0.7746479 0.7887324 0.8098592
##       Week7  Guessing
## 1 0.7042254 0.8309859
## 2 0.7253521 0.8309859
## 3 0.8028169 0.8309859
```

Now, the same calculation for only the people who were on the pass/fail boundary (2/0):

```r
succRateJust <- getSucc(predJustPS, predJustCD, predJustICS, "justpass")
succRateJust
```

```
##   Layer nK  N     Week1     Week2     Week3     Week4     Week5     Week6
## 1    PS  1 55 0.5272727 0.4363636 0.5454545 0.5454545 0.4545455 0.5272727
## 2    CD  1 55 0.6727273 0.4545455 0.5272727 0.5454545 0.5454545 0.5636364
## 3   ICS  1 55 0.5272727 0.5454545 0.6000000 0.7090909 0.6000000 0.6363636
##       Week7  Guessing
## 1 0.5272727 0.6181818
## 2 0.6000000 0.6181818
## 3 0.6000000 0.6181818
```

**BELOW NOT UPDATED**

## Summary of results

The success rate tables have the punchline here. If the success rate for a given week and layer is higher than the value in the last column (the "assume everyone passes" default classifier), that's good news. 

Doing a quick boolean comparison,

```r
succRate[, c(3:9)] > succRate[1, 10]
```

```
##         N Week1 Week2 Week3 Week4 Week5 Week6
## [1,] TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE
## [2,] TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
## [3,] TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
```

```r
rowSums(succRate[, c(3:9)] > succRate[1, 10])
```

```
## [1] 6 7 7
```

In many weeks, the logistic regression success rate beats the default, especially on the PS and ICS layers. 


```r
(succRate[, c(3:9)] - succRate[1, 10]) * 100
```

```
##          N     Week1    Week2    Week3    Week4    Week5     Week6
## 1 14129.58  9.859155 0.000000 5.633803 3.521127 2.112676  1.408451
## 2 14129.58 11.971831 3.521127 6.338028 7.746479 6.338028  2.112676
## 3 14129.58 14.084507 9.859155 8.450704 7.042254 8.450704 10.563380
```

If you look at the amount by which it wins, though, it's not too exciting---always less than 3% better, and generally less than 2%. 

For grades on the boundary, 


```r
succRateJust[, c(3:9)] > succRateJust[1, 10]
```

```
##         N Week1 Week2 Week3 Week4 Week5 Week6
## [1,] TRUE FALSE FALSE  TRUE  TRUE FALSE FALSE
## [2,] TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE
## [3,] TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
```

```r
rowSums(succRateJust[, c(3:9)] > succRateJust[1, 10])
```

```
## [1] 3 5 6
```

```r
(succRateJust[, c(3:9)] - succRateJust[1, 10]) * 100
```

```
##          N    Week1     Week2    Week3     Week4     Week5     Week6
## 1 5447.273  0.00000 -9.090909 1.818182  1.818182 -7.272727  0.000000
## 2 5447.273 14.54545 -7.272727 0.000000  1.818182  1.818182  3.636364
## 3 5447.273  0.00000  1.818182 7.272727 18.181818  7.272727 10.909091
```

Here, logistic regression succeeds less often (and never for CD), though sometimes by higher percentages when it does. Also interesting (if disappointing) is that the logistic regression classifier isn't really getting better as more weekly data accumulates. 

The overall verdict is that logistic regression often beats the default classifier (at least for PS and ICS), but not in an obviously time-dependent way as the weeks progress, and not by amounts to get excited about. 

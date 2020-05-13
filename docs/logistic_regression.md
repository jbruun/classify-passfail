---
title: "Logistic regression"
author: "Jesper Bruun & Adrienne Traxler"
date: "3/30/2020--"
output: 
  html_document: 
    keep_md: yes
---



Goal for this document is to run logistic regression for passing and failing in the (single-layer) PS, CD, and ICS weekly networks.

**Update 5/8:** Logistic regression results calculated and imported; success rates calculated. 


```
## 
## Attaching package: 'igraph'
```

```
## The following objects are masked from 'package:stats':
## 
##     decompose, spectrum
```

```
## The following object is masked from 'package:base':
## 
##     union
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:igraph':
## 
##     as_data_frame, groups, union
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

## Import data

At this point, the following Rmd files have already been run: `loadAllNetworks`, `calculatePR_TE_H`, and `make_node_data_frames`. Importing the results of that gives me three long data frames. Each has all the node info and centrality values, by week, for a single layer. 


```r
(load("../data/centrality_data_frames.Rdata"))
```

```
## [1] "dfPS"      "dfCD"      "dfICS"     "nPass"     "nJustPass"
```


## Logistic regression

As I recall, we had decided to skip the half-training, half-test approach because the data set isn't large enough to support that. If we stick with the "jackknife" approach, I can port in my old code.

The first thing I need is a list of data frames, rather than a single long data frame. Tidyverse to the rescue:


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

```r
centPS[[2]]
```

```
## # A tibble: 166 x 17
##    Week  name  grade gender   age cohort   sog fci_pre fci_pre_0 fci_pre_s
##    <fct> <chr> <int>  <int> <int>  <int> <dbl>   <dbl>     <dbl>     <dbl>
##  1 2     Pers~     2      1    19      3     6      17        17        17
##  2 2     Pers~    -3      1    20     10    NA      NA         0         6
##  3 2     Pers~     0      1    22     10     9      NA         0        15
##  4 2     Pers~     4      1    19      6    20      28        28        28
##  5 2     Pers~     2      1    27      1     7      26        26        26
##  6 2     Pers~     4      1    20      2     0      18        18        18
##  7 2     Pers~     0      1    19     10    11      NA         0        15
##  8 2     Pers~     7      1    20      2    NA      15        15        15
##  9 2     Pers~    12      1   100      4    NA      26        26        26
## 10 2     Pers~     7      1    20      3    11      13        13        13
## # ... with 156 more rows, and 7 more variables: fci_pre_c <dbl>, pass <fct>,
## #   justpass <fct>, Week.1 <int>, PageRank <dbl>, tarEnt <dbl>, Hide <dbl>
```

### Jackknife logistic regression loop

Next, the function that actually does the calculations. 

* Input: A list of weekly data frames, optional outcome (pass/justpass, defaults to "pass"), and an optional subset of predictors to use (defaults gender, cohort, FCI pre, and centrality)
* Output: A data frame with node names, pass/fail outcome, and prediction vectors for that weekly aggregate network. Each node is predicted using all the other nodes in its week.


```r
jackPred <- function(layer, outcome = "pass", 
                     predictors = c("gender", "cohort", "fci_pre", "PageRank", 
                                    "tarEnt", "Hide")) {
  if (outcome == "pass" | outcome == "justpass") {
    choices <- c("0", "1")
  } else {
    stop("Not a valid outcome variable.")
  }
  # remove incomplete rows
  userows <- complete.cases(layer[[length(layer)]][, c(outcome,predictors)])  

  allprob <- matrix(nrow = sum(userows), ncol = length(layer))
  fitStr <- paste(predictors, collapse = " + ")
  fitForm <- paste0(outcome, " ~ ", fitStr)
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
  allpred[allprob < 0.5] <- choices[1]    # 0
  allpred[allprob >= 0.5] <- choices[2]   # 1
  
  # To return: node name, actual outcome, predicted outcome columns
  alldata <- data.frame(layer[[1]][userows, "name"], data[, outcome], as.data.frame(allpred))
  
  # Turn outcomes into factor
  for(i in seq_along(layer)) {
    alldata[, i+2] <- as.factor(alldata[, i+2])
  }
  
  names(alldata) <- c("name", outcome, paste0("Week", c(1:length(layer))))
  print(paste0("Fit: ", fitForm, ", complete N = ", dim(alldata)[1]))
  return(alldata)
}
```

It takes a few seconds to run the loop, so I'll import the results rather than executing it here. I did this for each centrality data frame (PS, CD, and ICS), using demographics (Gender/Section), FCI pretest score, and all three centrality measures as predictors. The function predicts passing if P > 0.5. 


```r
# Predict pass/fail
#predPS <- jackPred(centPS)
#predCD <- jackPred(centCD)
#predICS <- jackPred(centICS)

# Predict just-pass/just-fail (2/0)
#predJustPS <- jackPred(centPS, outcome = "justpass")
#predJustCD <- jackPred(centCD, outcome = "justpass")
#predJustICS <- jackPred(centICS, outcome = "justpass")

load("../data/jackknife_logistic_predictions.Rdata")
```


### Success rates

We're interested in a couple of success rate comparisons: how predictions compared with reality for each week, and how well it would have worked to just guess that everyone passed.

First, the success rate for predictions based on each week's accumulated centrality data, for all students (who have complete records):

```r
compareSucc <- rbind(sapply(predPS[, 3:9], function(x) mean(x == predPS$pass)),
                     sapply(predCD[, 3:9], function(x) mean(x == predCD$pass)),
                     sapply(predICS[, 3:9], function(x) mean(x == predICS$pass)))
succRate <- data.frame(Layer = c("PS","CD","ICS"), 
                       N = c(dim(predPS)[1], dim(predCD)[1], dim(predICS)[1]),
                       compareSucc, 
                       Guessing = c(mean(predPS$pass == "1"), mean(predCD$pass == "1"),
                                    mean(predICS$pass == "1")))
succRate
```

```
##   Layer   N     Week1     Week2     Week3     Week4     Week5     Week6
## 1    PS 142 0.8098592 0.8591549 0.8450704 0.8380282 0.8521127 0.8450704
## 2    CD 142 0.8028169 0.8028169 0.8309859 0.8239437 0.8450704 0.8380282
## 3   ICS 142 0.8239437 0.8309859 0.8591549 0.8450704 0.8450704 0.8380282
##       Week7  Guessing
## 1 0.8450704 0.8309859
## 2 0.8169014 0.8309859
## 3 0.8521127 0.8309859
```

Now, the same calculation for only the people who were on the pass/fail boundary (2/0):

```r
compareJust <- rbind(sapply(predJustPS[, 3:9], function(x) mean(x == predJustPS$justpass)),
                     sapply(predJustCD[, 3:9], function(x) mean(x == predJustCD$justpass)),
                     sapply(predJustICS[, 3:9], function(x) mean(x == predJustICS$justpass)))
succRateJust <- data.frame(Layer = c("PS", "CD", "ICS"),
                           N = c(dim(predJustPS)[1], dim(predJustCD)[1], dim(predJustICS)[1]),
                           compareJust,
                           Guessing = c(mean(predJustPS$justpass == "1"),
                                        mean(predJustCD$justpass == "1"),
                                        mean(predJustICS$justpass == "1")))
succRateJust
```

```
##   Layer  N     Week1     Week2     Week3     Week4     Week5     Week6
## 1    PS 55 0.7090909 0.6000000 0.6545455 0.6363636 0.6545455 0.6181818
## 2    CD 55 0.5636364 0.6000000 0.5272727 0.5818182 0.6000000 0.6181818
## 3   ICS 55 0.6181818 0.6545455 0.6181818 0.6363636 0.6363636 0.6181818
##       Week7  Guessing
## 1 0.6181818 0.6181818
## 2 0.6181818 0.6181818
## 3 0.6000000 0.6181818
```


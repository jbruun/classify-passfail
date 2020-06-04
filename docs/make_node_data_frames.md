---
title: "Logistic regression"
author: "Jesper Bruun & Adrienne Traxler"
date: "5/8/2020"
output: 
  html_document: 
    keep_md: yes
---



Read in Jesper's canonical network data and turn into node/centrality data frames. 

**Update 5/8:** 


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

At this point, `loadAllNetworks` and `calculatePR_TE_H` have already been run. Importing the results of that,


```r
(load("../data/PRTEH.RData"))
```

```
##  [1] "nominator"         "singleICS_S"       "accCD_S"          
##  [4] "graphsICS"         "files"             "weeksPS"          
##  [7] "FCI_PRE_S"         "accCD"             ".Random.seed"     
## [10] "weeksCD"           "accPS_TE"          "singlePS_TE"      
## [13] "singleCD_TE"       "singleICS_H"       "singleCD_H"       
## [16] "singlePS_H"        "accCD_H"           "biggraph"         
## [19] "sInfMatrix"        "weightedAdjacency" "targetEntropy"    
## [22] "applyAttr"         "accCD_PR"          "accICS_PR"        
## [25] "accICS_S"          "pass"              "paths"            
## [28] "FCI_PRE_C"         "JUSTPASS"          "ccWeekNets"       
## [31] "accPS_S"           "FCI_PRE"           "SOG"              
## [34] "accPS_H"           "searchInformation" "FCI_PRE_0"        
## [37] "denominator"       "singleICS_PR"      "accPS_PR"         
## [40] "singlePS_PR"       "singleCD_PR"       "singleCD_S"       
## [43] "singlePS_S"        "weeksICS"          "graphsPS"         
## [46] "dirs"              "accWeekNets"       "PASS"             
## [49] "attributes"        "accICS"            "accPS"            
## [52] "justpass"          "graphsCD"          "accICS_TE"        
## [55] "singleICS_TE"      "accCD_TE"          "accICS_H"         
## [58] "TargetEntropy"     "gzero"
```

There's a lot in there, but I'm mostly interested in the `accXX` objects, which hold the accumulated networks as of each week. 


```r
summary(accPS[[7]])
```

```
## IGRAPH f9c3ed0 DNW- 166 1199 -- 
## + attr: name (v/c), id (v/c), grade (v/n), gender (v/n), age (v/n),
## | cohort (v/n), sog (v/n), fci_pre (v/n), fci_pre_0 (v/n), fci_pre_s
## | (v/n), fci_pre_c (v/n), pass (v/n), justpass (v/n), weight (e/n)
```

```r
table(E(accPS[[7]])$weight)
```

```
## 
##   1   2   3   4   5   6   7 
## 634 226 159  66  69  42   3
```

Take a look at the pass/fail and just pass/just fail counts (these are the same in all three network layers):

```r
table(V(accPS[[7]])$pass, useNA = "ifany")
```

```
## 
##   0   1 
##  38 128
```

```r
table(V(accPS[[7]])$justpass, useNA = "ifany")
```

```
## 
##    0    1 <NA> 
##   28   39   99
```

```r
nPass <- table(V(accPS[[7]])$pass, useNA = "ifany")
nJustPass <- table(V(accPS[[7]])$justpass, useNA = "ifany")
```


## Build data frames

To do pass/fail boxplots, logistic regression, and other analyses, I need a data frame with people's IDs, their pass/fail outcomes, and their centrality scores. I need this information for each week, with a column for week number as well. 

**TEMP HACK:** Need to avoid duplicate node names, so second "Person14" gets named "Person16


Now I collect the node information into a long data frame. 


```r
# accNW should be one of accPS, accCD, or accICS
# accPR, accTE, and accH should be the corresponding centrality lists
make_node_df <- function(accNW, accPR, accTE, accH) {
  dflist <- vector("list", length = length(accNW))
  for (i in seq(dflist)) {
    df <- igraph::as_data_frame(accNW[[i]], what = "vertices")
    df$Week <- i
    df$PageRank <- accPR[[i]]$vector
    df$tarEnt <- accTE[[i]]
    df$Hide <- accH[[i]]
    
    dflist[[i]] <- df %>% select(-id)  # ditch id column, it duplicates name + Person14 typo
  }
  dfNW <- bind_rows(dflist)
  dfNW <- subset(dfNW, select = c(Week, name:Hide))  # reorder to put Week first
  
  # Need factors for plotting
  dfNW$Week <- as.factor(dfNW$Week)   
  dfNW$pass <- as.factor(dfNW$pass)
  dfNW$justpass <- as.factor(dfNW$justpass)   
  return(dfNW)
}
dfPS <- make_node_df(accPS, accPS_PR, accPS_TE, accPS_H)

head(dfPS)
```

```
##   Week    name grade gender age cohort sog fci_pre fci_pre_0 fci_pre_s
## 1    1 Person1     2      1  19      3   6      17        17        17
## 2    1 Person2    -3      1  20     10  NA      NA         0         6
## 3    1 Person3     0      1  22     10   9      NA         0        15
## 4    1 Person4     4      1  19      6  20      28        28        28
## 5    1 Person5     2      1  27      1   7      26        26        26
## 6    1 Person6     4      1  20      2   0      18        18        18
##   fci_pre_c pass justpass Week.1    PageRank    tarEnt       Hide
## 1         2    1        1      1 0.004780164 0.0000000   0.000000
## 2         1    0     <NA>      1 0.005915686 0.4138169 114.230046
## 3         1    0        0      1 0.004780164 0.0000000   0.000000
## 4         4    1     <NA>      1 0.005997961 0.0000000  13.228819
## 5         4    1        1      1 0.004780164 0.0000000   0.000000
## 6         3    1     <NA>      1 0.009249618 1.0000000   3.321928
```

Now concept discussion:


```r
dfCD <- make_node_df(accCD, accCD_PR, accCD_TE, accCD_H)
head(dfCD)
```

```
##   Week    name grade gender age cohort sog fci_pre fci_pre_0 fci_pre_s
## 1    1 Person1     2      1  19      3   6      17        17        17
## 2    1 Person2    -3      1  20     10  NA      NA         0         6
## 3    1 Person3     0      1  22     10   9      NA         0        15
## 4    1 Person4     4      1  19      6  20      28        28        28
## 5    1 Person5     2      1  27      1   7      26        26        26
## 6    1 Person6     4      1  20      2   0      18        18        18
##   fci_pre_c pass justpass Week.1    PageRank tarEnt Hide
## 1         2    1        1      1 0.005117532      0    0
## 2         1    0     <NA>      1 0.007292484      0    1
## 3         1    0        0      1 0.005117532      0    0
## 4         4    1     <NA>      1 0.005117532      0    0
## 5         4    1        1      1 0.005117532      0    0
## 6         3    1     <NA>      1 0.009467435      0    0
```

And in-class socializing:


```r
dfICS <- make_node_df(accICS, accICS_PR, accICS_TE, accICS_H)
head(dfICS)
```

```
##   Week    name grade gender age cohort sog fci_pre fci_pre_0 fci_pre_s
## 1    1 Person1     2      1  19      3   6      17        17        17
## 2    1 Person2    -3      1  20     10  NA      NA         0         6
## 3    1 Person3     0      1  22     10   9      NA         0        15
## 4    1 Person4     4      1  19      6  20      28        28        28
## 5    1 Person5     2      1  27      1   7      26        26        26
## 6    1 Person6     4      1  20      2   0      18        18        18
##   fci_pre_c pass justpass Week.1    PageRank   tarEnt       Hide
## 1         2    1        1      1 0.004912848 0.000000   0.000000
## 2         1    0     <NA>      1 0.005191243 0.000000   3.906891
## 3         1    0        0      1 0.004912848 0.000000   0.000000
## 4         4    1     <NA>      1 0.006882272 1.500000  13.943064
## 5         4    1        1      1 0.004912848 0.000000   0.000000
## 6         3    1     <NA>      1 0.012240235 1.378419 144.061667
```

There's some redundancy here--everything is the same except for the last three columns, which have the centrality values for that particular network layer. But rather than wrangling everything into a single data frame (and having to rewrite downstream code), I'm going to run with this for now. 

Save the output so I can use it in the next set of files:


```r
save(dfPS, dfCD, dfICS, nPass, nJustPass, file = "../data/centrality_data_frames.Rdata")
```


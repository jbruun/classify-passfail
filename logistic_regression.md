Logistic regression
================
Jesper Bruun & Adrienne Traxler
3/30/2020

Goal for this document is to make exploratory plots and run logistic
regression for passing and failing in the (single-layer) PS, CD, and ICS
weekly networks.

**Update 3/30:** Imported data and identified which objects I need. Do
pass/fail centrality boxplots next.

    ## 
    ## Attaching package: 'igraph'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

## Import data

At this point, `loadAllNetworks` and `calculatePR_TE_H` have already
been run. Importing the results of that,

``` r
(load("data/PRTEH.RData"))
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

There’s a lot in there, but I’m mostly interested in the `accXX`
objects, which hold the accumulated networks as of each week.

``` r
summary(accPS[[7]])
```

    ## IGRAPH f9c3ed0 DNW- 166 1199 -- 
    ## + attr: name (v/c), id (v/c), grade (v/n), gender (v/n), age (v/n),
    ## | cohort (v/n), sog (v/n), fci_pre (v/n), fci_pre_0 (v/n), fci_pre_s
    ## | (v/n), fci_pre_c (v/n), pass (v/n), justpass (v/n), weight (e/n)

``` r
table(E(accPS[[7]])$weight)
```

    ## 
    ##   1   2   3   4   5   6   7 
    ## 634 226 159  66  69  42   3

Take a look at the pass/fail and just pass/just fail counts (these are
the same in all three network layers):

``` r
table(V(accPS[[7]])$pass, useNA = "ifany")
```

    ## 
    ##   0   1 
    ##  38 128

``` r
table(V(accPS[[7]])$justpass, useNA = "ifany")
```

    ## 
    ##    0    1 <NA> 
    ##   28   39   99

## Pass/fail boxplots

DO THIS NEXT

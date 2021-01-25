---
title: "Cross-validation notes"
author: "Jesper Bruun & Adrienne Traxler"
date: "25 Jan 2021--"
output: 
  html_document: 
    keep_md: yes
---



Goal for this document is to summarize results from the cross-validation methods we used: Leave One Out Cross-Validation aka LOOCV aka jackknife, and k-fold cross-validation with `k=5` and `k=10`.

**Update 25 Jan:** Starting this file to document calculations I worked out in `passfail_XX_kfold.R` files. 


```
## Warning: package 'igraph' was built under R version 4.0.3
```

## Import data

At this point, the following R scripts have already been run. 

* For LOOCV: `passfail_logreg_jackknife`, `passfail_lda_jackknife`, `passfail_qda_jackknife`, and `passfail_knn_jackknife`. 
* For k-fold cross-validation: `passfail_logreg_kfold`, `passfail_lda_lfold`, `passfail_qda_kfold` (ultimately not used, see notes below), and `passfail_knn_kfold`. 

Each of those files saves the predictions in an .Rdata file in the `data` directory and the success rate table in the `results` directory. Results are saved separately for the full set of pass/fail outcomes and for students who were just on the pass/fail border (the justpass outcome, saved in `Just` files). The k-fold results were saved for `k=5` and `k=10`. 


```r
filelist <- list.files(path = "../results", pattern = "*.csv")
infiles <- file.path("../results", filelist)
allSuccRates <- lapply(infiles, read.csv)
```

I want to name these appropriately and probably reorder them. ***IN PROGRESS***


```r
gsub("succRate", "", filelist)
```

```
##  [1] "_knn.csv"                "_knn_kfold10.csv"       
##  [3] "_knn_kfold5.csv"         "_knn_noFCI.csv"         
##  [5] "_lda.csv"                "_lda_kfold10.csv"       
##  [7] "_lda_kfold5.csv"         "_logReg.csv"            
##  [9] "_logReg_kfold10.csv"     "_logReg_kfold5.csv"     
## [11] "_qda.csv"                "Just_knn.csv"           
## [13] "Just_knn_kfold10.csv"    "Just_knn_kfold5.csv"    
## [15] "Just_knn_noFCI.csv"      "Just_lda.csv"           
## [17] "Just_lda_kfold10.csv"    "Just_lda_kfold5.csv"    
## [19] "Just_logReg.csv"         "Just_logReg_kfold10.csv"
## [21] "Just_logReg_kfold5.csv"  "Just_qda.csv"
```



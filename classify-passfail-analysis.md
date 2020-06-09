R markdown for Classifying student passing and failing from network
measures
================
Adrienne Traxler & Jesper Bruun
9/3/2020

Summarize results here.

**Update 6/9:** Started with collecting data information (since I keep
forgetting which variables to use).

## Summary of data

After the network data has been cleaned and assigned node attributes (by
`loadAllNetworks.r`), and centrality values are calculated (by
`calculatePR_TE_H.r`), the igraph objects are stored in
`data/PRTEH.RData`.

``` r
load("data/PRTEH.RData")
```

That file includes:

  - `accPS`, `accCD`, and `accICS`: Accumulated network objects for each
    week for the problem solving, concept discussion, and in-class
    social network layers.

  - 
## Logisitic regression results

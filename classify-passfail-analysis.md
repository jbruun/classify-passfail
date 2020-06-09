R markdown for Classifying student passing and failing from network
measures
================
Adrienne Traxler & Jesper Bruun
9/3/2020

Summarize results here.

**Update 6/9:** Started with collecting data information (since I keep
forgetting which variables to use).

## Summary of data

### Networks

After the network data has been cleaned and assigned node attributes (by
`loadAllNetworks.r`), and centrality values are calculated (by
`calculatePR_TE_H.r`), the igraph objects are stored in
`data/PRTEH.RData`.

That file includes:

  - `accPS`, `accCD`, and `accICS`: Accumulated network objects for each
    week for the problem solving, concept discussion, and in-class
    social network layers.
  - `accPS_PR`, etc.: The `accXX_Y`-named objects hold centrality
    calculations for each layer for PageRank, Target Entropy, Hide. For
    TE and Hide, these are lists of vectors (one per week). For
    PageRank, it’s a list of lists (one per week), with the centrality
    values we want in the `vector` entry of the list.

As part of standardizing the networks, they all have the same number of
nodes, 166.

``` r
sapply(accPS, vcount)
```

    ## [1] 166 166 166 166 166 166 166

``` r
sapply(accCD, vcount)
```

    ## [1] 166 166 166 166 166 166 166

``` r
sapply(accICS, vcount)
```

    ## [1] 166 166 166 166 166 166 166

### Node information

There are also data frames of node information (after
`make_node_data_frames.Rmd`), in `data/centrality_data_frames.Rdata`.
This has:

  - `dfPS`, `dfCD`, `dfICS`: A data frame for each layer listing week,
    node name, all predictor variables, and pass/justpass outcomes.
  - `nPass` and `nJustPass`: Tables of pass/fail counts (coded 1/0), for
    everyone and for people who were right on the border. For
    `nJustPass`, people not on the border are coded `NA`.

<!-- end list -->

``` r
load("data/centrality_data_frames.Rdata")
nPass
```

    ## 
    ##   0   1 
    ##  38 128

``` r
nJustPass
```

    ## 
    ##    0    1 <NA> 
    ##   28   39   99

## Logisitic regression results

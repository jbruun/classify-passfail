# Read in pass/fail tagged centrality data and plot alluvial diagrams
# Last modified:  6/30/21 (updating with new data)
# 
# Status: Counting quantiles updated, time to fix alluvial plots.

rm(list = ls())

library(igraph)
library(ggalluvial)
library(tidyverse)
#library(dplyr)
#library(ggplot2)

# Import pass/fail centrality data
#loadvars <- load("../data/centPassFail.Rdata")
loadvars <- load("data/centrality_data_frames.Rdata")

# Tibble-ify
dfPS <- as_tibble(dfPS)
dfCD <- as_tibble(dfCD)
dfICS <- as_tibble(dfICS)


## Bin weekly aggregate centrality values

# Calculate centrality quantiles for a layer and set min/max values to 0/1
quantbin <- function(x,cent) {
  dat <- lapply(x,function(y) y[,c(cent)])  # pull out desired centrality measure
  bin <- lapply(dat,quantile)
  for (i in 1:length(bin)) {
    bunique <- names(bin[[i]])[!duplicated(bin[[i]])]
    bin[[i]] <- unique(bin[[i]])  # could be done above, but it's easier to get the names this way
    names(bin[[i]]) <- bunique
    #bin[[i]][1] <- 0
    #bin[[i]][length(bin[[i]])] <- 1 
  }
  names(bin) <- paste0("Week",c(1:length(bin)))
  return(bin)
}

# Calculate centrality quantiles for a layer and set min/max values to 0/1
# Input: List of data frames with centrality values and Week number; name of 
#  centrality measure
# Output: List of quantiles for each week for the given centrality measure
quantbin2 <- function(x, cent) {
  bin <- x %>% 
    map(pull, "PageRank") %>% 
    map(quantile)
  for (i in 1:length(bin)) {
    bunique <- names(bin[[i]])[!duplicated(bin[[i]])]
    bin[[i]] <- unique(bin[[i]])  # could be done above, but it's easier to get the names this way
    names(bin[[i]]) <- bunique
    #bin[[i]][1] <- 0
    #bin[[i]][length(bin[[i]])] <- 1 
  }
  names(bin) <- paste0("Week",c(1:length(bin)))
  return(bin)
}

#binPSPR <- quantbin(centPS,"PageRank")
#binCDTE <- quantbin(centCD,"tarEnt")

# List-ify long data frames
listPS <- dfPS %>% group_by(Week) %>% group_split() 
listCD <- dfCD %>% group_by(Week) %>% group_split() 
listICS <- dfICS %>% group_by(Week) %>% group_split() 


binPSPR <- quantbin2(listPS,"PageRank")
binCDTE <- quantbin2(listCD,"tarEnt")


# Make cuts for one layer/measure
# Input: A list of centrality data frames (listPS, listCD, etc.), centrality measure to use, 
#  list of quartiles.
# Output: A list of data frames by week, with id, week, bin (Q1, Q2, etc.) and pass/fail outcome
cutbin <- function(x, cent, bin) {
  binlist <- vector(mode = "list", length = length(x))
  for (i in 1:length(x)) {
    breaks <- 2  # if only two bins, cut in the middle
    labels <- c("50%","100%")
    if (length(bin[[i]]) > 2) {
      breaks <- bin[[i]]
      labels <- names(bin[[i]])[-1]
    }
    binlist[[i]] <- data.frame(id = x[[i]]$id, 
                               Week = paste0("W", as.character(i)),
                               Bin = cut(x[[i]][, c(cent)],
                                         breaks = breaks,#breaks=bin[[i]],
                                         labels = labels),#names(bin[[i]])[-1]),
                               Pass = x[[i]]$Pass)
    # Manually set NAs (people not yet in network?) to lowest bin
    binlist[[i]][is.na(binlist[[i]]$Bin), "Bin"] <- levels(binlist[[i]]$Bin)[1] #names(bin[[i]])[1]
  }
  return(binlist)
}

# Make cuts for one layer/measure
# Input: A list of centrality data frames (listPS, listCD, etc.), centrality measure to use, 
#  list of quartiles.
# Output: A list of data frames by week, with id, week, bin (Q1, Q2, etc.) and pass/fail outcome
cutbin2 <- function(x, cent, binlist) {
  allbins <- vector(mode = "list", length = length(x))
  for (i in 1:length(x)) {
    dat <- x[[i]]
    bin <- binlist[[i]]
    breaks <- 2  # if only two bins, cut in the middle
    labels <- c("50%","100%")
    if (length(bin) > 2) {
      breaks <- bin
      labels <- names(bin)[-1]
    }
    
    dfbin <- data.frame(id = dat$name, 
                        Week = paste0("W", as.character(i)),
                        Bin = dat %>% pull(cent) %>% 
                          cut(breaks = breaks, labels = labels), 
                        Pass = dat$pass)
    # Manually set NAs (people not yet in network?) to lowest bin
    dfbin[is.na(dfbin$Bin), "Bin"] <- levels(dfbin$Bin)[1] #names(bin[[i]])[1]
    allbins[[i]] <- dfbin
  }
  return(allbins)
}


#binlistPSPR <- cutbin(centPS,"PageRank",binPSPR)
#binlistCDTE <- cutbin(centCD,"tarEnt",binCDTE)

binlistPSPR <- cutbin2(listPS, "PageRank", binPSPR)
binlistCDTE <- cutbin2(listCD, "tarEnt", binCDTE)


# Node quartiles for each week
# Input list of weekly bins by node
# Output data frame with one row per node, column for each week showing quartile, and pass/fail column
wideframe <- function(binlist) {
  binwide <- data.frame(id = binlist[[1]]$id)
  for (i in 1:length(binlist)) {
    binwide[, i+1] <- binlist[[i]]$Bin
  }
  names(binwide)[1:length(binlist) + 1] <- paste0("Week",c(1:length(binlist)))
  binwide$Pass <- binlist[[1]]$Pass
  return(binwide)
}

binwidePSPR <- wideframe(binlistPSPR)
binwideCDTE <- wideframe(binlistCDTE)


# Count frequencies of each factor combination
countPSPR <- count(binwidePSPR, Week1, Week2, Week3, Week4, Week5, Week6, Week7)
countCDTE <- count(binwideCDTE, Week1, Week2, Week3, Week4, Week5, Week6, Week7)

countPassPSPR <- count(binwidePSPR, Week1, Week2, Week3, Week4, Week5, Week6, 
                       Week7, Pass)
countPassCDTE <- count(binwidePSPR, Week1, Week2, Week3, Week4, Week5, Week6, 
                       Week7, Pass)


## Plot alluvial diagram from tidy frame

alluvial(countPSPR[1:7], freq = countPSPR$n, hide = countPSPR$n < 2)
alluvial(countCDTE[1:7], freq = countCDTE$n, hide = countCDTE$n < 2)

# Include pass/fail column
alluvial(countPassPSPR[1:8],freq=countPassPSPR$n)
alluvial(countPassPSPR[1:8],freq=countPassPSPR$n,hide=countPassPSPR$n<2,
         col=ifelse(countPassPSPR$Pass=="Pass", "blue", "grey" ))
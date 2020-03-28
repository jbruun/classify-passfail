#This script loads PS, CD, and ICS networks from 2010. 
#Furthermore, it creates accumulated networks for each PS, CD, and ICS week. 
#Also, it assigns attributes to all networks. 

###Packages###
library(igraph)

###Loading node attributes###
##These will be attached to networks as node attributes later###

attributes<-read.csv("data/SNA_ESERA2013.csv")
FCI_PRE<-c(17,NA,NA,28,26,18,NA,15,26,13,30,27,23,20,27,24,24,15,24,19,9,5,14,28,16,29,NA,19,NA,27,NA,9,17,12,NA,29,29,NA,24,8,NA,15,18,NA,21,NA,12,28,18,16,NA,NA,NA,27,24,23,NA,13,16,26,10,23,25,22,NA,29,6,10,NA,23,NA,NA,29,21,26,NA,24,17,15,NA,20,23,25,27,NA,11,15,NA,29,NA,12,20,10,20,9,17,26,22,NA,NA,23,24,NA,18,7,25,16,19,23,27,29,23,22,NA,7,17,NA,24,25,6,26,NA,19,21,14,23,28,13,NA,24,19,19,10,NA,24,NA,26,12,27,18,29,12,27,23,28,11,14,NA,23,NA,16,21,26,8,20,NA,NA,19,10,15,29,NA,6,18,NA,8,20,26,8,NA,25,18,20,8,NA,15,16,NA,26,26,21,29,NA,11,NA,24,24)
#New FCI_PRE attribute, where NAs are set to zero.
FCI_PRE_0<-FCI_PRE
FCI_PRE_0[is.na(FCI_PRE_0)]<-0

#New FCI_PRE attribute where NAs are replaced by a sample
FCI_PRE_S<-FCI_PRE
FCI_PRE_S[is.na(FCI_PRE) & attributes$Course.Grade==-3]<-sample(FCI_PRE[!is.na(FCI_PRE) & attributes$Course.Grade==-3],1)
FCI_PRE_S[is.na(FCI_PRE)& attributes$Course.Grade==0]<-sample(FCI_PRE[!is.na(FCI_PRE)& attributes$Course.Grade==0],8)
FCI_PRE_S[is.na(FCI_PRE)& attributes$Course.Grade==2]<-sample(FCI_PRE[!is.na(FCI_PRE)& attributes$Course.Grade==2],5)
FCI_PRE_S[is.na(FCI_PRE)& attributes$Course.Grade==4]<-sample(FCI_PRE[!is.na(FCI_PRE)& attributes$Course.Grade==4],1)
FCI_PRE_S[is.na(FCI_PRE)& attributes$Course.Grade==7]<-sample(FCI_PRE[!is.na(FCI_PRE)& attributes$Course.Grade==7],3)
FCI_PRE_S[is.na(FCI_PRE)& attributes$Course.Grade==10]<-sample(FCI_PRE[!is.na(FCI_PRE)& attributes$Course.Grade==10],1)

attributes$Course.Grade[attributes$Course.Grade==100]<-NA #R counts NA in the above...
#New FCI_PRE attribute, where we make four classes. Three based on score and 1 based on NAs. 
#Classification based on Halloun & Hestenes 1995 (Interpreting the Force Concept Inventory): http://modeling.asu.edu/R&E/InterFCI.pdf 
#NA is Class 1. Below 60% correct (17 or less) is Class 2, "below entry level" (see p. 6 in H&H). Between 60% and 85% (17-25) is "entry level". Above 85% (26) is expert. 
FCI_PRE_C<-vector()
FCI_PRE_C[is.na(FCI_PRE)]<-1
FCI_PRE_C[FCI_PRE<18]<-2
FCI_PRE_C[FCI_PRE>=18 & FCI_PRE<=25]<-3
FCI_PRE_C[FCI_PRE>25]<-4


SOG<-c(6,NA,9,20,7,0,11,NA,NA,11,24,NA,NA,20,10,9,6,14,14,17,NA,17,10,NA,NA,22,NA,NA,22,8,24,11,6,NA,NA,24,24,NA,20,14,NA,14,-1,NA,16,NA,NA,24,NA,6,NA,NA,NA,22,22,22,NA,-3,11,NA,NA,-3,7,NA,NA,22,NA,4,NA,20,NA,NA,NA,12,14,NA,14,NA,14,NA,17,NA,14,17,NA,6,0,9,NA,22,9,14,14,NA,-3,NA,24,2,NA,NA,14,8,19,NA,NA,11,17,20,24,24,6,NA,10,NA,NA,11,4,20,NA,9,17,2,22,2,22,14,11,6,NA,NA,2,NA,9,0,0,0,22,NA,0,19,17,NA,22,11,14,4,8,NA,NA,NA,17,9,20,NA,9,NA,NA,NA,4,NA,17,NA,NA,17,NA,12,11,20,19,NA,9,NA,11,2,NA,9,17,NA,9,14,14,24,NA,0,14,24,NA)
attributes$fci_pre<-FCI_PRE
attributes$fci_pre_0<-FCI_PRE_0
attributes$fci_pre_s<-FCI_PRE_S
attributes$fci_pre_c<-FCI_PRE_C
attributes$sog<-SOG
PASS<-vector(length = 187)
PASS[attributes$Course.Grade<2]<-0
PASS[attributes$Course.Grade>=2]<-1
JUSTPASS<-vector(length=187)
JUSTPASS<-NA
JUSTPASS[attributes$Course.Grade==0]<-0
JUSTPASS[attributes$Course.Grade==2]<-1
attributes$pass<-PASS
attributes$justpass<-JUSTPASS

# Import PS weekly networks
dirs <- list.files("data/networks/")
files <- c("week36-37physStandQ1.net","AnonymousWeek38physStandQ1.net",
           "week39physStandQ1.net","week40physStandQ1.net","week42physStandQ1.net",
           "week43physStandQ1.net","week44physQ1Standardized.net")
paths <- paste("data/networks",dirs,files,sep="/")
weeksPS <- lapply(paths,read.graph,format="pajek")
names(weeksPS) <- c("week36-37","week38","week39","week40","week42","week43","week44")
weeksPS

# Import networks from their various directories
dirs <- list.files("data/networks/")
files <- c("week36-37physStandQ2.net","AnonymousWeek38physStandQ2.net",
           "week39physStandQ2.net","week40physStandQ2.net","week42physStandQ2.net",
           "week43physStandQ2.net","week44physQ2Standardized.net")
paths <- paste("data/networks",dirs,files,sep="/")
weeksCD <- lapply(paths,read.graph,format="pajek")
names(weeksCD) <- c("week36-37","week38","week39","week40","week42","week43","week44")
weeksCD

dirs <- list.files("data/networks/")
files <- c("week36-37socStandQ1.net","AnonymousWeek38SocStandQ1.net",
           "week39socStandQ1.net","week40socStandQ1.net","week42socStandQ1.net",
           "week43socStandQ1.net","week44socQ1Standardized.net")
paths <- paste("data/networks",dirs,files,sep="/")
weeksICS <- lapply(paths,read.graph,format="pajek")
names(weeksICS) <- c("week36-37","week38","week39","week40","week42","week43","week44")
weeksICS

#CLEAN UP
###PS###
#In one network, links were given the weight "NA". 
lapply(weeksPS,function(x) table(E(x)$weight,useNA="ifany"))
E(weeksPS$week38)$weight[is.na(E(weeksPS$week38)$weight)] <- 1
# Remove zero-weight edges -- these appear when links in another layer but not in this layer
gzero <- lapply(weeksPS,function(x) x-E(x)[weight==0])
lapply(gzero,function(x) sum(is.multiple(x)))
graphsPS <- lapply(gzero,simplify,edge.attr.comb="first")
#Evidence of cleansing
lapply(graphsPS,function(x) table(E(x)$weight,useNA="ifany"))

##CD###
#In one network, links were given the weight "NA". 
lapply(weeksCD,function(x) table(E(x)$weight,useNA="ifany"))
E(weeksCD$week38)$weight[is.na(E(weeksCD$week38)$weight)] <- 1
# Remove zero-weight edges -- these appear when links in another layer but not in this layer
gzero <- lapply(weeksCD,function(x) x-E(x)[weight==0])
lapply(gzero,function(x) sum(is.multiple(x)))
graphsCD <- lapply(gzero,simplify,edge.attr.comb="first")
#Evidence of cleansing
lapply(graphsCD,function(x) table(E(x)$weight,useNA="ifany"))

###ICS###
#In one network, links were given the weight "NA" and in another no weight was given.
lapply(weeksICS,function(x) table(E(x)$weight,useNA="ifany"))
# Add weight 1 to edges to weeksICS 38 and 44
E(weeksICS$week38)$weight[is.na(E(weeksICS$week38)$weight)] <- 1
weeksICS$week44 <- set.edge.attribute(weeksICS$week44, "weight", value=1)
# Remove zero-weight edges
gzero <- lapply(weeksICS,function(x) x-E(x)[weight==0])
lapply(gzero,function(x) sum(is.multiple(x)))
graphsICS <- lapply(gzero,simplify,edge.attr.comb="first") # defaults to remove-loops=TRUE
#Evidence of cleansing
lapply(graphsICS,function(x) table(E(x)$weight,useNA="ifany"))

###MAKING ACCUMULATED NETWORKS####
accWeekNets<-function(graphlist,attributes){
  n<-length(graphlist)
  accNets<-list()
  accNets[[1]]<-graphlist[[1]]
  for(i in 2:n){
    accNets[[i]]<-graph_from_adjacency_matrix(as_adj(accNets[[i-1]],attr="weight") + as_adj(graphlist[[i]],attr="weight"),weighted=T)
  }
  for(i in 1:n){
    V(accNets[[i]])$id<-V(accNets[[i]])$name
    V(accNets[[i]])$grade<-attributes$Course.Grade
    V(accNets[[i]])$gender<-attributes$Gender
    V(accNets[[i]])$age<-attributes$Age
    V(accNets[[i]])$cohort<-attributes$Cohort
    V(accNets[[i]])$sog<-attributes$sog
    V(accNets[[i]])$fci_pre<-attributes$fci_pre
    V(accNets[[i]])$fci_pre_0<-attributes$fci_pre_0
    V(accNets[[i]])$fci_pre_s<-attributes$fci_pre_s
    V(accNets[[i]])$fci_pre_c<-attributes$fci_pre_c
    V(accNets[[i]])$pass<-attributes$pass
    V(accNets[[i]])$justpass<-attributes$justpass
    
  }
  return(accNets)
}
accPS<-accWeekNets(graphsPS,attributes)
accCD<-accWeekNets(graphsCD,attributes)
accICS<-accWeekNets(graphsICS,attributes)

###APPLY ATTRIBUTES TO SINGLE NETWORKS####
##NB! Our analyses focus on accumulated networks. 
##This here for completeness
applyAttr<-function(g,attributesFrame){
  
  V(g)$grade<-attributes$Course.Grade
  V(g)$gender<-attributes$Gender
  V(g)$age<-attributes$Age
  V(g)$cohort<-attributes$Cohort
  V(g)$sog<-attributes$sog
  V(g)$fci_pre<-attributes$fci_pre
  V(g)$fci_pre_0<-attributes$fci_pre_0
  V(g)$fci_pre_s<-attributes$fci_pre_s
  V(g)$fci_pre_c<-attributes$fci_pre_c
  V(g)$pass<-attributes$pass
  V(g)$justpass<-attributes$justpass
  
  return(g)
}
weeksPS<-lapply(weeksPS,applyAttr)
weeksCD<-lapply(weeksCD,applyAttr)
weeksICS<-lapply(weeksICS,applyAttr)

###REMOVE NODES THAT  THAT REPRESENT TEACHERS

accPS<-lapply(accPS,delete.vertices,is.na(attributes$Course.Grade))
accCD<-lapply(accCD,delete.vertices,is.na(attributes$Course.Grade))
accICS<-lapply(accICS,delete.vertices,is.na(attributes$Course.Grade))
weeksPS<-lapply(weeksPS,delete.vertices,is.na(attributes$Course.Grade))
weeksCD<-lapply(weeksCD,delete.vertices,is.na(attributes$Course.Grade))
weeksICS<-lapply(weeksICS,delete.vertices,is.na(attributes$Course.Grade))

### NON-PARTICIPATING STUDENTS?###
biggraph<-graph_from_adjacency_matrix(as_adj(accPS[[7]])+as_adj(accCD[[7]])+as_adj(accICS[[7]]),weighted = T)

which(degree(biggraph)==0) #There are three isolates (these have a degree of at least 1 in other SINs, just not PS, CD, and ICS)

accPS<-lapply(accPS,delete.vertices,degree(biggraph)==0)
accCD<-lapply(accCD,delete.vertices,degree(biggraph)==0)
accICS<-lapply(accICS,delete.vertices,degree(biggraph)==0)
weeksPS<-lapply(weeksPS,delete.vertices,degree(biggraph)==0)
weeksCD<-lapply(weeksCD,delete.vertices,degree(biggraph)==0)
weeksICS<-lapply(weeksICS,delete.vertices,degree(biggraph)==0)

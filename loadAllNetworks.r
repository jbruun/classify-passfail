#THis script loads PS, CD, and ICS networks from 2010. 
#Furthermore, it creates accumulated networks for each PS, CD, and ICS week. 
#Also, it assigns attributes to all networks. 

###Packages###
library(igraph)

###Loading node attributes###
##These will be attached to networks as node attributes later###

attributes<-read.csv("data/SNA_ESERA2013.csv")
<<<<<<< HEAD
=======
attributes$Course.Grade[attributes$Course.Grade==100]<-NA
>>>>>>> 2b24fbd02d911c1c8f34b0ffd0a201d6ed65acb5
FCI_PRE<-c(17,NA,NA,28,26,18,NA,15,26,13,30,27,23,20,27,24,24,15,24,19,9,5,14,28,16,29,NA,19,NA,27,NA,9,17,12,NA,29,29,NA,24,8,NA,15,18,NA,21,NA,12,28,18,16,NA,NA,NA,27,24,23,NA,13,16,26,10,23,25,22,NA,29,6,10,NA,23,NA,NA,29,21,26,NA,24,17,15,NA,20,23,25,27,NA,11,15,NA,29,NA,12,20,10,20,9,17,26,22,NA,NA,23,24,NA,18,7,25,16,19,23,27,29,23,22,NA,7,17,NA,24,25,6,26,NA,19,21,14,23,28,13,NA,24,19,19,10,NA,24,NA,26,12,27,18,29,12,27,23,28,11,14,NA,23,NA,16,21,26,8,20,NA,NA,19,10,15,29,NA,6,18,NA,8,20,26,8,NA,25,18,20,8,NA,15,16,NA,26,26,21,29,NA,11,NA,24,24)
#New FCI_PRE attribute, where NAs are set to zero.
FCI_PRE_0<-FCI_PRE
FCI_PRE_0[is.na(FCI_PRE_0)]<-0
<<<<<<< HEAD
#New FCI_PRE attribute where NAs are replaced by a sample
FCI_PRE_S<-FCI_PRE
FCI_PRE_S[is.na(FCI_PRE)& attributes$Course.Grade==-3]<-sample(FCI_PRE[!is.na(FCI_PRE)& attributes$Course.Grade==-3],3)
FCI_PRE_S[is.na(FCI_PRE)& attributes$Course.Grade==0]<-sample(FCI_PRE[!is.na(FCI_PRE)& attributes$Course.Grade==0],8)
FCI_PRE_S[is.na(FCI_PRE)& attributes$Course.Grade==2]<-sample(FCI_PRE[!is.na(FCI_PRE)& attributes$Course.Grade==2],5)
FCI_PRE_S[is.na(FCI_PRE)& attributes$Course.Grade==4]<-sample(FCI_PRE[!is.na(FCI_PRE)& attributes$Course.Grade==4],1)
FCI_PRE_S[is.na(FCI_PRE)& attributes$Course.Grade==7]<-sample(FCI_PRE[!is.na(FCI_PRE)& attributes$Course.Grade==7],3)
FCI_PRE_S[is.na(FCI_PRE)& attributes$Course.Grade==10]<-sample(FCI_PRE[!is.na(FCI_PRE)& attributes$Course.Grade==10],1)
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

=======
#New FCI_PRE attribute where NAs are 
FCI_PRE_S<-FCI_PRE
FCI_PRE_S[is.na(FCI_PRE_S)]<-sample(FCI_PRE[!is.na(FCI_PRE)],length(FCI_PRE[is.na(FCI_PRE)]))

SOG<-c(6,NA,9,20,7,0,11,NA,NA,11,24,NA,NA,20,10,9,6,14,14,17,NA,17,10,NA,NA,22,NA,NA,22,8,24,11,6,NA,NA,24,24,NA,20,14,NA,14,-1,NA,16,NA,NA,24,NA,6,NA,NA,NA,22,22,22,NA,-3,11,NA,NA,-3,7,NA,NA,22,NA,4,NA,20,NA,NA,NA,12,14,NA,14,NA,14,NA,17,NA,14,17,NA,6,0,9,NA,22,9,14,14,NA,-3,NA,24,2,NA,NA,14,8,19,NA,NA,11,17,20,24,24,6,NA,10,NA,NA,11,4,20,NA,9,17,2,22,2,22,14,11,6,NA,NA,2,NA,9,0,0,0,22,NA,0,19,17,NA,22,11,14,4,8,NA,NA,NA,17,9,20,NA,9,NA,NA,NA,4,NA,17,NA,NA,17,NA,12,11,20,19,NA,9,NA,11,2,NA,9,17,NA,9,14,14,24,NA,0,14,24,NA)
attributes$fci_pre<-FCI_PRE
>>>>>>> 2b24fbd02d911c1c8f34b0ffd0a201d6ed65acb5
attributes$sog<-SOG
pass<-vector(length = 187)
pass[attributes$Course.Grade<2]<-0
pass[attributes$Course.Grade>=2]<-1
justpass<-vector(length=187)
justpass<-NA
justpass[attributes$Course.Grade==0]<-0
justpass[attributes$Course.Grade==2]<-1
attributes$pass<-pass
attributes$justpass<-justpass


library(igraph)
library(dplyr)
library(class)   # for knn
library(tidyr)
library(ggplot2)  # for plotting success rates
library(MASS)
library(boot)
load("data/centrality_data_frames.Rdata")
# Turn long data frame into list of weekly frames
centPS <- dfPS %>% group_split(Week)
centCD <- dfCD %>% group_split(Week)
centICS <- dfICS %>% group_split(Week)

background<-data.frame(age=centPS[[1]]$age,gender=centPS[[1]]$gender,cohort=centPS[[1]]$cohort,fci_pre=centPS[[1]]$fci_pre,
                       fci_pre_0=centPS[[1]]$fci_pre_0,fci_pre_c=centPS[[1]]$fci_pre_c,pass=centPS[[1]]$pass,justpass=centPS[[1]]$justpass)
background$age[background$age==100]<-NA
background$cohort[background$cohort==100]<-NA
background$justpass<-as.numeric(background$justpass)


####Distributions of variables####
dev.off()
pdf(file="plots/distributionCentralityMeasures.pdf",width = 8.3, height = 5.8)
par(cex=0.7)
par(oma = c(4,1,1,1), mfrow = c(3, 3), mar = c(2, 2, 1, 1))
plot(sort(centPS[[1]]$PageRank,decreasing = T),log="xy",pch=1, 
      sub="PageRank",ylab="PageRank",xlab="Ranked students",cex=0.8)
points(sort(centPS[[2]]$PageRank,decreasing = T),log = "xy",col="darkred",pch=2,cex=0.8)
points(sort(centPS[[3]]$PageRank,decreasing = T),log = "xy",col="darkblue",pch=3,cex=0.8)
points(sort(centPS[[4]]$PageRank,decreasing = T),log = "xy",col="brown",pch=4,cex=0.8)
points(sort(centPS[[5]]$PageRank,decreasing = T),log = "xy",col="orange",pch=5,cex=0.8)
points(sort(centPS[[6]]$PageRank,decreasing = T),log = "xy",col="purple",pch=6,cex=0.8)
points(sort(centPS[[7]]$PageRank,decreasing = T),log = "xy",col="khaki",pch=7,cex=0.8)


plot(sort(centPS[[1]]$tarEnt,decreasing = T),pch=1,ylim=c(0,5),
     main="Problem solving layer",ylab="Target Entropy",xlab="Ranked students")
points(sort(centPS[[2]]$tarEnt,decreasing = T),col="darkred",pch=2,cex=0.8)
points(sort(centPS[[3]]$tarEnt,decreasing = T),col="darkblue",pch=3,cex=0.8)
points(sort(centPS[[4]]$tarEnt,decreasing = T),col="brown",pch=4,cex=0.8)
points(sort(centPS[[5]]$tarEnt,decreasing = T),col="orange",pch=5,cex=0.8)
points(sort(centPS[[6]]$tarEnt,decreasing = T),col="purple",pch=6,cex=0.8)
points(sort(centPS[[7]]$tarEnt,decreasing = T),col="khaki",pch=7,cex=0.8)


plot(sort(centPS[[1]]$Hide,decreasing = T),pch=1,ylim=c(0,3500),
     main="",ylab="Hide",xlab="Ranked students",cex=0.8)
points(sort(centPS[[2]]$Hide,decreasing = T),col="darkred",pch=2,cex=0.8)
points(sort(centPS[[3]]$Hide,decreasing = T),col="darkblue",pch=3,cex=0.8)
points(sort(centPS[[4]]$Hide,decreasing = T),col="brown",pch=4,cex=0.8)
points(sort(centPS[[5]]$Hide,decreasing = T),col="orange",pch=5,cex=0.8)
points(sort(centPS[[6]]$Hide,decreasing = T),col="purple",pch=6,cex=0.8)
points(sort(centPS[[7]]$Hide,decreasing = T),col="khaki",pch=7,cex=0.8)

plot(sort(centCD[[1]]$PageRank,decreasing = T),log="xy",pch=1, 
     main="",ylab="PageRank",xlab="Ranked students",cex=0.8)
points(sort(centCD[[2]]$PageRank,decreasing = T),log = "xy",col="darkred",pch=2,cex=0.8)
points(sort(centCD[[3]]$PageRank,decreasing = T),log = "xy",col="darkblue",pch=3,cex=0.8)
points(sort(centCD[[4]]$PageRank,decreasing = T),log = "xy",col="brown",pch=4,cex=0.8)
points(sort(centCD[[5]]$PageRank,decreasing = T),log = "xy",col="orange",pch=5,cex=0.8)
points(sort(centCD[[6]]$PageRank,decreasing = T),log = "xy",col="purple",pch=6,cex=0.8)
points(sort(centCD[[7]]$PageRank,decreasing = T),log = "xy",col="khaki",pch=7,cex=0.8)


plot(sort(centCD[[1]]$tarEnt,decreasing = T),pch=1,ylim=c(0,5),
     main="Concept discussion layer",ylab="Target Entropy",xlab="Ranked students")
points(sort(centCD[[2]]$tarEnt,decreasing = T),col="darkred",pch=2,cex=0.8)
points(sort(centCD[[3]]$tarEnt,decreasing = T),col="darkblue",pch=3,cex=0.8)
points(sort(centCD[[4]]$tarEnt,decreasing = T),col="brown",pch=4,cex=0.8)
points(sort(centCD[[5]]$tarEnt,decreasing = T),col="orange",pch=5,cex=0.8)
points(sort(centCD[[6]]$tarEnt,decreasing = T),col="purple",pch=6,cex=0.8)
points(sort(centCD[[7]]$tarEnt,decreasing = T),col="khaki",pch=7,cex=0.8)


plot(sort(centCD[[1]]$Hide,decreasing = T),pch=1,ylim=c(0,3500),
     main="",ylab="Hide",xlab="Ranked students",cex=0.8)
points(sort(centCD[[2]]$Hide,decreasing = T),col="darkred",pch=2,cex=0.8)
points(sort(centCD[[3]]$Hide,decreasing = T),col="darkblue",pch=3,cex=0.8)
points(sort(centCD[[4]]$Hide,decreasing = T),col="brown",pch=4,cex=0.8)
points(sort(centCD[[5]]$Hide,decreasing = T),col="orange",pch=5,cex=0.8)
points(sort(centCD[[6]]$Hide,decreasing = T),col="purple",pch=6,cex=0.8)
points(sort(centCD[[7]]$Hide,decreasing = T),col="khaki",pch=7,cex=0.8)

plot(sort(centICS[[1]]$PageRank,decreasing = T),log="xy",pch=1, 
     main="",ylab="PageRank",xlab="Ranked students",cex=0.8)
points(sort(centICS[[2]]$PageRank,decreasing = T),log = "xy",col="darkred",pch=2,cex=0.8)
points(sort(centICS[[3]]$PageRank,decreasing = T),log = "xy",col="darkblue",pch=3,cex=0.8)
points(sort(centICS[[4]]$PageRank,decreasing = T),log = "xy",col="brown",pch=4,cex=0.8)
points(sort(centICS[[5]]$PageRank,decreasing = T),log = "xy",col="orange",pch=5,cex=0.8)
points(sort(centICS[[6]]$PageRank,decreasing = T),log = "xy",col="purple",pch=6,cex=0.8)
points(sort(centICS[[7]]$PageRank,decreasing = T),log = "xy",col="khaki",pch=7,cex=0.8)


plot(sort(centICS[[1]]$tarEnt,decreasing = T),pch=1,ylim=c(0,5),
     main="In-class social layer",ylab="Target Entropy",xlab="Ranked students")
points(sort(centICS[[2]]$tarEnt,decreasing = T),col="darkred",pch=2,cex=0.8)
points(sort(centICS[[3]]$tarEnt,decreasing = T),col="darkblue",pch=3,cex=0.8)
points(sort(centICS[[4]]$tarEnt,decreasing = T),col="brown",pch=4,cex=0.8)
points(sort(centICS[[5]]$tarEnt,decreasing = T),col="orange",pch=5,cex=0.8)
points(sort(centICS[[6]]$tarEnt,decreasing = T),col="purple",pch=6,cex=0.8)
points(sort(centICS[[7]]$tarEnt,decreasing = T),col="khaki",pch=7,cex=0.8)


plot(sort(centICS[[1]]$Hide,decreasing = T),pch=1,ylim=c(0,3500),
     main="",ylab="Hide",xlab="Ranked students",cex=0.8)
points(sort(centICS[[2]]$Hide,decreasing = T),col="darkred",pch=2,cex=0.8)
points(sort(centICS[[3]]$Hide,decreasing = T),col="darkblue",pch=3,cex=0.8)
points(sort(centICS[[4]]$Hide,decreasing = T),col="brown",pch=4,cex=0.8)
points(sort(centICS[[5]]$Hide,decreasing = T),col="orange",pch=5,cex=0.8)
points(sort(centICS[[6]]$Hide,decreasing = T),col="purple",pch=6,cex=0.8)
points(sort(centICS[[7]]$Hide,decreasing = T),col="khaki",pch=7,cex=0.8)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom',legend = c("Week 1","Week 2","Week 3","Week 4","Week 5","Week 6","Week 7"), 
       col = c("black","darkred","darkblue","brown","orange", "purple","khaki"), 
       pch=c(1:7), xpd = TRUE, horiz = TRUE, cex = 1, 
       seg.len=1, bty = 'n')
dev.off()




####Correlations####
table(background$pass,background$justpass) #Nobrainer

table(background$pass,centPS[[1]]$grade) #Distribution of grades
table(background$pass,centPS[[1]]$sog) #Distribution of grades

table(background$pass,background$gender)

table(background$pass,background$age)

table(background$pass,background$cohort)

table(background$pass,background$cohort)


#FCI PRE

p <- ggplot(background, aes(x=pass, y=fci_pre)) + 
  geom_boxplot()
# Box plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)





####NETWORK MEASURES Box plots####

dev.off()
pdf(file="plots/pageRankPassFailPS.pdf",width = 8.3, height = 5.8)
boxplot(PageRank~Week:pf, notch=T,
        data=dfPS,
        main="PS layer passing and failing",
        xlab="Week",
        ylab="PageRank",
        col="dodgerblue",
        border="blue",
        lex.order=T,
        sep=":"
)
dev.off()
pdf(file="plots/pageRankPassFailCD.pdf",width = 8.3, height = 5.8)
boxplot(PageRank~Week:pass, notch=T,
        data=dfCD,
        main="CD layer passing and failing",
        xlab="Week",
        ylab="PageRank",
        col="plum",
        border="purple",
        lex.order=T,
        sep=":"
)
dev.off()
pdf(file="plots/pageRankPassFailICS.pdf",width = 8.3, height = 5.8)
boxplot(PageRank~Week:pass, notch=T,
        data=dfICS,
        main="ICS layer passing and failing",
        xlab="Week",
        ylab="PageRank",
        col="orange",
        border="brown",
        lex.order=T,
        sep=":"
)
dev.off()
pdf(file="plots/tarEntPassFailPS.pdf",width = 8.3, height = 5.8)
boxplot(tarEnt~Week:pass, notch=T,
        data=dfPS,
        main="PS layer passing and failing",
        xlab="Week",
        ylab="tarEnt",
        col="dodgerblue",
        border="blue",
        lex.order=T,
        sep=":"
)
dev.off()
pdf(file="plots/tarEntPassFailCD.pdf",width = 8.3, height = 5.8)

boxplot(tarEnt~Week:pass, notch=T,
        data=dfCD,
        main="CD layer passing and failing",
        xlab="Week",
        ylab="tarEnt",
        col="plum",
        border="purple",
        lex.order=T,
        sep=":"
)

dev.off()
pdf(file="plots/tarEntPassFailICS.pdf",width = 8.3, height = 5.8)
boxplot(tarEnt~Week:pass, notch=T,
        data=dfICS,
        main="ICS layer passing and failing",
        xlab="Week",
        ylab="tarEnt",
        col="orange",
        border="brown",
        lex.order=T,
        sep=":"
)
dev.off()
pdf(file="plots/hidePassFailPS.pdf",width = 8.3, height = 5.8)
boxplot(Hide~Week:pass, notch=T,
        data=dfPS,
        main="PS layer passing and failing",
        xlab="Week",
        ylab="Hide",
        col="dodgerblue",
        border="blue",
        lex.order=T,
        sep=":"
)
dev.off()
pdf(file="plots/hidePassFailCD.pdf",width = 8.3, height = 5.8)
boxplot(Hide~Week:pass, notch=T,
        data=dfCD,
        main="CD layer passing and failing",
        xlab="Week",
        ylab="Hide",
        col="plum",
        border="purple",
        lex.order=T,
        sep=":"
)

dev.off()
pdf(file="plots/hidePassFailICS.pdf",width = 8.3, height = 5.8)
boxplot(Hide~Week:pass, notch=T,
        data=dfICS,
        main="ICS layer passing and failing",
        xlab="Week",
        ylab="Hide",
        col="orange",
        border="brown",
        lex.order=T,
        sep=":"
)

dev.off()
pdf(file="plots/pageRankJustPassFailPS.pdf",width = 8.3, height = 5.8)
boxplot(PageRank~Week:jpf, notch=T,
        data=dfPS,
        main="PS layer justpassing and failing",
        xlab="Week",
        ylab="PageRank",
        col="dodgerblue",
        border="blue",
        lex.order=T,
        sep=":"
)
dev.off()
pdf(file="plots/pageRankJustPassFailCD.pdf",width = 8.3, height = 5.8)
boxplot(PageRank~Week:justpass, notch=T,
        data=dfCD,
        main="CD layer justpassing and failing",
        xlab="Week",
        ylab="PageRank",
        col="plum",
        border="purple",
        lex.order=T,
        sep=":"
)

dev.off()
pdf(file="plots/pageRankJustPassFailICS.pdf",width = 8.3, height = 5.8)
boxplot(PageRank~Week:justpass, notch=T,
        data=dfICS,
        main="ICS layer justpassing and failing",
        xlab="Week",
        ylab="PageRank",
        col="orange",
        border="brown",
        lex.order=T,
        sep=":"
)
dev.off()
pdf(file="plots/tarEntJustPassFailPS.pdf",width = 8.3, height = 5.8)
boxplot(tarEnt~Week:justpass, notch=T,
        data=dfPS,
        main="PS layer justpassing and failing",
        xlab="Week",
        ylab="tarEnt",
        col="dodgerblue",
        border="blue",
        lex.order=T,
        sep=":"
)
dev.off()
pdf(file="plots/tarEntJustPassFailCD.pdf",width = 8.3, height = 5.8)
boxplot(tarEnt~Week:justpass, notch=T,
        data=dfCD,
        main="CD layer justpassing and failing",
        xlab="Week",
        ylab="tarEnt",
        col="orange",
        border="brown",
        lex.order=T,
        sep=":"
)
dev.off()
pdf(file="plots/tarEntJustPassFailICS.pdf",width = 8.3, height = 5.8)
boxplot(tarEnt~Week:justpass, notch=T,
        data=dfICS,
        main="ICS layer justpassing and failing",
        xlab="Week",
        ylab="tarEnt",
        col="orange",
        border="brown",
        lex.order=T,
        sep=":"
)
dev.off()
pdf(file="plots/hideJustPassFailPS.pdf",width = 8.3, height = 5.8)
boxplot(Hide~Week:justpass, notch=T,
        data=dfPS,
        main="PS layer justpassing and failing",
        xlab="Week",
        ylab="Hide",
        col="dodgerblue",
        border="blue",
        lex.order=T,
        sep=":"
)
dev.off()
pdf(file="plots/hideJustPassFailCD.pdf",width = 8.3, height = 5.8)
boxplot(Hide~Week:justpass, notch=T,
        data=dfCD,
        main="CD layer justpassing and failing",
        xlab="Week",
        ylab="Hide",
        col="orange",
        border="brown",
        lex.order=T,
        sep=":"
)

dev.off()
pdf(file="plots/hideJustPassFailICS.pdf",width = 8.3, height = 5.8)
boxplot(Hide~Week:justpass, notch=T,
        data=dfICS,
        main="ICS layer justpassing and failing",
        xlab="Week",
        ylab="Hide",
        col="orange",
        border="brown",
        lex.order=T,
        sep=":"
)
dev.off()
#####Statistical Tests######
ksDiff <- function(data, indices,pv,cm){
  dt<-data[indices,]
  if(pv=="pass"){
    if(cm=="PageRank"){
        c(ks.test(dt$PageRank[dt$pass==1], dt$PageRank[dt$pass==0])$statistic)
      }else if (cm=="tarEnt"){
        c(ks.test(dt$tarEnt[dt$pass==1], dt$tarEnt[dt$pass==0])$statistic)
      }else if (cm=="Hide"){
        c(ks.test(dt$Hide[dt$pass==1], dt$Hide[dt$pass==0])$statistic)
    }
      
  }else{
    if(cm=="PageRank"){
      c(ks.test(dt$PageRank[dt$justpass==1], dt$PageRank[dt$justpass==0])$statistic)
    }else if (cm=="tarEnt"){
      c(ks.test(dt$tarEnt[dt$justpass==1], dt$tarEnt[dt$justpass==0])$statistic)
    }else if (cm=="Hide"){
      c(ks.test(dt$Hide[dt$justpass==1], dt$Hide[dt$justpass==0])$statistic)
    }
  }
}

wilcoxDiff <- function(data, indices,pv,cm){
  dt<-data[indices,]
  if(pv=="pass"){
    if(cm=="PageRank"){
      c(wilcox.test(dt$PageRank[dt$pass==1], dt$PageRank[dt$pass==0])$statistic)
    }else if (cm=="tarEnt"){
      c(wilcox.test(dt$tarEnt[dt$pass==1], dt$tarEnt[dt$pass==0])$statistic)
    }else if (cm=="Hide"){
      c(wilcox.test(dt$Hide[dt$pass==1], dt$Hide[dt$pass==0])$statistic)
    }
    
  }else{
    if(cm=="PageRank"){
      c(wilcox.test(dt$PageRank[dt$justpass==1], dt$PageRank[dt$justpass==0])$statistic)
    }else if (cm=="tarEnt"){
      c(wilcox.test(dt$tarEnt[dt$justpass==1], dt$tarEnt[dt$justpass==0])$statistic)
    }else if (cm=="Hide"){
      c(wilcox.test(dt$Hide[dt$justpass==1], dt$Hide[dt$justpass==0])$statistic)
    }
  }
}

tDiff <- function(data, indices,pv,cm){
  dt<-data[indices,]
  if(pv=="pass"){
    if(cm=="PageRank"){
      c(t.test(dt$PageRank[dt$pass==1], dt$PageRank[dt$pass==0])$statistic)
    }else if (cm=="tarEnt"){
      c(t.test(dt$tarEnt[dt$pass==1], dt$tarEnt[dt$pass==0])$statistic)
    }else if (cm=="Hide"){
      c(t.test(dt$Hide[dt$pass==1], dt$Hide[dt$pass==0])$statistic)
    }
    
  }else{
    if(cm=="PageRank"){
      c(t.test(dt$PageRank[dt$justpass==1], dt$PageRank[dt$justpass==0])$statistic)
    }else if (cm=="tarEnt"){
      c(t.test(dt$tarEnt[dt$justpass==1], dt$tarEnt[dt$justpass==0])$statistic)
    }else if (cm=="Hide"){
      c(t.test(dt$Hide[dt$justpass==1], dt$Hide[dt$justpass==0])$statistic)
    }
  }
}

######KS tests#######
D<-matrix(NA,ncol=7,nrow=9)
p<-matrix(NA,ncol=7,nrow=9)
for(i in 1:7){
  D[1,i]<-ks.test(centPS[[i]]$PageRank[centPS[[i]]$pass==1],centPS[[i]]$PageRank[centPS[[i]]$pass==0])$statistic 
  D[2,i]<-ks.test(centCD[[i]]$PageRank[centCD[[i]]$pass==1],centCD[[i]]$PageRank[centCD[[i]]$pass==0])$statistic
  D[3,i]<-ks.test(centICS[[i]]$PageRank[centICS[[i]]$pass==1],centICS[[i]]$PageRank[centICS[[i]]$pass==0])$statistic
  D[4,i]<-ks.test(centPS[[i]]$tarEnt[centPS[[i]]$pass==1],centPS[[i]]$tarEnt[centPS[[i]]$pass==0])$statistic 
  D[5,i]<-ks.test(centCD[[i]]$tarEnt[centCD[[i]]$pass==1],centCD[[i]]$tarEnt[centCD[[i]]$pass==0])$statistic
  D[6,i]<-ks.test(centICS[[i]]$tarEnt[centICS[[i]]$pass==1],centICS[[i]]$tarEnt[centICS[[i]]$pass==0])$statistic
  D[7,i]<-ks.test(centPS[[i]]$Hide[centPS[[i]]$pass==1],centPS[[i]]$Hide[centPS[[i]]$pass==0])$statistic 
  D[8,i]<-ks.test(centCD[[i]]$Hide[centCD[[i]]$pass==1],centCD[[i]]$Hide[centCD[[i]]$pass==0])$statistic
  D[9,i]<-ks.test(centICS[[i]]$Hide[centICS[[i]]$pass==1],centICS[[i]]$Hide[centICS[[i]]$pass==0])$statistic
  p[1,i]<-ks.test(centPS[[i]]$PageRank[centPS[[i]]$pass==1],centPS[[i]]$PageRank[centPS[[i]]$pass==0])$p.value 
  p[2,i]<-ks.test(centCD[[i]]$PageRank[centCD[[i]]$pass==1],centCD[[i]]$PageRank[centCD[[i]]$pass==0])$p.value
  p[3,i]<-ks.test(centICS[[i]]$PageRank[centICS[[i]]$pass==1],centICS[[i]]$PageRank[centICS[[i]]$pass==0])$p.value
  p[4,i]<-ks.test(centPS[[i]]$tarEnt[centPS[[i]]$pass==1],centPS[[i]]$tarEnt[centPS[[i]]$pass==0])$p.value 
  p[5,i]<-ks.test(centCD[[i]]$tarEnt[centCD[[i]]$pass==1],centCD[[i]]$tarEnt[centCD[[i]]$pass==0])$p.value
  p[6,i]<-ks.test(centICS[[i]]$tarEnt[centICS[[i]]$pass==1],centICS[[i]]$tarEnt[centICS[[i]]$pass==0])$p.value
  p[7,i]<-ks.test(centPS[[i]]$Hide[centPS[[i]]$pass==1],centPS[[i]]$Hide[centPS[[i]]$pass==0])$p.value 
  p[8,i]<-ks.test(centCD[[i]]$Hide[centCD[[i]]$pass==1],centCD[[i]]$Hide[centCD[[i]]$pass==0])$p.value
  p[9,i]<-ks.test(centICS[[i]]$Hide[centICS[[i]]$pass==1],centICS[[i]]$Hide[centICS[[i]]$pass==0])$p.value
}

Dj<-matrix(NA,ncol=7,nrow=9)
pj<-matrix(NA,ncol=7,nrow=9)
for(i in 1:7){
  Dj[1,i]<-ks.test(centPS[[i]]$PageRank[centPS[[i]]$justpass==1],centPS[[i]]$PageRank[centPS[[i]]$justpass==0])$statistic 
  Dj[2,i]<-ks.test(centCD[[i]]$PageRank[centCD[[i]]$justpass==1],centCD[[i]]$PageRank[centCD[[i]]$justpass==0])$statistic
  Dj[3,i]<-ks.test(centICS[[i]]$PageRank[centICS[[i]]$justpass==1],centICS[[i]]$PageRank[centICS[[i]]$justpass==0])$statistic
  Dj[4,i]<-ks.test(centPS[[i]]$tarEnt[centPS[[i]]$justpass==1],centPS[[i]]$tarEnt[centPS[[i]]$justpass==0])$statistic 
  Dj[5,i]<-ks.test(centCD[[i]]$tarEnt[centCD[[i]]$justpass==1],centCD[[i]]$tarEnt[centCD[[i]]$justpass==0])$statistic
  Dj[6,i]<-ks.test(centICS[[i]]$tarEnt[centICS[[i]]$justpass==1],centICS[[i]]$tarEnt[centICS[[i]]$justpass==0])$statistic
  Dj[7,i]<-ks.test(centPS[[i]]$Hide[centPS[[i]]$justpass==1],centPS[[i]]$Hide[centPS[[i]]$justpass==0])$statistic 
  Dj[8,i]<-ks.test(centCD[[i]]$Hide[centCD[[i]]$justpass==1],centCD[[i]]$Hide[centCD[[i]]$justpass==0])$statistic
  Dj[9,i]<-ks.test(centICS[[i]]$Hide[centICS[[i]]$justpass==1],centICS[[i]]$Hide[centICS[[i]]$justpass==0])$statistic
  pj[1,i]<-ks.test(centPS[[i]]$PageRank[centPS[[i]]$justpass==1],centPS[[i]]$PageRank[centPS[[i]]$justpass==0])$p.value 
  pj[2,i]<-ks.test(centCD[[i]]$PageRank[centCD[[i]]$justpass==1],centCD[[i]]$PageRank[centCD[[i]]$justpass==0])$p.value
  pj[3,i]<-ks.test(centICS[[i]]$PageRank[centICS[[i]]$justpass==1],centICS[[i]]$PageRank[centICS[[i]]$justpass==0])$p.value
  pj[4,i]<-ks.test(centPS[[i]]$tarEnt[centPS[[i]]$justpass==1],centPS[[i]]$tarEnt[centPS[[i]]$justpass==0])$p.value 
  pj[5,i]<-ks.test(centCD[[i]]$tarEnt[centCD[[i]]$justpass==1],centCD[[i]]$tarEnt[centCD[[i]]$justpass==0])$p.value
  pj[6,i]<-ks.test(centICS[[i]]$tarEnt[centICS[[i]]$justpass==1],centICS[[i]]$tarEnt[centICS[[i]]$justpass==0])$p.value
  pj[7,i]<-ks.test(centPS[[i]]$Hide[centPS[[i]]$justpass==1],centPS[[i]]$Hide[centPS[[i]]$justpass==0])$p.value 
  pj[8,i]<-ks.test(centCD[[i]]$Hide[centCD[[i]]$justpass==1],centCD[[i]]$Hide[centCD[[i]]$justpass==0])$p.value
  pj[9,i]<-ks.test(centICS[[i]]$Hide[centICS[[i]]$justpass==1],centICS[[i]]$Hide[centICS[[i]]$justpass==0])$p.value
}

plot(c(D),c(p),log="y",xlim = c(0.2,0.45), xlab = "D",ylab="p-value",main="Results from KS-tests pass/fail")
abline(h=0.05)
abline(h=0.01)
abline(h=0.001)
abline(h=0.0001)
abline(v=0.41) #p<1e-4
abline(v=0.36)#p<1e-3
abline(v=0.302)#p<1e-2
abline(v=0.25)#p<5*1e-2
text(0.407,1.2,"****",)
text(0.357,1.2,"***")
text(0.299,1.2,"**")
text(0.247,1.2,"*")


plot(c(Dj),c(pj),log="y",xlim = c(0.3,0.45),ylim=c(0.001,0.1), xlab = "D",ylab="p-value",main="Results from KS-tests just pass/fail")
abline(h=0.05)
abline(h=0.01)
abline(h=0.001)
abline(v=0.336)#p<0.05
abline(v=0.403)#p<0.01



plot(c(Dj),c(pj),log="y",xlim = c(0.3,0.45),ylim=c(0.001,0.1), xlab = "D",ylab="p-value",main="Results from KS-tests just pass/fail")
abline(h=0.05)
abline(h=0.01)
abline(h=0.001)
abline(v=0.336)#p<0.05
abline(v=0.403)#p<0.01

text(0.333,0.1,"*",)
text(0.400,0.1,"**")
####Perform bootstrap KS tests####
#PS####

ks_PS_PR<-list()
for (i in 1:7){
  ks_PS_PR[[i]]<-boot(centPS[[i]], ksDiff, R=1000,pv="pass",cm="PageRank")
}
ks_PS_PR_D<-c(ks_PS_PR[[1]]$t0,ks_PS_PR[[2]]$t0,ks_PS_PR[[3]]$t0,ks_PS_PR[[4]]$t0,ks_PS_PR[[5]]$t0,
              ks_PS_PR[[6]]$t0,ks_PS_PR[[7]]$t0)
ks_PS_PR_D_SD<-c(sd(ks_PS_PR[[1]]$t),sd(ks_PS_PR[[2]]$t),sd(ks_PS_PR[[3]]$t),sd(ks_PS_PR[[4]]$t),sd(ks_PS_PR[[5]]$t),
                 sd(ks_PS_PR[[6]]$t),sd(ks_PS_PR[[7]]$t))


ks_PS_TE<-list()
for (i in 1:7){
  ks_PS_TE[[i]]<-boot(centPS[[i]], ksDiff, R=1000,pv="pass",cm="tarEnt")
}
ks_PS_TE_D<-c(ks_PS_TE[[1]]$t0,ks_PS_TE[[2]]$t0,ks_PS_TE[[3]]$t0,ks_PS_TE[[4]]$t0,ks_PS_TE[[5]]$t0,
              ks_PS_TE[[6]]$t0,ks_PS_TE[[7]]$t0)
ks_PS_TE_D_SD<-c(sd(ks_PS_TE[[1]]$t),sd(ks_PS_TE[[2]]$t),sd(ks_PS_TE[[3]]$t),sd(ks_PS_TE[[4]]$t),sd(ks_PS_TE[[5]]$t),
                 sd(ks_PS_TE[[6]]$t),sd(ks_PS_TE[[7]]$t))

ks_PS_H<-list()
for (i in 1:7){
  ks_PS_H[[i]]<-boot(centPS[[i]], ksDiff, R=1000,pv="pass",cm="Hide")
}
ks_PS_H_D<-c(ks_PS_H[[1]]$t0,ks_PS_H[[2]]$t0,ks_PS_H[[3]]$t0,ks_PS_H[[4]]$t0,ks_PS_H[[5]]$t0,
              ks_PS_H[[6]]$t0,ks_PS_H[[7]]$t0)
ks_PS_H_D_SD<-c(sd(ks_PS_H[[1]]$t),sd(ks_PS_H[[2]]$t),sd(ks_PS_H[[3]]$t),sd(ks_PS_H[[4]]$t),sd(ks_PS_H[[5]]$t),
                 sd(ks_PS_H[[6]]$t),sd(ks_PS_H[[7]]$t))

#CD####
ks_CD_PR<-list()
for (i in 1:7){
  ks_CD_PR[[i]]<-boot(centCD[[i]], ksDiff, R=1000,pv="pass",cm="PageRank")
}
ks_CD_PR_D<-c(ks_CD_PR[[1]]$t0,ks_CD_PR[[2]]$t0,ks_CD_PR[[3]]$t0,ks_CD_PR[[4]]$t0,ks_CD_PR[[5]]$t0,
              ks_CD_PR[[6]]$t0,ks_CD_PR[[7]]$t0)
ks_CD_PR_D_SD<-c(sd(ks_CD_PR[[1]]$t),sd(ks_CD_PR[[2]]$t),sd(ks_CD_PR[[3]]$t),sd(ks_CD_PR[[4]]$t),sd(ks_CD_PR[[5]]$t),
                 sd(ks_CD_PR[[6]]$t),sd(ks_CD_PR[[7]]$t))


ks_CD_TE<-list()
for (i in 1:7){
  ks_CD_TE[[i]]<-boot(centCD[[i]], ksDiff, R=1000,pv="pass",cm="tarEnt")
}
ks_CD_TE_D<-c(ks_CD_TE[[1]]$t0,ks_CD_TE[[2]]$t0,ks_CD_TE[[3]]$t0,ks_CD_TE[[4]]$t0,ks_CD_TE[[5]]$t0,
              ks_CD_TE[[6]]$t0,ks_CD_TE[[7]]$t0)
ks_CD_TE_D_SD<-c(sd(ks_CD_TE[[1]]$t),sd(ks_CD_TE[[2]]$t),sd(ks_CD_TE[[3]]$t),sd(ks_CD_TE[[4]]$t),sd(ks_CD_TE[[5]]$t),
                 sd(ks_CD_TE[[6]]$t),sd(ks_CD_TE[[7]]$t))

ks_CD_H<-list()
for (i in 1:7){
  ks_CD_H[[i]]<-boot(centCD[[i]], ksDiff, R=1000,pv="pass",cm="Hide")
}
ks_CD_H_D<-c(ks_CD_H[[1]]$t0,ks_CD_H[[2]]$t0,ks_CD_H[[3]]$t0,ks_CD_H[[4]]$t0,ks_CD_H[[5]]$t0,
             ks_CD_H[[6]]$t0,ks_CD_H[[7]]$t0)
ks_CD_H_D_SD<-c(sd(ks_CD_H[[1]]$t),sd(ks_CD_H[[2]]$t),sd(ks_CD_H[[3]]$t),sd(ks_CD_H[[4]]$t),sd(ks_CD_H[[5]]$t),
                sd(ks_CD_H[[6]]$t),sd(ks_CD_H[[7]]$t))


#ICS####
ks_ICS_PR<-list()
for (i in 1:7){
  ks_ICS_PR[[i]]<-boot(centICS[[i]], ksDiff, R=1000,pv="pass",cm="PageRank")
}
ks_ICS_PR_D<-c(ks_ICS_PR[[1]]$t0,ks_ICS_PR[[2]]$t0,ks_ICS_PR[[3]]$t0,ks_ICS_PR[[4]]$t0,ks_ICS_PR[[5]]$t0,
              ks_ICS_PR[[6]]$t0,ks_ICS_PR[[7]]$t0)
ks_ICS_PR_D_SD<-c(sd(ks_ICS_PR[[1]]$t),sd(ks_ICS_PR[[2]]$t),sd(ks_ICS_PR[[3]]$t),sd(ks_ICS_PR[[4]]$t),sd(ks_ICS_PR[[5]]$t),
                 sd(ks_ICS_PR[[6]]$t),sd(ks_ICS_PR[[7]]$t))


ks_ICS_TE<-list()
for (i in 1:7){
  ks_ICS_TE[[i]]<-boot(centICS[[i]], ksDiff, R=1000,pv="pass",cm="tarEnt")
}
ks_ICS_TE_D<-c(ks_ICS_TE[[1]]$t0,ks_ICS_TE[[2]]$t0,ks_ICS_TE[[3]]$t0,ks_ICS_TE[[4]]$t0,ks_ICS_TE[[5]]$t0,
              ks_ICS_TE[[6]]$t0,ks_ICS_TE[[7]]$t0)
ks_ICS_TE_D_SD<-c(sd(ks_ICS_TE[[1]]$t),sd(ks_ICS_TE[[2]]$t),sd(ks_ICS_TE[[3]]$t),sd(ks_ICS_TE[[4]]$t),sd(ks_ICS_TE[[5]]$t),
                 sd(ks_ICS_TE[[6]]$t),sd(ks_ICS_TE[[7]]$t))

ks_ICS_H<-list()
for (i in 1:7){
  ks_ICS_H[[i]]<-boot(centICS[[i]], ksDiff, R=1000,pv="pass",cm="Hide")
}
ks_ICS_H_D<-c(ks_ICS_H[[1]]$t0,ks_ICS_H[[2]]$t0,ks_ICS_H[[3]]$t0,ks_ICS_H[[4]]$t0,ks_ICS_H[[5]]$t0,
             ks_ICS_H[[6]]$t0,ks_ICS_H[[7]]$t0)
ks_ICS_H_D_SD<-c(sd(ks_ICS_H[[1]]$t),sd(ks_ICS_H[[2]]$t),sd(ks_ICS_H[[3]]$t),sd(ks_ICS_H[[4]]$t),sd(ks_ICS_H[[5]]$t),
                sd(ks_ICS_H[[6]]$t),sd(ks_ICS_H[[7]]$t))

#####PLOT WEEKLY KS-DIFFERENCES####

dev.off()
pdf(file="plots/kstestsPerWeekPR.pdf",width = 5.3, height = 4.1)
x<-c(1:7)
plot(x, ks_PS_PR_D,
     ylim=range(0, max(ks_PS_PR_D+ks_PS_PR_D_SD)),
     pch=19, xlab="Week", ylab="D",
     main="Per week KS-test for differences: pass vs. fail",type="b",sub="PageRank"
)
lines(x+0.15,ks_CD_PR_D,type="b",col="darkblue",pch=4)
lines(x-0.15,ks_ICS_PR_D,type="b",col="darkred",pch=5)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, ks_PS_PR_D-4.89*ks_PS_PR_D_SD/sqrt(1000), x, ks_PS_PR_D+4.89*ks_PS_PR_D_SD/sqrt(1000), length=0.05, angle=90, code=3)
arrows(x+0.15, ks_CD_PR_D-4.89*ks_CD_PR_D_SD/sqrt(1000), x+0.15, ks_CD_PR_D+4.89*ks_CD_PR_D_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkblue")
arrows(x-0.15, ks_ICS_PR_D-4.89*ks_ICS_PR_D_SD/sqrt(1000), x-0.15, ks_ICS_PR_D+4.89*ks_ICS_PR_D_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkred")
abline(h=0.25) #p=0.05 line
text(x = 7, y = 0.235, "*") 
abline(h=0.302) #p=0.01 line
text(x = 7, y = 0.292, "**") 
abline(h=0.36) #p=0.01 line
text(x = 7, y = 0.35, "***") 

legend(6,0.15,c("PS","CD","ICS"),pch=c(19,4,5),col=c("black","darkblue","darkred"))



dev.off()


pdf(file="plots/kstestsPerWeekTE.pdf",width = 5.3, height = 4.1)
x<-c(1:7)
plot(x, ks_PS_TE_D,
     ylim=range(0, max(ks_PS_TE_D+ks_PS_TE_D_SD)),
     pch=19, xlab="Week", ylab="D",
     main="Per week KS-test for differences: pass vs. fail",type="b",sub="Target Entropy"
)
lines(x+0.15,ks_CD_TE_D,type="b",col="darkblue",pch=4)
lines(x-0.15,ks_ICS_TE_D,type="b",col="darkred",pch=5)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, ks_PS_TE_D-4.89*ks_PS_TE_D_SD/sqrt(1000), x, ks_PS_TE_D+4.89*ks_PS_TE_D_SD/sqrt(1000), length=0.05, angle=90, code=3)
arrows(x+0.15, ks_CD_TE_D-4.89*ks_CD_TE_D_SD/sqrt(1000), x+0.15, ks_CD_TE_D+4.89*ks_CD_TE_D_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkblue")
arrows(x-0.15, ks_ICS_TE_D-4.89*ks_ICS_TE_D_SD/sqrt(1000), x-0.15, ks_ICS_TE_D+4.89*ks_ICS_TE_D_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkred")
abline(h=0.25) #p=0.05 line
text(x = 7, y = 0.235, "*") 
abline(h=0.302) #p=0.01 line
text(x = 7, y = 0.292, "**") 
abline(h=0.36) #p=0.01 line
text(x = 7, y = 0.35, "***") 


legend(6,0.15,c("PS","CD","ICS"),pch=c(19,4,5),col=c("black","darkblue","darkred"))


dev.off()
pdf(file="plots/kstestsPerWeekH.pdf",width = 5.3, height = 4.1)
x<-c(1:7)
plot(x, ks_PS_H_D,
     ylim=range(0, max(ks_ICS_H_D+ks_ICS_H_D_SD)),
     pch=19, xlab="Week", ylab="D",
     main="Per week KS-test for differences: pass vs. fail",type="b",sub="Hide"
)
lines(x+0.15,ks_CD_H_D,type="b",col="darkblue",pch=4)
lines(x-0.15,ks_ICS_H_D,type="b",col="darkred",pch=5)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, ks_PS_H_D-4.89*ks_PS_H_D_SD/sqrt(1000), x, ks_PS_H_D+4.89*ks_PS_H_D_SD/sqrt(1000), length=0.05, angle=90, code=3)
arrows(x+0.15, ks_CD_H_D-4.89*ks_CD_H_D_SD/sqrt(1000), x+0.15, ks_CD_H_D+4.89*ks_CD_H_D_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkblue")
arrows(x-0.15, ks_ICS_H_D-4.89*ks_ICS_H_D_SD/sqrt(1000), x-0.15, ks_ICS_H_D+4.89*ks_ICS_H_D_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkred")
abline(h=0.25) #p=0.05 line
text(x = 7, y = 0.235, "*") 
abline(h=0.302) #p=0.01 line
text(x = 7, y = 0.292, "**") 
abline(h=0.36) #p=0.01 line
text(x = 7, y = 0.35, "***") 
legend(6,0.15,c("PS","CD","ICS"),pch=c(19,4,5),col=c("black","darkblue","darkred"))

dev.off()

####JP Perform bootstrap####
#PS

ks_jp_PS_PR<-list()
for (i in 1:7){
  ks_jp_PS_PR[[i]]<-boot(centPS[[i]], ksDiff, R=1000,pv="justpass",cm="PageRank")
}
ks_jp_PS_PR_D<-c(ks_jp_PS_PR[[1]]$t0,ks_jp_PS_PR[[2]]$t0,ks_jp_PS_PR[[3]]$t0,ks_jp_PS_PR[[4]]$t0,ks_jp_PS_PR[[5]]$t0,
              ks_jp_PS_PR[[6]]$t0,ks_jp_PS_PR[[7]]$t0)
ks_jp_PS_PR_D_SD<-c(sd(ks_jp_PS_PR[[1]]$t),sd(ks_jp_PS_PR[[2]]$t),sd(ks_jp_PS_PR[[3]]$t),sd(ks_jp_PS_PR[[4]]$t),sd(ks_jp_PS_PR[[5]]$t),
                 sd(ks_jp_PS_PR[[6]]$t),sd(ks_jp_PS_PR[[7]]$t))


ks_jp_PS_TE<-list()
for (i in 1:7){
  ks_jp_PS_TE[[i]]<-boot(centPS[[i]], ksDiff, R=1000,pv="justpass",cm="tarEnt")
}
ks_jp_PS_TE_D<-c(ks_jp_PS_TE[[1]]$t0,ks_jp_PS_TE[[2]]$t0,ks_jp_PS_TE[[3]]$t0,ks_jp_PS_TE[[4]]$t0,ks_jp_PS_TE[[5]]$t0,
              ks_jp_PS_TE[[6]]$t0,ks_jp_PS_TE[[7]]$t0)
ks_jp_PS_TE_D_SD<-c(sd(ks_jp_PS_TE[[1]]$t),sd(ks_jp_PS_TE[[2]]$t),sd(ks_jp_PS_TE[[3]]$t),sd(ks_jp_PS_TE[[4]]$t),sd(ks_jp_PS_TE[[5]]$t),
                 sd(ks_jp_PS_TE[[6]]$t),sd(ks_jp_PS_TE[[7]]$t))

ks_jp_PS_H<-list()
for (i in 1:7){
  ks_jp_PS_H[[i]]<-boot(centPS[[i]], ksDiff, R=1000,pv="justpass",cm="Hide")
}
ks_jp_PS_H_D<-c(ks_jp_PS_H[[1]]$t0,ks_jp_PS_H[[2]]$t0,ks_jp_PS_H[[3]]$t0,ks_jp_PS_H[[4]]$t0,ks_jp_PS_H[[5]]$t0,
             ks_jp_PS_H[[6]]$t0,ks_jp_PS_H[[7]]$t0)
ks_jp_PS_H_D_SD<-c(sd(ks_jp_PS_H[[1]]$t),sd(ks_jp_PS_H[[2]]$t),sd(ks_jp_PS_H[[3]]$t),sd(ks_jp_PS_H[[4]]$t),sd(ks_jp_PS_H[[5]]$t),
                sd(ks_jp_PS_H[[6]]$t),sd(ks_jp_PS_H[[7]]$t))

#CD
ks_jp_CD_PR<-list()
for (i in 1:7){
  ks_jp_CD_PR[[i]]<-boot(centCD[[i]], ksDiff, R=1000,pv="justpass",cm="PageRank")
}
ks_jp_CD_PR_D<-c(ks_jp_CD_PR[[1]]$t0,ks_jp_CD_PR[[2]]$t0,ks_jp_CD_PR[[3]]$t0,ks_jp_CD_PR[[4]]$t0,ks_jp_CD_PR[[5]]$t0,
              ks_jp_CD_PR[[6]]$t0,ks_jp_CD_PR[[7]]$t0)
ks_jp_CD_PR_D_SD<-c(sd(ks_jp_CD_PR[[1]]$t),sd(ks_jp_CD_PR[[2]]$t),sd(ks_jp_CD_PR[[3]]$t),sd(ks_jp_CD_PR[[4]]$t),sd(ks_jp_CD_PR[[5]]$t),
                 sd(ks_jp_CD_PR[[6]]$t),sd(ks_jp_CD_PR[[7]]$t))


ks_jp_CD_TE<-list()
for (i in 1:7){
  ks_jp_CD_TE[[i]]<-boot(centCD[[i]], ksDiff, R=1000,pv="justpass",cm="tarEnt")
}
ks_jp_CD_TE_D<-c(ks_jp_CD_TE[[1]]$t0,ks_jp_CD_TE[[2]]$t0,ks_jp_CD_TE[[3]]$t0,ks_jp_CD_TE[[4]]$t0,ks_jp_CD_TE[[5]]$t0,
              ks_jp_CD_TE[[6]]$t0,ks_jp_CD_TE[[7]]$t0)
ks_jp_CD_TE_D_SD<-c(sd(ks_jp_CD_TE[[1]]$t),sd(ks_jp_CD_TE[[2]]$t),sd(ks_jp_CD_TE[[3]]$t),sd(ks_jp_CD_TE[[4]]$t),sd(ks_jp_CD_TE[[5]]$t),
                 sd(ks_jp_CD_TE[[6]]$t),sd(ks_jp_CD_TE[[7]]$t))

ks_jp_CD_H<-list()
for (i in 1:7){
  ks_jp_CD_H[[i]]<-boot(centCD[[i]], ksDiff, R=1000,pv="justpass",cm="Hide")
}
ks_jp_CD_H_D<-c(ks_jp_CD_H[[1]]$t0,ks_jp_CD_H[[2]]$t0,ks_jp_CD_H[[3]]$t0,ks_jp_CD_H[[4]]$t0,ks_jp_CD_H[[5]]$t0,
             ks_jp_CD_H[[6]]$t0,ks_jp_CD_H[[7]]$t0)
ks_jp_CD_H_D_SD<-c(sd(ks_jp_CD_H[[1]]$t),sd(ks_jp_CD_H[[2]]$t),sd(ks_jp_CD_H[[3]]$t),sd(ks_jp_CD_H[[4]]$t),sd(ks_jp_CD_H[[5]]$t),
                sd(ks_jp_CD_H[[6]]$t),sd(ks_jp_CD_H[[7]]$t))


#ICS
ks_jp_ICS_PR<-list()
for (i in 1:7){
  ks_jp_ICS_PR[[i]]<-boot(centICS[[i]], ksDiff, R=1000,pv="justpass",cm="PageRank")
}
ks_jp_ICS_PR_D<-c(ks_jp_ICS_PR[[1]]$t0,ks_jp_ICS_PR[[2]]$t0,ks_jp_ICS_PR[[3]]$t0,ks_jp_ICS_PR[[4]]$t0,ks_jp_ICS_PR[[5]]$t0,
               ks_jp_ICS_PR[[6]]$t0,ks_jp_ICS_PR[[7]]$t0)
ks_jp_ICS_PR_D_SD<-c(sd(ks_jp_ICS_PR[[1]]$t),sd(ks_jp_ICS_PR[[2]]$t),sd(ks_jp_ICS_PR[[3]]$t),sd(ks_jp_ICS_PR[[4]]$t),sd(ks_jp_ICS_PR[[5]]$t),
                  sd(ks_jp_ICS_PR[[6]]$t),sd(ks_jp_ICS_PR[[7]]$t))


ks_jp_ICS_TE<-list()
for (i in 1:7){
  ks_jp_ICS_TE[[i]]<-boot(centICS[[i]], ksDiff, R=1000,pv="justpass",cm="tarEnt")
}
ks_jp_ICS_TE_D<-c(ks_jp_ICS_TE[[1]]$t0,ks_jp_ICS_TE[[2]]$t0,ks_jp_ICS_TE[[3]]$t0,ks_jp_ICS_TE[[4]]$t0,ks_jp_ICS_TE[[5]]$t0,
               ks_jp_ICS_TE[[6]]$t0,ks_jp_ICS_TE[[7]]$t0)
ks_jp_ICS_TE_D_SD<-c(sd(ks_jp_ICS_TE[[1]]$t),sd(ks_jp_ICS_TE[[2]]$t),sd(ks_jp_ICS_TE[[3]]$t),sd(ks_jp_ICS_TE[[4]]$t),sd(ks_jp_ICS_TE[[5]]$t),
                  sd(ks_jp_ICS_TE[[6]]$t),sd(ks_jp_ICS_TE[[7]]$t))

ks_jp_ICS_H<-list()
for (i in 1:7){
  ks_jp_ICS_H[[i]]<-boot(centICS[[i]], ksDiff, R=1000,pv="justpass",cm="Hide")
}
ks_jp_ICS_H_D<-c(ks_jp_ICS_H[[1]]$t0,ks_jp_ICS_H[[2]]$t0,ks_jp_ICS_H[[3]]$t0,ks_jp_ICS_H[[4]]$t0,ks_jp_ICS_H[[5]]$t0,
              ks_jp_ICS_H[[6]]$t0,ks_jp_ICS_H[[7]]$t0)
ks_jp_ICS_H_D_SD<-c(sd(ks_jp_ICS_H[[1]]$t),sd(ks_jp_ICS_H[[2]]$t),sd(ks_jp_ICS_H[[3]]$t),sd(ks_jp_ICS_H[[4]]$t),sd(ks_jp_ICS_H[[5]]$t),
                 sd(ks_jp_ICS_H[[6]]$t),sd(ks_jp_ICS_H[[7]]$t))

#####PLOT JP WEEKLY KS-DIFFERENCES####

dev.off()
pdf(file="plots/kstestsPerWeekPR_jp.pdf",width = 5.3, height = 4.1)
x<-c(1:7)
plot(x, ks_jp_PS_PR_D,
     ylim=range(0, max(ks_jp_ICS_PR_D+ks_jp_ICS_PR_D_SD)),
     pch=19, xlab="Week", ylab="D",
     main="Per week KS-test for differences: just pass vs. just fail",type="b",sub="PageRank"
)
lines(x+0.15,ks_jp_CD_PR_D,type="b",col="darkblue",pch=4)
lines(x-0.15,ks_jp_ICS_PR_D,type="b",col="darkred",pch=5)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, ks_jp_PS_PR_D-4.89*ks_jp_PS_PR_D_SD/sqrt(1000), x, ks_jp_PS_PR_D+4.89*ks_jp_PS_PR_D_SD/sqrt(1000), length=0.05, angle=90, code=3)
arrows(x+0.15, ks_jp_CD_PR_D-4.89*ks_jp_CD_PR_D_SD/sqrt(1000), x+0.15, ks_jp_CD_PR_D+4.89*ks_jp_CD_PR_D_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkblue")
arrows(x-0.15, ks_jp_ICS_PR_D-4.89*ks_jp_ICS_PR_D_SD/sqrt(1000), x-0.15, ks_jp_ICS_PR_D+4.89*ks_jp_ICS_PR_D_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkred")
abline(h=0.336) #p=0.05 line
text(x = 7, y = 0.333, "*") 
abline(h=0.403) #p=0.01 line
text(x = 7, y = 0.400, "**") 
legend(6,0.15,c("PS","CD","ICS"),pch=c(19,4,5),col=c("black","darkblue","darkred"))
dev.off()


pdf(file="plots/kstestsPerWeekTE_jp.pdf",width = 5.3, height = 4.1)
x<-c(1:7)
plot(x, ks_jp_PS_TE_D,
     ylim=range(0, max(ks_jp_ICS_TE_D+ks_jp_ICS_TE_D_SD)),
     pch=19, xlab="Week", ylab="D",
     main="Per week KS-test for differences: just pass vs. just fail",type="b",sub="Target Entropy"
)
lines(x+0.15,ks_jp_CD_TE_D,type="b",col="darkblue",pch=4)
lines(x-0.15,ks_jp_ICS_TE_D,type="b",col="darkred",pch=5)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, ks_jp_PS_TE_D-4.89*ks_jp_PS_TE_D_SD/sqrt(1000), x, ks_jp_PS_TE_D+4.89*ks_jp_PS_TE_D_SD/sqrt(1000), length=0.05, angle=90, code=3)
arrows(x+0.15, ks_jp_CD_TE_D-4.89*ks_jp_CD_TE_D_SD/sqrt(1000), x+0.15, ks_jp_CD_TE_D+4.89*ks_jp_CD_TE_D_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkblue")
arrows(x-0.15, ks_jp_ICS_TE_D-4.89*ks_jp_ICS_TE_D_SD/sqrt(1000), x-0.15, ks_jp_ICS_TE_D+4.89*ks_jp_ICS_TE_D_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkred")
abline(h=0.336) #p=0.05 line
text(x = 7, y = 0.333, "*") 
abline(h=0.403) #p=0.01 line
text(x = 7, y = 0.400, "**") 
legend(6,0.15,c("PS","CD","ICS"),pch=c(19,4,5),col=c("black","darkblue","darkred"))
dev.off()

pdf(file="plots/kstestsPerWeekH_jp.pdf",width = 5.3, height = 4.1)
x<-c(1:7)
plot(x, ks_jp_PS_H_D,
     ylim=range(0, max(ks_jp_ICS_H_D+ks_jp_ICS_H_D_SD)),
     pch=19, xlab="Week", ylab="D",
     main="Per week KS-test for differences: just pass vs. just fail",type="b",sub="Hide"
)
lines(x+0.15,ks_jp_CD_H_D,type="b",col="darkblue",pch=4)
lines(x-0.15,ks_jp_ICS_H_D,type="b",col="darkred",pch=5)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, ks_jp_PS_H_D-4.89*ks_jp_PS_H_D_SD/sqrt(1000), x, ks_jp_PS_H_D+4.89*ks_jp_PS_H_D_SD/sqrt(1000), length=0.05, angle=90, code=3)
arrows(x+0.15, ks_jp_CD_H_D-4.89*ks_jp_CD_H_D_SD/sqrt(1000), x+0.15, ks_jp_CD_H_D+4.89*ks_jp_CD_H_D_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkblue")
arrows(x-0.15, ks_jp_ICS_H_D-4.89*ks_jp_ICS_H_D_SD/sqrt(1000), x-0.15, ks_jp_ICS_H_D+4.89*ks_jp_ICS_H_D_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkred")
abline(h=0.336) #p=0.05 line
text(x = 7, y = 0.333, "*") 
abline(h=0.403) #p=0.01 line
text(x = 7, y = 0.400, "**")  
legend(6,0.15,c("PS","CD","ICS"),pch=c(19,4,5),col=c("black","darkblue","darkred"))
dev.off()


####WILCOX TESTS####
W<-matrix(NA,ncol=7,nrow=9)
p<-matrix(NA,ncol=7,nrow=9)
for(i in 1:7){
  W[1,i]<-wilcox.test(centPS[[i]]$PageRank[centPS[[i]]$pass==1],centPS[[i]]$PageRank[centPS[[i]]$pass==0])$statistic 
  W[2,i]<-wilcox.test(centCD[[i]]$PageRank[centCD[[i]]$pass==1],centCD[[i]]$PageRank[centCD[[i]]$pass==0])$statistic
  W[3,i]<-wilcox.test(centICS[[i]]$PageRank[centICS[[i]]$pass==1],centICS[[i]]$PageRank[centICS[[i]]$pass==0])$statistic
  W[4,i]<-wilcox.test(centPS[[i]]$tarEnt[centPS[[i]]$pass==1],centPS[[i]]$tarEnt[centPS[[i]]$pass==0])$statistic 
  W[5,i]<-wilcox.test(centCD[[i]]$tarEnt[centCD[[i]]$pass==1],centCD[[i]]$tarEnt[centCD[[i]]$pass==0])$statistic
  W[6,i]<-wilcox.test(centICS[[i]]$tarEnt[centICS[[i]]$pass==1],centICS[[i]]$tarEnt[centICS[[i]]$pass==0])$statistic
  W[7,i]<-wilcox.test(centPS[[i]]$Hide[centPS[[i]]$pass==1],centPS[[i]]$Hide[centPS[[i]]$pass==0])$statistic 
  W[8,i]<-wilcox.test(centCD[[i]]$Hide[centCD[[i]]$pass==1],centCD[[i]]$Hide[centCD[[i]]$pass==0])$statistic
  W[9,i]<-wilcox.test(centICS[[i]]$Hide[centICS[[i]]$pass==1],centICS[[i]]$Hide[centICS[[i]]$pass==0])$statistic
  p[1,i]<-wilcox.test(centPS[[i]]$PageRank[centPS[[i]]$pass==1],centPS[[i]]$PageRank[centPS[[i]]$pass==0])$p.value 
  p[2,i]<-wilcox.test(centCD[[i]]$PageRank[centCD[[i]]$pass==1],centCD[[i]]$PageRank[centCD[[i]]$pass==0])$p.value
  p[3,i]<-wilcox.test(centICS[[i]]$PageRank[centICS[[i]]$pass==1],centICS[[i]]$PageRank[centICS[[i]]$pass==0])$p.value
  p[4,i]<-wilcox.test(centPS[[i]]$tarEnt[centPS[[i]]$pass==1],centPS[[i]]$tarEnt[centPS[[i]]$pass==0])$p.value 
  p[5,i]<-wilcox.test(centCD[[i]]$tarEnt[centCD[[i]]$pass==1],centCD[[i]]$tarEnt[centCD[[i]]$pass==0])$p.value
  p[6,i]<-wilcox.test(centICS[[i]]$tarEnt[centICS[[i]]$pass==1],centICS[[i]]$tarEnt[centICS[[i]]$pass==0])$p.value
  p[7,i]<-wilcox.test(centPS[[i]]$Hide[centPS[[i]]$pass==1],centPS[[i]]$Hide[centPS[[i]]$pass==0])$p.value 
  p[8,i]<-wilcox.test(centCD[[i]]$Hide[centCD[[i]]$pass==1],centCD[[i]]$Hide[centCD[[i]]$pass==0])$p.value
  p[9,i]<-wilcox.test(centICS[[i]]$Hide[centICS[[i]]$pass==1],centICS[[i]]$Hide[centICS[[i]]$pass==0])$p.value
}

Wj<-matrix(NA,ncol=7,nrow=9)
pj<-matrix(NA,ncol=7,nrow=9)
for(i in 1:7){
  Wj[1,i]<-wilcox.test(centPS[[i]]$PageRank[centPS[[i]]$justpass==1],centPS[[i]]$PageRank[centPS[[i]]$justpass==0])$statistic 
  Wj[2,i]<-wilcox.test(centCD[[i]]$PageRank[centCD[[i]]$justpass==1],centCD[[i]]$PageRank[centCD[[i]]$justpass==0])$statistic
  Wj[3,i]<-wilcox.test(centICS[[i]]$PageRank[centICS[[i]]$justpass==1],centICS[[i]]$PageRank[centICS[[i]]$justpass==0])$statistic
  Wj[4,i]<-wilcox.test(centPS[[i]]$tarEnt[centPS[[i]]$justpass==1],centPS[[i]]$tarEnt[centPS[[i]]$justpass==0])$statistic 
  Wj[5,i]<-wilcox.test(centCD[[i]]$tarEnt[centCD[[i]]$justpass==1],centCD[[i]]$tarEnt[centCD[[i]]$justpass==0])$statistic
  Wj[6,i]<-wilcox.test(centICS[[i]]$tarEnt[centICS[[i]]$justpass==1],centICS[[i]]$tarEnt[centICS[[i]]$justpass==0])$statistic
  Wj[7,i]<-wilcox.test(centPS[[i]]$Hide[centPS[[i]]$justpass==1],centPS[[i]]$Hide[centPS[[i]]$justpass==0])$statistic 
  Wj[8,i]<-wilcox.test(centCD[[i]]$Hide[centCD[[i]]$justpass==1],centCD[[i]]$Hide[centCD[[i]]$justpass==0])$statistic
  Wj[9,i]<-wilcox.test(centICS[[i]]$Hide[centICS[[i]]$justpass==1],centICS[[i]]$Hide[centICS[[i]]$justpass==0])$statistic
  pj[1,i]<-wilcox.test(centPS[[i]]$PageRank[centPS[[i]]$justpass==1],centPS[[i]]$PageRank[centPS[[i]]$justpass==0])$p.value 
  pj[2,i]<-wilcox.test(centCD[[i]]$PageRank[centCD[[i]]$justpass==1],centCD[[i]]$PageRank[centCD[[i]]$justpass==0])$p.value
  pj[3,i]<-wilcox.test(centICS[[i]]$PageRank[centICS[[i]]$justpass==1],centICS[[i]]$PageRank[centICS[[i]]$justpass==0])$p.value
  pj[4,i]<-wilcox.test(centPS[[i]]$tarEnt[centPS[[i]]$justpass==1],centPS[[i]]$tarEnt[centPS[[i]]$justpass==0])$p.value 
  pj[5,i]<-wilcox.test(centCD[[i]]$tarEnt[centCD[[i]]$justpass==1],centCD[[i]]$tarEnt[centCD[[i]]$justpass==0])$p.value
  pj[6,i]<-wilcox.test(centICS[[i]]$tarEnt[centICS[[i]]$justpass==1],centICS[[i]]$tarEnt[centICS[[i]]$justpass==0])$p.value
  pj[7,i]<-wilcox.test(centPS[[i]]$Hide[centPS[[i]]$justpass==1],centPS[[i]]$Hide[centPS[[i]]$justpass==0])$p.value 
  pj[8,i]<-wilcox.test(centCD[[i]]$Hide[centCD[[i]]$justpass==1],centCD[[i]]$Hide[centCD[[i]]$justpass==0])$p.value
  pj[9,i]<-wilcox.test(centICS[[i]]$Hide[centICS[[i]]$justpass==1],centICS[[i]]$Hide[centICS[[i]]$justpass==0])$p.value
}

plot(c(W),c(p),xlim=c(1400,3600),log="y",xlab="W",ylab="p-value",main="Results from Wilcoxon-tests pass/fail")

abline(h=0.05)
abline(h=0.01)
abline(h=0.001)
abline(h=0.0001)
abline(v=2940)
abline(v=3100)
abline(v=3300)
abline(v=3450)
text(2910,1.2,"*")
text(3070,1.2,"**")
text(3270,1.2,"***")
text(3420,1.2,"****")
abline(v=1920)
abline(v=1760)
abline(v=1580)
text(1900,1.2,"*")
text(1730,1.2,"**")
text(1550,1.2,"***")

plot(c(Wj),c(pj),xlim=c(300,900),log="y",xlab="W",ylab="p-value",main="Results from Wilcoxon-tests just pass/fail")

abline(h=0.05)
abline(h=0.01)
abline(h=0.001)
abline(h=0.0001)
abline(v=806) #p<0.001
abline(v=750) #p<0.01
abline(v=700) #p<0.05
abline(v=392) #p<0.05
abline(v=343) #p<0.01
text(697,1.2,"*")
text(747,1.2,"**")
text(803,1.2,"***")
text(389,1.2,"*")
text(340,1.2,"**")

####WILCOX PS LAYER####
wilcox_PS_PR<-list()
for (i in 1:7){
  wilcox_PS_PR[[i]]<-boot(centPS[[i]], wilcoxDiff, R=1000,pv="pass",cm="PageRank")
}
wilcox_PS_PR_W<-c(wilcox_PS_PR[[1]]$t0,wilcox_PS_PR[[2]]$t0,wilcox_PS_PR[[3]]$t0,wilcox_PS_PR[[4]]$t0,wilcox_PS_PR[[5]]$t0,
              wilcox_PS_PR[[6]]$t0,wilcox_PS_PR[[7]]$t0)
wilcox_PS_PR_W_SD<-c(sd(wilcox_PS_PR[[1]]$t),sd(wilcox_PS_PR[[2]]$t),sd(wilcox_PS_PR[[3]]$t),sd(wilcox_PS_PR[[4]]$t),sd(wilcox_PS_PR[[5]]$t),
                  sd(wilcox_PS_PR[[6]]$t),sd(wilcox_PS_PR[[7]]$t))

wilcox_PS_TE<-list()
for (i in 1:7){
  wilcox_PS_TE[[i]]<-boot(centPS[[i]], wilcoxDiff, R=1000,pv="pass",cm="tarEnt")
}
wilcox_PS_TE_W<-c(wilcox_PS_TE[[1]]$t0,wilcox_PS_TE[[2]]$t0,wilcox_PS_TE[[3]]$t0,wilcox_PS_TE[[4]]$t0,wilcox_PS_TE[[5]]$t0,
                  wilcox_PS_TE[[6]]$t0,wilcox_PS_TE[[7]]$t0)
wilcox_PS_TE_W_SD<-c(sd(wilcox_PS_TE[[1]]$t),sd(wilcox_PS_TE[[2]]$t),sd(wilcox_PS_TE[[3]]$t),sd(wilcox_PS_TE[[4]]$t),sd(wilcox_PS_TE[[5]]$t),
                     sd(wilcox_PS_TE[[6]]$t),sd(wilcox_PS_TE[[7]]$t))

wilcox_PS_H<-list()
for (i in 1:7){
  wilcox_PS_H[[i]]<-boot(centPS[[i]], wilcoxDiff, R=1000,pv="pass",cm="Hide")
}
wilcox_PS_H_W<-c(wilcox_PS_H[[1]]$t0,wilcox_PS_H[[2]]$t0,wilcox_PS_H[[3]]$t0,wilcox_PS_H[[4]]$t0,wilcox_PS_H[[5]]$t0,
                  wilcox_PS_H[[6]]$t0,wilcox_PS_H[[7]]$t0)
wilcox_PS_H_W_SD<-c(sd(wilcox_PS_H[[1]]$t),sd(wilcox_PS_H[[2]]$t),sd(wilcox_PS_H[[3]]$t),sd(wilcox_PS_H[[4]]$t),sd(wilcox_PS_H[[5]]$t),
                     sd(wilcox_PS_H[[6]]$t),sd(wilcox_PS_H[[7]]$t))

####WILCOX CD LAYER####
wilcox_CD_PR<-list()
for (i in 1:7){
  wilcox_CD_PR[[i]]<-boot(centCD[[i]], wilcoxDiff, R=1000,pv="pass",cm="PageRank")
}
wilcox_CD_PR_W<-c(wilcox_CD_PR[[1]]$t0,wilcox_CD_PR[[2]]$t0,wilcox_CD_PR[[3]]$t0,wilcox_CD_PR[[4]]$t0,wilcox_CD_PR[[5]]$t0,
                  wilcox_CD_PR[[6]]$t0,wilcox_CD_PR[[7]]$t0)
wilcox_CD_PR_W_SD<-c(sd(wilcox_CD_PR[[1]]$t),sd(wilcox_CD_PR[[2]]$t),sd(wilcox_CD_PR[[3]]$t),sd(wilcox_CD_PR[[4]]$t),sd(wilcox_CD_PR[[5]]$t),
                     sd(wilcox_CD_PR[[6]]$t),sd(wilcox_CD_PR[[7]]$t))

wilcox_CD_TE<-list()
for (i in 1:7){
  wilcox_CD_TE[[i]]<-boot(centCD[[i]], wilcoxDiff, R=1000,pv="pass",cm="tarEnt")
}
wilcox_CD_TE_W<-c(wilcox_CD_TE[[1]]$t0,wilcox_CD_TE[[2]]$t0,wilcox_CD_TE[[3]]$t0,wilcox_CD_TE[[4]]$t0,wilcox_CD_TE[[5]]$t0,
                  wilcox_CD_TE[[6]]$t0,wilcox_CD_TE[[7]]$t0)
wilcox_CD_TE_W_SD<-c(sd(wilcox_CD_TE[[1]]$t),sd(wilcox_CD_TE[[2]]$t),sd(wilcox_CD_TE[[3]]$t),sd(wilcox_CD_TE[[4]]$t),sd(wilcox_CD_TE[[5]]$t),
                     sd(wilcox_CD_TE[[6]]$t),sd(wilcox_CD_TE[[7]]$t))

wilcox_CD_H<-list()
for (i in 1:7){
  wilcox_CD_H[[i]]<-boot(centCD[[i]], wilcoxDiff, R=1000,pv="pass",cm="Hide")
}
wilcox_CD_H_W<-c(wilcox_CD_H[[1]]$t0,wilcox_CD_H[[2]]$t0,wilcox_CD_H[[3]]$t0,wilcox_CD_H[[4]]$t0,wilcox_CD_H[[5]]$t0,
                 wilcox_CD_H[[6]]$t0,wilcox_CD_H[[7]]$t0)
wilcox_CD_H_W_SD<-c(sd(wilcox_CD_H[[1]]$t),sd(wilcox_CD_H[[2]]$t),sd(wilcox_CD_H[[3]]$t),sd(wilcox_CD_H[[4]]$t),sd(wilcox_CD_H[[5]]$t),
                    sd(wilcox_CD_H[[6]]$t),sd(wilcox_CD_H[[7]]$t))



####WILCOX ICS LAYER####
wilcox_ICS_PR<-list()
for (i in 1:7){
  wilcox_ICS_PR[[i]]<-boot(centCD[[i]], wilcoxDiff, R=1000,pv="pass",cm="PageRank")
}
wilcox_ICS_PR_W<-c(wilcox_ICS_PR[[1]]$t0,wilcox_ICS_PR[[2]]$t0,wilcox_ICS_PR[[3]]$t0,wilcox_ICS_PR[[4]]$t0,wilcox_ICS_PR[[5]]$t0,
                  wilcox_ICS_PR[[6]]$t0,wilcox_ICS_PR[[7]]$t0)
wilcox_ICS_PR_W_SD<-c(sd(wilcox_ICS_PR[[1]]$t),sd(wilcox_ICS_PR[[2]]$t),sd(wilcox_ICS_PR[[3]]$t),sd(wilcox_ICS_PR[[4]]$t),sd(wilcox_ICS_PR[[5]]$t),
                     sd(wilcox_ICS_PR[[6]]$t),sd(wilcox_ICS_PR[[7]]$t))

wilcox_ICS_TE<-list()
for (i in 1:7){
  wilcox_ICS_TE[[i]]<-boot(centCD[[i]], wilcoxDiff, R=1000,pv="pass",cm="tarEnt")
}
wilcox_ICS_TE_W<-c(wilcox_ICS_TE[[1]]$t0,wilcox_ICS_TE[[2]]$t0,wilcox_ICS_TE[[3]]$t0,wilcox_ICS_TE[[4]]$t0,wilcox_ICS_TE[[5]]$t0,
                  wilcox_ICS_TE[[6]]$t0,wilcox_ICS_TE[[7]]$t0)
wilcox_ICS_TE_W_SD<-c(sd(wilcox_ICS_TE[[1]]$t),sd(wilcox_ICS_TE[[2]]$t),sd(wilcox_ICS_TE[[3]]$t),sd(wilcox_ICS_TE[[4]]$t),sd(wilcox_ICS_TE[[5]]$t),
                     sd(wilcox_ICS_TE[[6]]$t),sd(wilcox_ICS_TE[[7]]$t))

wilcox_ICS_H<-list()
for (i in 1:7){
  wilcox_ICS_H[[i]]<-boot(centCD[[i]], wilcoxDiff, R=1000,pv="pass",cm="Hide")
}
wilcox_ICS_H_W<-c(wilcox_ICS_H[[1]]$t0,wilcox_ICS_H[[2]]$t0,wilcox_ICS_H[[3]]$t0,wilcox_ICS_H[[4]]$t0,wilcox_ICS_H[[5]]$t0,
                 wilcox_ICS_H[[6]]$t0,wilcox_ICS_H[[7]]$t0)
wilcox_ICS_H_W_SD<-c(sd(wilcox_ICS_H[[1]]$t),sd(wilcox_ICS_H[[2]]$t),sd(wilcox_ICS_H[[3]]$t),sd(wilcox_ICS_H[[4]]$t),sd(wilcox_ICS_H[[5]]$t),
                    sd(wilcox_ICS_H[[6]]$t),sd(wilcox_ICS_H[[7]]$t))





####PLOT WEEKLY WILCOXON TESTS####
##PR####
plot(x, wilcox_PS_PR_W,
     ylim=range(0, max(wilcox_PS_PR_W+wilcox_PS_PR_W_SD)),
     pch=19, xlab="Weeks", ylab="W",sub="PageRank",
     main="Per Wilcoxon test for difference between pass and fail",type="b"
)
lines(x+0.15,wilcox_CD_PR_W,type="b",col="darkblue",pch=4)
lines(x-0.15,wilcox_ICS_PR_W,type="b",col="darkred",pch=5)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, wilcox_PS_PR_W-4.89*wilcox_PS_PR_W_SD/sqrt(1000), x, wilcox_PS_PR_W+4.89*wilcox_PS_PR_W_SD/sqrt(1000), length=0.05, angle=90, code=3)
arrows(x+0.15, wilcox_CD_PR_W-4.89*wilcox_CD_PR_W_SD/sqrt(1000), x+0.15, wilcox_CD_PR_W+4.89*wilcox_CD_PR_W_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkblue")
arrows(x-0.15, wilcox_ICS_PR_W-4.89*wilcox_ICS_PR_W_SD/sqrt(1000), x-0.15, wilcox_ICS_PR_W+4.89*wilcox_ICS_PR_W_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkred")
abline(h=2940,col="red")
abline(h=3070 ,col="blue")
abline(h=3270,col="green")
legend(6,1000,c("PS","CD","ICS"),pch=c(19,4,5),col=c("black","darkblue","darkred"))


##TE####
plot(x, wilcox_PS_TE_W,
     ylim=range(0, max(wilcox_PS_TE_W+wilcox_PS_TE_W_SD)),
     pch=19, xlab="Weeks", ylab="W",sub="Target Entropy",
     main="Per Wilcoxon test for difference between pass and fail",type="b"
)
lines(x+0.15,wilcox_CD_TE_W,type="b",col="darkblue",pch=4)
lines(x-0.15,wilcox_ICS_TE_W,type="b",col="darkred",pch=5)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, wilcox_PS_TE_W-4.89*wilcox_PS_TE_W_SD/sqrt(1000), x, wilcox_PS_TE_W+4.89*wilcox_PS_TE_W_SD/sqrt(1000), length=0.05, angle=90, code=3)
arrows(x+0.15, wilcox_CD_TE_W-4.89*wilcox_CD_TE_W_SD/sqrt(1000), x+0.15, wilcox_CD_TE_W+4.89*wilcox_CD_TE_W_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkblue")
arrows(x-0.15, wilcox_ICS_TE_W-4.89*wilcox_ICS_TE_W_SD/sqrt(1000), x-0.15, wilcox_ICS_TE_W+4.89*wilcox_ICS_TE_W_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkred")
abline(h=2940,col="red")
abline(h=3070 ,col="blue")
abline(h=3270,col="green")
legend(6,1000,c("PS","CD","ICS"),pch=c(19,4,5),col=c("black","darkblue","darkred"))

##H####
plot(x, wilcox_PS_H_W,
     ylim=range(0, max(wilcox_PS_H_W+wilcox_PS_H_W_SD)),
     pch=19, xlab="Weeks", ylab="W",sub="Hide",
     main="Per Wilcoxon test for difference between pass and fail",type="b"
)
lines(x+0.15,wilcox_CD_H_W,type="b",col="darkblue",pch=4)
lines(x-0.15,wilcox_ICS_H_W,type="b",col="darkred",pch=5)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, wilcox_PS_H_W-wilcox_PS_H_W_SD/sqrt(1000), x, wilcox_PS_H_W+wilcox_PS_H_W_SD/sqrt(1000), length=0.05, angle=90, code=3)
arrows(x+0.15, wilcox_CD_H_W-wilcox_CD_H_W_SD/sqrt(1000), x+0.15, wilcox_CD_H_W+wilcox_CD_H_W_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkblue")
arrows(x-0.15, wilcox_ICS_H_W-wilcox_ICS_H_W_SD/sqrt(1000), x-0.15, wilcox_ICS_H_W+wilcox_ICS_H_W_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkred")
abline(h=1900,col="red")
legend(6,1000,c("PS","CD","ICS"),pch=c(19,4,5),col=c("black","darkblue","darkred"))


####JP WILCOX PS LAYER####

wilcox_jp_PS_PR<-list()
for (i in 1:7){
  wilcox_jp_PS_PR[[i]]<-boot(centPS[[i]], wilcoxDiff, R=1000,pv="justpass",cm="PageRank")
}
wilcox_jp_PS_PR_W<-c(wilcox_jp_PS_PR[[1]]$t0,wilcox_jp_PS_PR[[2]]$t0,wilcox_jp_PS_PR[[3]]$t0,wilcox_jp_PS_PR[[4]]$t0,wilcox_jp_PS_PR[[5]]$t0,
                  wilcox_jp_PS_PR[[6]]$t0,wilcox_jp_PS_PR[[7]]$t0)
wilcox_jp_PS_PR_W_SD<-c(sd(wilcox_jp_PS_PR[[1]]$t),sd(wilcox_jp_PS_PR[[2]]$t),sd(wilcox_jp_PS_PR[[3]]$t),sd(wilcox_jp_PS_PR[[4]]$t),sd(wilcox_jp_PS_PR[[5]]$t),
                     sd(wilcox_jp_PS_PR[[6]]$t),sd(wilcox_jp_PS_PR[[7]]$t))

wilcox_jp_PS_TE<-list()
for (i in 1:7){
  wilcox_jp_PS_TE[[i]]<-boot(centPS[[i]], wilcoxDiff, R=1000,pv="justpass",cm="tarEnt")
}
wilcox_jp_PS_TE_W<-c(wilcox_jp_PS_TE[[1]]$t0,wilcox_jp_PS_TE[[2]]$t0,wilcox_jp_PS_TE[[3]]$t0,wilcox_jp_PS_TE[[4]]$t0,wilcox_jp_PS_TE[[5]]$t0,
                  wilcox_jp_PS_TE[[6]]$t0,wilcox_jp_PS_TE[[7]]$t0)
wilcox_jp_PS_TE_W_SD<-c(sd(wilcox_jp_PS_TE[[1]]$t),sd(wilcox_jp_PS_TE[[2]]$t),sd(wilcox_jp_PS_TE[[3]]$t),sd(wilcox_jp_PS_TE[[4]]$t),sd(wilcox_jp_PS_TE[[5]]$t),
                     sd(wilcox_jp_PS_TE[[6]]$t),sd(wilcox_jp_PS_TE[[7]]$t))

wilcox_jp_PS_H<-list()
for (i in 1:7){
  wilcox_jp_PS_H[[i]]<-boot(centPS[[i]], wilcoxDiff, R=1000,pv="justpass",cm="Hide")
}
wilcox_jp_PS_H_W<-c(wilcox_jp_PS_H[[1]]$t0,wilcox_jp_PS_H[[2]]$t0,wilcox_jp_PS_H[[3]]$t0,wilcox_jp_PS_H[[4]]$t0,wilcox_jp_PS_H[[5]]$t0,
                 wilcox_jp_PS_H[[6]]$t0,wilcox_jp_PS_H[[7]]$t0)
wilcox_jp_PS_H_W_SD<-c(sd(wilcox_jp_PS_H[[1]]$t),sd(wilcox_jp_PS_H[[2]]$t),sd(wilcox_jp_PS_H[[3]]$t),sd(wilcox_jp_PS_H[[4]]$t),sd(wilcox_jp_PS_H[[5]]$t),
                    sd(wilcox_jp_PS_H[[6]]$t),sd(wilcox_jp_PS_H[[7]]$t))

####JP WILCOX CD LAYER####
wilcox_jp_CD_PR<-list()
for (i in 1:7){
  wilcox_jp_CD_PR[[i]]<-boot(centCD[[i]], wilcoxDiff, R=1000,pv="justpass",cm="PageRank")
}
wilcox_jp_CD_PR_W<-c(wilcox_jp_CD_PR[[1]]$t0,wilcox_jp_CD_PR[[2]]$t0,wilcox_jp_CD_PR[[3]]$t0,wilcox_jp_CD_PR[[4]]$t0,wilcox_jp_CD_PR[[5]]$t0,
                  wilcox_jp_CD_PR[[6]]$t0,wilcox_jp_CD_PR[[7]]$t0)
wilcox_jp_CD_PR_W_SD<-c(sd(wilcox_jp_CD_PR[[1]]$t),sd(wilcox_jp_CD_PR[[2]]$t),sd(wilcox_jp_CD_PR[[3]]$t),sd(wilcox_jp_CD_PR[[4]]$t),sd(wilcox_jp_CD_PR[[5]]$t),
                     sd(wilcox_jp_CD_PR[[6]]$t),sd(wilcox_jp_CD_PR[[7]]$t))

wilcox_jp_CD_TE<-list()
for (i in 1:7){
  wilcox_jp_CD_TE[[i]]<-boot(centCD[[i]], wilcoxDiff, R=1000,pv="justpass",cm="tarEnt")
}
wilcox_jp_CD_TE_W<-c(wilcox_jp_CD_TE[[1]]$t0,wilcox_jp_CD_TE[[2]]$t0,wilcox_jp_CD_TE[[3]]$t0,wilcox_jp_CD_TE[[4]]$t0,wilcox_jp_CD_TE[[5]]$t0,
                  wilcox_jp_CD_TE[[6]]$t0,wilcox_jp_CD_TE[[7]]$t0)
wilcox_jp_CD_TE_W_SD<-c(sd(wilcox_jp_CD_TE[[1]]$t),sd(wilcox_jp_CD_TE[[2]]$t),sd(wilcox_jp_CD_TE[[3]]$t),sd(wilcox_jp_CD_TE[[4]]$t),sd(wilcox_jp_CD_TE[[5]]$t),
                     sd(wilcox_jp_CD_TE[[6]]$t),sd(wilcox_jp_CD_TE[[7]]$t))

wilcox_jp_CD_H<-list()
for (i in 1:7){
  wilcox_jp_CD_H[[i]]<-boot(centCD[[i]], wilcoxDiff, R=1000,pv="justpass",cm="Hide")
}
wilcox_jp_CD_H_W<-c(wilcox_jp_CD_H[[1]]$t0,wilcox_jp_CD_H[[2]]$t0,wilcox_jp_CD_H[[3]]$t0,wilcox_jp_CD_H[[4]]$t0,wilcox_jp_CD_H[[5]]$t0,
                 wilcox_jp_CD_H[[6]]$t0,wilcox_jp_CD_H[[7]]$t0)
wilcox_jp_CD_H_W_SD<-c(sd(wilcox_jp_CD_H[[1]]$t),sd(wilcox_jp_CD_H[[2]]$t),sd(wilcox_jp_CD_H[[3]]$t),sd(wilcox_jp_CD_H[[4]]$t),sd(wilcox_jp_CD_H[[5]]$t),
                    sd(wilcox_jp_CD_H[[6]]$t),sd(wilcox_jp_CD_H[[7]]$t))



####JP WILCOX ICS LAYER####
wilcox_jp_ICS_PR<-list()
for (i in 1:7){
  wilcox_jp_ICS_PR[[i]]<-boot(centICS[[i]], wilcoxDiff, R=1000,pv="justpass",cm="PageRank")
}
wilcox_jp_ICS_PR_W<-c(wilcox_jp_ICS_PR[[1]]$t0,wilcox_jp_ICS_PR[[2]]$t0,wilcox_jp_ICS_PR[[3]]$t0,wilcox_jp_ICS_PR[[4]]$t0,wilcox_jp_ICS_PR[[5]]$t0,
                   wilcox_jp_ICS_PR[[6]]$t0,wilcox_jp_ICS_PR[[7]]$t0)
wilcox_jp_ICS_PR_W_SD<-c(sd(wilcox_jp_ICS_PR[[1]]$t),sd(wilcox_jp_ICS_PR[[2]]$t),sd(wilcox_jp_ICS_PR[[3]]$t),sd(wilcox_jp_ICS_PR[[4]]$t),sd(wilcox_jp_ICS_PR[[5]]$t),
                     sd(wilcox_jp_ICS_PR[[6]]$t),sd(wilcox_jp_ICS_PR[[7]]$t))

wilcox_jp_ICS_TE<-list()
for (i in 1:7){
  wilcox_jp_ICS_TE[[i]]<-boot(centICS[[i]], wilcoxDiff, R=1000,pv="justpass",cm="tarEnt")
}
wilcox_jp_ICS_TE_W<-c(wilcox_jp_ICS_TE[[1]]$t0,wilcox_jp_ICS_TE[[2]]$t0,wilcox_jp_ICS_TE[[3]]$t0,wilcox_jp_ICS_TE[[4]]$t0,wilcox_jp_ICS_TE[[5]]$t0,
                   wilcox_jp_ICS_TE[[6]]$t0,wilcox_jp_ICS_TE[[7]]$t0)
wilcox_jp_ICS_TE_W_SD<-c(sd(wilcox_jp_ICS_TE[[1]]$t),sd(wilcox_jp_ICS_TE[[2]]$t),sd(wilcox_jp_ICS_TE[[3]]$t),sd(wilcox_jp_ICS_TE[[4]]$t),sd(wilcox_jp_ICS_TE[[5]]$t),
                      sd(wilcox_jp_ICS_TE[[6]]$t),sd(wilcox_jp_ICS_TE[[7]]$t))

wilcox_jp_ICS_H<-list()
for (i in 1:7){
  wilcox_jp_ICS_H[[i]]<-boot(centICS[[i]], wilcoxDiff, R=1000,pv="justpass",cm="Hide")
}
wilcox_jp_ICS_H_W<-c(wilcox_jp_ICS_H[[1]]$t0,wilcox_jp_ICS_H[[2]]$t0,wilcox_jp_ICS_H[[3]]$t0,wilcox_jp_ICS_H[[4]]$t0,wilcox_jp_ICS_H[[5]]$t0,
                  wilcox_jp_ICS_H[[6]]$t0,wilcox_jp_ICS_H[[7]]$t0)
wilcox_jp_ICS_H_W_SD<-c(sd(wilcox_jp_ICS_H[[1]]$t),sd(wilcox_jp_ICS_H[[2]]$t),sd(wilcox_jp_ICS_H[[3]]$t),sd(wilcox_jp_ICS_H[[4]]$t),sd(wilcox_jp_ICS_H[[5]]$t),
                     sd(wilcox_jp_ICS_H[[6]]$t),sd(wilcox_jp_ICS_H[[7]]$t))




####JP PLOT WEEKLY WILCOXON TESTS####
##PR####
plot(x, wilcox_jp_PS_PR_W,
     ylim=range(0, max(wilcox_jp_ICS_PR_W+wilcox_jp_ICS_PR_W_SD)),
     pch=19, xlab="Weeks", ylab="W",sub="PageRank",
     main="Per Wilcoxon test for difference between just pass and just fail",type="b"
)
lines(x+0.15,wilcox_jp_CD_PR_W,type="b",col="darkblue",pch=4)
lines(x-0.15,wilcox_jp_ICS_PR_W,type="b",col="darkred",pch=5)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, wilcox_jp_PS_PR_W-4.89*wilcox_jp_PS_PR_W_SD/sqrt(1000), x, wilcox_jp_PS_PR_W+4.89*wilcox_jp_PS_PR_W_SD/sqrt(1000), length=0.05, angle=90, code=3)
arrows(x+0.15, wilcox_jp_CD_PR_W-4.89*wilcox_jp_CD_PR_W_SD/sqrt(1000), x+0.15, wilcox_jp_CD_PR_W+4.89*wilcox_jp_CD_PR_W_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkblue")
arrows(x-0.15, wilcox_jp_ICS_PR_W-4.89*wilcox_jp_ICS_PR_W_SD/sqrt(1000), x-0.15, wilcox_jp_ICS_PR_W+4.89*wilcox_jp_ICS_PR_W_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkred")
abline(h=697,col="red")
legend(6,200,c("PS","CD","ICS"),pch=c(19,4,5),col=c("black","darkblue","darkred"))

##TE####
plot(x, wilcox_jp_PS_TE_W,
     ylim=range(0, max(wilcox_jp_ICS_TE_W+wilcox_jp_ICS_TE_W_SD)),
     pch=19, xlab="Weeks", ylab="W",
     main="Per week difference",type="b"
)
lines(x+0.15,wilcox_jp_CD_TE_W,type="b",col="darkblue",pch=4)
lines(x-0.15,wilcox_jp_ICS_TE_W,type="b",col="darkred",pch=5)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, wilcox_jp_PS_TE_W-wilcox_jp_PS_TE_W_SD/sqrt(1000), x, wilcox_jp_PS_TE_W+wilcox_jp_PS_TE_W_SD/sqrt(1000), length=0.05, angle=90, code=3)
arrows(x+0.15, wilcox_jp_CD_TE_W-wilcox_jp_CD_TE_W_SD/sqrt(1000), x+0.15, wilcox_jp_CD_TE_W+wilcox_jp_CD_TE_W_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkblue")
arrows(x-0.15, wilcox_jp_ICS_TE_W-wilcox_jp_ICS_TE_W_SD/sqrt(1000), x-0.15, wilcox_jp_ICS_TE_W+wilcox_jp_ICS_TE_W_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkred")
abline(h=697,col="red")
legend(6,200,c("PS","CD","ICS"),pch=c(19,4,5),col=c("black","darkblue","darkred"))

##H####
plot(x, wilcox_jp_PS_H_W,
     ylim=range(0, max(wilcox_jp_PS_PR_W+wilcox_jp_PR_PR_W_SD)),
     pch=19, xlab="Weeks", ylab="W",
     main="Per week difference",type="b"
)
lines(x+0.15,wilcox_jp_CD_H_W,type="b",col="darkblue",pch=4)
lines(x-0.15,wilcox_jp_ICS_H_W,type="b",col="darkred",pch=5)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, wilcox_jp_PS_H_W-wilcox_jp_PS_H_W_SD/sqrt(1000), x, wilcox_jp_PS_H_W+wilcox_jp_PS_H_W_SD/sqrt(1000), length=0.05, angle=90, code=3)
arrows(x+0.15, wilcox_jp_CD_H_W-wilcox_jp_CD_H_W_SD/sqrt(1000), x+0.15, wilcox_jp_CD_H_W+wilcox_jp_CD_H_W_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkblue")
arrows(x-0.15, wilcox_jp_ICS_H_W-wilcox_jp_ICS_H_W_SD/sqrt(1000), x-0.15, wilcox_jp_ICS_H_W+wilcox_jp_ICS_H_W_SD/sqrt(1000), length=0.05, angle=90, code=3,col="darkred")
abline(h=697,col="red")

####Week by week correlation of centrality measures####


corKen <- function(data, indices,i){
  dt<-data[indices,]
    c(cor.test(dt[,i], dt[,i+1], method='k')$estimate,
      cor.test(dt[,i], dt[,i+1], method='k')$p.value)
      

}

####PAGE RANK####

PS_PR<-data.frame(centPS[[1]]$PageRank,centPS[[2]]$PageRank,centPS[[3]]$PageRank,centPS[[4]]$PageRank,centPS[[5]]$PageRank,centPS[[6]]$PageRank,centPS[[7]]$PageRank)
x<-c(1:166)
PS_PR_rankcor<-matrix(data=NA,nrow=6,ncol=3)
colnames(PS_PR_rankcor)<-c("tau","ciL","ciH")
PS_PR_rankcor[1,] <- c(corKen(PS_PR,x,1)[1],boot.ci(boot(PS_PR, corKen, R=1000,i=1))$bca[c(4,5)])
PS_PR_rankcor[2,] <- c(corKen(PS_PR,x,2)[1],boot.ci(boot(PS_PR, corKen, R=1000,i=2))$bca[c(4,5)])
PS_PR_rankcor[3,] <- c(corKen(PS_PR,x,3)[1],boot.ci(boot(PS_PR, corKen, R=1000,i=3))$bca[c(4,5)])
PS_PR_rankcor[4,] <- c(corKen(PS_PR,x,4)[1],boot.ci(boot(PS_PR, corKen, R=1000,i=4))$bca[c(4,5)])
PS_PR_rankcor[5,] <- c(corKen(PS_PR,x,5)[1],boot.ci(boot(PS_PR, corKen, R=1000,i=5))$bca[c(4,5)])
PS_PR_rankcor[6,] <- c(corKen(PS_PR,x,6)[1],boot.ci(boot(PS_PR, corKen, R=1000,i=6))$bca[c(4,5)])

CD_PR<-data.frame(centCD[[1]]$PageRank,centCD[[2]]$PageRank,centCD[[3]]$PageRank,centCD[[4]]$PageRank,centCD[[5]]$PageRank,centCD[[6]]$PageRank,centCD[[7]]$PageRank)
x<-c(1:166)
CD_PR_rankcor<-matrix(data=NA,nrow=6,ncol=3)
colnames(CD_PR_rankcor)<-c("tau","ciL","ciH")
CD_PR_rankcor[1,] <- c(corKen(CD_PR,x,1)[1],boot.ci(boot(CD_PR, corKen, R=1000,i=1))$bca[c(4,5)])
CD_PR_rankcor[2,] <- c(corKen(CD_PR,x,2)[1],boot.ci(boot(CD_PR, corKen, R=1000,i=2))$bca[c(4,5)])
CD_PR_rankcor[3,] <- c(corKen(CD_PR,x,3)[1],boot.ci(boot(CD_PR, corKen, R=1000,i=3))$bca[c(4,5)])
CD_PR_rankcor[4,] <- c(corKen(CD_PR,x,4)[1],boot.ci(boot(CD_PR, corKen, R=1000,i=4))$bca[c(4,5)])
CD_PR_rankcor[5,] <- c(corKen(CD_PR,x,5)[1],boot.ci(boot(CD_PR, corKen, R=1000,i=5))$bca[c(4,5)])
CD_PR_rankcor[6,] <- c(corKen(CD_PR,x,6)[1],boot.ci(boot(CD_PR, corKen, R=1000,i=6))$bca[c(4,5)])

ICS_PR<-data.frame(centICS[[1]]$PageRank,centICS[[2]]$PageRank,centICS[[3]]$PageRank,centICS[[4]]$PageRank,centICS[[5]]$PageRank,centICS[[6]]$PageRank,centICS[[7]]$PageRank)
x<-c(1:166)
ICS_PR_rankcor<-matrix(data=NA,nrow=6,ncol=3)
colnames(ICS_PR_rankcor)<-c("tau","ciL","ciH")
ICS_PR_rankcor[1,] <- c(corKen(ICS_PR,x,1)[1],boot.ci(boot(ICS_PR, corKen, R=1000,i=1))$bca[c(4,5)])
ICS_PR_rankcor[2,] <- c(corKen(ICS_PR,x,2)[1],boot.ci(boot(ICS_PR, corKen, R=1000,i=2))$bca[c(4,5)])
ICS_PR_rankcor[3,] <- c(corKen(ICS_PR,x,3)[1],boot.ci(boot(ICS_PR, corKen, R=1000,i=3))$bca[c(4,5)])
ICS_PR_rankcor[4,] <- c(corKen(ICS_PR,x,4)[1],boot.ci(boot(ICS_PR, corKen, R=1000,i=4))$bca[c(4,5)])
ICS_PR_rankcor[5,] <- c(corKen(ICS_PR,x,5)[1],boot.ci(boot(ICS_PR, corKen, R=1000,i=5))$bca[c(4,5)])
ICS_PR_rankcor[6,] <- c(corKen(ICS_PR,x,6)[1],boot.ci(boot(ICS_PR, corKen, R=1000,i=6))$bca[c(4,5)])

y<-c(1:6)
plot(y, PS_PR_rankcor[,1],
     ylim=range(c(0, 1)),
     pch=19, xlab="Weeks", ylab="Kendall's Tau estimator",
     main="Week to week rank correlations PageRank",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
lines(y,CD_PR_rankcor[,1],type="b",col="darkblue",pch=4)
lines(y,ICS_PR_rankcor[,1],type="b",col="darkred",pch=5)
arrows(y, PS_PR_rankcor[,2], y, PS_PR_rankcor[,3], length=0.05, angle=90, code=3)
arrows(y, CD_PR_rankcor[,2], y, CD_PR_rankcor[,3], length=0.05, angle=90, code=3,col="darkblue")
arrows(y, ICS_PR_rankcor[,2], y, ICS_PR_rankcor[,3], length=0.05, angle=90, code=3,col="darkred")
legend(4,0.4,c("Problem Solving","Concept Discussion","In-Class Social"),pch=c(19,4,5),col = c("black","darkblue","darkred"))

####TARGET ENTROPY####
PS_TE<-data.frame(centPS[[1]]$tarEnt,centPS[[2]]$tarEnt,centPS[[3]]$tarEnt,centPS[[4]]$tarEnt,centPS[[5]]$tarEnt,centPS[[6]]$tarEnt,centPS[[7]]$tarEnt)
x<-c(1:166)
PS_TE_rankcor<-matrix(data=NA,nrow=6,ncol=3)
colnames(PS_TE_rankcor)<-c("tau","ciL","ciH")
PS_TE_rankcor[1,] <- c(corKen(PS_TE,x,1)[1],boot.ci(boot(PS_TE, corKen, R=1000,i=1))$bca[c(4,5)])
PS_TE_rankcor[2,] <- c(corKen(PS_TE,x,2)[1],boot.ci(boot(PS_TE, corKen, R=1000,i=2))$bca[c(4,5)])
PS_TE_rankcor[3,] <- c(corKen(PS_TE,x,3)[1],boot.ci(boot(PS_TE, corKen, R=1000,i=3))$bca[c(4,5)])
PS_TE_rankcor[4,] <- c(corKen(PS_TE,x,4)[1],boot.ci(boot(PS_TE, corKen, R=1000,i=4))$bca[c(4,5)])
PS_TE_rankcor[5,] <- c(corKen(PS_TE,x,5)[1],boot.ci(boot(PS_TE, corKen, R=1000,i=5))$bca[c(4,5)])
PS_TE_rankcor[6,] <- c(corKen(PS_TE,x,6)[1],boot.ci(boot(PS_TE, corKen, R=1000,i=6))$bca[c(4,5)])

CD_TE<-data.frame(centCD[[1]]$tarEnt,centCD[[2]]$tarEnt,centCD[[3]]$tarEnt,centCD[[4]]$tarEnt,centCD[[5]]$tarEnt,centCD[[6]]$tarEnt,centCD[[7]]$tarEnt)
x<-c(1:166)
CD_TE_rankcor<-matrix(data=NA,nrow=6,ncol=3)
colnames(CD_TE_rankcor)<-c("tau","ciL","ciH")
CD_TE_rankcor[1,] <- c(corKen(CD_TE,x,1)[1],boot.ci(boot(CD_TE, corKen, R=1000,i=1))$bca[c(4,5)])
CD_TE_rankcor[2,] <- c(corKen(CD_TE,x,2)[1],boot.ci(boot(CD_TE, corKen, R=1000,i=2))$bca[c(4,5)])
CD_TE_rankcor[3,] <- c(corKen(CD_TE,x,3)[1],boot.ci(boot(CD_TE, corKen, R=1000,i=3))$bca[c(4,5)])
CD_TE_rankcor[4,] <- c(corKen(CD_TE,x,4)[1],boot.ci(boot(CD_TE, corKen, R=1000,i=4))$bca[c(4,5)])
CD_TE_rankcor[5,] <- c(corKen(CD_TE,x,5)[1],boot.ci(boot(CD_TE, corKen, R=1000,i=5))$bca[c(4,5)])
CD_TE_rankcor[6,] <- c(corKen(CD_TE,x,6)[1],boot.ci(boot(CD_TE, corKen, R=1000,i=6))$bca[c(4,5)])

ICS_TE<-data.frame(centICS[[1]]$tarEnt,centICS[[2]]$tarEnt,centICS[[3]]$tarEnt,centICS[[4]]$tarEnt,centICS[[5]]$tarEnt,centICS[[6]]$tarEnt,centICS[[7]]$tarEnt)
x<-c(1:166)
ICS_TE_rankcor<-matrix(data=NA,nrow=6,ncol=3)
colnames(ICS_TE_rankcor)<-c("tau","ciL","ciH")
ICS_TE_rankcor[1,] <- c(corKen(ICS_TE,x,1)[1],boot.ci(boot(ICS_TE, corKen, R=1000,i=1))$bca[c(4,5)])
ICS_TE_rankcor[2,] <- c(corKen(ICS_TE,x,2)[1],boot.ci(boot(ICS_TE, corKen, R=1000,i=2))$bca[c(4,5)])
ICS_TE_rankcor[3,] <- c(corKen(ICS_TE,x,3)[1],boot.ci(boot(ICS_TE, corKen, R=1000,i=3))$bca[c(4,5)])
ICS_TE_rankcor[4,] <- c(corKen(ICS_TE,x,4)[1],boot.ci(boot(ICS_TE, corKen, R=1000,i=4))$bca[c(4,5)])
ICS_TE_rankcor[5,] <- c(corKen(ICS_TE,x,5)[1],boot.ci(boot(ICS_TE, corKen, R=1000,i=5))$bca[c(4,5)])
ICS_TE_rankcor[6,] <- c(corKen(ICS_TE,x,6)[1],boot.ci(boot(ICS_TE, corKen, R=1000,i=6))$bca[c(4,5)])

y<-c(1:6)
plot(y, PS_TE_rankcor[,1],
     ylim=range(c(0, 1)),
     pch=19, xlab="Weeks", ylab="Kendall's Tau estimator",
     main="Week to week rank correlations tarEnt",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
lines(y,CD_TE_rankcor[,1],type="b",col="darkblue",pch=4)
lines(y,ICS_TE_rankcor[,1],type="b",col="darkred",pch=5)
arrows(y, PS_TE_rankcor[,2], y, PS_TE_rankcor[,3], length=0.05, angle=90, code=3)
arrows(y, CD_TE_rankcor[,2], y, CD_TE_rankcor[,3], length=0.05, angle=90, code=3,col="darkblue")
arrows(y, ICS_TE_rankcor[,2], y, ICS_TE_rankcor[,3], length=0.05, angle=90, code=3,col="darkred")
legend(4,0.4,c("Problem Solving","Concept Discussion","In-Class Social"),pch=c(19,4,5),col = c("black","darkblue","darkred"))


####HIDE#####
PS_H<-data.frame(centPS[[1]]$Hide,centPS[[2]]$Hide,centPS[[3]]$Hide,centPS[[4]]$Hide,centPS[[5]]$Hide,centPS[[6]]$Hide,centPS[[7]]$Hide)
x<-c(1:166)
PS_H_rankcor<-matrix(data=NA,nrow=6,ncol=3)
colnames(PS_H_rankcor)<-c("tau","ciL","ciH")
PS_H_rankcor[1,] <- c(corKen(PS_H,x,1)[1],boot.ci(boot(PS_H, corKen, R=1000,i=1))$bca[c(4,5)])
PS_H_rankcor[2,] <- c(corKen(PS_H,x,2)[1],boot.ci(boot(PS_H, corKen, R=1000,i=2))$bca[c(4,5)])
PS_H_rankcor[3,] <- c(corKen(PS_H,x,3)[1],boot.ci(boot(PS_H, corKen, R=1000,i=3))$bca[c(4,5)])
PS_H_rankcor[4,] <- c(corKen(PS_H,x,4)[1],boot.ci(boot(PS_H, corKen, R=1000,i=4))$bca[c(4,5)])
PS_H_rankcor[5,] <- c(corKen(PS_H,x,5)[1],boot.ci(boot(PS_H, corKen, R=1000,i=5))$bca[c(4,5)])
PS_H_rankcor[6,] <- c(corKen(PS_H,x,6)[1],boot.ci(boot(PS_H, corKen, R=1000,i=6))$bca[c(4,5)])

CD_H<-data.frame(centCD[[1]]$Hide,centCD[[2]]$Hide,centCD[[3]]$Hide,centCD[[4]]$Hide,centCD[[5]]$Hide,centCD[[6]]$Hide,centCD[[7]]$Hide)
x<-c(1:166)
CD_H_rankcor<-matrix(data=NA,nrow=6,ncol=3)
colnames(CD_H_rankcor)<-c("tau","ciL","ciH")
CD_H_rankcor[1,] <- c(corKen(CD_H,x,1)[1],boot.ci(boot(CD_H, corKen, R=1000,i=1))$bca[c(4,5)])
CD_H_rankcor[2,] <- c(corKen(CD_H,x,2)[1],boot.ci(boot(CD_H, corKen, R=1000,i=2))$bca[c(4,5)])
CD_H_rankcor[3,] <- c(corKen(CD_H,x,3)[1],boot.ci(boot(CD_H, corKen, R=1000,i=3))$bca[c(4,5)])
CD_H_rankcor[4,] <- c(corKen(CD_H,x,4)[1],boot.ci(boot(CD_H, corKen, R=1000,i=4))$bca[c(4,5)])
CD_H_rankcor[5,] <- c(corKen(CD_H,x,5)[1],boot.ci(boot(CD_H, corKen, R=1000,i=5))$bca[c(4,5)])
CD_H_rankcor[6,] <- c(corKen(CD_H,x,6)[1],boot.ci(boot(CD_H, corKen, R=1000,i=6))$bca[c(4,5)])

ICS_H<-data.frame(centICS[[1]]$Hide,centICS[[2]]$Hide,centICS[[3]]$Hide,centICS[[4]]$Hide,centICS[[5]]$Hide,centICS[[6]]$Hide,centICS[[7]]$Hide)
x<-c(1:166)
ICS_H_rankcor<-matrix(data=NA,nrow=6,ncol=3)
colnames(ICS_H_rankcor)<-c("tau","ciL","ciH")
ICS_H_rankcor[1,] <- c(corKen(ICS_H,x,1)[1],boot.ci(boot(ICS_H, corKen, R=1000,i=1))$bca[c(4,5)])
ICS_H_rankcor[2,] <- c(corKen(ICS_H,x,2)[1],boot.ci(boot(ICS_H, corKen, R=1000,i=2))$bca[c(4,5)])
ICS_H_rankcor[3,] <- c(corKen(ICS_H,x,3)[1],boot.ci(boot(ICS_H, corKen, R=1000,i=3))$bca[c(4,5)])
ICS_H_rankcor[4,] <- c(corKen(ICS_H,x,4)[1],boot.ci(boot(ICS_H, corKen, R=1000,i=4))$bca[c(4,5)])
ICS_H_rankcor[5,] <- c(corKen(ICS_H,x,5)[1],boot.ci(boot(ICS_H, corKen, R=1000,i=5))$bca[c(4,5)])
ICS_H_rankcor[6,] <- c(corKen(ICS_H,x,6)[1],boot.ci(boot(ICS_H, corKen, R=1000,i=6))$bca[c(4,5)])

y<-c(1:6)
plot(y, PS_H_rankcor[,1],
     ylim=range(c(0, 1)),
     pch=19, xlab="Weeks", ylab="Kendall's Tau estimator",
     main="Week to week rank correlations Hide",type="b"
)
# hack: we draw arrows but with very special "arrowheads"
lines(y,CD_H_rankcor[,1],type="b",col="darkblue",pch=4)
lines(y,ICS_H_rankcor[,1],type="b",col="darkred",pch=5)
arrows(y, PS_H_rankcor[,2], y, PS_H_rankcor[,3], length=0.05, angle=90, code=3)
arrows(y, CD_H_rankcor[,2], y, CD_H_rankcor[,3], length=0.05, angle=90, code=3,col="darkblue")
arrows(y, ICS_H_rankcor[,2], y, ICS_H_rankcor[,3], length=0.05, angle=90, code=3,col="darkblue")
legend(4,0.4,c("Problem Solving","Concept Discussion","In-Class Social"),pch=c(19,4,5),col = c("black","darkblue","darkred"))

######CORRELATION ALL NETWORK MEASURE NON-BOOT STRAP####
cmMeasuresWeek<-data.frame(centPS[[1]]$PageRank,centPS[[2]]$PageRank,centPS[[3]]$PageRank,centPS[[4]]$PageRank,centPS[[5]]$PageRank,centPS[[6]]$PageRank,centPS[[7]]$PageRank,
           centPS[[1]]$tarEnt,centPS[[2]]$tarEnt,centPS[[3]]$tarEnt,centPS[[4]]$tarEnt,centPS[[5]]$tarEnt,centPS[[6]]$tarEnt,centPS[[7]]$tarEnt,
           centPS[[1]]$Hide,centPS[[2]]$Hide,centPS[[3]]$Hide,centPS[[4]]$Hide,centPS[[5]]$Hide,centPS[[6]]$Hide,centPS[[7]]$Hide,
           centCD[[1]]$PageRank,centCD[[2]]$PageRank,centCD[[3]]$PageRank,centCD[[4]]$PageRank,centCD[[5]]$PageRank,centCD[[6]]$PageRank,centCD[[7]]$PageRank,
           centCD[[1]]$tarEnt,centCD[[2]]$tarEnt,centCD[[3]]$tarEnt,centCD[[4]]$tarEnt,centCD[[5]]$tarEnt,centCD[[6]]$tarEnt,centCD[[7]]$tarEnt,
           centCD[[1]]$Hide,centCD[[2]]$Hide,centCD[[3]]$Hide,centCD[[4]]$Hide,centCD[[5]]$Hide,centCD[[6]]$Hide,centCD[[7]]$Hide,
           centICS[[1]]$PageRank,centICS[[2]]$PageRank,centICS[[3]]$PageRank,centICS[[4]]$PageRank,centICS[[5]]$PageRank,centICS[[6]]$PageRank,centICS[[7]]$PageRank,
           centICS[[1]]$tarEnt,centICS[[2]]$tarEnt,centICS[[3]]$tarEnt,centICS[[4]]$tarEnt,centICS[[5]]$tarEnt,centICS[[6]]$tarEnt,centICS[[7]]$tarEnt,
           centICS[[1]]$Hide,centICS[[2]]$Hide,centICS[[3]]$Hide,centICS[[4]]$Hide,centICS[[5]]$Hide,centICS[[6]]$Hide,centICS[[7]]$Hide)


cmCorMatrix<-matrix(data=NA,nrow=63,ncol=63)
PS_name<-c(paste("PS_PR",c(1:7),sep="_"),paste("PS_TE",c(1:7),sep="_"),paste("PS_H",c(1:7),sep="_"))
CD_name<-c(paste("CD_PR",c(1:7),sep="_"),paste("CD_TE",c(1:7),sep="_"),paste("CD_H",c(1:7),sep="_"))
ICS_name<-c(paste("ICS_PR",c(1:7),sep="_"),paste("ICS_TE",c(1:7),sep="_"),paste("ICS_H",c(1:7),sep="_"))
l_name<-c(PS_name,CD_name,ICS_name)
rownames(cmCorMatrix)<-l_name
colnames(cmCorMatrix)<-l_name

cmCorVector<-function(i){
  res<-vector()
  for(j in 1:63){
  res[j]<-cor.test(cmMeasuresWeek[,i],cmMeasuresWeek[,j],method="kendall")$statistic
  }
  return(res)
}
for(i in 1:63){
  cmCorMatrix[i,]<-cmCorVector(i)
}
library(RColorBrewer)
coul <- colorRampPalette(brewer.pal(8, "PiYG"))(100)
levelplot(cmCorMatrix,col.regions=coul,scales=list(x=list(rot=90)),xlab="Centrality Measure", ylab="Centrality Measure",main="Z-statistic, kendall correlation")





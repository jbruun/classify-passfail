library(ggplot2)
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


#Distributions of variables
dev.off()
par(cex=0.7)
par(oma = c(4,1,1,1), mfrow = c(3, 3), mar = c(2, 2, 1, 1))
plot(sort(centPS[[1]]$PageRank,decreasing = T),log="xy",pch=1, 
      main="",ylab="PageRank",xlab="Ranked students",cex=0.8)
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

hist(centPS[[1]]$PageRank)
hist(centPS[[2]]$PageRank)
hist(centPS[[3]]$PageRank)
hist(centPS[[4]]$PageRank)
hist(centPS[[5]]$PageRank)
hist(centPS[[6]]$PageRank)
hist(centPS[[7]]$PageRank)

hist(centPS[[1]]$Hide)
hist(centPS[[2]]$Hide)
hist(centPS[[3]]$Hide)
hist(centPS[[4]]$Hide)
hist(centPS[[5]]$Hide)
hist(centPS[[6]]$Hide)
hist(centPS[[7]]$Hide)

hist(centPS[[1]]$tarEnt)
hist(centPS[[2]]$tarEnt)
hist(centPS[[3]]$tarEnt)
hist(centPS[[4]]$tarEnt)
hist(centPS[[5]]$tarEnt)
hist(centPS[[6]]$tarEnt)
hist(centPS[[7]]$tarEnt)



#Correlations
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





##NETWORK MEASURES

dev.off()
#par(oma = c(4,1,1,1), mfrow = c(3, 3), mar = c(2, 2, 1, 1))
#par(mfrow=c(3,1))
boxplot(PageRank~Week:pass, notch=T,
        data=dfPS,
        main="PS layer passing and failing",
        xlab="Week",
        ylab="PageRank",
        col="dodgerblue",
        border="blue",
        lex.order=T,
        sep=":"
)

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

boxplot(tarEnt~Week:pass, notch=T,
        data=dfPS,
        
        xlab="Week",
        ylab="tarEnt",
        col="dodgerblue",
        border="blue",
        lex.order=T,
        sep=":"
)

boxplot(tarEnt~Week:pass, notch=T,
        data=dfCD,
        
        xlab="Week",
        ylab="tarEnt",
        col="plum",
        border="purple",
        lex.order=T,
        sep=":"
)


boxplot(tarEnt~Week:pass, notch=T,
        data=dfICS,
        
        xlab="Week",
        ylab="tarEnt",
        col="orange",
        border="brown",
        lex.order=T,
        sep=":"
)

boxplot(Hide~Week:pass, notch=T,
        data=dfPS,
        
        xlab="Week",
        ylab="Hide",
        col="dodgerblue",
        border="blue",
        lex.order=T,
        sep=":"
)

boxplot(Hide~Week:pass, notch=T,
        data=dfCD,
        
        xlab="Week",
        ylab="Hide",
        col="plum",
        border="purple",
        lex.order=T,
        sep=":"
)


boxplot(Hide~Week:pass, notch=T,
        data=dfICS,
        
        xlab="Week",
        ylab="Hide",
        col="orange",
        border="brown",
        lex.order=T,
        sep=":"
)



boxplot(PageRank~Week:justpass, notch=T,
        data=dfPS,
        main="PS layer justpassing and failing",
        xlab="Week",
        ylab="PageRank",
        col="dodgerblue",
        border="blue",
        lex.order=T,
        sep=":"
)

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

##KS tests
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

plot(c(D),c(p),log="y",xlim = c(0.2,0.45))
points(c(Dj),c(pj),log="y",pch=2)
abline(h=0.05)
abline(h=0.01)
abline(h=0.001)
abline(h=0.0001)
abline(v=0.41)
#PS
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
x<-c(1:7)
plot(x, ks_PS_PR_D,
     ylim=range(0, max(ks_PS_PR_D_ciH)),
     pch=19, xlab="Weeks", ylab="KS statistic",
     main="Per week difference",type="b"
)
lines(x,ks_PS_TE_D,type="b",col="darkblue",pch=4)
lines(x,ks_PS_H_D,type="b",col="darkred",pch=5)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, ks_PS_PR_D-ks_PS_PR_D_SD, x, ks_PS_PR_D+ks_PS_PR_D_SD, length=0.05, angle=90, code=3)
arrows(x, ks_PS_TE_D-ks_PS_TE_D_SD, x, ks_PS_TE_D+ks_PS_TE_D_SD, length=0.05, angle=90, code=3,col="darkblue")
arrows(x, ks_PS_H_D-ks_PS_H_D_SD, x, ks_PS_H_D+ks_PS_H_D_SD, length=0.05, angle=90, code=3,col="darkred")

#WILCOX TESTS
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

plot(c(W),c(p),xlim=c(2800,3500),log="y")
points(c(Wj),c(pj),pch=2,log="y")
abline(h=0.05)
abline(h=0.01)
abline(h=0.001)
abline(h=0.0001)
abline(v=2940)
abline(v=3100)
abline(v=3300)
abline(v=3450)

wilcox_PS_PR<-list()
for (i in 1:7){
  wilcox_PS_PR[[i]]<-boot(centPS[[i]], wilcoxDiff, R=1000,pv="pass",cm="PageRank")
}
wilcox_PS_PR_D<-c(wilcox_PS_PR[[1]]$t0,wilcox_PS_PR[[2]]$t0,wilcox_PS_PR[[3]]$t0,wilcox_PS_PR[[4]]$t0,wilcox_PS_PR[[5]]$t0,
              wilcox_PS_PR[[6]]$t0,wilcox_PS_PR[[7]]$t0)
wilcox_PS_PR_D_ciL<-c(boot.ci(wilcox_PS_PR[[1]])$bca[4],boot.ci(wilcox_PS_PR[[2]])$bca[4],boot.ci(wilcox_PS_PR[[3]])$bca[4],
                  boot.ci(wilcox_PS_PR[[4]])$bca[4],boot.ci(wilcox_PS_PR[[5]])$bca[4],boot.ci(wilcox_PS_PR[[6]])$bca[4],
                  boot.ci(wilcox_PS_PR[[7]])$bca[4])
wilcox_PS_PR_D_ciH<-c(boot.ci(wilcox_PS_PR[[1]])$bca[5],boot.ci(wilcox_PS_PR[[2]])$bca[5],boot.ci(wilcox_PS_PR[[3]])$bca[5],
                  boot.ci(wilcox_PS_PR[[5]])$bca[5],boot.ci(wilcox_PS_PR[[5]])$bca[5],boot.ci(wilcox_PS_PR[[6]])$bca[5],
                  boot.ci(wilcox_PS_PR[[7]])$bca[5])
plot(x, wilcox_PS_PR_D,
     ylim=range(0, max(wilcox_PS_PR_D_ciH)),
     pch=19, xlab="Weeks", ylab="Wilcox statistic",
     main="Per week difference",type="b"
)

# hack: we draw arrows but with very special "arrowheads"
arrows(x, wilcox_PS_PR_D_ciL, x, wilcox_PS_PR_D_ciH, length=0.05, angle=90, code=3)



#T-TESTS
W<-matrix(NA,ncol=7,nrow=9)
p<-matrix(NA,ncol=7,nrow=9)
for(i in 1:7){
  W[1,i]<-t.test(centPS[[i]]$PageRank[centPS[[i]]$pass==1],centPS[[i]]$PageRank[centPS[[i]]$pass==0])$statistic 
  W[2,i]<-t.test(centCD[[i]]$PageRank[centCD[[i]]$pass==1],centCD[[i]]$PageRank[centCD[[i]]$pass==0])$statistic
  W[3,i]<-t.test(centICS[[i]]$PageRank[centICS[[i]]$pass==1],centICS[[i]]$PageRank[centICS[[i]]$pass==0])$statistic
  W[4,i]<-t.test(centPS[[i]]$tarEnt[centPS[[i]]$pass==1],centPS[[i]]$tarEnt[centPS[[i]]$pass==0])$statistic 
  W[5,i]<-t.test(centCD[[i]]$tarEnt[centCD[[i]]$pass==1],centCD[[i]]$tarEnt[centCD[[i]]$pass==0])$statistic
  W[6,i]<-t.test(centICS[[i]]$tarEnt[centICS[[i]]$pass==1],centICS[[i]]$tarEnt[centICS[[i]]$pass==0])$statistic
  W[7,i]<-t.test(centPS[[i]]$Hide[centPS[[i]]$pass==1],centPS[[i]]$Hide[centPS[[i]]$pass==0])$statistic 
  W[8,i]<-t.test(centCD[[i]]$Hide[centCD[[i]]$pass==1],centCD[[i]]$Hide[centCD[[i]]$pass==0])$statistic
  W[9,i]<-t.test(centICS[[i]]$Hide[centICS[[i]]$pass==1],centICS[[i]]$Hide[centICS[[i]]$pass==0])$statistic
  p[1,i]<-t.test(centPS[[i]]$PageRank[centPS[[i]]$pass==1],centPS[[i]]$PageRank[centPS[[i]]$pass==0])$p.value 
  p[2,i]<-t.test(centCD[[i]]$PageRank[centCD[[i]]$pass==1],centCD[[i]]$PageRank[centCD[[i]]$pass==0])$p.value
  p[3,i]<-t.test(centICS[[i]]$PageRank[centICS[[i]]$pass==1],centICS[[i]]$PageRank[centICS[[i]]$pass==0])$p.value
  p[4,i]<-t.test(centPS[[i]]$tarEnt[centPS[[i]]$pass==1],centPS[[i]]$tarEnt[centPS[[i]]$pass==0])$p.value 
  p[5,i]<-t.test(centCD[[i]]$tarEnt[centCD[[i]]$pass==1],centCD[[i]]$tarEnt[centCD[[i]]$pass==0])$p.value
  p[6,i]<-t.test(centICS[[i]]$tarEnt[centICS[[i]]$pass==1],centICS[[i]]$tarEnt[centICS[[i]]$pass==0])$p.value
  p[7,i]<-t.test(centPS[[i]]$Hide[centPS[[i]]$pass==1],centPS[[i]]$Hide[centPS[[i]]$pass==0])$p.value 
  p[8,i]<-t.test(centCD[[i]]$Hide[centCD[[i]]$pass==1],centCD[[i]]$Hide[centCD[[i]]$pass==0])$p.value
  p[9,i]<-t.test(centICS[[i]]$Hide[centICS[[i]]$pass==1],centICS[[i]]$Hide[centICS[[i]]$pass==0])$p.value
}

Wj<-matrix(NA,ncol=7,nrow=9)
pj<-matrix(NA,ncol=7,nrow=9)
for(i in 1:7){
  Wj[1,i]<-t.test(centPS[[i]]$PageRank[centPS[[i]]$justpass==1],centPS[[i]]$PageRank[centPS[[i]]$justpass==0])$statistic 
  Wj[2,i]<-t.test(centCD[[i]]$PageRank[centCD[[i]]$justpass==1],centCD[[i]]$PageRank[centCD[[i]]$justpass==0])$statistic
  Wj[3,i]<-t.test(centICS[[i]]$PageRank[centICS[[i]]$justpass==1],centICS[[i]]$PageRank[centICS[[i]]$justpass==0])$statistic
  Wj[4,i]<-t.test(centPS[[i]]$tarEnt[centPS[[i]]$justpass==1],centPS[[i]]$tarEnt[centPS[[i]]$justpass==0])$statistic 
  Wj[5,i]<-t.test(centCD[[i]]$tarEnt[centCD[[i]]$justpass==1],centCD[[i]]$tarEnt[centCD[[i]]$justpass==0])$statistic
  Wj[6,i]<-t.test(centICS[[i]]$tarEnt[centICS[[i]]$justpass==1],centICS[[i]]$tarEnt[centICS[[i]]$justpass==0])$statistic
  Wj[7,i]<-t.test(centPS[[i]]$Hide[centPS[[i]]$justpass==1],centPS[[i]]$Hide[centPS[[i]]$justpass==0])$statistic 
  Wj[8,i]<-t.test(centCD[[i]]$Hide[centCD[[i]]$justpass==1],centCD[[i]]$Hide[centCD[[i]]$justpass==0])$statistic
  Wj[9,i]<-t.test(centICS[[i]]$Hide[centICS[[i]]$justpass==1],centICS[[i]]$Hide[centICS[[i]]$justpass==0])$statistic
  pj[1,i]<-t.test(centPS[[i]]$PageRank[centPS[[i]]$justpass==1],centPS[[i]]$PageRank[centPS[[i]]$justpass==0])$p.value 
  pj[2,i]<-t.test(centCD[[i]]$PageRank[centCD[[i]]$justpass==1],centCD[[i]]$PageRank[centCD[[i]]$justpass==0])$p.value
  pj[3,i]<-t.test(centICS[[i]]$PageRank[centICS[[i]]$justpass==1],centICS[[i]]$PageRank[centICS[[i]]$justpass==0])$p.value
  pj[4,i]<-t.test(centPS[[i]]$tarEnt[centPS[[i]]$justpass==1],centPS[[i]]$tarEnt[centPS[[i]]$justpass==0])$p.value 
  pj[5,i]<-t.test(centCD[[i]]$tarEnt[centCD[[i]]$justpass==1],centCD[[i]]$tarEnt[centCD[[i]]$justpass==0])$p.value
  pj[6,i]<-t.test(centICS[[i]]$tarEnt[centICS[[i]]$justpass==1],centICS[[i]]$tarEnt[centICS[[i]]$justpass==0])$p.value
  pj[7,i]<-t.test(centPS[[i]]$Hide[centPS[[i]]$justpass==1],centPS[[i]]$Hide[centPS[[i]]$justpass==0])$p.value 
  pj[8,i]<-t.test(centCD[[i]]$Hide[centCD[[i]]$justpass==1],centCD[[i]]$Hide[centCD[[i]]$justpass==0])$p.value
  pj[9,i]<-t.test(centICS[[i]]$Hide[centICS[[i]]$justpass==1],centICS[[i]]$Hide[centICS[[i]]$justpass==0])$p.value
}

plot(c(W),c(p))
plot(c(Wj),c(pj),pch=2)
abline(h=0.05)
abline(h=0.01)
abline(h=0.001)


t_PS_PR<-list()
for (i in 1:7){
  t_PS_PR[[i]]<-boot(centPS[[i]], tDiff, R=1000,pv="pass",cm="PageRank")
}
t_PS_PR_D<-c(t_PS_PR[[1]]$t0,t_PS_PR[[2]]$t0,t_PS_PR[[3]]$t0,t_PS_PR[[4]]$t0,t_PS_PR[[5]]$t0,
                  t_PS_PR[[6]]$t0,t_PS_PR[[7]]$t0)
t_PS_PR_D_ciL<-c(boot.ci(t_PS_PR[[1]])$bca[4],boot.ci(t_PS_PR[[2]])$bca[4],boot.ci(t_PS_PR[[3]])$bca[4],
                      boot.ci(t_PS_PR[[4]])$bca[4],boot.ci(t_PS_PR[[5]])$bca[4],boot.ci(t_PS_PR[[6]])$bca[4],
                      boot.ci(t_PS_PR[[7]])$bca[4])
t_PS_PR_D_ciH<-c(boot.ci(t_PS_PR[[1]])$bca[5],boot.ci(t_PS_PR[[2]])$bca[5],boot.ci(t_PS_PR[[3]])$bca[5],
                      boot.ci(t_PS_PR[[5]])$bca[5],boot.ci(t_PS_PR[[5]])$bca[5],boot.ci(t_PS_PR[[6]])$bca[5],
                      boot.ci(t_PS_PR[[7]])$bca[5])
plot(x, t_PS_PR_D,
     ylim=range(0, max(t_PS_PR_D_ciH)),
     pch=19, xlab="Weeks", ylab="t-test statistic",
     main="Per week difference",type="b"
)

# hack: we draw arrows but with very special "arrowheads"
arrows(x, t_PS_PR_D_ciL, x, t_PS_PR_D_ciH, length=0.05, angle=90, code=3)





####Week by week correlation of centrality measures


corKen <- function(data, indices,i){
  dt<-data[indices,]
    c(cor.test(dt[,i], dt[,i+1], method='k')$estimate,
      cor.test(dt[,i], dt[,i+1], method='k')$p.value)
      

}



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

################TARGET ENTROPY
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


######HIDE#######
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



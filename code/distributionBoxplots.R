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
genderPlot <- ggplot(background, aes(x=gender)) + geom_bar() + labs(title="Gender")
plot(genderPlot)
fciPlot<-ggplot(background, aes(x=fci_pre_0)) + geom_bar() + labs(title="FCI pre-test scores")
plot(fciPlot)
fciPlotClasses<-ggplot(background, aes(x=fci_pre_c)) + geom_bar() + labs(title="FCI pre-test score classes")
plot(fciPlotClasses)
agePlot<-ggplot(background, aes(x=age)) + geom_bar() + labs(title="Age")
plot(agePlot)
cohortPlot<-ggplot(background, aes(x=cohort)) + geom_bar() + labs(title="Cohort")
plot(cohortPlot)
plotPass<-ggplot(background, aes(x=pass)) + geom_bar() + labs(title="Pass")
plot(plotPass)
plotjustPass<-ggplot(background, aes(x=justpass)) + geom_bar() + labs(title="Just pass")
plot(plotjustPass)

pr_w1_Plot <- ggplot(centPS[[1]], aes(x=PageRank)) + geom_bar() + labs(title="PageRank week 1")
plot(sort(centPS[[1]]$PageRank,decreasing = T),log = "xy")
plot(sort(centPS[[2]]$PageRank,decreasing = T),log = "xy")
plot(sort(centPS[[3]]$PageRank,decreasing = T),log = "xy")
plot(sort(centPS[[4]]$PageRank,decreasing = T),log = "xy")
plot(sort(centPS[[5]]$PageRank,decreasing = T),log = "xy")
plot(sort(centPS[[6]]$PageRank,decreasing = T),log = "xy")
plot(sort(centPS[[7]]$PageRank,decreasing = T),log = "xy")

plot(sort(centPS[[1]]$Hide,decreasing = T))
plot(sort(centPS[[2]]$Hide,decreasing = T))
plot(sort(centPS[[3]]$Hide,decreasing = T))
plot(sort(centPS[[4]]$Hide,decreasing = T))
plot(sort(centPS[[5]]$Hide,decreasing = T))
plot(sort(centPS[[6]]$Hide,decreasing = T))
plot(sort(centPS[[7]]$Hide,decreasing = T))

plot(sort(centPS[[1]]$tarEnt,decreasing = T))
plot(sort(centPS[[2]]$tarEnt,decreasing = T))
plot(sort(centPS[[3]]$tarEnt,decreasing = T))
plot(sort(centPS[[4]]$tarEnt,decreasing = T))
plot(sort(centPS[[5]]$tarEnt,decreasing = T))
plot(sort(centPS[[6]]$tarEnt,decreasing = T))
plot(sort(centPS[[7]]$tarEnt,decreasing = T))

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

#FCI PRE ZERO IF NOT REGISTERED
p <- ggplot(background, aes(x=pass, y=fci_pre_0)) + 
  geom_boxplot()
# Box plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)

#FCI PRE ZERO IF NOT REGISTERED
table(background$pass,background$fci_pre_c)

##NETWORK MEASURES
#PS
p <- ggplot(centPS[[1]], aes(x=pass, y=PageRank)) + 
  geom_boxplot(notch = T)
# Box plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.3,binwidth = 0.0001)

p <- ggplot(centPS[[2]], aes(x=pass, y=PageRank)) + 
  geom_boxplot(notch = T)
# Box plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.3,binwidth = )

p <- ggplot(centPS[[3]], aes(x=pass, y=PageRank)) + 
  geom_boxplot(notch = T)
# Box plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.3,binwidth = )

p <- ggplot(centPS[[4]], aes(x=pass, y=PageRank)) + 
  geom_boxplot(notch = T)
# Box plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.3,binwidth = )

p <- ggplot(centPS[[5]], aes(x=pass, y=PageRank)) + 
  geom_boxplot(notch = T)
# Box plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.3,binwidth = )

p <- ggplot(centPS[[6]], aes(x=pass, y=PageRank)) + 
  geom_boxplot(notch = T)
# Box plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.3,binwidth = )

p <- ggplot(centPS[[7]], aes(x=pass, y=PageRank)) + 
  geom_boxplot(notch = T)
# Box plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.3,binwidth = 0.001)

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
        main="PS layer passing and failing",
        xlab="Week",
        ylab="tarEnt",
        col="orange",
        border="brown",
        lex.order=T,
        sep=":"
)

boxplot(tarEnt~Week:pass, notch=T,
        data=dfCD,
        main="CD layer passing and failing",
        xlab="Week",
        ylab="tarEnt",
        col="orange",
        border="brown",
        lex.order=T,
        sep=":"
)


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

boxplot(Hide~Week:pass, notch=T,
        data=dfPS,
        main="PS layer passing and failing",
        xlab="Week",
        ylab="Hide",
        col="orange",
        border="brown",
        lex.order=T,
        sep=":"
)

boxplot(Hide~Week:pass, notch=T,
        data=dfCD,
        main="CD layer passing and failing",
        xlab="Week",
        ylab="Hide",
        col="orange",
        border="brown",
        lex.order=T,
        sep=":"
)


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


boxplot(PageRank~Week:justpass, notch=T,
        data=dfPS,
        main="PS layer justpassing and failing",
        xlab="Week",
        ylab="PageRank",
        col="orange",
        border="brown",
        lex.order=T,
        sep=":"
)

boxplot(PageRank~Week:justpass, notch=T,
        data=dfCD,
        main="CD layer justpassing and failing",
        xlab="Week",
        ylab="PageRank",
        col="orange",
        border="brown",
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
        col="orange",
        border="brown",
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
        col="orange",
        border="brown",
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



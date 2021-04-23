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
        col="orange",
        border="brown",
        lex.order=T,
        sep=":"
)

boxplot(PageRank~Week:pass, notch=T,
        data=dfCD,
        main="CD layer passing and failing",
        xlab="Week",
        ylab="PageRank",
        col="orange",
        border="brown",
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
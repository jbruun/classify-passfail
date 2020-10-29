library(ggplots2)
background<-data.frame(age=centPS[[1]]$age,gender=centPS[[1]]$gender,cohort=centPS[[1]]$cohort,fci_pre=centPS[[1]]$fci_pre,
                       fci_pre_0=centPS[[1]]$fci_pre_0,fci_pre_c=centPS[[1]]$fci_pre_c,pass=centPS[[1]]$pass,justpass=centPS[[1]]$justpass)
#Distributions of variables
genderPlot <- ggplot(background, aes(x=gender)) + geom_bar() + labs(title="Gender")
plot(genderPlot)
fciPlot<-ggplot(background, aes(x=fci_pre_0)) + geom_bar() + labs(title="FCI pre-test scores")
plot(fciPlot)
fciPlotClasses<-ggplot(background, aes(x=fci_pre_c)) + geom_bar() + labs(title="FCI pre-test score classes")
plot(fciPlotClasses)
agePlot<-ggplot(background, aes(x=age)) + geom_bar() + labs(title="FCI pre-test score classes")
plot(agePlot)
cohortPlot<-ggplot(background, aes(x=cohortc)) + geom_bar() + labs(title="FCI pre-test scores")
plot(cohortPlot)
plotPass<-ggplot(background, aes(x=pass)) + geom_bar() + labs(title="FCI pre-test scores")
plot(plotPass)

pr_w1_Plot <- ggplot(centPS[[1]], aes(x=PageRank)) + geom_bar() + labs(title="PageRank week 1")
plot(pr_w1_Plot)

#Box plots
# Basic box plot
p <- ggplot(background, aes(x=pass, y=gender)) + 
  geom_boxplot()
# Box plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
# Box plot with jittered points
# 0.2 : degree of jitter in x direction
p + geom_jitter(shape=16, position=position_jitter(0.2))


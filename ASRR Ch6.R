#ASRR Chapter 6

#6.4 example
ecls=read.table(file="ecls2577.txt",header=TRUE)
ecls$schtype=car::recode(ecls$s2kpupri,"2='0';'else'")

#do logistic regression and extract statistics from glm object
reg=glm(schtype ~ wksesl,data=ecls,family=binomial)  #y~x formula statement
exp(reg$coefficients)  #convert log-odds to OR
reg$null.deviance   #null deviance (intercept-only model)
reg$deviance   #deviance after using x
Rsq<-((reg$null.deviance)-(reg$deviance))/reg$null.deviance
Rsq

#plots
#scatterplot w/logistic function
fit = glm(schtype ~ wksesl, data=ecls, family=binomial)
newdat <- data.frame(wksesl=seq(20,85,len=100))
newdat$schtype = predict(fit, newdata=newdat, type="response")
plot(schtype ~ jitter(wksesl), data=ecls, col="red4",xlim=c(20,85),
     ylab="school type (0=private, 1=public)",xlab="SES")
lines(schtype ~ wksesl, newdat, col="green4", lwd=2)
#scatterplot w/linear function and log probabilities
library(effects)
plot(allEffects(fit,se=F))

#predict from model
predict(reg, data.frame(wksesl=c(30,50,70)), type="response")

#6.5 influential observations
table(ecls$schtype)

#6.6 Plots
library(Hmisc)
ecls$schtype=factor(ecls$schtype,level=c(0,1),labels=c("private","public"))

#example 1
ecls$ses5=cut2(ecls$wksesl,g=5)  #g=5 creates quintiles
tab=table(ecls$schtype,as.factor(ecls$ses5))
addmargins(tab)
p=prop.table(tab,margin=2)
p
barplot(p,col=c("blue","red"),
        ylim=c(0,1),beside=T,
        legend=rownames(tab),args.legend=list(x="topright"),
        main="Proportion of students in public and 
        private school by SES quintile")

#example 2
ecls$ses10=cut2(ecls$wksesl,g=10)  #g=10 creates deciles
tab=table(ecls$schtype,as.factor(ecls$ses10))
addmargins(tab)
p=prop.table(tab,margin=2)
p
barplot(p,col=c("blue","red"),
        ylim=c(0,1),beside=T,las=2,
        legend=rownames(tab),args.legend=list(x="topright"),
        main="Proportion of students in public and 
        private school by SES decile")






##ASRR Chapter 6 Statistics and Data Analysis in a Logistic Model

#6.4 Data analytic example #1
library(NHANES)
dat<-NHANES[NHANES$SurveyYr=="2011_12",]
#subset rows by levels of 2 factors
#dat<-dat[which(dat$MaritalStatus=="Married"),]
table(dat$SleepTrouble)
prop.table(table(dat$SleepTrouble))
library(survival)

#do logistic regression and extract statistics from glm object
reg=glm(SleepTrouble ~ DaysPhysHlthBad,data=dat,family=binomial)  #y~x formula statement
exp(reg$coefficients)  #convert log-odds to OR
reg$null.deviance   #null deviance (intercept-only model)
reg$deviance   #deviance after using x
Rsq<-((reg$null.deviance)-(reg$deviance))/reg$null.deviance
Rsq

#scatterplot w/linear function and log probabilities
library(effects)
plot(allEffects(reg,se=F))

#predict from model
predict(reg, data.frame(DaysPhysHlthBad=c(5,10,15)), type="response")

#6.5 influential observations
table(dat$SleepTrouble)
#boxplot rule
#find outlier residuals and their case numbers
library(car)
res<-residuals(reg,type="pearson")
Boxplot(res, id.method="y")

#6.6 Plots
library(Hmisc)

#g=3 in cut2() creates tertile-based groups
dat$badhealth=cut2(dat$DaysPhysHlthBad,g=3)  
tab=table(dat$SleepTrouble,as.factor(dat$badhealth))
addmargins(tab)
p=prop.table(tab,margin=2)
p
barplot(p,col=c("blue","red"),
        ylim=c(0,1),beside=T,
        legend=rownames(tab),args.legend=list(x="topright"),
        main="Proportion of cases reporting sleep trouble
        by days/month of bad physical health")

#6.8 Data analytic example 2
reg=glm(status ~ nodes,data=gbsg,family=binomial)
exp(reg$coefficients) 
reg$null.deviance 
reg$deviance  
Rsq<-((reg$null.deviance)-(reg$deviance))/reg$null.deviance
Rsq

#we label the levels of the 0,1 variable for plotting purposes
gbsg$illness.status=factor(gbsg$status,level=c(1,0),labels=c("recurrence/death","no recurrence"))
library(Hmisc)
gbsg$nodes3=cut2(gbsg$nodes,g=3)  
tab=table(gbsg$illness.status,as.factor(gbsg$nodes3))
addmargins(tab)
p=prop.table(tab,margin=2)
p
barplot(p,col=c("blue","red"),
        ylim=c(0,1),beside=T,
        legend=rownames(tab),args.legend=list(x="topright"),
        main="Proportion of cancer recurrence/death 
        by number of cancer nodes")

#boxplot rule
res<-residuals(reg,type="pearson")
library(car)
Boxplot(res, id.method="y")

#refit model
reg2=glm(status ~ nodes,data=gbsg,family=binomial,subset=-c(374,541))
exp(reg2$coefficients) 
reg2$null.deviance 
reg2$deviance  
Rsq<-((reg2$null.deviance)-(reg2$deviance))/reg2$null.deviance
Rsq




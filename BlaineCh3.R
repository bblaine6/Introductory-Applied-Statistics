#ASRR Chapter 3
##Statistics and Data Analysis in an ANOVA Model

##3.3 Data analytic example 1
library(NHANES)
dat<-NHANES[NHANES$SurveyYr=="2011_12",]

#find location statistics by group
tapply(dat$BPSys1,dat$Gender,mean,na.rm=TRUE)
tapply(dat$BPSys1,dat$Gender, function(x) mean(x,trim=0.1,na.rm=TRUE))
tapply(dat$BPSys1,dat$Gender,median,na.rm=TRUE)

#use R to find group means and calculate MD and MedD
MD<-mean(dat$BPSys1[dat$Gender=="male"],na.rm=T)-
  mean(dat$BPSys1[dat$Gender=="female"],na.rm=T)
MD

#use R to find group medians and calculate MedD
MedD<-median(dat$BPSys1[dat$Gender=="male"],na.rm=T)-
  median(dat$BPSys1[dat$Gender=="female"],na.rm=T)
MedD

#other statistics
#VR
tapply(dat$BPSys1,dat$Gender,var,na.rm=TRUE)
VR<-var(dat$BPSys1[dat$Gender=="female"],na.rm=T)/
  var(dat$BPSys1[dat$Gender=="male"],na.rm=T)
VR

#Cohen's d
library(effsize)
d<-cohen.d(dat$BPSys1,dat$Gender,na.rm=T)
d$estimate

#plots for data analytic example 1
library(lattice)
#histogram
histogram(~BPSys1|Gender,data=dat,
          type="count",
          breaks=seq(0,300,by=10),
          layout=c(1,2))

#density plot
densityplot(~BPSys1|Gender,data=dat,
            breaks=seq(0,300,by=10),
            plot.points="F",layout=c(1,2))

#boxplot
bwplot(Gender~BPSys1,data=dat)

#outlier analysis
b<-boxplot(BPSys1~Gender,plot=F,data=dat)
#5-number summaries and group ns
b$stats
b$n
#outlier values
b$out
length(b$out)
#group assignments of outlier values
b$group
b$names

##3.3 Data analytic example 2
library(MASS)
?gehan   #pulls up documentation for the dataset

#plots
histogram(~time|treat,data=gehan,
          type="count",
          breaks=seq(0,40,by=5),
          layout=c(1,2))

bwplot(treat~time,data=gehan)

#statistics
tapply(gehan$time,gehan$treat,mean)
tapply(gehan$time,gehan$treat,median)

MD<-mean(gehan$time[gehan$treat=="6-MP"],na.rm=T)-
  mean(gehan$time[gehan$treat=="control"],na.rm=T)
MD
d<-cohen.d(gehan$time,gehan$treat,na.rm=T)
d$estimate

VR<-var(gehan$time[gehan$treat=="6-MP"],na.rm=T)/
  var(gehan$time[gehan$treat=="control"],na.rm=T)
VR

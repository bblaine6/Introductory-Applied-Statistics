#ASRR Chapter 3

ecls=read.table(file="ecls200.txt",header=TRUE)

#3.3.1 
tapply(ecls$c1rmscal,ecls$gender,mean)
mdiff2=mean(ecls$c1rmscal[ecls$gender==2])-mean(ecls$c1rmscal[ecls$gender==1])
mdiff2

mdifftrim=mean(ecls$c1rmscal[ecls$gender==2],tr=.1)-
  mean(ecls$c1rmscal[ecls$gender==1],tr=.1)
mdifftrim

library(psych)
mdiffwin=winsor.mean(ecls$c1rmscal[ecls$gender==2],tr=.1)-
  winsor.mean(ecls$c1rmscal[ecls$gender==1],tr=.1)
mdiffwin

meddiff=median(ecls$c1rmscal[ecls$gender==2])-
  median(ecls$c1rmscal[ecls$gender==1])
meddiff

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
tapply(ecls$c1rmscal,ecls$gender,mode)

tapply(ecls$c1rmscal,ecls$gender,var)
vr=var(ecls$c1rmscal[ecls$gender==1])/var(ecls$c1rmscal[ecls$gender==2])
vr

range(ecls$c1rmscal[ecls$gender==1])    #boys range
range(ecls$c1rmscal[ecls$gender==2])    #girls range

IQR(ecls$c1rmscal[ecls$gender==1])    #boys IQR
IQR(ecls$c1rmscal[ecls$gender==2])    #girls IQR

tapply(ecls$c1rmscal,ecls$gender,skew)
table(ecls$c1rmscal[ecls$gender==1])   #boys
table(ecls$c1rmscal[ecls$gender==2])   #girls

#3.3.2
library(car)
ecls$gender=as.factor(recode(ecls$gender, "1='boys';2='girls'"))
table(ecls$gender)

library(lattice)
histogram(~c1rmscal|gender, data = ecls)
histogram(~c1rmscal|gender, 
          data = ecls,
          type="count",
          breaks=seq(0,50,by=2.5),
          layout=c(1,2))

densityplot(~c1rmscal|gender,data = ecls,layout=c(1,2))

bwplot(~c1rmscal|gender,data = ecls,layout=c(1,2))
b=boxplot(ecls$c1rmscal~ecls$gender,plot=FALSE)
b

#3.3.3
z=scale(ecls$c1rmscal[ecls$gender=="boys"])
new=as.data.frame(cbind(ecls$c1rmscal[ecls$gender=="boys"],z))
new[new$V2 < -3.0 | new$V2 > 3.0, ]

#3.4
table(ecls$p1hfamil)
library(car)
ecls$famtype=as.factor(recode(ecls$p1hfamil, "1='parentsib';2:5='other'"))
table(ecls$famtype)

table(ecls$wkdaded)
ecls$daded=as.factor(recode(ecls$wkdaded, "3='HS'; 6='Bachelors'; else=NA"))
table(ecls$daded)








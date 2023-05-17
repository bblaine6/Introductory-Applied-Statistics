#ASRR Chapter 2

ecls=read.table(file='C:/Users/bblaine/Desktop/ecls200.txt',header=TRUE)
str(ecls)     #structure

#2.2
mean(ecls$c1rmscal)    #dataframename$variablename
library(psych)
mean(ecls$c1rmscal)
median(ecls$c1rmscal)
mean(ecls$c1rmscal,tr=.1)
winsor.mean(ecls$c1rmscal,trim=.1)

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode(ecls$c1rmscal)

#2.3
sd(ecls$c1rmscal)
winsor.sd(ecls$c1rmscal,trim=.1)
mad(ecls$c1rmscal)
IQR(ecls$c1rmscal)

#2.4
skew(ecls$c1rmscal)
table(ecls$c1rmscal)
hist(ecls$c1rmscal)
h=hist(ecls$c1rmscal)
h
hist(ecls$c1rmscal, breaks=5)
hist(ecls$c1rmscal, breaks=20)
hist(ecls$c1rmscal,freq=FALSE)
rug(jitter(ecls$c1rmscal))
lines(density(ecls$c1rmscal))
boxplot(ecls$c1rmscal,horizontal=T)
b=boxplot(ecls$c1rmscal,horizontal = TRUE)
b

#2.5
sort(scale(ecls$c1rmscal))
z=scale(ecls$c1rmscal)
new=as.data.frame(cbind(ecls$c1rmscal,z))
new[new$V2 < -3.0 | new$V2 > 3.0, ]

#
table(ecls$p1disabl)
tab=table(ecls$p1disabl)
rownames(tab)=c("yes","no")
tab
p=prop.table(tab)
bp=barplot(p,ylim=c(0,1),xlab="Disability status",main = "Proportions of Kindergarten students who are disabled")
text(bp,0,round(p,2),cex = 1,pos = 3)

##Ch2 assigmt
library(MASS)
library(psych)
#2.7.1
mean(cats$Bwt)
mean(cats$Bwt,tr=.1)
winsor.mean(cats$Bwt,trim=.1)
median(cats$Bwt)
#2.7.2
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode(cats$Bwt)
table(cats$Bwt)
#2.7.3
sd(birthwt$age)
IQR(birthwt$age)
b=boxplot(birthwt$age)
sd(birthwt$age[birthwt$age<45])
IQR(birthwt$age[birthwt$age<45])
#2.7.4
hist(Melanoma$time)
hist(Melanoma$time,breaks=5)
hist(Melanoma$time,breaks=20)
#whatis lost?bimodality
#break=20 reveals random characteristics





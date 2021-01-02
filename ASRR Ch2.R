#ASRR Chapter 2

ecls=read.table(file="ecls200.txt",header=TRUE)
str(ecls)

#2.2
mean(ecls$c1rmscal)
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




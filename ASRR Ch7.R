#ASRR Chapter 7

#7.6
library(MASS)
str(Melanoma)
?Melanoma
boxplot(time~ulcer,data=Melanoma, horizontal = T)
tapply(Melanoma$time,Melanoma$ulcer,mean)
table(Melanoma$ulcer)  

#7.6 example 1
stat=(mean(Melanoma$time[Melanoma$ulcer == 0]))-(mean(Melanoma$time[Melanoma$ulcer == 1]))
stat
set.seed(1008)
N=5000
meandiff=numeric(N)
for (i in 1:N) {
  data <- sample(Melanoma$time,205,replace=FALSE)
  grp1 <- data[1:115]
  grp2 <- data[116:205]
  meandiff[i] <- mean(grp1)-mean(grp2)
}
hist(meandiff, main="reference distribution of survival time
mean difference under Ho")
pvalue=length(which(abs(meandiff)>=stat))/N
pvalue

#7.6 example 2
stat=(median(Melanoma$time[Melanoma$ulcer == 0]))-
  (median(Melanoma$time[Melanoma$ulcer == 1]))
stat
N=5000
meddiff=numeric(N)     #newly named numeric vector object
set.seed(1008)
for (i in 1:N) {
  data <- sample(Melanoma$time,205, replace=FALSE)
  grp1 <- data[1:115]
  grp2 <- data[116:205]
  meddiff[i] <- median(grp1)-median(grp2)      #median difference
}
hist(meddiff, main="reference distribution of survival time
     median difference under Ho")       #call meddiff object
pvalue=length(which(abs(meddiff)>=stat))/N       #call meddiff object
pvalue




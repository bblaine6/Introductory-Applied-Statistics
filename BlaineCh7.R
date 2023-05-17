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


#demo problem: test hypothesis that smoking causes lower birthwt babies
#explore relationship
boxplot(birthwt$bwt~birthwt$smoke,horizontal = T)
tapply(birthwt$bwt,birthwt$smoke,mean)
table(birthwt$smoke)

#find statistic of interest
stat=3055.7-2771.9

#generate distribution of mean difference under Ho
N=5000
meandiff=numeric(N)
for (i in 1:N) {
  data <- sample(birthwt$bwt,189,replace=FALSE)
  grp1 <- data[1:115]
  grp2 <- data[116:189]
  meandiff[i] <- mean(grp1)-mean(grp2)
}
hist(meandiff)
pvalue=length(which(abs(meandiff)>=stat))/N
pvalue

#ch7/8 assignmt
stat=(mean(gehan$time[gehan$treat == "6-MP"]))-
  (mean(gehan$time[gehan$treat == "control"]))
stat
N=5000
meandiff=numeric(N)
for (i in 1:N) {
  data <- sample(gehan$time,42,replace=FALSE)
  grp1 <- data[1:21]
  grp2 <- data[22:42]
  meandiff[i] <- mean(grp1)-mean(grp2)
}
hist(meandiff, main="reference distribution of remission time
mean difference under Ho")
pvalue=length(which(abs(meandiff)>=stat))/N
pvalue

#mean difference
N=2000
boot=numeric(N)
for (i in 1:N) {
  grp1 <- sample(gehan$time[gehan$treat == "6-MP"],22,replace=T)
  grp2 <- sample(gehan$time[gehan$treat == "control"],22,replace=T)
  boot[i] <- mean(grp1)-mean(grp2)
}
SEb=sd(boot)

#t interval
mdiff=(mean(gehan$time[gehan$treat == "6-MP"]))-
  (mean(gehan$time[gehan$treat == "control"]))
n=42
moe=qt(0.975,n-1)*SEb   
mdiff+c(-1,1)*moe
#percentile interval
quantile(boot,c(0.025,0.975))

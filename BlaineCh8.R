#ASRR Chapter 8

#8.4.1
ecls=read.table(file="ecls200.txt",header=TRUE)
str(ecls)

#bootstrap distribution of the sample mean
N=1000
n=104
boot=numeric(N) 
for (i in 1:N) {
  bootsamp <- sample(ecls$c1rmscal[ecls$gender == 1],n,replace=T)
  boot[i] <- mean(bootsamp)
}
hist(boot, main="bootstrapped distribution (n=104)
     Kindergarten girls' population",xlab="mean math score")
SEb=sd(boot)

xbar=mean(ecls$c1rmscal[ecls$gender == 1])
moe=qt(0.975,n-1)*SEb   
xbar-moe
xbar+moe
#or do in one operation
xbar+c(-1,1)*moe

#8.4.2
#95% percentile CI
quantile(boot,c(0.025,0.975))

#8.5
boxplot(ecls$c1rmscal[ecls$gender == 1],horizontal=T)
table(ecls$gender)

#bootstrap distribution of the sample median
N=1000
n=96
boot=numeric(N) 
for (i in 1:N) {
  bootsamp <- sample(ecls$c1rmscal[ecls$gender == 2],n,replace=T)
  boot[i] <- median(bootsamp)
}
hist(boot, main="bootstrapped distribution (n=96)
     Kindergarten boys' population", xlab="median math score")
SEb=sd(boot)

#confidence intervals
med=median(ecls$c1rmscal[ecls$gender == 2])
moe=qt(0.975,n-1)*SEb
med+c(-1,1)*moe                #t interval
quantile(boot,c(0.025,0.975))  #percentile interval

#8.7.1
dat=rnorm(10000, 5, 2)   #set up a normal population with mu=5, sigma=2
N=1000
n=100
boot=numeric(N) 
for (i in 1:N) {
  bootsamp <- sample(dat,n,replace=T)
  boot[i] <- mean(bootsamp)
}
#bootstrapped standard error of mean
SEb=sd(boot)
SEb


#8.9.6
#overall
N=1000
n=189
boot=numeric(N)
for (i in 1:N) {
  bootsamp <- sample(birthwt$bwt,n,replace=T)
  boot[i] <- median(bootsamp)
}
SEb=sd(boot)
med=median(birthwt$bwt)
moe=qt(0.975,n-1)*SEb
med+c(-1,1)*moe     

#smoke=0
n=115
boot1=numeric(N)
for (i in 1:N) {
  bootsamp <- sample(birthwt$bwt[birthwt$smoke == 0],n,replace=T)
  boot1[i] <- median(bootsamp)
}

SEb=sd(boot1)
med=median(birthwt$bwt[birthwt$smoke == 0])
moe=qt(0.975,n-1)*SEb   
med+c(-1,1)*moe

#smoke=1
n=74
boot2=numeric(N)
for (i in 1:N) {
  bootsamp <- sample(birthwt$bwt[birthwt$smoke == 1],n,replace=T)
  boot2[i] <- median(bootsamp)
}
SEb=sd(boot2)
med=median(birthwt$bwt[birthwt$smoke == 1])
moe=qt(0.975,n-1)*SEb   
med+c(-1,1)*moe

#notched boxplot as inferential tool
b=boxplot(birthwt$bwt~birthwt$smoke,horizontal = T,notch=T)

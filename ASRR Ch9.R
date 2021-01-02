#ASRR Chapter 9

#9.4
ecls=read.table(file="ecls200.txt",header=TRUE)
str(ecls)
table(ecls$p1disabl)
prop.table(table(ecls$p1disabl))
boxplot(c1rrscal~p1disabl,data=ecls, horizontal = T)

stat=(mean(ecls$c1rrscal[ecls$p1disabl == 2]))-(mean(ecls$c1rrscal[ecls$p1disabl == 1]))
stat
N=5000
meandiff=numeric(N)
for (i in 1:N) {
  data <- sample(ecls$c1rrscal,200,replace=FALSE)
  grp1 <- data[1:36]
  grp2 <- data[37:200]
  meandiff[i] <- mean(grp1)-mean(grp2)
}
hist(meandiff, main="distribution of reading achievement
     mean difference under Ho") 
pvalue=length(which(abs(meandiff)>=stat))/N
pvalue

#mean difference
N=2000
boot=numeric(N)
for (i in 1:N) {
  grp1 <- sample(ecls$c1rrscal[ecls$p1disabl == 1],36,replace=T)
  grp2 <- sample(ecls$c1rrscal[ecls$p1disabl == 2],164,replace=T)
  boot[i] <- mean(grp2)-mean(grp1)
}
SEb=sd(boot)

#t interval
mdiff=(mean(ecls$c1rrscal[ecls$p1disabl == 2]))-(mean(ecls$c1rrscal[ecls$p1disabl == 1]))
n=200
moe=qt(0.975,n-1)*SEb   
mdiff+c(-1,1)*moe
#percentile interval
quantile(boot,c(0.025,0.975))

#9.5
library(car)
ecls$daded=as.factor(recode(ecls$wkdaded, "1:3='low'; 6:9='high'; else=NA"))
ecls$schtype=as.factor(recode(ecls$s2kpupri,"2='0';'else'"))
ecls$schtype=factor(ecls$schtype,level=c(0,1),labels=c("private","public"))
tab=table(ecls$schtype,ecls$daded)
addmargins(tab)

prop.table(tab,margin=2)
ptab=prop.table(tab,margin=2)
RR=ptab[1,1]/ptab[1,2]
RR  

obs=RR
N=2000
riskratio=numeric(N) 
for (i in 1:N) {
  data <- sample(0:1,137, replace=T)
  grp1 <- data[1:55]
  grp2 <- data[56:137]
  riskratio[i] <- mean(grp1)/mean(grp2)
}
hist(riskratio,main="risk ratio distribution under Ho")
pvalue=length(which(riskratio>=obs))/N
pvalue

high=c(rep(1,17),rep(0,38))
low=c(rep(1,10),rep(0,72))

N=2000
boot=numeric(N) 
for (i in 1:N) {
  grp1 <- sample(high,55,replace=T)
  grp2 <- sample(low,82,replace=T)
  boot[i] <- mean(grp1)/mean(grp2)
}
hist(boot,main="bootstrapped risk ratio distribution")
quantile(boot,c(0.025,0.975))

#9.6
reg=lm(c4rrscal~c1rrscal,data=ecls)   
reg$coefficients[2] #retrieve regression coeff

beta=reg$coefficients[2] 
N=2000
rcoeff=numeric(N) 
for (i in 1:N) {
  y=sample(ecls$c4rrscal,200,replace=F)
  x=sample(ecls$c1rrscal,200,replace=F)
  mod<-lm(y~x)
  rcoeff[i] <- mod$coefficients[2]
}
hist(rcoeff,main="regression coefficient distribution under Ho")
pvalue=length(which(abs(rcoeff)>=beta))/N
pvalue

N=2000
boot=numeric(N) 
for (i in 1:N) {
  dat <- ecls[sample(nrow(ecls),200,replace=T),]
  mod <- lm(c4rrscal~c1rrscal,data=dat)
  boot[i] <- mod$coefficients[2]
}
hist(boot,main="bootstrapped regression coefficient distribution")
#t interval
SEb=sd(boot)
n=200
moe=qt(0.975,n-1)*SEb   
beta+c(-1,1)*moe
#percentile interval
quantile(boot,c(0.025,0.975))

#9.7
table(ecls$schtype)
mod=glm(schtype~wksesl,data=ecls,family=binomial)
exp(mod$coefficients[2])

OR=exp(mod$coefficients[2])
N=2000
rcoeff=numeric(N) 
for (i in 1:N) {
  y=sample(0:1,200, replace=T)
  x=sample(ecls$wksesl,200,replace=F)
  mod<-glm(y~x,family=binomial)
  rcoeff[i] <- exp(mod$coefficients[2])
}
hist(rcoeff,main="odds ratio distribution under Ho")
pvalue=length(which(rcoeff<=OR))/N
pvalue

N=3000
boot=numeric(N) 
for (i in 1:N) {
  dat <- ecls[sample(nrow(dat),200,replace=T),]
  mod <- glm(schtype~wksesl,data=dat,family=binomial)
  boot[i] <- exp(mod$coefficients[2])
}
hist(boot,main="bootstrapped odds ratio distribution")  

#t interval
SEb=sd(boot)
n=200
moe=qt(0.975,n-1)*SEb   
OR+c(-1,1)*moe
quantile(boot,c(0.05,0.95))





#9.3
library(MASS)

#subset participants in the FT condition
new<-anorexia[anorexia$Treat=="FT",]

#create change variable
new$change<-new$Postwt-new$Prewt

#statistics
Dbar<-mean(new$change)
Dmed<-median(new$change)
percentchange<-Dbar/mean(new$Prewt)
cohensd<-Dbar/sd(new$Prewt)
Dbar
Dmed
percentchange
cohensd

#plots
hist(new$change,prob=T)
lines(density(new$change))
boxplot(new$change,horizontal = T)

#randomization test of Ho
N=5000
diff=numeric(N)
for (i in 1:N) {
  signs=sample(c(1,-1),17,replace=T)
  samp=new$change*signs
  diff[i]=mean(samp)
}

hist(diff,main="probability distribution under Ho")
pvalue=length(which(abs(diff)>=Dbar))/N
pvalue

#bootstrapped parameter estimate using D-bar
N=2000
boot=numeric(N) 
for (i in 1:N) {
  df <- new[sample(nrow(new),17,replace=T),]
  df$change <- df$Postwt-df$Prewt
  boot[i] <- mean(df$change)
}

hist(boot,main="distribution of bootstrapped D-bar statistics")
#t interval
SEb=sd(boot)
n=17
moe=qt(0.975,n-1)*SEb   
Dbar+c(-1,1)*moe
#percentile interval
quantile(boot,c(0.025,0.975))

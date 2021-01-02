##ASRR Chapter 5
#5.2
ecls=read.table(file="ecls200.txt",header=TRUE)
tapply(ecls$c1rmscal,ecls$s2kpupri,mean)

#5.3
x=c(2,3,5,8,7)
y=c(3,7,6,9,4)
reg=lm(y~x)
reg

#5.4
library(lattice)
xyplot(c4rrscal ~ c1rrscal, data=ecls,
       grid=TRUE,
       type=c("p", "r"),    #p=scatterplot, r=regression line
       col.line="red")

reg=lm(c4rrscal~c1rrscal,data=ecls)   #lm() takes a y~x formula statement
reg$coefficients   #intercept and slope
summary(reg)$sigma    #RSE
summary(reg)$r.squared  #R squared
x=data.frame(c1rrscal=c(15,20,25,30,35)) 
predict.lm(reg, x, interval="none")

#5.5
cor(ecls$c4rrscal,ecls$c1rrscal,method="p")  #Pearson r
cor(ecls$c4rrscal,ecls$c1rrscal,method="s")  #Spearman rho
cor(ecls$c4rrscal,ecls$c1rrscal,method="k")  #Kendall tau

#5.6
library(MASS)
zres=stdres(reg)
new=as.data.frame(cbind(reg$residuals,zres))
new[new$zres < -3.0 | new$zres > 3.0, ]
ecls[94,]

#5.7
library(car)
scatterplot(c4rrscal~c1rrscal,
            data=ecls,
            smooth=F,
            regLine=list(method=lm, lty=1, lwd=1, col="red"),
            xlab="fall Kindergarten",ylab="spring 1st grade",
            main="Standardized reading scores for Kindergarten and 
            1st grade students with least-squares regression line")



##ASRR Chapter 5 Statistics and Data Analysis in a Regression Model

#5.4 Data analytic example #1
library(NHANES)
dat<-NHANES[NHANES$SurveyYr=="2011_12",]
#subset rows by levels of 2 factors
dat<-dat[which(dat$Gender=="female" & dat$SmokeNow=="Yes"),]

#scatterplot w/regression line
library(lattice)
xyplot(DaysMentHlthBad ~ DaysPhysHlthBad, data=dat,
       grid=TRUE,
       type=c("p", "r"),    #p=scatterplot, r=regression line
       col.line="red")

#do least squares regression and extract statistics from lm object
reg=lm(DaysMentHlthBad ~ DaysPhysHlthBad, data=dat)   #lm() takes a y~x formula statement
reg$coefficients   #intercept and slope
summary(reg)$sigma    #RSE
summary(reg)$r.squared  #R squared

#influential observations
#boxplot rule
b=boxplot(resid(reg),horizontal=T,main="residuals from regression analysis")
b

#scatterplot
library(car)
## Loading required package: carData
scatterplot(DaysMentHlthBad ~ DaysPhysHlthBad, data=dat,
            smooth=F,
            regLine=list(method=lm, lty=1, lwd=1, col="red"),
            xlab="Self-reported poor physical health (days)",
            ylab="Self-reported poor mental health (days)",
            main="Relationship between poor physical and
          mental health in females who smoke (NHANES data)")

#5.9 Data analytic example 2
#scatterplot w/regression line
library(survival)
library(lattice)
xyplot(ozone ~ temperature, data=environmental,
       grid=TRUE,
       type=c("p", "r"),    #p=scatterplot, r=regression line
       col.line="red")
reg=lm(ozone ~ temperature, data=environmental)
reg$coefficients
summary(reg)$sigma
summary(reg)$r.squared 

cor(environmental$ozone,environmental$temperature,method="p",use="complete.obs")
cor(environmental$ozone,environmental$temperature,method="s",use="complete.obs")

#find outlier residuals and their case numbers
library(car)
Boxplot(resid(reg), id.method="y")
#adjusted model
reg2=lm(ozone ~ temperature, data=environmental, subset = -c(23,34,63,77))
reg2$coefficients
summary(reg2)$sigma
summary(reg2)$r.squared 







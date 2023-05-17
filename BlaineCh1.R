##ASRR Ch 1 
##Foundations I: Introductory Data Analysis with R

#1.3 Data analytic example 1

#library needed packages
library(NHANES)

#create small dataset by selecting survey year and sampling from those cases
dat<-NHANES[NHANES$SurveyYr=="2011_12",]

#location statistics
#na.rm=T tells the function to ignore missing data
mean(dat$Weight,na.rm=T)
median(dat$Weight,na.rm=T)
mean(dat$Weight,tr=.1,na.rm=T)

#variability statistics
sd(dat$Weight,na.rm=T)
mad(dat$Weight,na.rm=T)
IQR(dat$Weight,na.rm=T)

#explore multi-modality
#create a frequency table, then sort it by frequency, then print just the first 6 rows of the table (i.e., the most frequent values) 
library(dplyr)

tab<-table(dat$Weight)
sorted_tab <- tab %>%                 
  as.data.frame() %>% 
  arrange(desc(Freq))
head(sorted_tab) 

#plots
#histogram
hist(dat$Weight)
h=hist(dat$Weight,plot=F)
h$counts
h$breaks

h1<-hist(dat$Weight, breaks=5)
h1$breaks
h1$counts
h2<-hist(dat$Weight, breaks=50)
h2$breaks
h2$counts

#density plot
X<-na.omit(dat$Weight)
h<-hist(X, breaks=20, na.omit=T,
        prob=T,
        main = "Weight of NHANES participant",
        xlab = "weight (kg)")
lines(density(X))

#boxplot
b<-boxplot(dat$Weight,horizontal=T)
b$stats
#generate boxplot rule-defined outliers, list in ascending order, and count the number of scores
sort(b$out)
length(b$out)


#1.5 Data analytic example 2
#frequency table with proportions
library(janitor)
tabyl(dat$Race1)

#create table of category percentages for plotting, then plot
tab<-prop.table(table(dat$Race1))*100
barplot(tab,ylim=c(0,100),ylab = "Percentage(%)",las=2)

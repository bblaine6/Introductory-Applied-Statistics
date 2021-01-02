#ASSR Chapter 4

#4.2
ecls=read.table(file="ecls200.txt",header=TRUE)
table(ecls$gender)
table(ecls$p1disabl)

tab=table(ecls$p1disabl,ecls$gender)   #table(row,column)
rownames(tab)=c("yes","no")
colnames(tab)=c("boy","girl")
addmargins(tab)
prop.table(tab,margin=2)  #margin=2 delivers conditional proportions within column 

#4.3
f=fisher.test(tab)
f$estimate

library(car)
ecls$daded=as.factor(recode(ecls$wkdaded, "1:3='low'; 6:9='high'; else=NA"))
ecls$schtype=as.factor(recode(ecls$s2kpupri,"2='0';'else'"))
ecls$schtype=factor(ecls$schtype,level=c(0,1),labels=c("private","public"))
table(ecls$daded)
table(ecls$schtype)

tab=table(ecls$schtype,ecls$daded)
addmargins(tab)
prop.table(tab,margin=2)

ptab=prop.table(tab,margin=2)
RD=ptab[1,1]-ptab[1,2]
RD   #risk difference
RR=ptab[1,1]/ptab[1,2]
RR   #risk ratio
OR=(ptab[1,1]/ptab[2,1])/(ptab[1,2]/ptab[2,2])
OR   #odds ratio

#4.4
#barplot() requires a table() object
p=prop.table(tab,margin=2)
barplot(p,col=c("blue","red"), 
        ylim=c(0,1),
        beside=T, args.legend=list(x="topleft"),
        legend=rownames(tab),
        main = "Proportions of Kindergarten students who are disabled")




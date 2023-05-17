#ASSR Chapter 4 Statistics and Data Analysis in a Proportions Model

#4.4 Data analytic example 1
library(survival)

#create functions for later use
#each takes a prop.table object
RD<-function(tab) {
  rd=tab[1,1]-tab[1,2]
  print(rd)
}
NNT<-function(tab) {
  nnt=1/((tab[1,1]/(tab[1,1]+tab[2,1]))-(tab[1,2]/(tab[1,2]+tab[2,2])))
  print(nnt)
}

RR<-function(tab){
  rr=tab[1,1]/tab[1,2]
  print(rr)
}

OR<-function(tab){
  or=(tab[1,1]/tab[2,1])/(tab[1,2]/tab[2,2])
  print(or)
}

#data wrangling 
ovarian$treatmt=factor(ovarian$rx,level=c(1,2),labels=c("standard","aggressive"))
ovarian$residual.disease=factor(ovarian$resid.ds,level=c(2,1),labels=c("yes","no"))

#create contingency tables
#frequency table
table(ovarian$residual.disease,ovarian$treatmt)

#proportions table
t<-prop.table(table(ovarian$residual.disease,ovarian$treatmt),margin = 2)
t

#statistics
RD(t)
NNT(t)
RR(t)
OR(t)

#4.6 Data analytic example 2
library(survival)
gbsg$menopause.status=factor(gbsg$meno,level=c(1,0),labels=c("post","pre"))
gbsg$illness.status=factor(gbsg$status,level=c(1,0),labels=c("recurrence/death","no recurrence"))

#create contingency tables
#frequency table
tab<-table(gbsg$illness.status,gbsg$menopause.status)

#proportions table
t<-prop.table(tab,margin = 2)
t

#statistics
RD(t)
RR(t)
OR(t)

barplot(t, 
        ylim=c(0,1),
        beside=T,
        legend=rownames(tab),args.legend=list(x="topleft"),
        main = "Menopause status and 5-year survival 
        of breast cancer patients")



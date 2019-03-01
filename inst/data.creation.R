# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

size1=150
size2=100
size3=80

baseline=rnorm((size1+size2+size3)/5,-1)

y_numeric=c(baseline,rnorm(size1,2),rnorm(size2,4),rnorm(size3,5))

y_logistic=c(round(rep(runif((size1+size2+size3)/5,0))),
		round(runif(size1,0)),round(runif(size2,0)),round(runif(size3,0)))

y_poisson=c(round(baseline+5),round(rnorm(size1,3)),round(rnorm(size2,9)),round(rnorm(size3,5)))

baseline=rep(baseline,6)

# Add some missing values

y_logistic[sample(1:length(y_logistic),10)]=NA
y_numeric[sample(1:length(y_numeric),10)]=NA
y_poisson[sample(1:length(y_poisson),10)]=NA

y_logistic=as.factor(y_logistic)

GROUP=c(c(rep("A",size1/5),rep("B",size2/5),rep("C",size3/5)),
		c(rep("A",size1),rep("B",size2),rep("C",size3)))

VAR=c(sample(c(rep("Cat 1",10),rep("Cat 2",10)),size1+size1/5,replace=T),
		sample(c(rep("Cat 1",10),rep("Cat 2",10)),size2+size2/5,replace=T),
sample(c(rep("Cat 1",10),rep("Cat 2",10)),size3+size3/5,replace=T))

TIMEPOINT=c(rep("D0",length(baseline)/6),rep(c("D1","D2","D3","D4","D5"),length(GROUP)/6))

SUBJID=rep(paste0("Subj ", c(1:(length(GROUP)/6))),rep(6,length(paste0("Subj ", c(1:(length(GROUP)/6))))))
	

data=data.frame(y_numeric,y_logistic,y_poisson,baseline,VAR,GROUP,TIMEPOINT,SUBJID)


save(data, file = "C:\\Users\\jfcollin\\git\\data\\data.RData")







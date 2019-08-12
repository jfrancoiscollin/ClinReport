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


data$AE_niv1=factor(sample(c("AE 1","AE 2","AE 3","AE 4","AE 5",NA,NA,NA,NA),nrow(data),replace=T))
data$AE_niv2=gsub("AE 1","AE 1 lower class",data$AE_niv1)
data$AE_niv2=gsub("AE 2","AE 1 lower class",data$AE_niv2)
data$AE_niv2=gsub("AE 3","AE 2 lower class",data$AE_niv2)
data$AE_niv2=gsub("AE 4","AE 2 lower class",data$AE_niv2)
data$AE_niv2=factor(gsub("AE 5","AE 3 lower class",data$AE_niv2))

save(data, file = "C:\\Users\\jfcollin\\git\\data\\data.RData")



time_to_cure$Subjid=time_to_cure$Animal
time_to_cure$Animal=NULL
time_to_cure$Subjid=1:length(time_to_cure$Subjid)
time_to_cure$time[1]=4
time_to_cure$time[22]=10
time_to_cure$time[57]=2


levels(time_to_cure$Group)[4]="Untreated"
		
#save(time_to_cure, file = "C:\\Users\\jfcollin\\git\\data\\time_to_cure.RData")
#saveRDS(time_to_cure, file = "C:\\Users\\jfcollin\\git\\data\\time_to_cure.RData",version = 2)




#save(datafake, file = "C:\\Users\\jfcollin\\git\\data\\datafake.rda")
#save(time_to_cure, file = "C:\\Users\\jfcollin\\git\\data\\time_to_cure.rda")













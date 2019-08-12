# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

library(reshape2)
library(ClinReport)


load("C:\\Users\\jfcollin\\git\\inst\\data.RData")

# Calculation of at least one subject with an AE

table(data$AE_niv1,data$AE_niv2)

upper=data$AE_niv1
lower=data$AE_niv2



define.key=function(data,upper="AE_niv1",lower="AE_niv2",
		upper.levels="Upper.Levels",
		lower.levels="Lower.Levels")
{

	relev=vector()
	for(i in 1:length(levels(data[,upper])))
	{
		relev[i]=levels(droplevels(data[,lower][data[,upper]==levels(data[,upper])[i]]))
	}
	

	d=data.frame(Levels=levels(data[,upper]),lower.levels=relev)
	colnames(d)[colnames(d)=="lower.levels"]=lower.levels
	colnames(d)[colnames(d)=="Levels"]=upper.levels
	d
}



at.least.one=function(data,subjid,var)
{

	t=table(data[,subjid],data[,var])
	
	at_least_one=apply(t,2,function(x)
			{
				n=length(x[x!=0])
				p=n/length(x)
				alo=paste0(n,"/",length(x)," = ",round(100*p,2),"%")
			})
	
	
	at_least_one
}




report.quali.hlev=function(data,subjid,var_upper,var_lower,
		lower.levels="Lower.Levels",upper.levels=
				"Upper.Levels")
{
	
	lower.levels.asked=lower.levels
	upper.levels.asked=upper.levels
	
	lower.levels=make.names(lower.levels)
	upper.levels=make.names(upper.levels)
	
	alo=at.least.one(data=data,subjid=subjid,var=var_upper)
	alo=data.frame(upper.levels=names(alo),n=alo)
	colnames(alo)[colnames(alo)=="upper.levels"]=upper.levels
	colnames(alo)[colnames(alo)=="n"]="At least one"
	dat=transpose(report.quali(data=data,y=var_upper,subjid=subjid,
					y.levels.label=upper.levels))$output
	dat=merge(dat,alo,by=upper.levels,all.x=T)
	
	dat=dat[,colnames(dat)!=" Missing n(%)"]
	dat=na.omit(dat)
	
	
	alo2=at.least.one(data=data,subjid=subjid,var=var_lower)
	alo2=data.frame(lower.levels=names(alo2),n=alo2)
	colnames(alo2)[colnames(alo2)=="lower.levels"]=lower.levels
	colnames(alo2)[colnames(alo2)=="n"]="At least one"
	dat2=transpose(report.quali(data=data,y=var_lower,subjid=subjid,
					y.levels.label=lower.levels))$output
	dat2=merge(dat2,alo2,by=lower.levels,all.x=T)
	dat2=dat2[,colnames(dat2)!=" Missing n(%)"]
	
	
	key=define.key(data,var_upper,var_lower,lower.levels=lower.levels)
	
	dat=merge(dat,key,by=upper.levels,all.x=T)
	dat=merge(dat,dat2,by=lower.levels,suffixes=c(paste0(" ",var_upper),paste0(" ",var_lower)),all.x=T)
	
	colnames(dat)[colnames(dat)==lower.levels]=lower.levels.asked
	colnames(dat)[colnames(dat)==upper.levels]=upper.levels.asked
	

	dat
	
	
}



report.quali.hlev(data=data,
		subjid="SUBJID",
		var_upper="AE_niv1",
		var_lower="AE_niv2",
		lower.levels="Lower Levels",
		upper.levels="Upper Levels")










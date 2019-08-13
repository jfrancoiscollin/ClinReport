# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

library(reshape2)
library(ClinReport)
library(sas7bdat)



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



at.least.one=function(data,subjid=NULL,var,total=FALSE,var.label="var")
{
	
	t=table(data[,subjid],data[,var])
	
	if(!is.null(subjid))
	{
		
		
		at_least_one=apply(t,2,function(x)
				{
					n=length(x[x!=0])
					p=n/length(x)
					alo=paste0(n,"/",length(x)," = ",round(100*p,2),"%")
				})
	}
	
	if(total)
	{
		
		at_least_one=apply(t,1,function(x)
				{
					n=length(x[x!=0])
					n
				})
		
		n=length(at_least_one[at_least_one!=0])
		p=round(100*(n/length(at_least_one)),2)
		
		at_least_one=data.frame(var="ALL",
				"tot"=paste0(sum(t),"(100%)"),
				"atlo"=paste0(n,"/",length(at_least_one),
						" = ",p,"%"))
		
		colnames(at_least_one)=c(var.label,"n (%)","At least one")
	}
	
	at_least_one
}













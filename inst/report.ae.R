# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

library(reshape2)
library(ClinReport)
library(sas7bdat)

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


#TODO: add a x1 variable argument in the functions, to get the results by
# treatment group


report.quali.hlev=function(data,subjid=NULL,x1=NULL,var_upper,var_lower,
		lower.levels="Lower.Levels",upper.levels=
				"Upper.Levels")
{
	
	if(is.null(subjid)) stop("This function needs a subjid argument")
	
	if(is.null(x1))
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
						y.levels.label=upper.levels,remove.missing=T))$output
		dat=merge(dat,alo,by=upper.levels,all.x=T)
		
		dat=dat[,colnames(dat)!=" Missing n(%)"]
		dat=na.omit(dat)
		
		# calculate the total
		
		alo_tot=at.least.one(data=data,subjid=subjid,var=var_upper,total=TRUE,
				var.label=upper.levels)
		
		# add the total
		
		dat=rbind(alo_tot,dat)
		
		alo2=at.least.one(data=data,subjid=subjid,var=var_lower)
		alo2=data.frame(lower.levels=names(alo2),n=alo2)
		colnames(alo2)[colnames(alo2)=="lower.levels"]=lower.levels
		colnames(alo2)[colnames(alo2)=="n"]="At least one"
		dat2=transpose(report.quali(data=data,y=var_lower,subjid=subjid,
						y.levels.label=lower.levels,remove.missing=T))$output
		dat2=merge(dat2,alo2,by=lower.levels,all.x=T)
		dat2=dat2[,colnames(dat2)!=" Missing n(%)"]
		dat2=na.omit(dat2)
		
		# add the total
		
		colnames(alo_tot)[1]=lower.levels
		dat2=rbind(alo_tot,dat2)
		
		key=define.key(data,var_upper,var_lower,upper.levels=upper.levels,
				lower.levels=lower.levels)
		
		all=data.frame(upper.levels="ALL",lower.levels="ALL")
		colnames(all)=c(upper.levels,lower.levels)
		key=rbind(all,key)
		
		dat=merge(dat,key,by=upper.levels,all.x=T)
		dat=merge(dat,dat2,by=lower.levels,suffixes=c(paste0(" ",var_upper),paste0(" ",var_lower)),all.x=T)
		
		colnames(dat)[colnames(dat)==lower.levels]=lower.levels.asked
		colnames(dat)[colnames(dat)==upper.levels]=upper.levels.asked
		
		dat=dat[,c(lower.levels.asked,paste0("n (%) ",var_lower),paste0("At least one ",var_lower),
						upper.levels.asked,paste0("n (%) ",var_upper),paste0("At least one ",var_upper))]
		
		dat=apply(dat,2,function(x) as.character(x))
		dat[duplicated(dat[,lower.levels.asked]),c(paste0("n (%) ",var_lower),paste0("At least one ",var_lower))]=rep("",2)
		
		dat=as.data.frame(dat)
		
		dat=spacetable(dat,lower.levels.asked)
		
		return(dat)
	}
	
	
	if(!is.null(x1))
	{
		
		dat=report.quali.hlev(data=data,
				subjid=subjid,x1=NULL,var_upper=var_upper,var_lower=var_lower,
				lower.levels=lower.levels,upper.levels=upper.levels)
		
		dat[,x1]=paste(levels(droplevels(data[,x1])),collapse="+")
		
		for(i in 1:length(levels(data[,x1])))
		{
			
			temp=report.quali.hlev(data=droplevels(data[data[,x1]==levels(data[,x1])[i],]),
					subjid=subjid,x1=NULL,var_upper=var_upper,var_lower=var_lower,
					lower.levels=lower.levels,upper.levels=upper.levels)
			
			temp[,x1]=levels(data[,x1])[i]
			
			dat=rbind(dat,temp)
		}
		
		dat=spacetable(dat,x1)
		
		return(dat)
		
	}
	
	
	
	
}



report.quali.hlev(data=data,
		subjid="SUBJID",
		var_upper="AE_niv1",
		var_lower="AE_niv2",
		lower.levels="Lower Levels",
		upper.levels="Upper Levels")


report.quali.hlev(data=data,
		subjid="SUBJID",
		var_upper="AE_niv1",
		var_lower="AE_niv2",
		lower.levels="AE_niv2 Levels",
		upper.levels="AE_niv1 Levels",
		x1="GROUP")


data=read.table(header=T,sep = "\t","C:\\Users\\jfcollin\\Google Drive\\ae_test.txt")

data$PT[data$PT==""]=NA
data$SOC[data$SOC==""]=NA

data=droplevels(data)

report.quali.hlev(data=data,
		subjid="SUBJID",
		var_upper="PT",
		var_lower="SOC",
		lower.levels="SOC",
		upper.levels="PT")




report.quali.hlev(data=data,
		subjid="SUBJID",
		var_upper="PT",
		var_lower="SOC",
		lower.levels="SOC",
		upper.levels="PT",
		x1="group")



data=read.csv("C:\\Users\\jfcollin\\Google Drive\\ADAE.csv")

data=droplevels(data[data$randtrt!="",])

data$PTNAME[data$PTNAME==""]=NA
data$SOCNAME[data$SOCNAME==""]=NA

data=droplevels(data)

test=report.quali.hlev(data=data,
		subjid="SUBJID",
		var_upper="PTNAME",
		var_lower="SOCNAME",
		lower.levels="SOC",
		upper.levels="PT",
		x1="randtrt")


test












#' 'Hierarchical Qualitative' statistics reporting (experimental)
#'
#' @param data A data frame
#' @param subjid A character
#' @param x1 A character. Indicates a factor in the data frame.
#' @param var_upper A character. Indicates a factor in the data frame which corresponds to the factor with the higher number of levels (typically SOC variable)
#' @param var_lower A character. Indicates a factor in the data frame which corresponds to the factor with the lower number of levels (typically PT variable)
#' @param lower.levels A character. The label to be displayed in the table for the lower terms
#' @param upper.levels  A character. The label to be displayed in the table for the upper terms
#' @param x1.label  A character. Not used for now
#' 
#' @description
#' This function is mainly used to compute qualitative statistics when there are several events per
#' statistical unit. Often used for reporting adverse events, medical history or concomitant treatments.
#' 
#' It reports frequencies and percentages according to hierarchical levels of two factors.
#' 
#' Typically, adverse event are classified according to System Organ Class (SOC)
#' and then sub classified by Prefered Terms (PT). Several observations of a same adverse event can 
#' be observed several times on the same subject. 
#' It's then useful to know how many persons are concerned by at least one of those adverse events
#' and report the frequencies for each classifications: SOC and PT.
#' 
#' This is exactly what this function does.
#' 
#' For more examples see the website: \href{https://jfrancoiscollin.github.io/ClinReport}{ClinReport website}
#' 
#' @details
#' 
#' The subjid argument is mandatory for this function.



#' @return  
#' A desc object that can be used by the \code{report.doc} function.
#' 
#' @seealso \code{\link{report.quali}} \code{\link{emmeans}} \code{\link{report.doc}} \code{\link{desc}}

#' @examples
#' 
#' data(adverse_event)
#' 
#'test=report.quali.hlev(data=adverse_event,subjid="SUBJID",var_upper="PTNAME",
#'var_lower="SOCNAME",lower.levels="System Organ Class",upper.levels="Prefered Terms",x1="randtrt")
#'
#' # show results in console
#' test
#' 
#' # show formatted results in HTML
#' ft=report.doc(test,valign=TRUE)
#' ft
#' 
#' @export 

report.quali.hlev=function(data,subjid=NULL,x1=NULL,var_upper,var_lower,
		lower.levels="Lower.Levels",upper.levels=
				"Upper.Levels",x1.label=NULL)
{
	
	if(is.null(subjid)) stop("This function needs a subjid argument")
	
	if(!is.null(x1))
	{
		if(is.null(x1.label)) x1.label=x1
	}
	
	
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
		
		colnames(dat)[-1]=colnames(alo_tot)[-1]
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
		colnames(dat2)[-1]=colnames(alo_tot)[-1]
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
		
		dat=ClinReport::desc(output=dat,total=FALSE,nbcol=6,y=NULL,x1=x1,x2=NULL,
				at.row=x1,
				subjid=subjid,type.desc="quali",type=NULL,y.label="",
				raw.output=dat,title="Hierarchichal",y.levels.label="Levels")
		
		
		return(dat)
	}
	
	
	if(!is.null(x1))
	{
		
		dat=report.quali.hlev(data=data,
				subjid=subjid,x1=NULL,var_upper=var_upper,var_lower=var_lower,
				lower.levels=lower.levels,upper.levels=upper.levels)$output
		
		dat[,x1]=paste(levels(droplevels(data[,x1])),collapse="+")
		
		for(i in 1:length(levels(data[,x1])))
		{
			
			temp=report.quali.hlev(data=droplevels(data[data[,x1]==levels(data[,x1])[i],]),
					subjid=subjid,x1=NULL,var_upper=var_upper,var_lower=var_lower,
					lower.levels=lower.levels,upper.levels=upper.levels)$output
			
			temp[,x1]=levels(data[,x1])[i]
			
			dat=rbind(dat,temp)
		}
		
		dat=spacetable(dat,x1)
		
		colnames(dat)[colnames(dat)==x1]=x1.label
		
		dat=ClinReport::desc(output=dat,total=FALSE,nbcol=6,y=NULL,x1=x1,x2=NULL,
				at.row=x1,
				subjid=subjid,type.desc="quali",type=NULL,y.label="",
				raw.output=dat,title="Hierarchichal"	,y.levels.label="Levels")
		
		
		return(dat)
		
	}
	

	
}






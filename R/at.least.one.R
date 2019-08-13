# TODO: Add comment
# 
# Author: jfcollin
###############################################################################


#' Create a key from two hierarchical factors
#'
#' @param data A data frame
#' @param subjid A character. Indicates the ID
#' @param var A character. Indicates a factor in the data frame
#' @param var.label A character. The label of var
#' @param total  Logical. if TRUE the total number of events is calculated
#' 
#' @description
#' Used to calculate the 'at least one' statistics for several occurence of an event per statistical unit.
#' 
#' @details 
#' Used internally by report.quali.hlev


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













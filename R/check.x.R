# TODO: Add comment
# 
# Author: jfcollin
###############################################################################


#' Check if the variable argument supplied in report.quanti and report.quali is ok
#' 
#'
#' @param data Data.frame object
#' @param x Character  
#' @param substitute Character. The name of the data frame
#' 
#' @details
#' Used internally by report.quanti and report.quali

check.x=function(data,x,substitute="data")
{
	
	if(class(x)!="character") stop("x argument should be a character")
	if(!any(colnames(data)==x)) stop("x argument should be in data colnames")
	if(!is.factor(data[,x]))
	{		
#		message(paste0(as.character(substitute),"[,'",x,"']","has been used as a factor"))
		data[,x]=as.factor(as.data.frame(data)[,x]) # just for the condition after
	}
	
	if(any(levels(data[,x])=="",na.rm=T))
	{

			stop(paste0("One on the levels/values of ",
							as.character(substitute),"[,'",x,"']"),
					" is equal to ''. check that the corresponding values are not NA.")
	}
	
	x
}

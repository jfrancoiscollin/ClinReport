# TODO: Add comment
# 
# Author: jfcollin
###############################################################################


#' Check if the variable argument supplied in report.quanti and report.quali is ok
#' 
#'
#' @param data Data.frame object
#' @param x Character  
#' 
#' @details
#' Used internally by report.quanti and report.quali

check.x=function(data,x)
{
	
	if(class(x)!="character") stop("x argument should be a character")
	if(!any(colnames(data)==x)) stop("x argument should be in data colnames")
	if(!is.factor(data[,x]))
	{		
		message(paste0(as.character(substitute(data)),"[,'",x,"']","has been transformed into a factor"))
		data[,x]=as.factor(data[x])
	}
	
	x
}

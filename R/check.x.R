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

check.x=function(data,x,substitute="data")
{
	
	if(class(x)!="character") stop("x argument should be a character")
	if(!any(colnames(data)==x)) stop("x argument should be in data colnames")
	if(!is.factor(data[,x]))
	{		
		message(paste0(as.character(substitute),"[,'",x,"']","has been used as a factor"))
	}
	
	if(any(levels(data[,x])==""))
	{

			stop(paste0("One on the levels of ",
							as.character(substitute),"[,'",x,"']"),
					" is equal to ''. check that the corresponding values are not NA.")
	}
	
	x
}

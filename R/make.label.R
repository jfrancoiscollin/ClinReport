# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

#' Make pretty labels from variable names
#'
#' @param x Character
#' @param l List Each first element is gsubised by the second element of the list
#' 
#' @description
#' \code{make.label} 
#' 
#' Mainly used to prettyfy the rownames of anova tables.
#' 
#' @details
#' 
#' Replace with \code{gsub} function each first element of \code{l[[i]]} by the second element of \code{l[[i]]} 
#' for each element of x. 
#'  
#' 
#' @return  
#' A character
#' 
#' @examples
#' data(data)
#' an=anova(lm(y_numeric~y_logistic+GROUP:TIMEPOINT,data=data))
#' 
#' # Raw output:
#' an
#' 
#' rownames(an)=make.label(rownames(an),
#' l=list(
#' l1=c("y_logistic","A logistic variable"),
#' l2=c(":"," interaction with "))
#' )
#' 
#' # Gives:
#' 
#' an
#'
#' @rdname make.label
#' @export 


make.label=function(x,l=list(l1=c("_"," at "),l2=c(":"," interaction with "),l3=c("."," ")))
{
	
	.simpleCap <- function(x) {
		s <- strsplit(x, " ")[[1]]
		paste(toupper(substring(s, 1, 1)), substring(s, 2),
				sep = "", collapse = " ")
	}
	
	for(i in 1:length(x))
	{
		for(j in 1:length(l))
		{
			x[i]=gsub(l[[j]][1],l[[j]][2],x[i],fixed=T)
			
		}

		x[i]=.simpleCap(x[i])
	}

	x
	
}



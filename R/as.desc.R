# TODO: Add comment
# 
# Author: jfcollin
###############################################################################


#' Converts a data frame or a tibble into a desc object
#' 
#' @param output A data frame or a tibble
#' @param ... Not used
#' 
#' 
#' @seealso \code{\link{desc}} 
#' 
#' @export

as.desc <- function(output,nbcol=ncol(x),total=FALSE,
		y.label="",type.desc="quanti",
		...)
{
	
	if(inherits(output,"tibble")| inherits(output,"data.frame"))
	{
		ClinReport:::desc(output =output,
				nbcol=nbcol,total=total,y.label=y.label,
				type.desc=type.desc)
		
	}else
	{
		message("as.desc is only implemented for data.frame or tibble object")
	}
	
}

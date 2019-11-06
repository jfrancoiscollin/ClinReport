# TODO: Add comment
# 
# Author: jfcollin
###############################################################################


#' Converts a data frame or a tibble into a desc object
#' 
#' @param output A data frame or a tibble
#' @param nbcol Numeric. Number of columns that are not statistics
#' @param total Logical. If yes, indicates a column Total in the table
#' @param y.label Character, the label of the response
#' @param type.desc Character, can be "quanti", "quali" "lsmeans" 
#' @param ... Not used
#' 
#' 
#' @seealso \code{\link{desc}} 
#' 
#' @export

as.desc <- function(output,nbcol=ncol(output),total=FALSE,
		y.label="",type.desc="quanti",
		...)
{
	
	if(inherits(output,"tibble")| inherits(output,"data.frame"))
	{
		desc(output =output,
				nbcol=nbcol,total=total,y.label=y.label,
				type.desc=type.desc)
		
	}else
	{
		message("as.desc is only implemented for data.frame or tibble object")
	}
	
}

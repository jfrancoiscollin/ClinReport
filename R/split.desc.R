# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

#' Split a table of statistics in two.
#' 
#' @param x A desc object
#' @param variable Character. Indicates the variable to use for the split
#' @param at Integer. Indicates the maximum number of levels to include in the first splitted table 
#' @param f see split documentation
#' @param drop see split documentation
#' @param ... Other parameters
#' 
#' @details 
#' Used for splitting an output in two. It can be used for example when the table
#' takes too much space in a page of a Word document (see the example below).
#' 
#' @seealso \code{\link{desc}} 
#' 
#' @examples
#' 
#'data(datafake)
#' 
#'tab1=report.quanti(data=datafake,y="y_numeric",
#'		x1="GROUP",x2="TIMEPOINT",at.row="TIMEPOINT",subjid="SUBJID")
#' 
#'
#'s=split(tab1,variable="TIMEPOINT",at=3)
#'
#'tab1.1=s$x1
#'tab1.2=s$x2
#' 
#'tab1.1
#'tab1.2 
#' 
#' @method split desc 
#' @export


split.desc=function(x, f, drop, ...,variable,at)
{
	
	x1=x
	x2=x
	
	lev=levels(x1$raw.output[,variable])
	
	lev.x1=lev[1:at]
	lev.x2=lev[!"%in%"(lev,lev.x1)]
	
		
	x1$output=droplevels(x1$output["%in%"(x1$output[,variable],lev.x1),])
	x1$raw.output=droplevels(x1$raw.output["%in%"(x1$raw.output[,variable],lev.x1),])
	
	x2$output=droplevels(x2$output["%in%"(x2$output[,variable],lev.x2),])
	x2$raw.output=droplevels(x2$raw.output["%in%"(x2$raw.output[,variable],lev.x2),])
	
	if(!is.null(x$at.row))
	{
		x1$output=spacetable(x1$output,at.row=x$at.row)
		x2$output=spacetable(x2$output,at.row=x$at.row)
	}
	
	return(list(x1=x1,x2=x2))
}

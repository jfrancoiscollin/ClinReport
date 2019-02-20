# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

#' regroup method for desc object
#' 
#' @param x A desc object
#' @param y A desc object
#' @param rbind.label Character. The label for rbind column header 
#' @param ... Other parameters
#' 
#' 
#' @description
#' \code{regroup} 
#' regroup 2 desc objects of different type.desc (quanti and quali) in a single desc object 
#' of type quali_quanti.
#' 
#' @details
#' 
#' It's only possible to regroup statistics for desc objects with one and only one explicative variable.
#' So it works if and only if \code{x1} argument in \code{x} and \code{y} objects are not NULL, are the same 
#' and if \code{x2} argument is NULL in both \code{x} and \code{y} objects.
#' 
#' The function takes the y.label argument of object \code{x} and \code{y} respectively
#' as label for the levels of the new column created under the name of rbind.label (see example below)
#' 
#' @return  
#' A desc object of type.desc="quali_quanti"
#' 
#' @seealso \code{\link{report.quali}} \code{\link{report.quanti}} \code{\link{report.doc}} \code{\link{desc}}
#' 
#' @examples
#' 
#' data(data)
#' 
#' #The argument y.label is stored in the desc object and 
#' # only used after by the regroup function
#' 
#'tab1=report.quanti(data=data,y="y_numeric",
#'		x1="GROUP",subjid="SUBJID",y.label="Y numeric")
#'
#'tab2=report.quali(data=data,y="y_logistic",
#'		x1="GROUP",subjid="SUBJID",y.label="Y logistic")
#'
#'regroup(tab1,tab2,rbind.label="The label of your choice")
#' 
#' 
#' 
#' @rdname regroup
#' 
#' @export


regroup <- function(x,y,...)
{
	UseMethod("regroup")
}

#' @rdname regroup
#' 
#' @export 

regroup.desc=function(x,y,rbind.label="Response",...)
{
	
	if(!is.null(x$x2) | !is.null(y$x2)) stop("Binding impossible with x2 argument not NULL")
	if(x$type.desc=="lsmeans") stop("Binding impossible for now for ls means table")
	if(y$type.desc=="lsmeans") stop("Binding impossible for now for ls means table")
	 
	
	out.x=x$output
	out.y=y$output
	
	if(x$type.desc!=y$type.desc)
	{
		#check
		if(x$total!=y$total) stop("Different Total argument: binding impossible")
		if(is.null(y$x1)) stop("x1 argument cannot be NULL: binding impossible")
		if(is.null(x$x1)) stop("x1 argument cannot be NULL: binding impossible")
		if(x$x1!=y$x1) stop("Different x1 argument: binding impossible")
		if(x$subjid!=y$subjid) stop("Different subjid argument: binding impossible")
		
		if(x$type.desc=="quanti") out.x$Levels=""
		if(y$type.desc=="quanti") out.y$Levels=""
		
		out.x$rbind=x$y.label
		
		out.y$rbind=y$y.label
		
		r=rbind(out.x,out.y)
		
		if(x$type.desc=="quali") r=r[,colnames(out.x)]
		if(y$type.desc=="quali") r=r[,colnames(out.y)]
		
		r=spacetable(r,"rbind")
		colnames(r)[colnames(r)=="rbind"]=rbind.label
		
		nbcol=max(x$nbcol,y$nbcol)
		
		r=r[,c(ncol(r),1:(ncol(r)-1))]
		
		r=ClinReport::desc(output=r,total=x$total,x1=x$x1,
				type.desc="quali_quanti",
				at.row=rbind.label,subjid=x$subjid,
				nbcol=nbcol)
		
		r
		
	}
	
}

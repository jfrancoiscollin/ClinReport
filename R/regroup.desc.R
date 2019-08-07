# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

#' Regroup two descriptive tables into one
#' 
#' @param x A desc object
#' @param y A desc object
#' @param rbind.label Character. The label for rbind column header 
#' @param ... Other parameters
#' 
#' 
#' @description
#' \code{regroup} 
#' regroup two descriptive tables (qualitative or quantitative) into one
#' 
#' @details
#' 
#' Regroup a quantitative table and a qualitative table, is only possible if there is one and only one explicative variable.
#' So it works if and only if \code{x1} argument in \code{x} and \code{y} objects are not NULL, are the same 
#' and if \code{x2} argument is NULL in both \code{x} and \code{y} objects.
#' 
#' The function takes the y.label argument of object \code{x} and \code{y} respectively
#' as label for the levels of the new column created under the name of rbind.label (see example below)
#' 
#' It's also possible to regroup two quantitative tables, in this case it's possible if there is one or 
#' two explicative variables. 
#' 
#' For now it's not possible to regroup two qualitative tables.
#' 
#' @return  
#' A desc object corresponding to a table of statistics. 
#' 
#' @seealso \code{\link{report.quali}} \code{\link{report.quanti}} \code{\link{report.doc}} \code{\link{desc}}
#' 
#' @examples
#' 
#' data(datafake)
#' 
#' # Example with a qualitative and a quantitative tables
#' #The argument y.label is stored in the desc object and 
#' # only used after by the regroup function
#' 
#'tab1=report.quanti(data=datafake,y="y_numeric",
#'		x1="GROUP",subjid="SUBJID",y.label="Y numeric")
#'
#'tab2=report.quali(data=datafake,y="y_logistic",
#'		x1="GROUP",subjid="SUBJID",y.label="Y logistic")
#'
#'regroup(tab1,tab2,rbind.label="The label of your choice")
#' 
#' 
#' # Example with 2 quantitative tables
#' 
#'tab1=report.quanti(data=datafake,y="y_numeric",
#'		x1="GROUP",subjid="SUBJID",y.label="Y numeric")
#'
#' datafake$y_numeric2=rnorm(length(datafake$y_numeric))
#' 
#'tab2=report.quanti(data=datafake,y="y_numeric2",
#'		x1="GROUP",subjid="SUBJID",y.label="Y Numeric 2")
#'
#'regroup(tab1,tab2,rbind.label="The label of your choice")
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
	
	
	if(x$type.desc=="lsmeans") stop("Binding impossible for now for ls means table")
	if(y$type.desc=="lsmeans") stop("Binding impossible for now for ls means table")
	
	
	if(x$regrouped==T & y$regrouped==F)
	{
		if(x$type.desc=="quali_quanti")
		{
			y$output$rbind.label=y$y.label
			colnames(y$output)[colnames(y$output)=="rbind.label"]=x$rbind.label
			rbind.label=x$rbind.label
		}
		
	}
	
	if(x$regrouped==F & y$regrouped==T)
	{
		if(y$type.desc=="quali_quanti")
		{
			x$output$rbind.label=x$y.label
			colnames(x$output)[colnames(x$output)=="rbind.label"]=y$rbind.label
			rbind.label=y$rbind.label
		}
	}
	
	out.x=droplevels(x$output)
	out.y=droplevels(y$output)
	
	# Mixed binding (quali-quanti or quanti-quali)
	
	if(x$type.desc!=y$type.desc)
	{
		
		
		
		if(!is.null(x$x2) | !is.null(y$x2)) stop("Binding impossible with x2 argument not NULL")
		
		#check
		if(x$total!=y$total) stop("Different Total argument: binding impossible")
		if(is.null(y$x1)) stop("x1 argument cannot be NULL: binding impossible")
		if(is.null(x$x1)) stop("x1 argument cannot be NULL: binding impossible")
		if(x$x1!=y$x1) stop("Different x1 argument: binding impossible")
		
		if(!is.null(x$subjid) & !is.null(y$subjid))
		{
			if(x$subjid!=y$subjid) stop("Different subjid argument: binding impossible")
		}
		
		if(is.null(x$subjid) & !is.null(y$subjid)) stop("Different subjid argument: binding impossible")
		if(!is.null(x$subjid) & is.null(y$subjid)) stop("Different subjid argument: binding impossible")
		
		if(x$type.desc=="quanti") out.x$Levels=""
		if(y$type.desc=="quanti") out.y$Levels=""
		
		if(x$regrouped==F & y$regrouped==F)
		{
			out.x$rbind=x$y.label
			out.y$rbind=y$y.label
		}
		
		r=rbind(out.x,out.y)
		
		if(x$type.desc=="quali") r=r[,colnames(out.x)]
		if(y$type.desc=="quali") r=r[,colnames(out.y)]
		
		if(x$regrouped==F & y$regrouped==F)
		{
			r=spacetable(r,"rbind")
			colnames(r)[colnames(r)=="rbind"]=rbind.label
		}else
		{
			r=droplevels(r[r[,rbind.label]!="",])	
			r=spacetable(r,rbind.label)
		}
		
		if(x$regrouped==F & y$regrouped==F)
		{
			nbcol=max(x$nbcol,y$nbcol)+1
		}else
		{
			nbcol=max(x$nbcol,y$nbcol)
		}
		
		if(x$regrouped==F & y$regrouped==F)
		{
			r=r[,c(ncol(r),1:(ncol(r)-1))]
		}
		
		r=ClinReport::desc(output=r,
				y=c(x$y,y$y),
				total=x$total,x1=x$x1,
				type.desc="quali_quanti",
				at.row=rbind.label,
				subjid=x$subjid,
				nbcol=nbcol,
				regrouped=T,
				rbind.label=rbind.label)
		
		return(r)
		
	}
	
	
	
	if(x$type.desc=="quanti" & y$type.desc=="quanti")
	{
		
		
		if(x$total!=y$total) stop("Different Total argument: binding impossible")
		if(is.null(y$x1)) stop("x1 argument cannot be NULL: binding impossible")
		if(is.null(x$x1)) stop("x1 argument cannot be NULL: binding impossible")
		if(x$x1!=y$x1) stop("Different x1 argument: binding impossible")
		
		if(!is.null(x$subjid) & !is.null(y$subjid))
		{
			if(x$subjid!=y$subjid) stop("Different subjid argument: binding impossible")
		}
		
		if(is.null(x$subjid) & !is.null(y$subjid)) stop("Different subjid argument: binding impossible")
		if(!is.null(x$subjid) & is.null(y$subjid)) stop("Different subjid argument: binding impossible")
		
		
		nbcol=x$nbcol
		
		r=rbind(out.x,out.y)
		
		if(x$y!=y$y)
		{
			r$rbind="lab"
			r$rbind[1:nrow(out.x)]=x$y.label
			r$rbind[(nrow(out.x)+1):nrow(r)]=y$y.label
			colnames(r)[colnames(r)=="rbind"]=rbind.label
			
			r=spacetable(r,rbind.label)
			
			r=r[,c(ncol(r),1:(ncol(r)-1))]
		}
		
		if(x$y==y$y)
		{
			if(is.null(x$x2)) r=r[order(r[,x$stat.name]),]
			
			if(!is.null(x$x2))
			{
				r=r[order(r[,x$x2],r[,x$stat.name]),]
				if(!is.null(x$at.row))
				{
					r=droplevels(r[r[,x$at.row]!="",])				
					lev=levels(r[,x$stat.name])
					r=spacetable(r,x$at.row)
					r[,x$stat.name]=factor(r[,x$stat.name],levels=c(lev,""))
				}
				
				
			}
		}
		
		
		r=ClinReport::desc(output=r,
				y=x$y,
				total=x$total,x1=x$x1,x2=x$x2,
				type.desc=x$type.desc,subjid=x$subjid,
				nbcol=nbcol,
				stat.name=x$stat.name,
				at.row=x$at.row,
				regrouped=T,
				rbind.label=rbind.label)
		
		return(r)
		
	}
	
	
	if(x$type.desc=="quali" & y$type.desc=="quali")
	{
		message("The regroup function doesn't work yet with two 'quali' objects")
	}
	
	
	
}

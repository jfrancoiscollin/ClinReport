# TODO: Add comment
# 
# Author: jfcollin
###############################################################################


#' add.stat method for desc object
#' 
#' @param tab A desc object
#' @param data The data frame used to creat tab
#' @param func.stat Passed to report.quanti function
#' @param func.stat.name Passed to report.quanti function
#' @param ... Other parameters
#' 
#' 
#' @description
#' \code{add.stat.desc} 
#' Enables the user to add a specific statistic in a desc object.
#' 
#' @details
#' Enables the user to add a specific statistic in a desc object (only works for quantitative statistics for now).
#' It calls the report.quanti function with the same attributes as the tab argument and use the reproup function.
#' to bind the two results.
#' 
#' 
#' @return  
#' A "quanti" desc object (desc$type.desc="quanti")
#' 
#' @seealso \code{\link{regroup}} \code{\link{report.quanti}} 
#' 
#' @examples
#' 
#'data(data)
#'
#'cv=function(y) sd(y,na.rm=T)/mean(y,na.rm=T)
#'
#'tab1=report.quanti(data=data,y="y_numeric",x1="GROUP",total=TRUE,subjid="SUBJID")
#'
#'add.stat.desc(tab1,data,func.stat=cv,func.stat.name="Coef. Var")
#'
#'tab=report.quanti(data=data,y="y_numeric",x1="GROUP",x2="TIMEPOINT",total=TRUE,subjid="SUBJID",
#'		at.row="TIMEPOINT")
#'add.stat.desc(tab,data,func.stat=cv,func.stat.name="Coef. Var")
#' 
#' 
#' @rdname add.stat
#' 
#' @export


add.stat <- function(tab,data,func.stat,func.stat.name,...)
{
	UseMethod("add.stat")
}

#' 
#' 
#' @rdname add.stat
#' 
#' @export



add.stat.desc=function(tab,data,func.stat,func.stat.name,...)
{
	
	tab2=report.quanti(data=data,y=tab$y,x1=tab$x1,x2=tab$x2,total=tab$total,subjid=tab$subjid,
			default.stat=F,func.stat=func.stat,func.stat.name=func.stat.name)
	
	
	tab3=regroup(x=tab,y=tab2)
	
	tab3	
	
}




# TODO: create default stat option for report.quanti like option(default.stat=list(mean, max etc..))
# TODO: Add link to the website in the documentation
# Author: jfcollin
###############################################################################

#' @title  Add a new statistic to an existing table
#' 
#' @param tab A desc object
#' @param data The data frame used to creat tab
#' @param func.stat Passed to \code{report.quanti} function
#' @param func.stat.name Passed to \code{report.quanti} function
#' @param pos Numeric used to sepecify the position of the new statistics
#' @param ... Other parameters
#' 
#' 
#' @description
#' \code{add.stat.desc} 
#' It enables to add a specific quantitative statistic into an existing table.
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
#' # Load the data
#' 
#'data(data)
#'
#'# The default statistics are given here:
#' 
#'tab1=report.quanti(data=data,y="y_numeric",x1="GROUP",total=TRUE,subjid="SUBJID")
#'
#' # Define the function corresponding to the coefficient of variation for example
#' 
#'cv=function(y) sd(y,na.rm=TRUE)/mean(y,na.rm=TRUE)
#' 
#' # We use the add.stat function to add CV at the second row:
#' 
#'tab1.cv=add.stat(tab1,data,func.stat=cv,func.stat.name="Coef. Var",
#' pos=2)
#'
#'tab1.cv
#' 
#' # Same with 2 explicative variables
#' 
#'tab=report.quanti(data=data,y="y_numeric",x1="GROUP",
#' x2="TIMEPOINT",total=TRUE,subjid="SUBJID",
#'		at.row="TIMEPOINT")
#' 
#' tab=add.stat(tab,data,func.stat=cv,func.stat.name="Coef. Var",
#' pos=2)
#' tab
#' 
#' # And on position 5, we can add for example the mode
#' 
#' mode=function(x)
#' {
#'   x=na.omit(x)
#'   ux <- unique(x)
#'   ux[which.max(tabulate(match(x, ux)))]
#' }
#' 
#' 
#' tab=add.stat(tab,data,func.stat=mode,func.stat.name="Mode",
#' pos=5)
#' tab
#' 
#' 
#' 
#' @rdname add.stat
#' 
#' @export


add.stat <- function(tab,data,func.stat,func.stat.name,pos,...)
{
	UseMethod("add.stat")
}


#' @rdname add.stat
#' 
#' @export



add.stat.desc=function(tab,data,func.stat,func.stat.name,pos=NULL,...)
{
	if(tab$type.desc!="quanti") stop("This function only works for quantitative desc object")
	
	if(is.null(pos)) pos=1
	
	tab2=report.quanti(data=data,y=tab$y,x1=tab$x1,x2=tab$x2,total=tab$total,subjid=tab$subjid,
			default.stat=F,func.stat=func.stat,func.stat.name=func.stat.name)
	
	
	tab3=regroup(x=tab,y=tab2)
	
	lev2=levels(tab2$output[,tab2$stat.name])
	lev3=levels(tab3$output[,tab3$stat.name])
	lev3=lev3[lev3!=lev2]
	pos.lev=lev3[pos]
	
	
	if(!is.numeric(pos)) stop("pos should be a numeric")
	
	if(pos!=1 & pos!=length(lev3))
	{
		relevel=c(lev3[1:(pos-1)],lev2,pos.lev,lev3[(pos+1):length(lev3)])
	}
	
	if(pos<1) pos=1
	
	if(pos==1)
	{
		relevel=c(lev2,pos.lev,lev3[(pos+1):length(lev3)])
	}
	
	
	if(pos>length(lev3)) pos=length(lev3)
		
	if(pos==length(lev3))
	{
		relevel=c(lev3[1:(pos-1)],pos.lev,lev2)
	}
	
	
	
	tab3$output[,tab3$stat.name]=factor(tab3$output[,tab3$stat.name],
			levels=relevel)	
	
	tab3$output=droplevels(tab3$output[tab3$output[,tab3$stat.name]!="",])
	
	if(!is.null(tab$x2)) tab3$output=tab3$output[order(tab3$output[,tab$x2],tab3$output[,tab3$stat.name]),]
	if(is.null(tab$x2)) tab3$output=tab3$output[order(tab3$output[,tab3$stat.name]),]
	
	if(!is.null(tab$at.row))
	{
		lev=levels(tab3$output[,tab3$stat.name])
		tab3$output=spacetable(tab3$output,tab$at.row)
		tab3$output[,tab3$stat.name]=factor(tab3$output[,tab3$stat.name],
				levels=c(lev,""))	
	}
	
	tab3
}




# TODO: Add comment
# 
# Author: jfcollin
###############################################################################


#' Display the corresponding graphic of statistical table
#' 
#' @param x A desc object
#' @param title Character The title of the plot
#' @param ylim Numeric of length 2 for setting y axis limits
#' @param xlim Numeric of length 2 for setting x axis limits
#' @param xlab Character Label for x-axis
#' @param ylab Character Label for y-axis
#' @param legend.label Character Label for the legend (used only if x1 and x2 are not NULL in the desc object)
#' @param add.sd Logical. If TRUE it adds bars to the means representing +/-SD (for desc quanti reporting only)
#' @param add.ci Logical. If TRUE it adds bars to the means representing 95\% CI (for LS Means reporting only)
#' @param add.line Logical. If TRUE it joins the dots with a line (default to TRUE)
#' @param ... Other parameters
#' 
#' 
#' @details 
#' It's a wrapper function whiche uses  \code{\link{gg_desc_quanti}} 
#'  \code{\link{gg_desc_quali}}  or  \code{\link{gg_desc_lsmeans}} depending if
#' the desc object is of type "quali", "quanti" or "lsmeans"
#'  (argument\code{type.desc} in \code{\link{desc}} object, see examples below). 
#' 
#' @seealso \code{\link{desc}} 
#' 
#' @examples
#'  
#' data(data)
#'
#' desc=report.quali(data=data,y="y_logistic",x1="GROUP",
#'		x2="TIMEPOINT")
#' 
#' plot(desc,
#' title="Qualitative desc object with 2 explicative variables",
#' legend.label="Y levels")
#' 
#' 
#' 
#' desc2=report.quanti(data=data,y="y_numeric",x1="GROUP",
#'		x2="TIMEPOINT")
#' 
#' plot(desc2,
#' title="Quantitative desc object with 2 explicative variables",
#' legend.label="Treatment groups")
#' 
#' 
#' #Removing baseline data in the response, for the model
#' 
#'data.mod=droplevels(data[data$TIMEPOINT!="D0",]) 
#' 
#' library(nlme)
#' library(emmeans)
#' 
#'mod3=lme(y_numeric~baseline+GROUP+TIMEPOINT+GROUP*TIMEPOINT,
#'random=~1|SUBJID,data=data.mod,na.action=na.omit)
#' 
#'test3=emmeans(mod3,~GROUP|TIMEPOINT)
#' 
#'tab.mod3=report.lsmeans(lsm=test3,x1="GROUP",
#'		x2="TIMEPOINT",at.row="TIMEPOINT",data=data.mod)
#' 
#'gg=plot(tab.mod3,title="LS Means plot example")
#' 
#'#Print
#' 
#'gg
#' 
#' @method plot desc 
#' 
#' @export

plot.desc=function(x,...,title="",ylim=NULL,xlim,xlab="",ylab="",
		legend.label="Group",add.sd=F,add.ci=F,add.line=T)
{
	
	if(x$type.desc=="quanti")
	{
		gg=gg_desc_quanti(x,title=title,ylim=ylim,xlim=xlim,xlab=xlab,ylab=ylab,
				legend.label=legend.label,add.sd=add.sd)
		
		return(gg)
	}
	
	
	if(x$type.desc=="lsmeans")
	{
		gg=gg_desc_lsmeans(x,title=title,ylim=ylim,xlim=xlim,xlab=xlab,ylab=ylab,
				legend.label=legend.label,add.ci=add.ci,add.line=add.line)
		
		return(gg)
	}
	
	if(x$type.desc=="quali")
	{
		gg=gg_desc_quali(x,title=title,ylim=ylim,xlim=xlim,xlab=xlab,ylab=ylab,
				legend.label=legend.label)
		
		return(gg)
	}
	
}










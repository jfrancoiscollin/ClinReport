# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

#' Creates a ggplot object corresponding to a qualitative desc object
#' 
#' @param desc Desc object
#' @param title Character The title of the plot
#' @param ylim Numeric of length 2 for setting y axis limits
#' @param xlim Numeric of length 2 for setting x axis limits
#' @param xlab Character Label for x-axis
#' @param ylab Character Label for y-axis
#' @param legend.label Character Label for the legend (used only if x1 and x2 are not NULL in the desc object)
#' 
#' @description
#' \code{gg_desc_quali} 
#' ggplot object is printed. It is used internally in function \code{\link{plot.desc}}.
#' It's easier to use this last one.
#' 
#' 
#' @details
#' No detail
#' 
#' 
#' @seealso \code{\link{report.quali}} \code{\link{plot.desc}} \code{\link{gg_desc_quanti}}  \code{\link{gg_desc_quali}}   \code{\link{gg_desc_lsmeans}} 

#' @examples
#'  \dontshow{
#' data(data)
#'
#'desc=report.quali(data=data,y="y_logistic",x1="GROUP",
#'		x2="TIMEPOINT")
#' 
#'gg=ClinReport:::gg_desc_quali(desc,
#' title="Qualitative desc object with 2 explicative variables",
#' legend.label="Y levels")
#'
#'
#'desc2=report.quali(data=data,y="y_logistic",x1="GROUP")
#'ClinReport:::gg_desc_quali(desc2,title="Qualitative desc object with 1 explicative variable")
#' 
#' desc3=report.quali(data=data,y="y_logistic")
#' ClinReport:::gg_desc_quali(desc3,title="Qualitative desc object with 1 explicative variable")
#' 
#' 
#' 
#' 
#' 
#' 
#' }
#' 
#' @import ggplot2


gg_desc_quali=function(desc,title="",ylim=NULL,xlim=NULL,xlab="",
		ylab="Percentage",
		legend.label="Group")
{
	
	if(class(desc)!="desc") stop("\n desc should be a desc object")
	if(desc$type.desc!="quali") stop("This function should be used only for qualitative desc object")
	
	x1=desc$x1
	x2=desc$x2
	stat=desc$raw.output
	stat=na.omit(stat)
	stat$percent=100*(stat$Freq.x/stat$Freq.y)
	
	if(is.null(ylim)) ylim=c(0,100)
	
	
	Var1="Var1"
	Var2="Var2"
	percent="percent"
	
	th=theme_bw()+theme(plot.background = element_rect(
					colour = "black",
					size = 1,
					linetype = "solid"),legend.position="bottom",
			title=element_text(size = 10),
			axis.text.x=element_text(angle =45,hjust=1))
	
	
	if(!is.null(x1) & !is.null(x2))
	{
		gg=ggplot(stat, aes_(as.name(Var2),as.name(percent)
		,fill=as.name(Var1))) +
				geom_col()+facet_wrap(~Var3)+theme_bw()+
				scale_fill_discrete(name=legend.label)+th+
				ylim(ylim)+
				labs(title=title,x=xlab,y=ylab)
		
		return(gg)
	}
	
	
	if(!is.null(x1) & is.null(x2))
	{
		
		gg=ggplot(stat, aes_(as.name(Var2),as.name(percent),
								fill=as.name(Var1))) +
				geom_col()+
				scale_fill_discrete(name=legend.label)+th+
				ylim(ylim)+
				labs(title=title,x=xlab,y=ylab)
		
		return(gg)
		
	}
	
	if(is.null(x1) & is.null(x2))
	{
		
		gg=ggplot(stat, aes_(as.name(Var1),as.name(percent))) +
				geom_col()+th+
				ylim(ylim)+	labs(title=title,x=xlab,y=ylab)
		
		return(gg)
		
	}
}






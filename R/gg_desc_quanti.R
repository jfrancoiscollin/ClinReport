# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

#' Creates a ggplot object corresponding to a quantitative desc object
#' 
#'
#' @param desc Desc object
#' @param title Character The title of the plot
#' @param ylim Numeric of length 2 for setting y axis limits
#' @param xlim Numeric of length 2 for setting x axis limits
#' @param xlab Character Label for x-axis
#' @param ylab Character Label for y-axis
#' @param legend.label Character Label for the legend (used only if x1 and x2 are not NULL in the desc object)
#' @param add.sd Logical. If TRUE it adds bars to the means representing +/-SD
#' @description
#' \code{gg_desc_quanti} 
#' ggplot object is printed
#' 
#' 
#' @details
#' It is used internally in function \code{\link{plot.desc}}.
#' It's easier to use this last one.
#' 
#' @return  
#' A ggplot object.
#' 
#' @seealso \code{\link{plot.desc}} \code{\link{desc}} \code{\link{gg_desc_quali}} \code{\link{gg_desc_lsmeans}} 

#' @examples
#'  \dontshow{
#' data(data)
#'
#'desc=report.quanti(data=data,y="y_numeric",x1="GROUP",
#'		x2="TIMEPOINT")
#' 
#' ClinReport:::gg_desc_quanti(desc,title="My title",ylab="Hello dear",
#'		ylim=c(-2,8),xlab="Great!",legend.label="Treatment group")
#' 
#' # With SD
#' 
#' ClinReport:::gg_desc_quanti(desc,title="My title",ylab="Hello dear",
#'		ylim=c(-2,8),xlab="Great!",legend.label="Treatment group",add.sd=TRUE)
#' 
#' 
#'desc2=report.quanti(data=data,y="y_numeric",x1="GROUP")
#' 
#'desc3=report.quanti(data=data,y="y_numeric")
#'

#'
#'
#'ClinReport:::gg_desc_quanti(desc2,title="My title",ylab="Hello dear")
#'
#'
#'ClinReport:::gg_desc_quanti(desc3,title="My title",ylab="Hello dear")
#' }
#' 
#' @import ggplot2


gg_desc_quanti=function(desc,title="",ylim=NULL,xlim,xlab="",ylab="",
		legend.label="Group",add.sd=F)
{
	
	if(class(desc)!="desc") stop("\n desc should be a desc object")
	if(desc$type.desc!="quanti") stop("This function should be used only for quantitative desc object")
	
	x1=desc$x1
	x2=desc$x2
	stat=desc$raw.output
	
	stat$lower=stat$mean-stat$sd
	stat$upper=stat$mean+stat$sd
	
	mean="mean"
	lower="lower"
	upper="upper"
	
	if(!is.null(x1) & !is.null(x2))
	{
		gg=ggplot(stat, aes_(y=as.name(mean), x=as.name(x2),
								group=as.name(x1),colour=as.name(x1))) +
				geom_point()+
				geom_path()+theme_bw()+
				scale_colour_discrete(name=legend.label)+
				theme(plot.background = element_rect(
								colour = "black",
								size = 1,
								linetype = "solid"),legend.position="bottom",
						title=element_text(size = 10),
						axis.text.x=element_text(angle =45,hjust=1))+xlab("")+
				ylab(ylab)+xlab(xlab)+
				labs(title=title)
		
	
	}
	
	
	if(!is.null(x1) & is.null(x2))
	{
		gg=ggplot(stat, aes_(y=as.name(mean), x=as.name(x1))) +
				geom_bar(stat="identity")+theme_bw()+
				theme(plot.background = element_rect(
								colour = "black",
								size = 1,
								linetype = "solid"),legend.position="bottom",
						title=element_text(size = 10),
						axis.text.x=element_text(angle =45,hjust=1))+xlab("")+
				ylab(ylab)+
				labs(title=title)
		

	}
	
	if(is.null(x1) & is.null(x2))
	{
		gg=ggplot(stat, aes_(y=as.name(mean),x=1)) +
				geom_bar(stat="identity")+theme_bw()+
				theme(plot.background = element_rect(
								colour = "black",
								size = 1,
								linetype = "solid"),legend.position="bottom",
						title=element_text(size = 10),
						axis.text.x=element_text(colour="white",angle =45,hjust=1),
						axis.ticks.x=element_line(colour="white"))+xlab("")+
				ylab(ylab)+xlim(c(0,2))+
				labs(title=title)
		
	
	}
	
	
	
	if(add.sd)
	{
		

			gg=gg+geom_errorbar(aes_(ymin =as.name(lower), 
							ymax = as.name(upper)),width=0.15)

		
	}
	
	
	
	if(!is.null(ylim)) 
	{
		gg=gg+ylim(ylim)
	}
	
	return(gg)
	
}





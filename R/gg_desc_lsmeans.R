# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

#' Creates a ggplot object corresponding to a LS Means desc object
#' 
#'
#' @param desc Desc object
#' @param title Character The title of the plot
#' @param ylim Numeric of length 2 for setting y axis limits
#' @param xlim Numeric of length 2 for setting x axis limits
#' @param xlab Character Label for x-axis
#' @param ylab Character Label for y-axis
#' @param legend.label Character Label for the legend (used only if x1 and x2 are not NULL in the desc object)
#' @param add.ci Logical. If TRUE it adds bars to the means representing 95\% CI
#' @param add.line Logical. If TRUE it joins the dots with a line (default to TRUE)
#' 
#' @description
#' \code{gg_desc_lsmeans} 
#' ggplot object is created. It is used internally in function \code{\link{plot.desc}}.
#' It's easier to use this last one.
#' 
#' @details
#' It is used internally in function \code{\link{plot.desc}}.
#' It's easier to use this last one.
#' 
#' @return  
#' A ggplot object.
#' 
#' @seealso \code{\link{plot.desc}} \code{\link{report.lsmeans}} \code{\link{gg_desc_quali}} \code{\link{gg_desc_quanti}}

#' @examples
#' \dontshow{
#' 
#' library(nlme)
#' library(emmeans)
#' 
#' data(data)
#' #Removing baseline data in the response, for the model
#' 
#'data.mod=droplevels(data[data$TIMEPOINT!="D0",]) 
#'
#'mod3=lme(y_numeric~baseline+GROUP+TIMEPOINT+GROUP*TIMEPOINT,
#'random=~1|SUBJID,data=data.mod,na.action=na.omit)
#' 
#'test3=emmeans(mod3,~GROUP|TIMEPOINT)
#' 
#'tab.mod3=report.lsmeans(lsm=test3,x1="GROUP",
#'		x2="TIMEPOINT",at.row="TIMEPOINT",data=data.mod)
#' 
#'gg=gg_desc_lsmeans(tab.mod3,title="LS Means plot example")
#' 
#' 
#'test4=emmeans(mod3,~GROUP)
#'tab.mod4=report.lsmeans(lsm=test4,x1="GROUP",data=data.mod)
#' 
#'gg=gg_desc_lsmeans(tab.mod4,title="LS Means plot example")
#' 
#'gg2=gg_desc_lsmeans(tab.mod4,title="LS Means plot example",add.ci=TRUE)
#' 
#' }
#' 
#' @import ggplot2
#' @export


gg_desc_lsmeans=function(desc,title="",ylim=NULL,xlim,xlab="",ylab="",
		legend.label="Group",add.ci=F,add.line=T)
{
	
	if(class(desc)!="desc") stop("\n desc should be a desc object")
	if(desc$type.desc!="lsmeans") stop("This function should be used only for lsmeans desc object, see desc$type.desc")
	
	x1=desc$x1
	x2=desc$x2
	stat=desc$raw.output
	emmean="emmean"
	contrast=desc$contrast
	position = "identity"
	
	if(!is.null(contrast))
	{
		if(contrast)
		{
			
			position = position_dodge(width=0.3)
			
			if(!is.null(x1) & is.null(x2))
			{
				x2=x1
				x1=desc$contrast.name
			}
			
			if(is.null(x1)) x1=desc$contrast.name
		}
	}
	
	
	if(!is.null(x1) & !is.null(x2))
	{
		gg=ggplot(stat, aes_(y=as.name(emmean), x=as.name(x2),
								group=as.name(x1),colour=as.name(x1))) +
				geom_point(position=position)+
				theme_bw()+
				scale_colour_discrete(name=legend.label)+
				theme(plot.background = element_rect(
								colour = "black",
								size = 1,
								linetype = "solid"),legend.position="bottom",
						title=element_text(size = 10),
						axis.text.x=element_text(angle =45,hjust=1))+xlab("")+
				ylab(ylab)+xlab(xlab)+
				labs(title=title)
		
		if(add.line)
		{
			gg=gg+geom_path()
		}
		
	}
	
	
	if(!is.null(x1) & is.null(x2))
	{
		gg=ggplot(stat, aes_(y=as.name(emmean), x=as.name(x1))) +
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
		gg=ggplot(stat, aes_(y=as.name(emmean),x=1)) +
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
	
	
	if(add.ci)
	{
		
		if(!is.null(stat$lower.CL))
		{
			gg=gg+geom_errorbar(aes_(ymin =as.name("lower.CL"), 
							ymax = as.name("upper.CL")),width=0.15,position=position)
			
		}
		
		if(!is.null(stat$asymp.LCL))
		{
			gg=gg+geom_errorbar(aes_(ymin =as.name("asymp.LCL"), 
							ymax = as.name("asymp.UCL")),width=0.15,position=position)
			
			
		}
		
	}
	
	
	
	if(!is.null(ylim)) 
	{
		gg=gg+ylim(ylim)
	}
	
	
	
	return(gg)
	
}






# TODO: continue to test the function with specific  contrast
# 
# Author: jfcollin
###############################################################################


#' 'LS Means' statistics reporting
#'
#' @param lsm emmGrid object (result of a \code{emmeans} call)
#' @param x1 Character Mandatory. Indicating a factor in the data (can be an intercept: see example). Levels will be displayed in columns
#' @param x2 Character indicating a factor in the data. Levels will be displayed in rows
#' @param x3 Character indicating a factor in the data. Levels will be displayed in rows
#' @param x1.name Character. Deprecated (replaced by x1)
#' @param x2.name Character. Deprecated (replaced by x2)
#' @param x3.name Character Deprecated (replaced by x3)
#' @param data Data.frame object from which the Least Square means are coming from
#' @param variable.name Character. The label of the column which indicates the statistics reported.
#' @param infer A vector of one or two logical values. Passed to \code{summary.emmGrid} function.
#' @param at.row Character. Passed to spacetable function. Used to space the results per levels of the mentioned variable
#' @param type Character. Type of prediction desired. Passed to summary.emmGrid function. Can be "link" or "response"
#' @param contrast Logical. Specify if the contrast function has been used after the emmeans function (see examples)
#' @param contrast.name Character. Corresponds to the label of the column in which the contrasts are specified (see example).
#'  Default value is 'contrast'.
#' 
#' @description
#' Creates a desc object for "LS Means" statistics reporting. 
#' 
#' 
#' @details
#' You can produce formatted Least Square Means table for up to 3 factors.
#' It doesn't work for quantitative covariates.
#' 
#'  See examples below.


#' @return  
#' A desc object that can be used by the \code{report.doc} function.
#' 
#' @seealso \code{\link{report.quali}} \code{\link{emmeans}} \code{\link{report.doc}} \code{\link{desc}}

#' @examples
#' 
#' library(emmeans)
#' library(lme4)
#' 
#' data(data)
#' 
#' #Simple lm model
#' 
#' mod=lm(Petal.Width~Species,data=iris)
#' raw.lsm=emmeans(mod,~Species)
#' report.lsmeans(raw.lsm,"Species",data=iris)
#' 
#' # In case of just one intercept you must use a workaround...
#' iris$int=1
#' mod=glm(Species~int,data=iris,family=binomial)
#' raw.lsm=emmeans(mod,~int)
#' iris$int=as.factor(iris$int)
#' report.lsmeans(raw.lsm,"int",data=iris)
#' 
#' #Mixed model example using lme4
#' 
#' mod=lmer(y_numeric~GROUP+TIMEPOINT+GROUP*TIMEPOINT+(1|SUBJID),data=data) 
#' raw.lsm=emmeans(mod,~GROUP|TIMEPOINT)
#' report.lsmeans(lsm=raw.lsm,x1="GROUP",x2="TIMEPOINT",at.row="TIMEPOINT",data=data)
#' 
#' 
#' # GLM model with specific contrast
#' 
#' warp.lm <- lm(breaks ~ wool+tension+wool:tension, data = warpbreaks)
#' warp.emm <- emmeans(warp.lm, ~ tension | wool)
#' contr=contrast(warp.emm, "trt.vs.ctrl", ref = "M")
#' report.lsmeans(lsm=contr,x1="wool",data=warpbreaks,contrast=TRUE,at.row="contrast")
#' 
#' @import reshape2 stats
#' 
#' @export

report.lsmeans=function(lsm,x1="treatment",x2=NULL,x3=NULL,data,
		variable.name="Statistics",at.row=NULL,infer=c(T,T),type="link",contrast=F,
		contrast.name="contrast",x1.name="treatment",x2.name=NULL,x3.name=NULL)
{
	
	if (!missing(x1.name))
	{
		warning("argument x1.name is deprecated; please use x1 instead.", 
				call. = FALSE)
		
		x1=x1.name
	}
	
	if (!missing(x2.name))
	{
		warning("argument x2.name is deprecated; please use x2 instead.", 
				call. = FALSE)
		x2=x2.name
	}
	
	if (!missing(x3.name))
	{
		warning("argument x3.name is deprecated; please use x3 instead.", 
				call. = FALSE)
		
		x3=x3.name
	}
	
	# Control the name of the estimate column
	# so it's the same for all models
	
	if(type=="response")
	{
		lsm=update(lsm,inv.lbl="emmean",type="response")
	}
	
	lsm=update(lsm,estName="emmean")
	
	terms=attr(lsm@model.info$terms,"term.labels")
	
	# Checks
	
	if(class(lsm)!="emmGrid") stop("\n This function takes an emmGrid object only as lsm argument")
	if(!is.character(x1) & !is.null(x1)) stop("\n x1 must be a character")
	if(!is.character(x2) & !is.null(x2)) stop("\n x2 must be a character")
	if(!is.character(x3) & !is.null(x3)) stop("\n x3 must be a character")
	if(is.null(x1)) stop("\n It's not possible to have x1 argument NULL")
	
	
	if(!any(colnames(data)==x1) & !is.null(x1)) stop("\n x1 argument should be in data colnames")
	if(!any(colnames(data)==x2) & !is.null(x2)) stop("\n x2 argument should be in data colnames")
	if(!any(colnames(data)==x3) & !is.null(x3)) stop("\n x3 argument should be in data colnames")
	
	if(class(data[,x1])!="factor" & !is.null(x1)) stop("\n x1 argument should be a factor")
	if(class(data[,x2])!="factor" & !is.null(x2)) stop("\n x2 argument should be a factor")
	if(class(data[,x3])!="factor" & !is.null(x3)) stop("\n x3 argument should be a factor")
	
	
	
	if(!any("%in%"(c(x1,x2,x3),terms)))  stop("\n One of the explicative variable is not in the terms of the emmGrid object (see attr(lsm@model.info$terms,'term.labels'))")
	
	call=as.character(lsm@model.info$call)[1]
	
	
	if(is.null(x2) & is.null(x3)) nbcol=1
	if(!is.null(x2) & is.null(x3)) nbcol=1:2
	if(is.null(x2) & !is.null(x3)) stop("\n It's not possible to have x2 NULL and x3 not NULL")
	if(!is.null(x2) & !is.null(x3)) nbcol=1:3
	
	if(contrast) nbcol=c(nbcol,nbcol+1)
	
	if(any("%in%"(call,c("lm","lmer","lme.formula")))) type.mod="quanti"
	if(any("%in%"(call,c("glm","glmer")))) type.mod="quali"
	if(!any("%in%"(call,c("glm","glmer","lm","lmer","lme.formula")))) stop("\n This function only supports lm, lmer, lme, glm or glmer models")
	
	
	raw.output=data.frame(summary(lsm,infer=infer))
	lsm=raw.output
	
	
	lsm[,-nbcol]=format(round(lsm[,-nbcol],2), nsmall = 2)
	lsm[,-nbcol]=apply(lsm[,-nbcol],2,function(x)gsub(" ","",x))
	
	
	lsm$"Estimate (SE)"=paste(lsm$emmean,"(",lsm$SE,")",sep="")
	
	if(!is.null(lsm$lower.CL))
	{
		lsm$"95% CI"=paste("[",lsm$lower.CL,";",lsm$upper.CL,"]",sep="")	
	}
	
	if(!is.null(lsm$asymp.LCL))
	{
		lsm$"95% CI"=paste("[",lsm$asymp.LCL,";",lsm$asymp.UCL,"]",sep="")	
	}
	
	
	
	
	lsm$"P-value"=prettyp(suppressWarnings(as.numeric(lsm$p.value)))
	
	
	ind=which("%in%"(colnames(lsm),c("Estimate (SE)","P-value","95% CI")))
	
	if(type.mod=="quali")
	{
		ind=which("%in%"(colnames(lsm),c("Estimate (SE)","Probability","P-value","95% CI")))
	}
	
	lsm=lsm[,c(nbcol,ind)]
	
	if(!is.null(x2) & !is.null(x3))	
	{
		m=melt(data=lsm, id.vars=c(x1,x2,x3),
				measure.vars=colnames(lsm)[(length(nbcol)+1):length(colnames(lsm))],
				variable.name=variable.name)
		
		form=paste(x2,"+",x3,"+",variable.name,"~",x1,sep="")
		
		form=as.formula(form)			
		d=dcast(m,form)
	}
	
	if(!is.null(x2) & is.null(x3))	
	{
		m=melt(data=lsm, id.vars=c(x1,x2),
				measure.vars=colnames(lsm)[(length(nbcol)+1):length(colnames(lsm))],
				variable.name=variable.name)
		
		form=paste(x2,"+",variable.name,"~",x1,sep="")
		
		if(contrast) form=paste(x2,"+",variable.name,"+",contrast.name,"~",x1,sep="")
		
		form=as.formula(form)	
		
		d=dcast(m,form)
	}
	
	if(is.null(x2) & is.null(x3))	
	{
		m=melt(data=lsm, id.vars=colnames(lsm)[nbcol],
				measure.vars=colnames(lsm)[(length(nbcol)+1):length(colnames(lsm))],
				variable.name=variable.name)
		
		form=paste(variable.name,"~",x1,sep="")
		if(contrast) form=paste(contrast.name,"+",variable.name,"~",x1,sep="")
		form=as.formula(form)	
		
		d=dcast(m,form)
	}
	
	
	if(!is.null(at.row))
	{
		d=spacetable(d,at.row=at.row)
	}
	
	
	
	
	lsm=ClinReport::desc(output=d,x1=x1,x2=x2,total=F,nbcol=length(nbcol),
			type.desc="lsmeans",type=type,y.label="",type.mod=type.mod,
			raw.output=raw.output,contrast=contrast,contrast.name=contrast.name)
	
	
	
	lsm
	
}

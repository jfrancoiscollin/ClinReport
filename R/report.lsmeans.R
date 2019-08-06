# TODO: continue to test the function with specific  contrast
# 
# Author: jfcollin
###############################################################################


#' 'LS Means' statistics reporting
#'
#' @param lsm emmGrid object (result of a \code{emmeans} call)
#' @param infer A vector of one or two logical values. Passed to \code{summary.emmGrid} function.
#' @param at.row Character. Passed to spacetable function. Used to space the results per levels of the mentioned variable
#' @param round Numeric. Specify the number of digits to round the statistics
#' @param transpose Logical. If TRUE Statistics will be reported in columns
#' @param x1 deprecated 
#' @param x2 deprecated
#' @param x3 deprecated
#' @param x1.name deprecated
#' @param x2.name deprecated
#' @param x3.name deprecated
#' @param data deprecated
#' @param contrast deprecated
#' @param contrast.name deprecated
#' @param type deprecated
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
#' report.lsmeans(raw.lsm)
#' 
#' # You can display the Statistics in columns
#' 
#' report.lsmeans(raw.lsm,transpose=TRUE)
#' 
#' # In case of just one intercept 
#' 
#' mod=glm(Species~1,data=iris,family=binomial)
#' raw.lsm=emmeans(mod,~1)
#' report.lsmeans(raw.lsm)
#' 
#' # Display statistics in columns
#' 
#' report.lsmeans(raw.lsm,transpose=TRUE)
#' 
#' 
#' #Mixed model example using lme4
#' 
#' mod=lmer(y_numeric~GROUP+TIMEPOINT+GROUP*TIMEPOINT+(1|SUBJID),data=data) 
#' raw.lsm=emmeans(mod,~GROUP|TIMEPOINT)
#' report.lsmeans(lsm=raw.lsm,at="TIMEPOINT")
#' 
#' # Display statistics in columns
#' 
#' report.lsmeans(lsm=raw.lsm,at="TIMEPOINT",transpose=TRUE)
#' 
#' 
#' # GLM model with specific contrast
#' 
#' warp.lm <- lm(breaks ~ wool+tension+wool:tension, data = warpbreaks)
#' warp.emm <- emmeans(warp.lm, ~ tension | wool)
#' contr=contrast(warp.emm, "trt.vs.ctrl", ref = "M")
#' report.lsmeans(lsm=contr,at="wool")
#' 
#' 
#' # Display statistics in columns
#' 
#' report.lsmeans(lsm=contr,at="wool",transpose=TRUE)
#' 
#' 
#' # Cox model
#' 
#' library(survival)
#' 
#' data(time_to_cure)
#' 
#' fit <- coxph(Surv(time, status) ~ Group, data = time_to_cure) 
#' em=emmeans(fit,~Group,type="response")
#' pairs=pairs(em,adjust="none",exclude="Untreated")
#' pairs
#' 
#' report.lsmeans(pairs)
#' 
#' # Display statistics in columns
#' 
#' report.lsmeans(pairs,transpose=TRUE)
#' 
#' @import reshape2 stats
#' 
#' @export 

report.lsmeans=function(lsm,at.row=NULL,infer=c(T,T),round=2,x1,x2,x3,x1.name,x2.name,x3.name,data,
		contrast,contrast.name,type,transpose=FALSE)
{
	
	
	if (!missing("x1"))
		warning("argument deprecated. There is no need anymore to specify any variable name")
	
	
	if (!missing("x2"))
		warning("argument deprecated. There is no need anymore to specify any variable name")
	
	if (!missing("x3"))
		warning("argument deprecated. There is no need anymore to specify any variable name")
	
	
	if (!missing("data"))
		warning("argument deprecated. There is no need anymore to specify the name of the data frame")
	
	
	if (!missing("contrast"))
		warning("argument deprecated. There is no need anymore to specify if LS Means are contrast or not")
	
	if (!missing("contrast.name"))
		warning("argument deprecated. There is no need anymore to specify if LS Means are contrast or not")
	
	if (!missing("x1.name"))
		warning("argument deprecated. There is no need anymore to specify any variable name")
	
	if (!missing("x2.name"))
		warning("argument deprecated. There is no need anymore to specify any variable name")
	
	if (!missing("x3.name"))
		warning("argument deprecated. There is no need anymore to specify any variable name")
	
	if (!missing("type"))
		warning("argument deprecated. There is no need anymore to specify the type of the prediction")
	
	
	
	# Checks
	
	
	if(class(lsm)!="emmGrid") stop("\n This function takes an emmGrid object only as lsm argument")
	
	variable.name="Statistics"
	
	terms=attr(lsm@model.info$terms,"term.labels")
	
	
	if(lsm@misc$estType=="pairs")
	{
		contrast=TRUE
	}else
	{
		contrast=FALSE
	}
	
	estname=lsm@misc$estName
	
	if(!is.null(lsm@misc$predict.type))
	{
		type=lsm@misc$predict.type
		
		if(lsm@misc$predict.type=="response")
		{
			if(!is.null(lsm@misc$inv.lbl)) estname=lsm@misc$inv.lbl
		}
	}else
	{
		type="response"
	}

	vars=unique(c(lsm@misc$pri.vars,lsm@misc$by.vars))
	
	if(length(vars)==1)
	{
		x1=vars[1]
		x2=NULL
		x3=NULL
	}
	
	if(length(vars)==2)
	{
		x1=vars[1]
		x2=vars[2]
		x3=NULL
	}
	
	if(length(vars)==3)
	{
		x1=vars[1]
		x2=vars[2]
		x3=vars[3]
	}
	
	

	call=as.character(lsm@model.info$call)[1]
	response=all.vars(lsm@model.info$call)[1]
	
	title=paste0("LS-Means table of: ",response)
	
	if(lsm@misc$estType=="pairs") title=paste0("LS-Means comparisons of: ",response)
	
	nbcol=1:length(vars)
	
	if(any("%in%"(call,c("lm","lmer","lme.formula","coxph")))) type.mod="quanti"
	if(any("%in%"(call,c("glm","glmer")))) type.mod="quali"
	if(!any("%in%"(call,c("glm","glmer","lm","lmer","lme.formula","coxph")))) stop("\n This function only supports lm, lmer, lme, glm or glmer models")
	
	
	raw.output=data.frame(summary(lsm,infer=infer))
	lsm=raw.output
	
	
	lsm[,-nbcol]=format(round(lsm[,-nbcol],round), nsmall = round)
	lsm[,-nbcol]=apply(lsm[,-nbcol],2,function(x)gsub(" ","",x))
	
	
	lsm$"Estimate (SE)"=paste(lsm[,estname],"(",lsm$SE,")",sep="")
	
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
		
		if(transpose) form=paste(x2,"+",x3,"+",x1,"~",variable.name,sep="")	
		
		form=as.formula(form)			
		d=dcast(m,form)
	}
	
	if(!is.null(x2) & is.null(x3))	
	{
		m=melt(data=lsm, id.vars=c(x1,x2),
				measure.vars=colnames(lsm)[(length(nbcol)+1):length(colnames(lsm))],
				variable.name=variable.name)
		
		form=paste(x2,"+",variable.name,"~",x1,sep="")
		
		if(transpose) form=paste(x2,"+",x1,"~",variable.name,sep="")
		
		form=as.formula(form)	
		d=dcast(m,form)
	}
	
	if(is.null(x2) & is.null(x3))	
	{
		m=melt(data=lsm, id.vars=colnames(lsm)[nbcol],
				measure.vars=colnames(lsm)[(length(nbcol)+1):length(colnames(lsm))],
				variable.name=variable.name)
		
		form=paste(variable.name,"~",x1,sep="")
		
		if(transpose) form=paste(x1,"~",variable.name,sep="")
		
		form=as.formula(form)	
		d=dcast(m,form)
	}
	
	
	if(!is.null(at.row))
	{
		d=spacetable(d,at.row=at.row)
	}
	
	if(is.null(at.row) & !is.null(x2))
	{
		at.row=x2
		d=spacetable(d,at.row=at.row)
	}
	
	
	
	
	
	lsm=ClinReport::desc(output=d,x1=x1,x2=x2,total=F,nbcol=length(nbcol),
			type.desc="lsmeans",type=type,y.label="",type.mod=type.mod,
			raw.output=raw.output,contrast=contrast,
			at.row=at.row,title=title)
	
	
	
	return(lsm)
	
}

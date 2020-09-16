# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

#' Export model information (still experimental)
#'
#' @param object A model (for now glm, lme, lmer and coxph models are availlable)
#' @param doc NULL or a rdocx object
#' @param page.break Logical. If TRUE it adds a page break after the output. Default to TRUE
#' @param ... Other arguments
#' 
#' 
#' @description
#' \code{report.doc} 
#' This function enables to export the information of the model (the package, the name of the function,
#' the call etc...)
#' 
#' @details
#' Compatible only (for now) with GLM, LME and Cox models. For now those output are not numbered.
#' 
#' 
#' @return  
#' A flextable object (if doc=NULL) or a rdocx object (if doc= an rdocx object).
#' 
#' @seealso \code{\link{report.doc}}
#' @examples
#' 
#'library(officer)
#'library(flextable)
#'library(nlme)
#'library(lme4)
#' 
#' data(datafake)
#' 
#' #Model info for lme model
#' 
#' mod=lme(y_numeric~GROUP+TIMEPOINT,random=~1|SUBJID,data=datafake,na.action=na.omit)
#' 
#' # Show in HTML (can be inserted in an R markdown or a MS Word document)
#' 
#' report.modelinfo(mod)
#' 
#' #Model info for lmer model
#' 
#' mod=lmer(y_numeric~GROUP+TIMEPOINT+(1|SUBJID),data=datafake,na.action=na.omit)
#' 
#' report.modelinfo(mod)
#' 
#' @import lme4
#' 
#' @rdname report.modelinfo
#' @export







report.modelinfo <- function(object,...)
{
	UseMethod("report.modelinfo")
}


#' @rdname report.modelinfo
#' @export 

report.modelinfo.lme=function(object,doc=NULL,page.break=TRUE,...)
{
	
	rpack_func="R Package nlme , function lme"
	label="Linear Mixed Effect Model"	
	object.formula=paste0(deparse(object$call),collapse="\n")
	method=object$method
	na.handling=attr(object$na.action,"class")
	if(is.null(na.handling)) na.handling="None"
	dd <- object$dims
	nbobs=paste0(dd[["N"]])
	nbgrp=paste0(dd$ngrps[1:dd$Q])
	aic_bic=paste0("AIC =",round(summary(object)$AIC,3), "; BIC = ",round(summary(object)$BIC,3))
	
	
	names=c("R package / function","Type of model","Model formula",
			"Method of adjustment","NA handling",
			"Number of Observations",
			"Number of Groups",
			"Quality of adjustment")
	
	model.info=data.frame(Information=names,Details=c(rpack_func,label,object.formula,
					method,na.handling,nbobs,nbgrp,aic_bic))
	
	ft=flextable(model.info)
	ft=bg(ft, i = 1, bg = "#DCDCDC", part = "header")
	
	ft = width(ft, j=1,width = 2)
	ft = width(ft, j=2,width = 6)
	
	if (!is.null(doc)) {
		if (class(doc) != "rdocx") 
			stop("doc must be a rdocx object")
		doc <- body_add_par(doc, "", style = "Normal")
		doc <- body_add_flextable(doc, value = ft)
		if (page.break) 
			doc = body_add_break(doc)
		return(doc)
	}
	else {
		return(ft)
	}
	
}




#' @rdname report.modelinfo
#' @export 

report.modelinfo.lm=function(object,doc=NULL,page.break=TRUE,...)
{
	
	rpack_func="R Package stats , function lm"
	label="Linear Model"	
	object.formula=paste0(deparse(object$call),collapse="\n")
	aic_bic=paste0("AIC =",round(AIC(object),3), "; BIC = ",round(BIC(object),3))
	
	
	names=c("R package / function","Type of model","Model formula",
			"Quality of adjustment")
	
	model.info=data.frame(Information=names,Details=c(rpack_func,label,
					object.formula,aic_bic))
	
	ft=flextable(model.info)
	ft=bg(ft, i = 1, bg = "#DCDCDC", part = "header")
	
	ft = width(ft, j=1,width = 2)
	ft = width(ft, j=2,width = 6)
	
	if (!is.null(doc)) {
		if (class(doc) != "rdocx") 
			stop("doc must be a rdocx object")
		doc <- body_add_par(doc, "", style = "Normal")
		doc <- body_add_flextable(doc, value = ft)
		if (page.break) 
			doc = body_add_break(doc)
		return(doc)
	}
	else {
		return(ft)
	}
	
}



#' @rdname report.modelinfo
#' @export 

report.modelinfo.lmerMod=function(object,doc=NULL,page.break=TRUE,...)
{
	
	rpack_func="R Package lme4 , function lmer"
	if(isLMM(object))	label="Linear Mixed Effect Model"	
	object.formula=paste0(deparse(object@call),collapse="\n")
	if(isREML(object)) method="REML"
	if(!isREML(object)) method="ML"
	nbobs=nobs(object)
	nbgrp=ngrps(object)
	aic_bic=paste0("AIC =",round(AIC(object),3), "; BIC = ",round(BIC(object),3))

	names=c("R package / function","Type of model","Model formula",
			"Method of adjustment",
			"Number of Observations",
			"Number of Groups",
			"Quality of adjustment")
	
	model.info=data.frame(Information=names,Details=c(rpack_func,label,object.formula,
					method,nbobs,nbgrp,aic_bic))
	
	ft=flextable(model.info)
	ft=bg(ft, i = 1, bg = "#DCDCDC", part = "header")
	
	ft = width(ft, j=1,width = 2)
	ft = width(ft, j=2,width = 6)
	
	if (!is.null(doc)) {
		if (class(doc) != "rdocx") 
			stop("doc must be a rdocx object")
		doc <- body_add_par(doc, "", style = "Normal")
		doc <- body_add_flextable(doc, value = ft)
		if (page.break) 
			doc = body_add_break(doc)
		return(doc)
	}
	else {
		return(ft)
	}
	
}

#' @rdname report.modelinfo
#' @export 


report.modelinfo.glm=function(object,doc=NULL,page.break=TRUE,...)
{
	
	rpack_func="R Package stats , function glm"
	label="Generalized Linear Model"	
	object.formula=paste0(deparse(object$call),collapse="\n")
	family=paste(paste0(object$family$family),";",paste0(object$family$link))
	na.handling=attr(object$na.action,"class")
	if(is.null(na.handling)) na.handling="None"
	aic_bic=paste0("AIC =",round(AIC(object),3), "; BIC = ",round(BIC(object),3))
	
	
	names=c("R package / function","Type of model","Model formula",
			"Family","NA handling",
			"Quality of adjustment")
	
	model.info=data.frame(Information=names,Details=c(rpack_func,label,object.formula,
					family,na.handling,aic_bic))
	
	ft=flextable(model.info)
	ft=bg(ft, i = 1, bg = "#DCDCDC", part = "header")
	
	ft = width(ft, j=1,width = 2)
	ft = width(ft, j=2,width = 6)
	
	if (!is.null(doc)) {
		if (class(doc) != "rdocx") 
			stop("doc must be a rdocx object")
		doc <- body_add_par(doc, "", style = "Normal")
		doc <- body_add_flextable(doc, value = ft)
		if (page.break) 
			doc = body_add_break(doc)
		return(doc)
	}
	else {
		return(ft)
	}
	
}

#' @rdname report.modelinfo
#' @export 

report.modelinfo.coxph=function(object,doc=NULL,page.break=TRUE,...)
{
	
	rpack_func="R Package survival , function coxph"
	label="Cox model"	
	object.formula=paste0(deparse(object$call),collapse="\n")
	nevent=object$nevent
	nobs=object$n
	aic_bic=paste0("AIC =",round(AIC(object),3), "; BIC = ",round(BIC(object),3))
	
	
	names=c("R package / function","Type of model","Model formula",
			"Number of observations","Number of events",
			"Quality of adjustment")
	
	model.info=data.frame(Information=names,Details=c(rpack_func,label,object.formula,
					nobs,nevent,aic_bic))
	
	ft=flextable(model.info)
	ft=bg(ft, i = 1, bg = "#DCDCDC", part = "header")
	
	ft = width(ft, j=1,width = 2)
	ft = width(ft, j=2,width = 6)
	
	if (!is.null(doc)) {
		if (class(doc) != "rdocx") 
			stop("doc must be a rdocx object")
		doc <- body_add_par(doc, "", style = "Normal")
		doc <- body_add_flextable(doc, value = ft)
		if (page.break) 
			doc = body_add_break(doc)
		return(doc)
	}
	else {
		return(ft)
	}
	
}



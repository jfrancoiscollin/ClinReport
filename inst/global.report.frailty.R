# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

#' Returns a docx object from ReporteRs package which contains main information
#' about a survival model from a frailty model (from frailtypack packages)
#' 
#'
#' @param mod a frailtypack object
#' @param doc a docx object



#' @description
#' \code{global.report.frailty} 
#' Returns a docx object from ReporteRs package
#' 
#' 
#' @details
#' This function add to a docx object the main information from a survival model from frailtypack package.


#' @return  
#' A docx object 
#' 
#' @seealso \code{\link{report.chisq}} 

#' @examples
#' \dontrun{
#' doc=docx()
#' #' data(kidney)
#' mod=frailtyPenal(Surv(time,status)~sex+age,
#' n.knots=12,kappa=10000,data=kidney)
#' global.report.frailty(doc,mod)
#' }
#' 
#' @import stats

#' @export


global.report.frailty=function(doc,mod)
{
	tb=textProperties(font.weight ="bold")
	doc=addParagraph(doc,"")
	doc=addParagraph(doc,pot("Survival model results",format =tb))
	doc=addParagraph(doc,"")
	doc=addParagraph(doc,pot("Explicative variables:"))
	doc=addParagraph(doc,as.character(mod$formula)[3])
	doc=addParagraph(doc,"")
	doc=addParagraph(doc,pot("Quality criteria of the model:"))
	if(!is.null(mod$AIC))doc=addParagraph(doc,paste0("AIC=",round(mod$AIC,4)))
	if(!is.null(mod$LCV))doc=addParagraph(doc,paste0("LCV=",round(mod$LCV,4)))	
	
	if(!is.null(mod$eta))
	{
		doc=addParagraph(doc,"")
		doc=addParagraph(doc,pot("Random group effect",format =tb))
		doc=addFlexTable(doc,FlexTable(report.frailty(mod)))
		doc=addParagraph(doc,"")
	}
	
	
	doc=addParagraph(doc,pot("Hazard ratios",format =tb))
	tab=report.HR.frailty(mod)
	doc=addFlexTable(doc,FlexTable(tab))
	doc=addParagraph(doc,"")
	doc=addParagraph(doc,pot("Global chi-square tests",format =tb))
	tab2=report.chisq.frailty(mod)
	doc=addFlexTable(doc,FlexTable(tab2))
	
	doc
}

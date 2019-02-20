# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

#' Returns Chi-square tests from a frailtyPenal object into a data frame.
#' 
#'
#' @param mod a frailtypack object



#' @description
#' \code{report.chisq} 
#' Return a table with Chi-square tests
#' 
#' 
#' @details
#' This function extract the information of the print.frailtyPenal function into a data frame.


#' @return  
#' A data frame object. 
#' 
#' @seealso \code{\link{report.HR}} 

#' @examples
#' \dontrun{
#' #' data(kidney)
#' mod=frailtyPenal(Surv(time,status)~sex+age,
#' n.knots=12,kappa=10000,data=kidney)
#' report.chisq.frailty(mod)
#' }

#' @export


report.chisq.frailty=function(mod)
{
	pval=mod$"p.global_chisq"
	pvalf=format(round(pval,3),nsmall=3,scientific=F,digits=3)
	pvalf[pval<0.001]="<0.001"	
	
	global=data.frame(Var=mod$names.factor,
			Chi.square=mod$"global_chisq",             
			Dof=mod$"dof_chisq", 
			P.value=pvalf)
	global
}


# TODO: Add comment
# 
# Author: jfcollin
###############################################################################


#' Returns Frailty parameter estimates from a frailtyPenal object into a data frame.
#' 
#'
#' @param mod a frailtypack object



#' @description
#' \code{report.frail} 
#' Return a table with Frailty parameter estimates 
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
#' data(dataNested)
#'modClu <- frailtyPenal(Surv(t1,t2,event)~cluster(group)+
#'subcluster(subgroup)+cov1+cov2,data=dataNested,
#'n.knots=8,kappa=50000)
#' report.frailty(modClu)
#' }

#' @export

report.frailty=function(mod,r=3)
{
	if(class(mod)=="nestedPenal")
	{
		alpha <- mod$alpha
		temp <- diag(mod$varH)[1]
		seH.alpha <- sqrt(((2 * (alpha^0.5))^2) * temp)
		p.alpha=1 - pnorm(alpha/seH.alpha)
		pvalf.a=format(round(p.alpha,r),nsmall=r,scientific=F,digits=r)
		pvalf.a[p.alpha<0.001]="<0.001"	
		
		eta <- mod$eta
		temp <- diag(mod$varH)[2]
		seH.eta <- sqrt(((2 * (eta^0.5))^2) * temp)
		temp <- diag(mod$varHIH)[2]
		seHIH.eta <- sqrt(((2 * (eta^0.5))^2) * temp)
		p.eta=1 - pnorm(eta/seH.eta)
		
		pvalf.e=format(round(p.eta,r),nsmall=r,scientific=F,digits=r)
		pvalf.e[p.eta<0.001]="<0.001"	
	}
	
	
	res=data.frame(Effect=c("Group","Subgroup"),
			Parameter=format(round(c(alpha,eta),r),nsmall=r,scientific=F,digits=r),
			SE=format(round(c(seH.alpha,seHIH.eta),r),nsmall=r,scientific=F,digits=r),
			P.values=c(pvalf.a,pvalf.e))
	
	res
}


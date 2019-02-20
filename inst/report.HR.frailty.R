# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

#' Returns Hazard ratios from a frailtyPenal object into a data frame.
#' 
#'
#' @param mod a frailtypack object



#' @description
#' \code{report.HR.frailty} 
#' Return a table with Hazard ratios, p-values and 95% CI
#' 
#' 
#' @details
#' This function extract the information of the print.frailtyPenal function into a data frame.


#' @return  
#' A data frame object. 
#' 
#' @seealso \code{\link{report.chisq}} 

#' @examples
#' \dontrun{
#' data(kidney)
#' mod=frailtyPenal(Surv(time,status)~sex+age,
#' n.knots=12,kappa=10000,data=kidney)
#' report.HR.frailty(mod)
#' }

#' @export

report.HR.frailty=function(mod)
{
	
	# quantile for the CI 
	
	level = 0.95
	z <- abs(qnorm((1 - level)/2))
	
	# Coef
	co <- mod$coef
	nvar <- length(co)
	
	# SE
	if(class(mod)=="frailtyPenal")
	{
		if (nvar != 1) {
			seH <- sqrt(diag(mod$varH))
			seHIH <- sqrt(diag(mod$varHIH))
		}else {
			seH <- sqrt(mod$varH)
			seHIH <- sqrt(mod$varHIH)
		}
	}
	
	if(class(mod)=="nestedPenal")
	{
		
		seH <- sqrt(diag(mod$varH))[-c(1, 2)]
		seHIH <- sqrt(diag(mod$varHIH))[-c(1, 2)]
	}
	
	se=seH
	
	# Pval
	pval=1 -pchisq((co/se)^2,1)
	
	pvalf=format(round(pval,3),nsmall=3,scientific=F,digits=3)
	pvalf[pval<0.001]="<0.001"
	
	
	li <-  format(round(exp(co - z * se),2),nsmall=2,scientific=F,digits=2)
	ls <-  format(round(exp(co + z * se),2),nsmall=2,scientific=F,digits=2)
	ci=paste0("[",li,";",ls,"]")
	
	
	# Supress the name of the variable from the coef names to get the levels used
	
	if(!is.null(mod$Xlevels))
	{
		name=gsub(names(mod$Xlevels)[1],"",names(co))
		if(length(names(mod$Xlevels))>1)
		{
			for(i in 2:length(names(mod$Xlevels)))
			{
				name=gsub(names(mod$Xlevels)[i],"",name)
			}	
		}
	}else
	{
		name=names(co)
		
	}
	
	
	# Detect the Reference levels
	
	if(!is.null(mod$Xlevels))
	{
		ref=vector()
		
		for(i in 1:length(names(mod$Xlevels)))
		{
			ref[i]=mod$Xlevels[[i]][!"%in%"(mod$Xlevels[[i]],name)]
		}
		
	}else
	{
		ref=""
	}
	
	# Create contrast labels
	
	if(!is.null(mod$Xlevels))
	{
		contr=list()
		comp=list()
		
		for(i in 1:length(names(mod$Xlevels)))
		{
			contr[[i]]=mod$Xlevels[[i]]["%in%"(mod$Xlevels[[i]],name)]
			comp[[i]]=paste0(contr[[i]]," versus ",ref[i])
		}
	}else
	{
		contr=name
		comp=rep("Increase of 1 unit",length(contr))
	}
	
	var.all=names(mod$coef)
	var.all=gsub(unlist(contr)[1],"",var.all)
	if(length(unlist(contr))>1)
	{
		for(i in 2:length(unlist(contr)))
		{
			var.all=gsub(unlist(contr)[i],"",var.all)
		}	
	}
	
	data.var.all=data.frame(var=var.all,comp="Increase of 1 unit")
	
	order=order(data.var.all$var)
	
	# Create column variable name
	
	if(!is.null(mod$Xlevels))
	{
		var=list()
		for(i in 1:length(names(mod$Xlevels)))
		{
			var[[i]]=rep(names(mod$Xlevels)[i],length(comp[[i]]))
		}
		
		var=unlist(var)
		comp=unlist(comp)
		
		data.var=data.frame(var,comp)
	}
	
	
	
	
	data.var=rbind(data.var,data.var.all[!"%in%"(data.var.all$var,data.var$var),])
	
	data.var=data.var[order(order(data.var.all$var)),]
	
	colnames(data.var)=c("Variables","Comparisons")
	
	# Fianl data frame with all information
	
	results=data.frame(data.var,
			HR=format(round(exp(mod$coef),2),nsmall=2,scientific=F,digits=2),
			Pval=pvalf,
			CI=ci)
	
	rownames(results)=NULL
	
	results	
}



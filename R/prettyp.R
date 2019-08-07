# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

#'Return a p-value vector in nice format 
#' 
#'
#' @param p A vector of numerical p-values
#' @param r A numeric values that indicates the number of digits to round to
#' 
#' @description
#' \code{prettyp} 
#' Return a formatted version of pvalues with nice format.
#' 
#' 
#' @details
#' It takes a p and return another one, but better.
#' 

#' @return  
#' Return a formated p-value vector with rounded numbers and <0.001 instead of 0.00

#' @examples
#' 
#' prettyp(c(0.05,0.001,0.00001),3)
#'

#' @export

prettyp=function(p,r=3)
{
	pf=format(round(p,r),nsmall=r,scientific=F,digits=r)
	pf[which(p<0.001)]="<0.001"	
	pf
}

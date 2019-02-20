# TODO: Add comment
# 
# Author: jfcollin
###############################################################################


# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

#'Return a rounded vector with equal number of digits
#' 
#' @param p Vector of numerical values
#' @param r Integer indicates the number of digits to round to
#' 
#' @description
#' \code{prettyround} 
#' Return a formatted version of the numeric vector.
#' 
#' 
#' @details
#' No detail.
#' 

#' @return  
#' Return a formatted vector with rounded numbers 

#' @examples
#' 
#' prettyround(c(-0.05,0.001,0.00001),3)
#'

#' @export

prettyround=function(p,r=3)
{
	pf=format(round(p,r),nsmall=r,scientific=F,digits=r)	
	pf
}


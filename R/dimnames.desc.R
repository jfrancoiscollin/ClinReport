# TODO: Add comment
# 
# Author: jfcollin
###############################################################################


#' dimnames method for desc object
#' 
#' @param x A desc object
#' 
#' @seealso \code{\link{desc}} 
#' 
#' @method dimnames desc 
#' @export


dimnames.desc=function(x)
{
	list(row.names(x$output), names(x$output))
	
}



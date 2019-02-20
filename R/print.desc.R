# TODO: Add comment
# 
# Author: jfcollin
###############################################################################


#' Print method for desc object
#' 
#' @param x A desc object
#' @param ... Other parameters
#' 
#' @seealso \code{\link{desc}} 
#' 
#' @method print desc 
#' @export

print.desc=function(x,...)
{
	
		print(x$output)

}

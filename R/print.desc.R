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
	cat("",sep="\n")
	cat("############################################",sep="\n")
	cat(x$title,sep="\n")
	cat("############################################",sep="\n")
	cat("",sep="\n")
	print(x$output)
	cat("",sep="\n")
	cat("############################################",sep="\n")
	cat("",sep="\n")
}

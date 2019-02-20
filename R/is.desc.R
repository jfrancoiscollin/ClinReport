# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

#' Check if it is really a desc object
#' 
#' @param x Normally, a desc object, but it can be anything...
#' 
#' @seealso \code{\link{desc}} 
#' 
#' @export

is.desc <- function(x) inherits(x, "desc")

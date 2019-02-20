# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

#' Remove some special characters
#' 
#' @param x Character object
#' @seealso \code{\link{desc}} 
#' 
#' @examples
#'  
#' x="Un#e(chainede$char~act+er-bizarre%"
#' clean.character(x)
#' 
#' @export


clean.character=function(x)
{
	y=gsub("(",".",x,fixed=T)
	y=gsub(")",".",y,fixed=T)
	y=gsub("#",".",y,fixed=T)
	y=gsub("=",".",y,fixed=T)
	y=gsub(":",".",y,fixed=T)
	y=gsub(">",".",y,fixed=T)
	y=gsub("<",".",y,fixed=T)
	y=gsub("+",".",y,fixed=T)
	y=gsub("-",".",y,fixed=T)
	y=gsub("[",".",y,fixed=T)
	y=gsub("]",".",y,fixed=T)
	y=gsub("{",".",y,fixed=T)
	y=gsub("}",".",y,fixed=T)
	y=gsub("~",".",y,fixed=T)
	y=gsub("^",".",y,fixed=T)
	y=gsub("$",".",y,fixed=T)
	y=gsub("%",".",y,fixed=T)
	y=gsub("/",".",y,fixed=T)
	y=gsub(" ",".",y,fixed=T)
	
	y
}


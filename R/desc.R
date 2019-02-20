# TODO: Add comment
# 
# Author: jfcollin
###############################################################################


#' Constructor function for the desc object
#' 
#' @param output a data.frame object
#' @param y Character indicating the response variable
#' @param x1 Character indicating a factor in the data 
#' @param x2 Character indicating a factor in the data 
#' @param total Boolean. Indicates if a column Total should be added or not
#' @param at.row Character. Pass to spacetable function. Used to space the results (see \code{\link{spacetable}})
#' @param type Character. Can be "response" or "link"
#' @param subjid Character. Indicates in the data.frame the name of the column used to identify the Id of the subjects.
#' If not null, it adds in the headers the number of unique subject per levels of x1 or y (if x1 is null).
#' @param type.desc Character. Can be "quali" "quanti" "lsmeans" or "quali_quanti" 
#' @param nbcol Numeric. number of columns that are not statistics 
#' @param y.label Character. The label for y response variable
#' @param type.mod Character. The type of models the LS means are coming from (can be 'quali' or 'quanti')
#' @param raw.output Data.frame. The raw statistics unformatted from \code{report.quali}, \code{report.quanti} and  \code{report.lsmeans}
#' @param contrast Logical. Specify if the contrast function has been used after the emmeans function
#' @param contrast.name Character. Corresponds to the label of the column in which the contrasts are specified
#'  Default value is 'contrast'.
#' 
#' @description
#' Creates a desc object.
#' 
#' 
#' @details
#' No detail.
#' @return  
#' A desc object
#' 
#' @seealso \code{\link{report.quali}} \code{\link{report.quanti}} \code{\link{report.lsmeans}} 

#' @examples
#' 
#'\dontrun{
#' 
#' x=desc()
#' 
#' }
#' 
#' @export

desc=function(output=NULL,total=NULL,nbcol=NULL,y=NULL,x1=NULL,x2=NULL,
		at.row=NULL,
		subjid=NULL,type.desc=NULL,type=NULL,y.label=NULL,type.mod=NULL,
		raw.output=NULL,contrast=NULL,contrast.name=NULL)
{
	
	
	l=list(output=output,total=total,nbcol=nbcol,y=y,x1=x1,x2=x2,
			at.row=at.row,
			subjid=subjid,type.desc=type.desc,type=type,y.label=y.label,
			type.mod=type.mod,raw.output=raw.output,contrast=contrast,
			contrast.name=contrast.name)
	
	class(l)="desc"
	
	return(l)
	
	
}

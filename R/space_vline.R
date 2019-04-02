# TODO: Add comment
# 
# Author: jfcollin
###############################################################################



#' Compute the indices where to put blanks in report.doc
#'
#' @param tab data.frame from a desc object
#' @param at.row Character. at.row option of a desc object
#' 
#' @description
#' Compute the indices where to put blanks in \code{report.doc}
#' 
#' 
#' @details
#' None
#' 
#' @return  
#' A vector of integers
#' 
#' @seealso \code{\link{report.doc}}




space_vline=function(tab,at.row)
{
	
	tab=droplevels(tab[tab[,at.row]!="",])
	
	split=tab[,at.row]
	
	
	split=factor(split,levels=unique(split))
	
	
	s=split(tab,split)
	
	
	nb.elem=sapply(s,nrow)
	
	vect=vector()
	
	vect=1:nb.elem[1]
	
	for(i in 2:length(nb.elem))
	{
		vect=c(vect,(vect[length(vect)]+2):(vect[length(vect)]+1+nb.elem[i]))
		
	}
	
	
	vect
	
}
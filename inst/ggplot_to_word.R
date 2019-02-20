# 
# Author: jfcollin
###############################################################################

#' Print a ggplot graph in a word document
#' 
#'
#' @param g GGplot object
#' 
#' @description
#' \code{ggplot_to_word} 
#' Print a ggplot graph in a word document.
#' 
#' 
#' @details
#' This function creates a temporary file using \code{tempfile()} and creat a MS-Word
#' document which contains the ggplot graph and opens it.

#' @return  
#' Nothing

#' @examples
#' \dontrun{
#' data(data)
#' ggplot_to_word(g)
#'}
#' 
#' @import ReporteRs
#' 
#' @export

ggplot_to_word=function(g)
{
	doc=docx()
	doc=addPlot(doc,function() print(g))
	file=paste0(tempfile(),".docx")
	writeDoc(doc,file)
	shell.exec(file)	
}


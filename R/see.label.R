
#' @title  Match between the colnames of the tibble and their corresponding labels.
#' 
#' @param tibble A tibble object
#' 
#' @description
#' \code{see.label} 
#' enables to see the match between the colnames of the tibble and their corresponding labels.
#' 
#' @details
#' Usefull function to check the corresponance between colnames and their labels in a tibble object.
#' 
#' 
#' 
#' @return  
#' A matrix
#' 
#' @seealso \code{\link{apply.format}} 
#' 
#' @examples
#' library(haven)
#' 
#' path1 <- system.file("examples", "clinical_sas.sas7bdat", package = "ClinReport")
#' path2 <- system.file("examples", "formats.sas7bcat", package = "ClinReport")
#' data=read_sas(path1,path2)
#' 
#' see.label(data)
#' 
#' 
#' @rdname see.label
#' 
#' @export

see.label=function(tibble)
{
	if(is_tibble(tibble))
	{
		lab=cbind(colnames(tibble),colnames(apply.format(tibble)))
	}
	
	lab
}

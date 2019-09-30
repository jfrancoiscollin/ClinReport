
#' @title  Match between the colnames of the tibble and their corresponding labels.
#' 
#' @param tibble A tibble object
#' 
#' @description
#' \code{see.label} 
#' enables to see the match between the colnames of the tibble and their corresponding labels.
#' 
#' @details
#' No details
#' 
#' 
#' @return  
#' A matrix
#' 
#' @seealso \code{\link{apply.format}} 
#' 
#' @examples
#' # Not yet
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

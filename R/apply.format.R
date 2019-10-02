# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

#' Descriptive "Quantitative" statistics (mean, SD, median...) reporting
#' 
#'
#' @param tibble a tibble object

#' @description
#' \code{apply.format} 
#' 
#' Returns a data.frame with the values of each variable replaced by their corresponding
#'  formats (label and labels of the tibble).
#' @details
#' 
#' @return  
#' A data.frame
#' 
#' @seealso \code{\link{tibble}} 
#' @examples
#'  
#' @export

apply.format=function(tibble)
{
	# get the label of the colnames
	
	lab.colnames=unlist(sapply(tibble,function(x) attributes(x)$label))
	
# replace the values by their labels, when there are labels
# for each factors
	
	ind.null=sapply(sapply(tibble,function(x) attributes(x)$labels),function(x) is.null(x))
	
	if(length(ind.null)>0)
	{
		for(i in 1:length(ind.null))
		{
			if(!ind.null[i])
			{
				temp=as.factor(deframe(tibble[,names(ind.null)[i]]))
				lab=names(attributes(deframe(tibble[,names(ind.null)[i]]))$labels)
				lev=attributes(deframe(tibble[,names(ind.null)[i]]))$labels
				for(j in 1:length(lev))
				{
					levels(temp)[levels(temp)==lev[j]]=lab[j]
				}
				
				tibble[,names(ind.null)[i]]=temp
			}
		}
	}
	
	if(length(lab.colnames)>0)
	{	
		for(i in 1:length(lab.colnames))
		{
			colnames(tibble)[colnames(tibble)==names(lab.colnames)[i]]=lab.colnames[i]
		}
	}
	
	col=colnames(tibble)
	tibble=data.frame(tibble)
	colnames(tibble)=col
	
	tibble
	
}

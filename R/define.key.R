# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

#' Create a key from two hierarchical factors
#'
#' @param data A data frame
#' @param upper A character. Indicates a factor in the data frame which corresponds to the factor with the higher number of levels (typically SOC variable)
#' @param lower A character. Indicates a factor in the data frame which corresponds to the factor with the lower number of levels (typically PT variable)
#' @param lower.levels A character. The label to be displayed in the table for the lower terms
#' @param upper.levels  A character. The label to be displayed in the table for the upper terms
#' 
#' @description
#' Used to create a single key from two hierarchical factors.
#' 
#' @details
#' 
#' Used internally by report.quali.hlev




define.key=function(data,upper="",lower="",
		upper.levels="Upper.Levels",
		lower.levels="Lower.Levels")
{
	
	relev=vector()
	for(i in 1:length(levels(data[,upper])))
	{
		relev[i]=levels(droplevels(data[,lower][data[,upper]==levels(data[,upper])[i]]))
	}
	
	
	d=data.frame(Levels=levels(data[,upper]),lower.levels=relev)
	colnames(d)[colnames(d)=="lower.levels"]=lower.levels
	colnames(d)[colnames(d)=="Levels"]=upper.levels
	d
}

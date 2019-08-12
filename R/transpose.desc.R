#' Transpose the table of statistics (experimental)
#'
#' @param desc a desc object
#' @param ... Not used
#' 
#' @description
#' Used to get the statistics in column for example.
#' 
#' For more examples see the website: \href{https://jfrancoiscollin.github.io/ClinReport}{ClinReport website}
#' 
#' @details
#' None
#' 
#' @return  
#' A desc object
#' 
#' @seealso \code{\link{report.quanti}}  \code{\link{report.doc}} \code{\link{desc}}

#' @examples
#' 
#'library(reshape2)
#'
#'
#'data(datafake)
#'
#'desc=report.quanti(data=datafake,y="y_numeric",x1="GROUP",
#'		x2="TIMEPOINT",total=TRUE,at.row="TIMEPOINT",subjid="SUBJID")
#'
#'transpose(desc)
#'
#'desc=report.quanti(data=datafake,y="y_numeric",x1="GROUP")
#'
#'transpose(desc)
#'
#'desc=report.quanti(data=datafake,y="y_numeric")
#'
#'transpose(desc)
#'
#'desc=report.quali(data=datafake,y="y_logistic",x1="GROUP")
#'
#'
#'transpose(desc)
#'
#'
#'desc=report.quali(data=datafake,y="y_logistic",x1="GROUP",x2="TIMEPOINT")
#'
#'transpose(desc)
#'
#'
#'
#'desc=report.quali(data=datafake,y="y_logistic")
#'
#'transpose(desc)
#'
#' @export



transpose <- function(desc,...)
{
	UseMethod("transpose")
}


#' @rdname transpose
#' 
#' @export


transpose.desc=function(desc,...)
{
	
	

	output=desc$output
	at.row=desc$at.row
	x1=desc$x1
	x2=desc$x2
	y=desc$y
	nbcol=desc$nbcol
	type.desc= desc$type.desc
	y.levels.label=desc$y.levels.label
	
	
	if(type.desc=="lsmeans") stop("Not yet implemented. You can use the transpose argument inside the report.lsmeans function")
	
	
	if(!is.null(x1))
	{
		m=suppressWarnings(melt(output,measure.vars=colnames(output)[-c(1:nbcol)],
						variable.name=x1))
	}
	
	if(is.null(x1))
	{
		m=output
		old.names=colnames(m)
		colnames(m)=make.names(colnames(m))
		y=colnames(m)[length(colnames(m))]
	}
	
	if(!is.null(at.row)) m=m[m$value!="",]
	
	if(type.desc=="quanti")
	{
		
		if(!is.null(x1))
		{
			if(!is.null(x2)) form=as.formula(paste0(x2,"+",x1,"~Statistics"))
			if(is.null(x2)) form=as.formula(paste0(x1,"~Statistics")) 
			
			
			output=dcast(m,form)
		}
		
		if(is.null(x1))
		{
			form=as.formula(paste0(".~Statistics"))
			output=dcast(m,form,value.var=y)
			output=output[,colnames(output)!="."]
		}
		
	}
	
	
	if(type.desc=="quali")
	{
		
		if(!is.null(x1))
		{
			if(!is.null(x2)) form=as.formula(paste0(x2,"+",x1,"+",y.levels.label,"~Statistics"))
			if(is.null(x2)) form=as.formula(paste0(x1,"+",y.levels.label,"~Statistics")) 
			
			m$Statistics=factor(m$Statistics,levels=rev(levels(m$Statistics)))
			
			output=dcast(m,form)
			
			output=as.data.frame(apply(output,2,function(x)
							{
								x[is.na(x)]=""
								x
							}))
			
			colnames(output)=gsub("column %",paste0("",x1,"%"),colnames(output))
			
		}
		
		if(is.null(x1))
		{
			
			form=as.formula(paste0(y.levels.label,"~Statistics"))
			m$Statistics=factor(m$Statistics,levels=rev(levels(m$Statistics)))
			
			output=dcast(m,form,value.var=y)
			
			output=as.data.frame(apply(output,2,function(x)
							{
								x[is.na(x)]=""
								x
							}))
			
			colnames(output)=gsub("column %",paste0("%"),colnames(output))
			
		}
		
	}
	
	
	
	if(!is.null(at.row)) output=spacetable(output,at.row=at.row)
	
	desc$output=output
	
	desc
}

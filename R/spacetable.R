# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

#' Add space to the results of a statistic table
#' 
#'
#' @param tab a data.frame. Normally, the output part of a desc object
#' @param at.row Numeric (between 1 and \code{ncol(tab)}) or Character (a column name of tab)
#' 
#' 
#' @description
#' \code{spacetable} 
#' Add space at some interval in a table of statistics
#' 
#' 
#' @details
#' Used internally into \code{report.lsmeans}, \code{report.quanti} and \code{report.quali}
#' 
#' @return  
#' A data.frame with some space between levels of the variable specified by \code{at.row}
#' 
#' @seealso \code{\link{report.quanti}}  \code{\link{report.quali}}  \code{\link{report.lsmeans}}
#' 
#' @examples
#' 
#' data(data)
#' spacetable(report.quali(data=data,y="y_logistic",x1="GROUP",
#' x2="TIMEPOINT")$output,at.row="TIMEPOINT")
#' 
#' 
#' @export

spacetable=function(tab,at.row=6)
{
	
	col=colnames(tab)
	
	if(is.numeric(at.row))
	{
		split=rep(1:(nrow(tab)/at.row),rep(at.row,length(1:(nrow(tab)/at.row))))
		check=length(split)==nrow(tab)
		if(!check)stop("length(split)!=nrow(tab): the at.row argument is inappropriate")
		
		s=split(tab,split)
		#transform each element of s as a matrix of character vector only
		s=sapply(s,function(x)
				{
					x=sapply(x,as.character)
					x
				},simplify=F)
		

		
		if(length(s)>1)
		{
			empty=tab[1,]
			empty=apply(empty,2,function(x) x="")
			
			tab2=rbind(s[[1]])
			
			for(i in 2:length(s))
			{
				tab2=rbind(tab2,empty,s[[i]])
			}
			
			rownames(tab2)=NULL
			tab2=data.frame(tab2)
			colnames(tab2)=col
			return(tab2)
		}else
		{
			return(tab)
		}
		
	}
	
	if(is.factor(at.row))
	{
		split=at.row
		check=length(split)==nrow(tab)
		if(!check)stop("length(split)!=nrow(tab): the at.row argument is inappropriate)")
		
		s=split(tab,split)
		#transform each element of s as a matrix of character vector only
		s=sapply(s,function(x)
				{
					x=sapply(x,as.character)
					x
				},simplify=F)
		
		
		if(length(s)>1)
		{
			empty=tab[1,]
			empty=apply(empty,2,function(x) x="")
			
			tab2=rbind(s[[1]])
			
			for(i in 2:length(s))
			{
				tab2=rbind(tab2,empty,s[[i]])
			}
			
			rownames(tab2)=NULL
			tab2=data.frame(tab2)
			colnames(tab2)=col
			return(tab2)
		}else
		{
			return(tab)
		}
		
	}
	
	
	if(is.character(at.row))
	{
		split=tab[,at.row]
		
		split=factor(split,levels=unique(split))
		
		check=length(split)==nrow(tab)
		if(!check)stop("length(split)!=nrow(tab): the at.row argument is inappropriate")
		
		s=split(tab,split)
		#transform each element of s as a matrix of character vector only
		s=sapply(s,function(x)
				{
					x=sapply(x,as.character)
					x
				},simplify=F)
		
		if(length(s)>1)
		{
			empty=tab[1,]
			empty=apply(empty,2,function(x) x="")
			
			tab2=rbind(s[[1]])
			
			for(i in 2:length(s))
			{
				tab2=rbind(tab2,empty,s[[i]])
			}
			
			rownames(tab2)=NULL
			tab2=data.frame(tab2)
			colnames(tab2)=col
			return(tab2)
		}else
		{
			return(tab)
		}
		
	}
	
}

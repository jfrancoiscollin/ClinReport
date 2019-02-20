# TODO: Add comment
# 
# Author: jfcollin
###############################################################################


#' Returns "quantitative" statistics of one numerical variable, splited 
#' according to the levels of one factor.
#' 
#'
#' @param data Data frame 
#' @param y Character indicating a numerical vector in the data 
#' @param x1 Character indicating a factor in the data (levels will be displayed in columns)
#' @param total Boolean which indicates if a column Total should added or not

#' @description
#' \code{report.quanti} 
#' Returns quantitative descriptive statistics such as mean, median, standard deviation etc...
#' 
#' 
#' @details
#' This function computes and reports quantitative statistics on \code{y} by level of \code{x1}.
#' This function use the functions \code{\link{stat.quanti}}.
#' You can run the example to show the results. If total=T, the last column is the statistics
#' performed overall levels of \code{x1}. 

#' @return  
#' It returns a data frame object. The first column list the statistics. 
#' The other columns are the levels of the variable \code{x1} (and column Total, if \code{total=T}).   
#' 
#' @seealso \code{\link{stat.quanti}} \code{\link{report.quali}} \code{\link{report.quanti}}

#' @examples
#' \dontrun{
#' data(data)
#' report.quanti.1(data,"DEMEANOUR_num","GROUP")
#' report.quanti.1(data,"DEMEANOUR_num","GROUP",total=T)
#' report.quanti.1(data,"DEMEANOUR_num","GROUP",scientific=T)
#' }

#' @export

report.quanti.1=function(data,y,x1,total=F,scientific=F,digits=NULL)
{
	# set x1 to factor (just in case)
	
	data[,x1]=as.factor(data[,x1])
	
	l=tapply(data[,y],data[,x1],function(x)stat.quanti(x,scientific=scientific,digits=digits))
	d=data.frame(matrix(unlist(l), nrow=length(l), byrow=T),
			stringsAsFactors=FALSE)
	
	d$x1=names(l)
	colnames(d)=c(colnames(l[[1]]),x1)
	d=d[,c(7,1:6)]
	
	m=melt(d,measure.vars=c("n","meansd","median","q1q3","minmax","nmiss"))
	
	# reorder levels according to x1 levels order
	m[,x1]=factor(m[,x1],levels=levels(data[,x1]))
	
	# transpose table
	form=formula(paste("variable","~",x1))
	d=dcast(m,form,value.var="value")
	
	colnames(d)[1]="Statistics"
	
	levels(d$Statistics)[levels(d$Statistics)=="n"]="N"
	levels(d$Statistics)[levels(d$Statistics)=="meansd"]="Mean(SD)"
	levels(d$Statistics)[levels(d$Statistics)=="median"]="Median(MAD)"
	levels(d$Statistics)[levels(d$Statistics)=="minmax"]="[Min;Max]"
	levels(d$Statistics)[levels(d$Statistics)=="q1q3"]="[Q1;Q3]"
	levels(d$Statistics)[levels(d$Statistics)=="nmiss"]="Missing"
	
	
	if(total==T)
	{
		data$intercept=1
		temp=report.quanti.1(data=data,y=y,x1="intercept")
		d=data.frame(d,Total=temp[,2])
	}
	
	return(d)
}







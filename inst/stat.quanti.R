# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

#' Return a vector with one statistic by element
#'
#' @param x Numerical vector
#' @param na.rm Boolean. To remove or not na's from x
#' @param round  Number to indicate how to round statistic values

#' @description
#' \code{stat.quanti} compute and return a list of quantitative descriptive statistics
#
#' 
#' @details
#' It's used internnaly in the \code{\link{report.quanti}} function

#' @return  
#' A vector with one statistic by element (isn't that great?!).
#' 
#' @seealso \code{\link{report.quanti}} 
#' 
#' @examples
#' \dontrun{
#' data(data)
#' stat.quanti(data$DEMEANOUR_num)
#' }

#' @export

stat.quanti=function(x,na.rm=T,round=2,scientific=F,digits=NULL)
{
	
	nsmall=round
	
	n=round(length(x))
	mean=format(round(mean(x,na.rm=na.rm),round),nsmall=nsmall,scientific=scientific,digits=digits)
	sd=format(round(sd(x,na.rm=na.rm),round),nsmall=nsmall,scientific=scientific,digits=digits)
	med=format(round(median(x,na.rm=na.rm),round),nsmall=nsmall,scientific=scientific,digits=digits)
	mad=format(round(mad(x,na.rm=na.rm),round),nsmall=nsmall,scientific=scientific,digits=digits)
	min=format(round(min(x,na.rm=na.rm),round),nsmall=nsmall,scientific=scientific,digits=digits)
	max=format(round(max(x,na.rm=na.rm),round),nsmall=nsmall,scientific=scientific,digits=digits)
	q1=format(round(quantile(x,na.rm=na.rm,probs =0.25),round),nsmall=nsmall,scientific=scientific,digits=digits)
	q3=format(round(quantile(x,na.rm=na.rm,probs=0.75),round),nsmall=nsmall,scientific=scientific,digits=digits)
	nmiss=round(length(x[is.na(x)]))
	
	stat=cbind(n=n,meansd=paste(mean,"(",sd,")",sep=""),
			median=paste(med,"(",mad,")",sep=""),			
			q1q3=paste("[",q1,";",q3,"]",sep=""),
			minmax=paste("[",min,";",max,"]",sep=""),
			nmiss=nmiss)
	stat
}
















####################
# test 
#####################

#source("C:/Dev/Clinical studies/flormel 1604/data.management.R")
#stat.by1(data,y="RESPIRATION_num",x1="treatment")
#stat.by1(data,y="RESPIRATION_num",x1="treatment",total=T)
#stat.by2(data,y="RESPIRATION_num",x1="TIMEPOINT",x2="treatment")
#table=stat.by2(data,y="RESPIRATION_num",x1="TIMEPOINT",x2="treatment",total=T)
#table
#ft=report.doc(table,title="Respiration rate: descriptive statistics\nFAS population",
#rowspan=1,zebra=F,zebra.num=1,
#colspan=c(1,1,3,1),
#colspan.value=c("","","Treatment group","") ,nb.trt=4, nb.col=2,
#delim=6)
#doc=docx()
#doc=addFlexTable(doc,ft,par.properties = parProperties(text.align = "center"))
#writeDoc(doc,"C:\\Users\\jfcollin\\Desktop\\example.docx")
#shell.exec("C:\\Users\\jfcollin\\Desktop\\example.docx")






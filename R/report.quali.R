
# Author: jfcollin
###############################################################################

#' Descriptive "Qualitative" statistics (frequencies and percentages) reporting
#'
#' @param data a data.frame object
#' @param y Character indicating a factor in the data (the response)
#' @param x1 Character indicating a factor in the data (levels will be displayed in columns)
#' @param x2 Character indicating a factor in the data (levels will be displayed in rows). Only possible if x1 is not NULL.
#' @param y.label Character indicating the label for y to be displayed in the title of the table
#' @param y.levels.label Character. Indicates the label of the column in which the levels of y are displayed
#' @param x2.label Character indicating the label for x2 parameter, only if x2 is not null
#' @param total Logical Indicates if a column Total should be added or not
#' @param round  Integer Indicates the number of digits to round percentages
#' @param at.row Character. Pass to spacetable function. Used to space the results (see example below)
#' @param percent.col Logical By default it is set to T to indicate that column percentages should be reported. If set to False, row percentages are reported.
#' @param subjid Character Indicates in the data.frame the name of the column used to identify the Id of the subjects. If not null, it adds in the headers the number of unique subject per levels of x1 or y (if x1 is null).
#' @param remove.zero Logical. Remove the levels in the contingency table for which there is no observation.
#' @param drop.y Character. Indicates one or several levels of the y factor that you want to drop in the result
#' @param drop.x1 Character. Indicates one or several levels of the x1 factor that you want to drop in the result
#' @param drop.x2 Character. Indicates one or several levels of the x2 factor that you want to drop in the result
#'@param remove.missing Logical. default to TRUE. If TRUE number of missing values are reported and percentages
#' take into account the number of missing value in the calculation. If set to FALSE, the missing values regarding the response factor y are ignored
#' and percentages are computed on non missing values only.
#' 
#' @description
#' Compute and report frequencies and percentages by levels of \code{y} (in rows) and by levels of \code{x1} (in columns)
#' and \code{x2} in rows.
#' 
#' For more examples see the website: \href{https://jfrancoiscollin.github.io/ClinReport}{ClinReport website}
#' 
#' @details
#' This function computes and reports qualitative statistics by level of \code{y} and by level of \code{x1} (if not null)
#' and \code{x2} (if not null).
#' 
#' See examples to show the results. If \code{total=T}, the last column is the statistics
#' performed overall levels of the explicative variables x1.
#' 
#' Note that missing values are counted in the calculation of the percentages.

#' @return  
#' A desc object
#' 
#' @seealso \code{\link{report.quanti}}  \code{\link{report.doc}} \code{\link{desc}}

#' @examples
#' 
#'  
#' data(datafake)
#' 
#' # No explicative variable changing y labels (option y.levels.label)
#' report.quali(data=datafake,y="y_logistic",
#' y.label="Clinical cure",y.levels.label="Levels")
#' 
#' # 1 explicative variable (option x1:  levels in columns)
#' report.quali(data=datafake,y="y_logistic",x1="GROUP",y.levels.label="Clinical cure")
#' 
#' # 2 explicative variables (x1, levels in columns, option x2, levels in rows),
#' # spcacing the results (option at.row)
#' report.quali(data=datafake,y="y_logistic",x1="GROUP",
#' x2="TIMEPOINT",y.levels.label="Clinical cure",x2.label="Days",at.row="Days")
#' 
#' # 2 explicative variables with row percentages (option percent.col=F)
#' report.quali(data=datafake,y="y_logistic",x1="GROUP",
#' x2="TIMEPOINT",percent.col=FALSE,x2.label="Days",y.levels.label="Clinical cure")
#' 
#' # Add Total number of subjects in headers (option subjid="SUBJID")
#' report.quali(data=datafake,y="y_logistic",x1="GROUP",
#' x2="TIMEPOINT",at.row="TIMEPOINT",subjid="SUBJID")
#' 
#' # Round percentages to 1 digit (option round=1)
#' report.quali(data=datafake,y="y_logistic",x1="GROUP",
#' x2="TIMEPOINT",at.row="TIMEPOINT",subjid="SUBJID",round=1)
#' 
#' 
#' # Qualitative statistics with a response with more than one levels
#' tab=report.quali(data=datafake,y="y_poisson",x1="GROUP",
#' x2="TIMEPOINT",at.row="TIMEPOINT",subjid="SUBJID",round=1)
#' 
#' # Print formatted results
#' tab
#' 
#' 
#' #Getting raw output (unformatted) 
#' tab$raw.output


#' @export


report.quali=function(data,y=NULL,x1=NULL,x2=NULL,y.label=y,
		x2.label=NULL,
		y.levels.label="Levels",total=F,
		round=2,at.row=NULL,percent.col=T,subjid=NULL,remove.zero=F,
		drop.y=NULL,drop.x1=NULL,drop.x2=NULL,remove.missing=F)
{
	
#   y="DEMEANOUR"
#	x1="GROUP"
#	x2="TIMEPOINT"
	
#	y.label=y
#	y.levels.label=y	
	
#	x2.label="Factor"
#	round=2
#	total=T
#	at.row=NULL
#	round=2
#	at.row=NULL
#	percent.col=T
#	subjid=NULL

	#checks on y and data arguments
	
	if(is.null(y)) stop("y argument cannot be NULL")
	if(class(data)!="data.frame") stop("data argument should be a data.frame")
	if(class(y)!="character") stop("y argument should be a character")
	
	y=check.x(data,y,substitute=substitute(data))
	
	if(!is.null(x2))
	{
		if(is.null(x2.label)) x2.label=x2
	}
	
	
	
	if(!is.null(drop.x2))
	{
		if(is.null(x2))
		{
			drop.x2=NULL
			message("drop.x2 argument not used because x2 argument is missing")
		}
		
		if(!is.null(x2))
		{
			check=any(!"%in%"(drop.x2,levels(data[,x2])))
			if(!check)
			{
				data=droplevels(data[!"%in%"(data[,x2],drop.x2),])
			}else
			{
				message("drop.x2 argument not used because it contains levels that are not in x2 factor")
			}
		}
		
	}
	
	if(!is.null(drop.x1))
	{
		if(is.null(x1))
		{
			drop.x1=NULL
			message("drop.x1 argument not used because x1 argument is missing")
		}
		
		if(!is.null(x1))
		{
			check=any(!"%in%"(drop.x1,levels(data[,x1])))
			if(!check)
			{
				data=droplevels(data[!"%in%"(data[,x1],drop.x1),])
			}else
			{
				message("drop.x1 argument not used because it contains levels that are not in x1 factor")
			}
		}
		
	}
	
	if(!is.null(drop.y))
	{
		check=any(!"%in%"(drop.y,levels(data[,y])))
		if(!check)
		{
			data=droplevels(data[!"%in%"(data[,y],drop.y),])
		}else
		{
			message("drop.y argument not used because it contains levels that are not in y factor")
		}
		
	}
	
	
	# Recursive call in case x1 and/or x2 are NULL
	
	
	if(is.null(x1) & is.null(x2))
	{
		
		temp=data
		temp$int=as.factor(y.label)
		
		freq=report.quali(temp,y,x1="int",x2.label=x2.label,y.label=y.label,
				x2="int",y.levels.label=y.levels.label,total=F,
				,percent.col=percent.col,subjid=subjid,
				round=round,remove.missing=remove.missing)
		
		freq$output=freq$output[,-1]
		freq$x1=NULL
		freq$x2=NULL
		freq$at.row=NULL
		
		return(freq)
	}
	
	
	if(!is.null(x1) & is.null(x2))
	{
		
		temp=data
		temp$int=as.factor(1)
		freq=report.quali(temp,y,x1,x2="int",y.levels.label=y.levels.label,
				total=total,
				y.label=y.label,
				,percent.col=percent.col,subjid=subjid,
				round=round,remove.missing=remove.missing)
		
		freq$output=freq$output[,-1]
		freq$x2=NULL
		freq$nbcol=freq$nbcol-1
		freq$at.row=NULL
		
		return(freq)
		
	}
	
	if(is.null(x1) & !is.null(x2))
	{
		stop("x1 argument cannot be NULL if x2 argument is not null")
	}
	
	
	
# from now on we continue only if x1 and x2 are not null
# if they were. they had been replaced by an intercept(s) and this function calls itself recursively.
	
# check
	
	x1=check.x(data,x1,substitute=substitute(data))
	x2=check.x(data,x2,substitute=substitute(data))
	
	
	# check
	if(any(levels(data[,x1])=="")) stop(paste0("One of the levels of ",x1," is equal to '' and this function doesn't like that. Can you please change this level?"))
	if(any(levels(data[,x2])=="")) stop(paste0("One of the levels of ",x2," is equal to '' and this function doesn't like that. Can you please change this level?"))
	
	
	
	# add NA as category
	# to count the number of missing values (if it's not already the case)
	
	if(!remove.missing)
	{
		if(!any(is.na(levels(data[,y]))))
		{
			data[,y]=addNA(data[,y])
		}	
	}
	
	if(remove.missing)
	{
		data=data[!is.na(data[,y]),]
	}
	
	
	# Compute frequency and total sample size for percentage
	
	freq=data.frame(table(Var1=data[,y],Var2=data[,x1],Var3=data[,x2]))
	
	
	# If percent.col n total by column (sum on y)
	# else n total by row (sum on x1)
	
	if(percent.col)
	{
		n=data.frame(table(Var2=data[,x1],Var3=data[,x2]))
	}else
	{
		n=data.frame(table(Var1=data[,y],Var3=data[,x2]))
	}
	
	
	# add percent to frequencies
	
	if(percent.col)
	{
		freq=merge(freq,n,by=c("Var2","Var3"))
		
	}else
	{
		freq=merge(freq,n,by=c("Var1","Var3"))
	}
	
	if(remove.zero)
	{
		freq=droplevels(freq[freq$Freq.x!=0,])
	}
	
	raw.freq=freq
	
	if(!is.null(x1)) colnames(raw.freq)[colnames(raw.freq)=="Var2"]=x1
	if(!is.null(x2)) colnames(raw.freq)[colnames(raw.freq)=="Var3"]=x2
	colnames(raw.freq)[colnames(raw.freq)=="Var1"]=y
	
	if(x1==x2) raw.freq=raw.freq[,colnames(raw.freq)!=x2]
	
	freq$percent=paste0("(",format(round(100*(freq$Freq.x/freq$Freq.y),round), nsmall = round),"%)")
	freq$percent=gsub(" ","",freq$percent,fixed=T)
	freq$value=paste0(freq$Freq.x,freq$percent)
	freq$Freq.y=NULL
	freq$Freq.x=NULL
	freq$percent=NULL
	
	freq[,"Var1"]=as.character(freq[,"Var1"])
	freq[is.na(freq[,"Var1"]),"Var1"]=paste0("")
	
	f=as.formula(paste0("Var3","+","Var1","~","Var2"))
	
	freq[,"Var1"]=factor(freq[,"Var1"],levels=c(levels(data[,y]),""))
	freq=dcast(freq,f,value.var="value")
	
	colnames(freq)[colnames(freq)=="Var1"]=y.levels.label
	colnames(freq)[colnames(freq)=="Var3"]=x2.label
	colnames(freq)[colnames(freq)=="value"]=y.label
	
	# Add column Total if requested
	
	if(total)
	{
		freq.tot=report.quali(data=data,y=y,
				x1=x2,round=round)$output
		
		freq.tot=suppressWarnings(melt(freq.tot,measure.vars=colnames(freq.tot)[-1],
						y.levels.label=x2.label,value.name="Total"))
		
		freq=data.frame(freq,Total=freq.tot[freq.tot[,2]!="Statistics","Total"],
				fix.empty.names =F,check.names=F)
		
		if(length(which(colnames(freq)=="NA"))>0)
		{
			freq=freq[,-which(colnames(freq)=="NA")]
		}
		
		freq[,-c(1,2)]=apply(freq[,-c(1,2)],2,function(x)gsub(" ","",x))
		
		
	}
	
	
	# Add numbers of subject N= in headers
	
	if(!is.null(subjid))
	{
		
		if(!any("%in%"(colnames(data),subjid))) stop(paste0(subjid," variable is not in data colnames"))
		
		if(!total)
		{
			N=tapply(data[,subjid],data[,x1],function(x)length(unique(x)))
			colnames(freq)[-c(1,2)]=paste0(colnames(freq)[-c(1,2)]," (N=",N,")")
			
		}
		
		
		if(total)
		{
			N=tapply(data[,subjid],data[,x1],function(x)length(unique(x)))
			N=c(N,sum(N))
			colnames(freq)[-c(1,2)]=paste0(colnames(freq)[-c(1,2)]," (N=",N,")")
			
		}
		
	}
	
	
	
	if(percent.col)
	{
		freq$Statistics=rep("n (column %)",nrow(freq))
	}else
	{
		freq$Statistics=rep("n (row %)",nrow(freq))
	}
	
	
	# determination of the number of columns (outside the levels of x1)
	
	if( is.null(x2) )
	{
		nbcol=2
	}else
	{
		nbcol=3
	}
	
	#Add missing labels to stat labels
	
	freq$Statistics[freq[,y.levels.label]==""]=" Missing n(%)"
	freq=freq[,c(1,2,ncol(freq),(3:(ncol(freq)-1)))]
	
	#Remove (NaN%) and (0.00%) from SD if n=0 and " " if any
	freq=as.data.frame(apply(freq,2,function(x)gsub("(NaN%)","",x,fixed=T)))
	freq=as.data.frame(apply(freq,2,function(x)gsub("(0.00%)","(0%)",x,fixed=T)))
	
	# Spacing results
	
	# check: si c'est mal renseigne on le met a null avec un message
	
	if(!is.null(at.row))
	{	
		if(!any(colnames(freq)==at.row)) 
		{
			message("at.row argument was not found in the colnames of the statistic table produced (probably mispelled)\n
							so it has been set to NULL")
			at.row=NULL
		}	
	}
	
	if(!is.null(at.row))
	{	
		freq=spacetable(freq,at.row=at.row)
	}
	
	
	if(is.null(at.row) & !is.null(x2))
	{	
		if(is.null(x2.label)) at.row=x2
		if(!is.null(x2.label)) at.row=x2.label
		
		freq=spacetable(freq,at.row=at.row)
	}
	
	title=paste0("Qualitative descriptive statistics of : ",y.label)
	
	freq=ClinReport::desc(output=freq,total=total,nbcol=nbcol,y=y,x1=x1,x2=x2,
			at.row=at.row,
			subjid=subjid,type.desc="quali",type=NULL,y.label=y.label,
			raw.output=raw.freq,title=title	,y.levels.label=y.levels.label
	)
	
	
	return(freq)
}








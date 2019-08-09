# 
# Author: jfcollin
###############################################################################

#' Descriptive "Quantitative" statistics (mean, SD, median...) reporting
#' 
#'
#' @param data Data.frame object
#' @param y Character indicating a numerical vector in the data frame passed to \code{data} argument
#' @param x1 Character indicating a factor in the data (levels will be displayed in columns)
#' @param x2 Character indicating a factor in the data (levels will be displayed in lines)
#' @param round Numeric to indicate how to round statistics
#' @param total Logical to indicate if a "Total" column should be added
#' @param scientific Logical Indicates if statistics should be displayed in scientific notations or not
#' @param digits Numeric (used if scientific=TRUE) to indicate how many digits to use in scientific notation
#' @param at.row Character Used to space the results (see examples)
#' @param y.label Character Indicates the label for y parameter to be displayed in the title of the table
#' @param subjid Character Indicates the column in which there is the subject Id to add the number of subjects in the column header if x1 and x2 are not null.
#' @param geomean Logical If yes geometric mean is calculated  instead of arithmetic mean: \code{exp(mean(log(x),na.rm=TRUE))} for x>0
#' @param add.mad Logical If yes the Median Absolute Deviance is added to the median statistics (see function \code{\link{mad}}) 
#' @param default.stat Logical (default to TRUE). If FALSE you can specify your own example
#' @param func.stat Function. If specified then default.stat=FALSE and only the specified statistic is reported
#' @param func.stat.name Character. Used only if default.stat=FALSE.  Indicates the name of specific statistic you want to report
#' @param stat.name Character. Indicates the name of the variable that report the statistics Default = "Statistics"
#' @param drop.x1 Character. Indicates one or several levels of the x1 factor that you want to drop in the result
#' @param drop.x2 Character. Indicates one or several levels of the x2 factor that you want to drop in the result
#' 
#' @description
#' \code{report.quanti} 
#' Returns quantitative descriptive statistics such as mean, median, standard deviation etc...
#' 
#' For more examples see the website: \href{https://jfrancoiscollin.github.io/ClinReport}{ClinReport website}
#' 
#' @details
#' This function computes and reports quantitative statistics on \code{y}. And can gives the statistics by level of two factors (\code{x1}
#' in columns and/or \code{x2} in rows). 
#' See the example to show the results. If \code{total=TRUE}, the last column is the statistics
#' performed overall levels of \code{x1} for each levels of \code{x2}. 
#' Quantiles are calculated using type 3 (SAS presumed definition) algorithms, but even though,
#' some differences between SAS and R can appear on quantile values.
#' 
#' "geomean" compute the geometric mean defined as exp(mean(log(y))). The values below or equal 0 are removed and
#' a message is printed  to indicate how many values were deleted to calculate the geometric mean.
#' 
#' \code{N} returns the number of observations (including NA values)
#' 
#' stat.name is automatically transformed using \code{\link{make.names}} function.

#' @return  
#' A desc object.
#' 
#' @seealso \code{\link{report.quali}} \code{\link{report.doc}} \code{\link{desc}}

#' @examples
#'  
#' data(datafake)
#' 
#' # Quantitative statistics with no factor
#' 
#' report.quanti(data=datafake,y="y_numeric",total=TRUE,y.label="Awesome results")
#' 
#' #' # Quantitative statistics with no factor with geometric mean (option geomean=TRUE)
#' 
#' report.quanti(data=datafake,y="y_numeric",y.label="Awesome results",geomean=TRUE)
#' 
#' # Quantitative statistics with one factor
#' 
#' report.quanti(data=datafake,y="y_numeric",x1="GROUP")
#' 
#' # One factor with total column
#' 
#' report.quanti(data=datafake,y="y_numeric",x1="GROUP",total=TRUE)
#' 
#' # Quantitative statistics with two factors
#' 
#' report.quanti(data=datafake,y="y_numeric",x1="GROUP",x2="TIMEPOINT")
#' 
#' # Quantitative statistics with two factors and a total column
#' 
#' report.quanti(data=datafake,y="y_numeric",x1="GROUP",x2="TIMEPOINT",total=TRUE)
#' 
#' # Add median absolute deviance to the median statistics
#' 
#' report.quanti(data=datafake,y="y_numeric",x1="GROUP",x2="TIMEPOINT",total=TRUE,add.mad=TRUE)
#' 
#' # Quantitative statistics with spacing rows (option at.row)
#' 
#' report.quanti(data=datafake,y="y_numeric",x1="GROUP",
#' x2="TIMEPOINT",total=TRUE,at.row="TIMEPOINT")
#' 
#' # Add number of subjects in headers (option subjid)
#' 
#' tab=report.quanti(data=datafake,y="y_numeric",x1="GROUP",
#' x2="TIMEPOINT",total=TRUE,at.row="TIMEPOINT",subjid="SUBJID")
#' 
#' # Print tab output
#' tab
#' 
#' 
#' #Getting a specific statistic and not the default ones
#' 
#' mystat=function(x) quantile(x,0.99,na.rm=TRUE)
#' 
#' tab=report.quanti(data=datafake,y="y_numeric",x1="GROUP",
#' x2="TIMEPOINT",total=TRUE,subjid="SUBJID",
#' func.stat=mystat,func.stat.name="99% quantile")
#' tab
#' 
#' mystat2=function(x) mean(x,na.rm=TRUE)/sd(x,na.rm=TRUE)
#' 
#' tab=report.quanti(data=datafake,y="y_numeric",x1="GROUP",
#' total=TRUE,subjid="SUBJID",func.stat=mystat2,
#' func.stat.name="Coefficient of variation")
#' tab
#' 
#' mode=function(x) {
#'   x=na.omit(x)
#'   ux <- unique(x)
#'   ux[which.max(tabulate(match(x, ux)))]
#' }
#' 
#' tab=report.quanti(data=datafake,y="y_numeric",
#' func.stat=mode,func.stat.name="Mode")
#' 
#' 
#' #Getting raw output
#' tab$raw.output
#' 
#' #Getting a data.frame version of the output
#' tab$output
#' 
#' @import reshape2
#' 
#' @importFrom dplyr %>% summarise_at group_by 
#' 
#' @export




report.quanti=function(data,y,x1=NULL,x2=NULL,y.label=y,
		round=2,
		total=F,scientific=F,digits=NULL,at.row=NULL,subjid=NULL,geomean=F,
		add.mad=F,default.stat=T,func.stat=NULL,stat.name="Statistics",func.stat.name="",
		drop.x1=NULL,drop.x2=NULL)
{
	
	stat.name=make.names(stat.name)
	
#	y="DEMEANOUR_num"
#	x1="GROUP"
#	x2="TIMEPOINT"
#	y.label=y	
#	round=2
#	total=T
#	scientific=F
#	digits=NULL
#	at.row=NULL
	
	
	################################
	# Check 
	################################
	
	if(is.null(y)) stop("y argument cannot be NULL")
	if(class(data)!="data.frame") stop("data argument should be a data.frame")
	
	if(class(y)!="character") stop("Dear user. y argument should be a character")
	if(!any(colnames(data)==y)) stop("y argument should be in data colnames")
	if(!is.numeric(data[,y])) stop(paste0(as.character(substitute(data)),"[,'",y,"']","should be a numeric variable"))
	
	if(!is.logical(total))		stop("Argument total argument must be logical")
	if(!is.numeric(digits) & !is.null(digits)) stop("Argument digits must be numeric")
	
	
	
	if(!is.numeric(round)) stop(paste("round should be numeric"))
	
	
	if(is.null(x1) & !is.null(x2)) stop("If you have only one explicative variable, then use x1 and not x2 argument")
	
	
	if(!is.null(func.stat))
	{
		# check it's a function
		if(!is.function(func.stat)) stop("func.stat argument should be a function")
		
		default.stat=F
		
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
	
	################################
	# start function
	################################
	
	# group data by factors
	
	if(is.null(x1) & is.null(x2))
	{
		by_GROUP=data 
		
	}
	
	if(!is.null(x1) & is.null(x2))
	{
		x1=check.x(data,x1)
		
		by_GROUP=data %>% group_by(!!as.name(x1))
	}
	
	if(!is.null(x1) & !is.null(x2))
	{
		
		x1=check.x(data,x1)
		x2=check.x(data,x2)
		
		by_GROUP=data %>% group_by(!!as.name(x1))%>% group_by(!!as.name(x2),add=T)	
	}
	
	# define statistics
	
	
	if(default.stat)
	{
		
		N=as.formula(paste0("~","length(.)"))
		n=as.formula(paste0("~","length(",y,")"))
		mean=as.formula(paste0("~","mean(",y,",na.rm=T)"))
		sd=as.formula(paste0("~","sd(",y,",na.rm=T)"))
		median=as.formula(paste0("~","median(",y,",na.rm=T)"))
		mad=as.formula(paste0("~","mad(",y,",na.rm=T)"))
		q1=as.formula(paste0("~","quantile(",y,",na.rm=T,0.25,type =3)"))
		q3=as.formula(paste0("~","quantile(",y,",na.rm=T,0.75,type =3)"))
		min=as.formula(paste0("~","min(",y,",na.rm=T)"))
		max=as.formula(paste0("~","max(",y,",na.rm=T)"))
		missing=as.formula(paste0("~","length(",y,"[is.na(",y,")])"))
		
		geomean_func=function(x)
		{
			if(any(x<=0)) message(paste0(length(x[x<=0])," values were removed to calculate the Geometric mean"))
			x=x[x>0]
			exp(mean(log(x),na.rm=T))
		}
		
		
		geo.mean=as.formula(paste0("~","geomean_func(",y,")"))
		
#	geomean1=as.formula(paste0("~","geomean_func1(",y,")"))
#	
		#	select statistics
		
#	stat_list=c("N"=N,"n"=n,"mean"=mean,
#			"sd"=sd,"median"=median,"mad"=mad,
#			"q1"=q1,"q3"=q3,"min"=min,"max"=max,
#			"missing"=missing,
#			"geomean"=geomean,
#			"geomean1"=geomean1)
		
		if(geomean)
		{
			stat_list=c("N"=N,"mean"=geo.mean,
					"sd"=sd,"median"=median,
					"q1"=q1,"q3"=q3,"min"=min,"max"=max,
					"missing"=missing)
			
		}else
		{
			
			stat_list=c("N"=N,"mean"=mean,
					"sd"=sd,"median"=median,
					"q1"=q1,"q3"=q3,"min"=min,"max"=max,
					"missing"=missing)
			
		}
		
		if(add.mad)
		{
			stat_list=c(stat_list,"mad"=mad)
		}
		
		
		# compute statistics
		
		stat=data.frame(by_GROUP %>% summarise_at(.funs=stat_list,.vars=y))
		
		# in case there are integers
		#we transform into numeric so that values can be in the proper format
		
		stat$mean=as.numeric(stat$mean)
		stat$sd=as.numeric(stat$sd)
		stat$median=as.numeric(stat$median)
		stat$q1=as.numeric(stat$q1)
		stat$q3=as.numeric(stat$q3)
		stat$min=as.numeric(stat$min)
		stat$max=as.numeric(stat$max)
	}
	
	
	
	if(!default.stat)
	{
		stat=paste0(substitute(func.stat))
		stat_list=as.formula(paste0("~",stat,"(",y,")"))
		
		# compute statistics
		
		stat=data.frame(by_GROUP %>% summarise_at(.funs=stat_list,.vars=y))
		
	}
	
	
	# Save raw output for graphics
	raw.stat=stat
	
	# format outputs	
	ind=which(sapply(stat,class)=="numeric") 	
	stat[ind]=format(round(stat[ind],round),nsmall=round,scientific=scientific,digits=digits)
	
	# Regroup stat
	
	if(default.stat)
	{
		stat$mean_sd=paste0(stat$mean,"(",stat$sd,")")
		stat$q1_q3=paste0("[",stat$q1,";",stat$q3,"]")
		stat$min_max=paste0("[",stat$min,";",stat$max,"]")
		
		if(add.mad)
		{
			stat$median_mad=paste0(stat$median,"(",stat$mad,")")
		}else
		{
			stat$median_mad=paste0(stat$median)
		}
		
		
		stat$mean=NULL
		stat$sd=NULL
		stat$median=NULL
		stat$mad=NULL
		stat$min=NULL
		stat$max=NULL
		stat$q1=NULL
		stat$q3=NULL
	}
	
	
	
	# reshape 
	
	m=melt(data=stat,id.vars=c(x1,x2),variable.name=stat.name,value.name ="value")
	
	if(!is.null(x1) & !is.null(x2))
	{
		stat2=dcast(m,as.formula(paste0(x2,"+",stat.name,"~",x1)),
				value.var="value")
	}
	
	if(!is.null(x1) & is.null(x2))
	{
		stat2=dcast(m,as.formula(paste0(stat.name,"~",x1)),
				value.var="value")
	}
	
	if(is.null(x1) & is.null(x2))
	{
		stat2=m
	}
	
	
	colnames(stat2)[colnames(stat2)=="value"]=y.label
	
	
	if(default.stat)
	{
		
		levels(stat2[,stat.name])[levels(stat2[,stat.name])=="N"]="N"
		if(geomean)
		{
			levels(stat2[,stat.name])[levels(stat2[,stat.name])=="mean_sd"]="Geo Mean (SD)"
		}else
		{
			levels(stat2[,stat.name])[levels(stat2[,stat.name])=="mean_sd"]="Mean (SD)"	
		}
		
		if(add.mad)
		{
			levels(stat2[,stat.name])[levels(stat2[,stat.name])=="median_mad"]="Median (MAD)"
		}else
		{
			levels(stat2[,stat.name])[levels(stat2[,stat.name])=="median_mad"]="Median"
		}
		
		
		levels(stat2[,stat.name])[levels(stat2[,stat.name])=="min_max"]="[Min;Max]"
		levels(stat2[,stat.name])[levels(stat2[,stat.name])=="missing"]="Missing"
		levels(stat2[,stat.name])[levels(stat2[,stat.name])=="q1_q3"]="[Q1;Q3]"
		
		if(!is.null(x2)) stat2=stat2[order(stat2[,x2],stat2[,stat.name]),]
		if(is.null(x2)) stat2=stat2[order(stat2[,stat.name]),]
		
	}
	
	
	if(!default.stat)
	{
		
		levels(stat2[,stat.name])[levels(stat2[,stat.name])==y]=func.stat.name
	}
	
# add Total, if requested
	
	if(total)
	{
		if(is.null(x2))
		{
			
			temp=report.quanti(data=data,y=y,y.label="Total",add.mad=add.mad,
					default.stat=default.stat,func.stat=func.stat,stat.name=stat.name,
					func.stat.name=func.stat.name,round=round,digits=digits,
					scientific=scientific)$output
			stat2=merge(stat2,temp,by=stat.name)
		}
		
		if(!is.null(x2))
		{
			temp=report.quanti(data=data,y=y,x1=x2,add.mad=add.mad,
					default.stat=default.stat,func.stat=func.stat,stat.name=stat.name,
					func.stat.name=func.stat.name,round=round,digits=digits,
					scientific=scientific)$output
			temp=melt(temp,id.vars=stat.name,variable.name=x2,value.name = "Total")
			stat2=merge(stat2,temp,by=c(x2,stat.name))
		}
		
	}
	
	
	if(default.stat)
	{
		if(add.mad)
		{
			if(geomean)
			{
				stat2[,stat.name]=factor(stat2[,stat.name],levels=c("N","Geo Mean (SD)","Median (MAD)",
								"[Q1;Q3]","[Min;Max]","Missing"))
			}else
			{
				stat2[,stat.name]=factor(stat2[,stat.name],levels=c("N","Mean (SD)","Median (MAD)",
								"[Q1;Q3]","[Min;Max]","Missing"))
			}
			
		}else
		{
			if(geomean)
			{
				stat2[,stat.name]=factor(stat2[,stat.name],levels=c("N","Geo Mean (SD)","Median",
								"[Q1;Q3]","[Min;Max]","Missing"))
			}else
			{
				stat2[,stat.name]=factor(stat2[,stat.name],levels=c("N","Mean (SD)","Median",
								"[Q1;Q3]","[Min;Max]","Missing"))
			}
		}
		
		if(!is.null(x2)) stat2=stat2[order(stat2[,x2],stat2[,stat.name]),]
		if(is.null(x2)) stat2=stat2[order(stat2[,stat.name]),]
	}
	
	
	if(!is.null(subjid))
	{
		if(!is.null(x1) & !is.null(x2))
		{
			
			if(!any("%in%"(colnames(data),subjid))) stop(paste0(subjid," variable is not in data colnames"))
			
			if(!total)
			{
				N=tapply(data[,subjid],data[,x1],function(x)length(unique(x)))
				colnames(stat2)[-c(1,2)]=paste0(colnames(stat2)[-c(1,2)]," (N=",N,")")
				
			}
			
			
			if(total)
			{
				N=tapply(data[,subjid],data[,x1],function(x)length(unique(x)))
				N=c(N,sum(N))
				colnames(stat2)[-c(1,2)]=paste0(colnames(stat2)[-c(1,2)]," (N=",N,")")
				
			}
		}
		
		if(!is.null(x1) & is.null(x2))
		{
			
			if(!any("%in%"(colnames(data),subjid))) stop(paste0(subjid," variable is not in data colnames"))
			
			if(!total)
			{
				N=tapply(data[,subjid],data[,x1],function(x)length(unique(x)))
				colnames(stat2)[-c(1)]=paste0(colnames(stat2)[-c(1)]," (N=",N,")")
				
			}
			
			
			if(total)
			{
				N=tapply(data[,subjid],data[,x1],function(x)length(unique(x)))
				N=c(N,sum(N))
				colnames(stat2)[-c(1)]=paste0(colnames(stat2)[-c(1)]," (N=",N,")")
				
			}
		}
		
		
		
	}
	
	# Spacing the results
	
	# check: si c'est mal renseigne on le met a null avec un message
	
	if(!is.null(at.row))
	{
		if(!any(colnames(stat2)==at.row)) 
		{
			message("at.row argument was not found in the colnames of the statistic table produced (probably mispelled)\n
							so it has been set to NULL")
			at.row=NULL
		}	
	}	
	
	if(!is.null(at.row))
	{
		lev=levels(stat2[,stat.name])
		stat2=spacetable(stat2,at.row=at.row)
		stat2[,stat.name]=factor(stat2[,stat.name],levels=c(lev,""))
	}
	
	
	if(is.null(at.row) & !is.null(x2))
	{
		at.row=x2
		lev=levels(stat2[,stat.name])
		stat2=spacetable(stat2,at.row=at.row)
		stat2[,stat.name]=factor(stat2[,stat.name],levels=c(lev,""))
	}
	
	# determination of the number of columns
	if(is.null(x2) )
	{
		nbcol=1
	}else
	{
		nbcol=2
	}
	

	
	
	title=paste0("Quantitative descriptive statistics of: ",y.label)
	
	stat2=ClinReport::desc(output=stat2,total=total,nbcol=nbcol,y=y,x1=x1,x2=x2,at.row=at.row,
			subjid=subjid,type.desc="quanti",type=NULL,y.label=y.label,
			raw.output=raw.stat,stat.name=stat.name,title=title)
	
	
	
	return(stat2)
}












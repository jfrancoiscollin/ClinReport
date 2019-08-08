# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

data(datafake)

desc=report.quanti(data=data,y="y_numeric",x1="GROUP",
x2="TIMEPOINT",total=TRUE,at.row="TIMEPOINT",subjid="SUBJID")
 
 # Print tab output
desc

transpose=function(desc)
	{
		
		output=desc$output
		at.row=desc$at.row
		x1=desc$x1
		x2=desc$x2
		nbcol=desc$nbcol
		type.desc= desc$type.desc
		
	   m=melt(output,measure.vars=colnames(output)[-c(1:nbcol)],
			   variable.name=x1)
	   
	   if(!is.null(at.row)) m=m[m$value!="",]
	   
	   if(type.desc=="quanti")
	   {
		   if(!is.null(x2)) form=as.formula(paste0(x2,"+",x1,"~Statistics"))
		   if(is.null(x2)) form=as.formula(paste0(x1,"~Statistics")) 
		   
		   output=dcast(m,form)
	   }

	   
       if(!is.null(at.row)) output=spacetable(output,at.row=at.row)
	   
	   output
	   
	}

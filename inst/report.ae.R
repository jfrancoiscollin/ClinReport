# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

library(reshape2)
library(ClinReport)


load("C:\\Users\\jfcollin\\git\\data\\data.RData")

# Calculation of at least one subject with an AE



at.least.one=function(data,subjid,var)
{

	t=table(data[,subjid],data[,var])
	
	at_least_one=apply(t,2,function(x)
			{
				length(x[x!=0])
			})
	
	
	at_least_one
}




at.least.one(data=data,subjid="SUBJID",var="AE_niv1")
at.least.one(data=data,subjid="SUBJID",var="AE_niv2")


transpose(report.quali(data=data,y="AE_niv1"))




# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

library(reshape2)
library(ClinReport)


data=read.csv2("C:\\Users\\jfcollin\\Google Drive\\ClinReport\\AE.csv")
data=data[c("SUBJID","ABON_PV","SOCNAME","PTNAME")]

data=droplevels(data[data$ABON_PV!="",])

stat=report.quali(data=data,y="PTNAME",x1="ABON_PV",x2="SOCNAME",
		subjid="SUBJID")

head(stat)

freq=stat$raw.output

freq$value=paste0(freq$Freq.x,freq$percent)
f=as.formula(paste0("Var1","+","Var2","~","Var3"))

freq=dcast(freq,f,value.var="value")
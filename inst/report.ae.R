# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

library(reshape2)
library(ClinReport)


data=read.csv2("C:\\Users\\jfcollin\\Google Drive\\ClinReport\\AE.csv")
data=data[c("SUBJID","ABON_PV","SOCNAME","PTNAME")]

data=droplevels(data[data$ABON_PV!="",])
#head(data)

# Construction of the AE table

split.soc=list()
for(i in 1:nrow(data))
{
	split.soc[[i]]=strsplit(as.character(data$SOCNAME[i]),"; ")[[1]]
}
names(split.soc)=data$SUBJID
split.soc=melt(split.soc)
colnames(split.soc)=c("SOCNAME","SUBJID")


split.pt=list()
for(i in 1:nrow(data))
{
	split.pt[[i]]=strsplit(as.character(data$PTNAME[i]),"; ")[[1]]
}
names(split.pt)=data$ABON_PV
split.pt=melt(split.pt)
colnames(split.pt)=c("PTNAME","ABON_PV")

AE=droplevels(data.frame(cbind(split.soc,split.pt)))

# check
#AE[AE$SUBJID=="SP-HEJA-1-5905",]
#head(AE)

# Stat reporting at AE level

stat=report.quali(data=AE,y="PTNAME",x1="ABON_PV",x2="SOCNAME",
		x2.label="System Organ Class",
		y.levels.label="Prefered Terms",
		at.row="System Organ Class",remove.zero=T)


report.doc(stat,title="Adverse Event reporting",
		colspan.value="Pharmaco Vigilance Clissification")


#TODO check why non missing in report.quali if remove.zero=T
# do we display the 0(0%) values?

# Calculation of at least one subject with an AE















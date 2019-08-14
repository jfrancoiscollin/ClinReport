## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = TRUE, message=FALSE, warning=FALSE--------------------------
library(ClinReport)
library(officer)
library(flextable)
library(dplyr)
library(reshape2)
library(nlme)
library(emmeans)
library(car)

## ---- include=TRUE-------------------------------------------------------
# We will use fake data
data(datafake)
print(head(datafake))

## ---- include=TRUE-------------------------------------------------------
tab1=report.quanti(data=datafake,y="y_numeric",
		x1="GROUP",x2="TIMEPOINT",at.row="TIMEPOINT",
		subjid="SUBJID")

tab1

## ---- include=TRUE-------------------------------------------------------
g1=plot(tab1,title="The title that you want to display")
print(g1)

## ---- include=TRUE-------------------------------------------------------
args(ClinReport:::plot.desc)

## ---- include=TRUE-------------------------------------------------------
report.doc(tab1,title="Quantitative statistics (2 explicative variables)",
		colspan.value="Treatment group", init.numbering =T )			

## ------------------------------------------------------------------------
doc=read_docx()
doc=report.doc(tab1,title="Quantitative statistics (2 explicative variables)",
		colspan.value="Treatment group",doc=doc,init.numbering=T)
doc=body_add_gg(doc, value = g1, style = "centered" )

## ----results='hide'------------------------------------------------------
file=paste(tempfile(),".docx",sep="")
print(doc, target =file)

#Open it
#shell.exec(file)

## ------------------------------------------------------------------------
tab=report.quali(data=datafake,y="y_logistic",
		x1="VAR",total=T,subjid="SUBJID")
		
report.doc(tab,title="Qualitative table with two variables",
colspan.value="A variable")	


## ------------------------------------------------------------------------
tab=report.quali(data=datafake,y="y_logistic",
		x1="GROUP",x2="TIMEPOINT",at.row="TIMEPOINT",
		total=T,subjid="SUBJID")
		
report.doc(tab,title="Qualitative table with two variables",
colspan.value="Treatment group")	


## ------------------------------------------------------------------------
tab=report.quanti(data=datafake,y="y_numeric",
		x1="VAR",total=T,subjid="SUBJID")
		
report.doc(tab,title="Quantitative table with one explicative variable",
colspan.value="A variable")	


## ------------------------------------------------------------------------
tab=report.quanti(data=datafake,y="y_numeric",
		x1="GROUP",x2="TIMEPOINT",at.row="TIMEPOINT",
		total=T,subjid="SUBJID")
		
report.doc(tab,title="Quantitative table with two explicative variables",
colspan.value="Treatment group")	

## ------------------------------------------------------------------------
tab1=report.quanti(data=datafake,y="y_numeric",
		x1="GROUP",subjid="SUBJID",y.label="Y numeric")

tab2=report.quali(data=datafake,y="y_logistic",
		x1="GROUP",subjid="SUBJID",y.label="Y logistic")

tab3=regroup(tab1,tab2,rbind.label="The label of your choice")


report.doc(tab3,title="Mixed Qualitative and Quantitative outputs",
colspan.value="Treatment group")

## ------------------------------------------------------------------------
# We use a fake standard adverse event data set
# In this data sets there are several observations per subject
# and the factor PTNAME is a sub classification of the factor SOCNAME

data(adverse_event)

# In the report.quali.hlev we specify which factor has the more levels in the var_upper
# argument. The var_lower argument indicates the classification with less levels.
# The x1 argument is used to split the results according to the levels of another factor.

test=report.quali.hlev(data=adverse_event,subjid="SUBJID",var_upper="PTNAME",
var_lower="SOCNAME",lower.levels="System Organ Class",upper.levels="Prefered Terms",x1="randtrt")

# Frequencies and Percentages for each level are shown in the 
# formatted table in HTML, using the usual report.doc function

ft=report.doc(test,valign=TRUE)
ft

## ------------------------------------------------------------------------
# Removing baseline data for the model

data.mod=droplevels(datafake[datafake$TIMEPOINT!="D0",])

mod=lme(y_numeric~baseline+GROUP+TIMEPOINT+GROUP*TIMEPOINT,
random=~1|SUBJID,data=data.mod,na.action=na.omit)
 
report.modelinfo(mod)

## ------------------------------------------------------------------------
# Removing baseline data for the model

anov3=Anova(mod,3)

# Make pretty names for the table

rownames(anov3)=make.label(rownames(anov3),
list(c("GROUP","Treatment"),
c("TIMEPOINT","Visit"),
c("Treatment:Visit","Interaction Treatment-Visit")))

report.doc(anov3,title="Mixed Qualitative and Quantitative output")

## ------------------------------------------------------------------------
lsm=emmeans(mod,~GROUP|TIMEPOINT)

tab=report.lsmeans(lsm,at.row="TIMEPOINT")

report.doc(tab,title="LS-Means example",
colspan.value="Treatment Group")

## ------------------------------------------------------------------------
contr=contrast(lsm, "trt.vs.ctrl", ref = "A")

# There is just only one explicative variable

tab.contr=report.lsmeans(lsm=contr,at="TIMEPOINT")
		
		
report.doc(tab.contr,title="LS-Means contrast example",
colspan.value="Contrasts")		

## ------------------------------------------------------------------------
library(survival)
 
data(time_to_cure)
 
fit <- coxph(Surv(time, status) ~ Group, data = time_to_cure) 
em=emmeans(fit,~Group,type="response")
pairs=pairs(em,adjust="none",exclude="Untreated")
tab.pairs=report.lsmeans(pairs)

tab.pairs

report.doc(tab.pairs,title="Hazard ratios of a Cox model")


## ------------------------------------------------------------------------
report.sessionInfo()


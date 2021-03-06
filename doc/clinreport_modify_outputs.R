## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = TRUE, message=FALSE, warning=FALSE-------------------------------
library(ClinReport)
library(officer)
library(flextable)
library(emmeans)

## ---- include=TRUE------------------------------------------------------------
# We will use fake data
data(datafake)
print(head(datafake))

## -----------------------------------------------------------------------------
tab1=report.quanti(data=datafake,y="y_numeric",
		x1="GROUP",subjid="SUBJID",y.label="Y numeric")

tab2=report.quali(data=datafake,y="y_logistic",
		x1="GROUP",subjid="SUBJID",y.label="Y logistic")

tab3=regroup(tab1,tab2,rbind.label="The label of your choice")


report.doc(tab3,title="Mixed Qualitative and Quantitative output",
colspan.value="Treatment group")

## -----------------------------------------------------------------------------
es=function(x) mean(x,na.rm=T)/sd(x,na.rm=T) 

tab=report.quanti(data=datafake,y="y_numeric",x1="GROUP",
total=TRUE,subjid="SUBJID",
func.stat=es,
func.stat.name="Effect size")

report.doc(tab,title="Example of a specific statistic reporting",
colspan.value="Treatment group")

## -----------------------------------------------------------------------------
# The default statistics are given here:
 
tab1=report.quanti(data=datafake,y="y_numeric",x1="GROUP",total=TRUE,subjid="SUBJID")

 # Define the function corresponding to the coefficient of variation for example
 
cv=function(y) sd(y,na.rm=TRUE)/mean(y,na.rm=TRUE)
 
 # We use the add.stat function to add CV at the second row:
 
tab1.cv=add.stat(tab1,datafake,func.stat=cv,func.stat.name="Coef. Var",
 pos=2)

report.doc(tab1.cv,title="Example of adding a coefficient of variation")

 # Same with 2 explicative variables
 
tab=report.quanti(data=datafake,y="y_numeric",x1="GROUP",
 x2="TIMEPOINT",total=TRUE,subjid="SUBJID",
		at.row="TIMEPOINT")
 
 tab=add.stat(tab,datafake,func.stat=cv,func.stat.name="Coef. Var",
 pos=2)

 
 # And on position 5, we can add for example the mode
 
 mode=function(x)
 {
   x=na.omit(x)
   ux <- unique(x)
   ux[which.max(tabulate(match(x, ux)))]
 }
 
 
 tab=add.stat(tab,datafake,func.stat=mode,func.stat.name="Mode",pos=5)
 
 report.doc(tab,title="Example of adding 2 more statistics in an existing table",
 colspan.value="Treatment Group")


## -----------------------------------------------------------------------------
tab=report.quali(data=datafake,y="y_logistic",x1="GROUP",
total=TRUE,subjid="SUBJID",percent.col=FALSE)

report.doc(tab,title="Example of row percentage reporting",
colspan.value="Treatment group")

## -----------------------------------------------------------------------------
tab=report.quali(data=datafake,y="y_logistic",x1="GROUP",
subjid="SUBJID",drop.x1=c("B","C"))

report.doc(tab,title="Example of row percentage reporting",
colspan.value="Treatment group")

## -----------------------------------------------------------------------------
tab=report.quali(data=datafake,y="y_logistic",x1="GROUP",
remove.missing=TRUE)

report.doc(tab,title="Example of dropping missing values",
colspan.value="Treatment group")

## -----------------------------------------------------------------------------

tab=report.quali(data=datafake,
y="y_logistic",x1="GROUP",
subjid="SUBJID",remove.missing=T)

# The default output
report.doc(tab)

# The transposed output
report.doc(transpose(tab))


## -----------------------------------------------------------------------------
mod=lm(y_numeric~GROUP,data=datafake)
pairs=pairs(emmeans(mod,~GROUP))

tab=report.lsmeans(pairs,transpose=TRUE)

report.doc(tab,title="Example of transposing LS-Means in column",
colspan.value="Treatment group")


## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(ClinReport)
library(officer)
library(flextable)
library(dplyr)
library(reshape2)
library(nlme)
library(emmeans)

## ---- include=T----------------------------------------------------------
# We will use fake date
data(data)
print(head(data))

## ---- include=TRUE-------------------------------------------------------
tab1=report.quanti(data=data,y="y_numeric",
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
		colspan.value="Treatment group")			

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


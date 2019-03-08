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
data(data)
print(head(data))

## ------------------------------------------------------------------------
# Only one numerical response

tab1=report.quanti(data=data,y="y_numeric")
plot(tab1,title="Mean (+/-SD) of y",add.sd=T,ylab="Mean Response value",xlab="")

## ------------------------------------------------------------------------
#  one numerical response ; one explicative variable

tab2=report.quanti(data=data,y="y_numeric",x1="GROUP")
plot(tab2,title="Mean (+/-SD) of y per treatment group",add.sd=T,ylab="Mean of y",xlab="",
ylim=c(-1,8))

## ------------------------------------------------------------------------
#  one numerical response ; two explicative variable

tab3=report.quanti(data=data,y="y_numeric",x1="GROUP",x2="TIMEPOINT")
plot(tab3,title="Mean (+/-SD) of y per treatment group",add.sd=T,ylab="Mean of y",xlab="",
ylim=c(-1,8))


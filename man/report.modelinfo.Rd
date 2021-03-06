% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report.modelinfo.R
\name{report.modelinfo}
\alias{report.modelinfo}
\alias{report.modelinfo.lme}
\alias{report.modelinfo.lmerMod}
\alias{report.modelinfo.glm}
\alias{report.modelinfo.coxph}
\title{Export model information (still experimental)}
\usage{
report.modelinfo(object, ...)

\method{report.modelinfo}{lme}(object, doc = NULL, page.break = TRUE,
  ...)

\method{report.modelinfo}{lmerMod}(object, doc = NULL,
  page.break = TRUE, ...)

\method{report.modelinfo}{glm}(object, doc = NULL, page.break = TRUE,
  ...)

\method{report.modelinfo}{coxph}(object, doc = NULL, page.break = TRUE,
  ...)
}
\arguments{
\item{object}{A model (for now glm, lme, lmer and coxph models are availlable)}

\item{...}{Other arguments}

\item{doc}{NULL or a rdocx object}

\item{page.break}{Logical. If TRUE it adds a page break after the output. Default to TRUE}
}
\value{
A flextable object (if doc=NULL) or a rdocx object (if doc= an rdocx object).
}
\description{
\code{report.doc} 
This function enables to export the information of the model (the package, the name of the function,
the call etc...)
}
\details{
Compatible only (for now) with GLM, LME and Cox models. For now those output are not numbered.
}
\examples{

library(officer)
library(flextable)
library(nlme)
library(lme4)

data(datafake)

#Model info for lme model

mod=lme(y_numeric~GROUP+TIMEPOINT,random=~1|SUBJID,data=datafake,na.action=na.omit)

# Show in HTML (can be inserted in an R markdown or a MS Word document)

report.modelinfo(mod)

#Model info for lmer model

mod=lmer(y_numeric~GROUP+TIMEPOINT+(1|SUBJID),data=datafake,na.action=na.omit)

report.modelinfo(mod)

}
\seealso{
\code{\link{report.doc}}
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg_desc_quanti.R
\name{gg_desc_quanti}
\alias{gg_desc_quanti}
\title{Creates a ggplot object corresponding to a quantitative desc object}
\usage{
gg_desc_quanti(desc, title = "", ylim = NULL, xlim = NULL,
  xlab = "", ylab = "", legend.label = "Group", add.sd = F)
}
\arguments{
\item{desc}{Desc object}

\item{title}{Character The title of the plot}

\item{ylim}{Numeric of length 2 for setting y axis limits}

\item{xlim}{Numeric of length 2 for setting x axis limits}

\item{xlab}{Character Label for x-axis}

\item{ylab}{Character Label for y-axis}

\item{legend.label}{Character Label for the legend (used only if x1 and x2 are not NULL in the desc object)}

\item{add.sd}{Logical. If TRUE it adds bars to the means representing +/-SD}
}
\value{
A ggplot object.
}
\description{
\code{gg_desc_quanti} 
ggplot object is printed
}
\details{
It is used internally in function \code{\link{plot.desc}}.
It's easier to use this last one.
}
\examples{
 \dontshow{
data(datafake)

desc=report.quanti(data=datafake,y="y_numeric",x1="GROUP",
	x2="TIMEPOINT")

ClinReport:::gg_desc_quanti(desc,title="My title",ylab="Hello dear",
	ylim=c(-2,8),xlab="Great!",legend.label="Treatment group")

# With SD

ClinReport:::gg_desc_quanti(desc,title="My title",ylab="Hello dear",
	ylim=c(-2,8),xlab="Great!",legend.label="Treatment group",add.sd=TRUE)


desc2=report.quanti(data=datafake,y="y_numeric",x1="GROUP")

desc3=report.quanti(data=datafake,y="y_numeric")



ClinReport:::gg_desc_quanti(desc2,title="My title",ylab="Hello dear")


ClinReport:::gg_desc_quanti(desc3,title="My title",ylab="Hello dear")
}

}
\seealso{
\code{\link{plot.desc}} \code{\link{desc}} \code{\link{gg_desc_quali}} \code{\link{gg_desc_lsmeans}}
}

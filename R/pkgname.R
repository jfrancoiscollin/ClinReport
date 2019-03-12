# TODO: Add comment
# 
# Author: jfcollin
###############################################################################


#' R documentation for ClinReport package
#' 
#' It enables to create easily a statistical reporting in Microsoft Word documents with tables in a pretty format according to "clinical standards".
#' 
#' It can also be used outside the scope of clinical trials, for any statistical reporting in Word. 
#' 
#' Descriptive tables for quantitative statistics (mean, median, max etc..) and qualitative statistics (frequencies and percentages) 
#' are available and pretty tables of Least Square Means of LM, LME and GLM models coming from \code{emmeans} function in emmeans package are 
#' also available.
#' 
#' The package works with officer and flextable packages to export the outputs
#' into Microsoft Word documents.   
#' 
#' There are two main functions for descriptive reporting : \code{report.quanti} and \code{report.quali}
#' 
#' There is one unique function for LS means reporting: \code{report.lsmeans}
#'  
#' Each of those 3 functions creates a desc object that can be used with \code{report.doc} to export the result(s) 
#' into a Word document. This is the unique function to export the output(s) into a Word document.
#' 
#' Future versions will improve the "standard" graphic reporting, 
#' the anova table reporting and statistics tables specific for Adverse Events reporting. Survival models will also be included. 
#'     
#' You can visit the website for more exmaples: \href{https://jfrancoiscollin.github.io/ClinReport}{ClinReport website}
#' 
#' @author Jean-Francois Collin, \email{jfcollin@@live.fr}
#' @docType package
#' @name ClinReport
NULL

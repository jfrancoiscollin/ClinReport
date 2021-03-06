# TODO: Add comment
# 
# Author: jfcollin
###############################################################################


#' R documentation for ClinReport package
#' 
#' The aim is to create easily a statistical reporting in Microsoft Word documents with tables in a pretty format according to "clinical standards".
#' 
#' It can also be used outside the scope of clinical trials, for any statistical reporting in Word. 
#' 
#' Descriptive tables for quantitative statistics (mean, median, max etc..) and qualitative statistics (frequencies and percentages) 
#' are available and pretty tables of Least Square Means of LM, LME, Cox and GLM models coming from \code{emmeans} function in emmeans package are 
#' also available.
#' 
#' The package works with officer and flextable packages to export the outputs
#' into Microsoft Word documents.   
#' 
#' There are two main functions for getting descriptive statistics : \code{report.quanti} and \code{report.quali}
#' 
#' There is one unique function for prettyfying LS means: \code{report.lsmeans}
#'  
#' Each of those 3 functions creates a desc object that can be used with \code{report.doc} to export the results.
#' This is the unique function to export the outputs into a Word or an R markdown document.
#' 
#' Future versions will improve the "standard" graphic reporting and specific statistics tables used for reporting Adverse Events.
#'     
#' You can visit the website for more examples: \href{https://jfrancoiscollin.github.io/ClinReport}{ClinReport website}
#' 
#' @author Jean-Francois Collin, \email{jfcollin@@live.fr}
#' @docType package
#' @name ClinReport
NULL

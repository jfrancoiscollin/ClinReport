# TODO: Add comment
# 
# Author: jfcollin
###############################################################################


#' Fake clinical data example
#'
#' Fictive data created only for the purpose of testing the package and showing examples.
#'
#' @format A data frame
#' \describe{
#'   \item{y_numeric}{Fake numerical response}
#'   \item{y_logistic}{Fake Logistic response}
#'   \item{y_poisson}{Fake Poisson response}
#'   \item{baseline}{A baseline covariate}
#'   \item{VAR}{A factor}
#'   \item{GROUP}{A fake treatment factor}
#'   \item{TIMEPOINT}{A fake time factor}
#'   \item{SUBJID}{A fake id factor}
#' }
#' 
#' 
#' @docType data
#'
#' @keywords datasets
#' 
#' @examples
#' data(data)
#' \donttest{head(data)}
"data"




#' Fake survival data set
#'
#' Simulated survival data created only for the purpose of testing the package and showing examples.
#'
#' @format A data frame
#' 
#' @docType data
#' 
#' @keywords datasets
#' 
#' @examples
#' data(time_to_cure)
#' \donttest{head(time_to_cure)}
"time_to_cure"
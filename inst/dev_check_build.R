# TODO: Add comment
# 
# Author: jfcollin
###############################################################################


#Sys.setenv(PATH = paste(Sys.getenv("PATH"),
# "C:/Users/jfcollin/AppData/Local/Programs/MiKTeX 2.9/miktex/bin/x64",
# sep=.Platform$path.sep))
#
#

Sys.setenv(PATH = paste(Sys.getenv("PATH"),
				"C:/Users/jfcollin/AppData/Local/Pandoc",
				sep=.Platform$path.sep))

Sys.setenv(PATH = paste(Sys.getenv("PATH"),
				"D:/qpdf-8.4.0/bin",
				sep=.Platform$path.sep))

#


library(devtools)
library(roxygen2)
library(usethis)
library(rmarkdown)

setwd("C:\\Users\\jfcollin\\git")



###############################################
#  readme
###############################################

render("C:\\Users\\jfcollin\\git\\inst\\README.Rmd")
#shell.exec("C:\\Users\\jfcollin\\git\\ClinReport\\README.html")


# Vignette

render("C:\\Users\\jfcollin\\git\\vignettes\\clinreport-vignette.Rmd")
#shell.exec("C:\\Users\\jfcollin\\git\\vignettes\\clinreport-vignette.html")







devtools::build_vignettes()
roxygenize("C:\\Users\\jfcollin\\git")


check(args ="--as-cran")


install(build_vignettes =T)
build(path = "C:\\Users\\jfcollin\\Google Drive\\R packages")












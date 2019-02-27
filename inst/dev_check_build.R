# TODO: Add comment
# 
# Author: jfcollin
###############################################################################


#Sys.setenv(PATH = paste(Sys.getenv("PATH"),
# "C:/Users/jfcollin/AppData/Local/Programs/MiKTeX 2.9/miktex/bin/x64",
# sep=.Platform$path.sep))
#
#
#Sys.setenv(PATH = paste(Sys.getenv("PATH"),
#				"C:/Users/jfcollin/AppData/Local/Pandoc",
#				sep=.Platform$path.sep))
#


library(devtools)
library(roxygen2)
library(usethis)
library(rmarkdown)

setwd("C:\\Users\\jfcollin\\git\\ClinReport")

roxygenize("C:\\Users\\jfcollin\\git\\ClinReport")

check(args ="--as-cran")


install()
build()




###############################################
#  readme
###############################################

render("C:\\Users\\jfcollin\\git\\ClinReport\\inst\\README.Rmd")
#shell.exec("C:\\Users\\jfcollin\\git\\ClinReport\\README.html")













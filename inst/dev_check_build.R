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

#use_cran_badge()
#use_lifecycle_badge("stable")

render("C:\\Users\\jfcollin\\git\\README.Rmd")
#shell.exec("C:\\Users\\jfcollin\\git\\README.html")

###############################################
# Vignette
###############################################

render("C:\\Users\\jfcollin\\git\\vignettes\\clinreport_vignette_get_started.Rmd")
render("C:\\Users\\jfcollin\\git\\vignettes\\clinreport_modify_outputs.Rmd")
render("C:\\Users\\jfcollin\\git\\vignettes\\clinreport_graphics.Rmd")

#shell.exec("C:\\Users\\jfcollin\\git\\vignettes\\clinreport-vignette.html")
#shell.exec("C:\\Users\\jfcollin\\git\\vignettes\\clinreport_modify_outputs.html")
#shell.exec("C:\\Users\\jfcollin\\git\\vignettes\\clinreport_graphics.html")


devtools::build_vignettes()

###############################################
# Check install and build
###############################################



roxygenize("C:\\Users\\jfcollin\\git")


#usethis::use_build_ignore("docs")

check(args ="--as-cran")


#install()
install(build_vignettes =T)
build(path = "C:\\Users\\jfcollin\\Google Drive\\R packages")




###############################################
# Website
###############################################

## create _pkgdown.yml to customize the website
#usethis::use_pkgdown()
#
## show a template for all functions to reference
#pkgdown::template_reference()
#
## To create the corresponding web page
#pkgdown::build_reference()
#
## show a template for the vignette
#pkgdown::template_articles()
#
## To create the corresponding web page
#pkgdown::build_articles()
#
#
## Navigation bar
#pkgdown::template_navbar()


# Build the site
pkgdown::build_site()




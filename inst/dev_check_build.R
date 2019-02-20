# TODO: Add comment
# 
# Author: jfcollin
###############################################################################


#Sys.setenv(PATH = paste(Sys.getenv("PATH"),
# "C:/Users/jfcollin/AppData/Local/Programs/MiKTeX 2.9/miktex/bin/x64",
# sep=.Platform$path.sep))


library(devtools)
library(roxygen2)
library(usethis)

setwd("C:\\Users\\jfcollin\\Google Drive\\Dev\\ClinReport")

roxygenize("C:\\Users\\jfcollin\\Google Drive\\Dev\\ClinReport")

check(args ="--as-cran")

#usethis::use_readme_rmd()
#usethis::use_news_md()

install()
build()



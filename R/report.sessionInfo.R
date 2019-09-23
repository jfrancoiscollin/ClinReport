# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

#' Export the table of the sessionInfo() to Word or R markdown documents


#' @param doc NULL or a rdocx object' 
#' 
#' @description
#' \code{report.sessionInfo}
#' 
#' This function enables to export the table of the \code{\link{sessionInfo}}
#' 
#' @details
#' None
#' 
#' @return  
#' A flextable object (if doc=NULL) or a rdocx object (if doc= an rdocx object).
#' 
#' @seealso \code{\link{report.doc}} \code{\link{report.modelinfo}} 
#' 
#' @examples
#' 
#' # For an R markdown document just do
#' report.sessionInfo()
#' 
#' # Fro a Word document use the doc argument
#' library(officer)
#' doc=read_docx()
#' doc=report.sessionInfo(doc)
#' 
#' @import utils officer flextable
#' 
#' @export




report.sessionInfo=function(doc=NULL)
{
	

mkLabel <- function(L, n) {
	vers <- sapply(L[[n]], function(x) x[["Version"]])
	pkg <- sapply(L[[n]], function(x) x[["Package"]])
	paste(pkg, vers, sep = "_")
}



s=sessionInfo()



version=s$R.version$version.string
platform=s$platform
osversion=s$running
locale=strsplit(Sys.getlocale(),";")[[1]]
matrix=paste("Matrix products: ", s$matprod, "\n", sep = "")
basepkg=s$basePkgs
otherpkg=mkLabel(s, "otherPkgs")
loaded=mkLabel(s, "loadedOnly")

lab_r="R version:"
lab_platform="Platform:"
lab_osversion="Running under:"
lab_loc=rep("Locale:",length(locale))
lab_matrix="Matrix:"
lab_attach=rep("Attached base packages:",length(basepkg))
lab_other=rep("Other attached packages:",length(otherpkg))
lab_loaded=rep("Loaded via a namespace (and not attached):",length(loaded))



val=c(version,platform,
		osversion,locale,matrix,
		basepkg,
		otherpkg,
		loaded)

var=c(lab_r,
		lab_platform,
		lab_osversion,
		lab_loc,
		lab_matrix,
		lab_attach,
		lab_other,
		lab_loaded)



d=data.frame(Label=var,
		Information=val)


d=spacetable(d,"Label")

ft=flextable(d)
ft=merge_v(ft,1)

ft <- bold(ft, part = "header")
ft <- bg(ft,i=1,bg="#DCDCDC", part = "header")


ft=valign(ft, j = 1, valign = "top", part = "body")

ft=autofit(ft)

if(!is.null(doc))
{	
	if(class(doc)!="rdocx") stop("doc must be a rdocx object")	
	doc <- body_add_par(doc,"", style = "Normal")
	doc <- body_add_flextable(doc, value = ft)		
	
	
	return(doc)
	
}else
{
	return(ft)
}


}



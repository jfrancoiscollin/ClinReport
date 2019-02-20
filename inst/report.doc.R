###############################################################################
# Author: jfcollin
###############################################################################

#' Creates a FlexTable object to export to Word (how nice? :))
#'
#' @param table a data.frame object that report statistics per group (typically treatment groups).
#' @param title the title of the table
#' @param colspan.value a character vector of length 1. Add the label of the groups (typically "Treatment Groups").
#' @param nb.col A numerical values. It indicates the number of groups, not counting the column Total.
#' @param total Boolean. If TRUE, it adjust the label of the colspan.value so that the total column is separated.
#' @param doc NULL or a docx object
#' @param init.numbering Boolean. If TRUE Start numbering of the output at 1, otherwise it increase the output numbering of 1 unit
#' @description
#' \code{report.doc} 
#' Export the table to MS word with a nice format (according to the author's standards, of course...).
#' 
#' @details
#' It creates a FlexTable objects.
#' @return  
#' A FlexTable objects
#' 
#' @seealso \code{\link{report.quali}}
#' @examples
#' \dontrun{
#' doc=docx()
#' data(data)
#' tab=report.quanti(data=data,y="DEMEANOUR_num",
#' x1="GROUP",x2="TIMEPOINT",at.row="TIMEPOINT")
#' 
#' tab2=report.quanti(data=data,y="DEMEANOUR_num",
#' x1="GROUP",x2="TIMEPOINT",at.row="TIMEPOINT",total=T)
#' 
#' # Initialize the numbering
#' luke=report.doc(table=tab2,title="Table example",
#' colspan.value="Treatment Groups",nb.col=3,total=T,init.numbering=T)
#' 
#' # The numbering is increased automatically 
#' c3po=report.doc(table=tab,title="Table example",
#' colspan.value="Treatment Groups",nb.col=3)
#' r2d2=report.doc(table=tab2,title="Table example",
#' colspan.value="Treatment Groups",nb.col=3,total=T)

#' 
#' #Add flex table to the doc
#' 
#' doc=addFlexTable(doc,c3po)
#' doc=addFlexTable(doc,r2d2)
#' file=paste(tempfile(),".docx",sep="")
#' writeDoc(doc,file)
#' shell.exec(file)
#' 
#' 
#' 
#' }
#' 
#' @import ReporteRs utils
#' 
#' @export

report.doc=function(table,title,colspan.value="",
		nb.col,total=F,doc=NULL,init.numbering=F)
{
	
	if(init.numbering)
	{
		
		if(getRversion() >= "2.15.1")  utils::globalVariables(c(".output"))
		
		if(!exists(".output",envir =.GlobalEnv))
		{
			.output=new.env() 
			assign(".output",.output, envir = .GlobalEnv)
		}else
		{
			if(is.environment(get(".output",envir=.GlobalEnv)))
			{
				assign("number", 1, envir = .GlobalEnv$.output)
			}else
			{
				stop(".output object already exists in the Global Environment. Please delete it so we can start numbering properly like civilized persons do")
			}
		}
		
	}
	
	bp_none=borderProperties(style="none")
	
	
	table.report.ft=FlexTable(table, header.columns = FALSE,
			body.text.props = textProperties(font.size = 10),
			body.cell.props = cellProperties(border.style="none"))

	table.report.ft = addHeaderRow(table.report.ft, 
			value = paste0("Output ",get("number",envir=.GlobalEnv$.output),": ",c(title)), 
			colspan =ncol(table),
			cell.properties=cellProperties(background.color ="#C0C0C0" ,
					border.left=bp_none,
					border.right=bp_none))
	
	
	assign("number",get("number",envir=.GlobalEnv$.output)+1, envir = .GlobalEnv$.output)
		
		if(total)
		{	
			nb.id.col=length(colnames(table))-(nb.col+1)
			ind=(nb.id.col+nb.col-1)
			colspan=c(rep(1,nb.id.col),nb.col,1)
			colspan.value=c(rep("",nb.id.col),colspan.value,"")
		}else
		{
			nb.id.col=length(colnames(table))-nb.col
			ind=(nb.id.col+nb.col)
			colspan=c(rep(1,nb.id.col),nb.col)
			colspan.value=c(rep("",nb.id.col),colspan.value)
		}
		
		table.report.ft = addHeaderRow(table.report.ft, 
				value = colspan.value, 
				colspan =colspan,
				cell.properties=cellProperties(border.left=bp_none,
						border.right=bp_none,
						border.bottom=bp_none),
				par.properties=parProperties(text.align = "center"))
		
		table.report.ft[,((nb.col+1):ind),
				to="header"]=cellProperties(
						border.left=bp_none,
						border.right=bp_none)
				
	table.report.ft = addHeaderRow(table.report.ft, 
			value =colnames(table),
			cell.properties=cellProperties(border.left=bp_none,
					border.right=bp_none,
					border.top=bp_none),
			par.properties=parProperties(text.align = "center"))
	
	
	table.report.ft = addHeaderRow(table.report.ft, 
			value = "", 
			colspan =ncol(table),
			cell.properties=cellProperties(border.top=bp_none,
					border.bottom=bp_none,
					border.left=bp_none,
					border.right=bp_none))
	
	table.report.ft =addFooterRow( table.report.ft,value="",
			colspan=ncol(table),cell.properties=cellProperties(
					border.top=bp_none,
					border.left=bp_none,
					border.right=bp_none))
	
	if(nb.col>1)
	{
		table.report.ft[,1]= textProperties(font.weight ="bold")
	}
	
	if(nb.col>=2)
	{
		table.report.ft[,2]=cell.properties=cellProperties(border.right=
						borderProperties(style="solid"),
				border.bottom=bp_none,
				border.bottom.style = "none",
				border.top=bp_none,
				border.top.style = "none",
				border.left=borderProperties(style="solid"))
	}
	
	table.report.ft[,(nb.col+1):(nb.col+nb.id.col)]=parProperties(text.align = "center")
	table.report.ft[1, , to = "header"]=cellProperties(background.color ="#C0C0C0" )
	
	if(!is.null(doc))
	{
		
		doc=addParagraph(doc,"")
		return(addFlexTable(doc,table.report.ft,
						par.properties = parProperties(text.align = "center")))
	}else
	{
		return(table.report.ft)
	}
	
}




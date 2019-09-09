# TODO: Add comment
# 
# Author: jfcollin
###############################################################################



# find the template
library(officer)

path=find.package("ClinReport", quiet=TRUE, verbose=TRUE)
path=paste0(path,"/template.docx")
doc=read_docx(path)


doc=headers_replace_text_at_bkm(doc, bookmark="header_bkm", value="Statistical report")
doc=body_replace_text_at_bkm(doc, bookmark="name_study_bkm", value="Jeff")

target = tempfile(fileext = ".docx")

print(doc,target =target)


shell.exec(target)



# ClinReport 0.9.1.6

## Enhancement

* Deprecated arguments (x1.name, x2.name, x3.name) in report.lsmeans function
it's now replaced by x1, x2 and x3 so it's consistent with report.quanti and report.quali

* It's now possible to export anova tables in report.doc function (see examples in report.doc)

* It's now possible to export a ggplot corresponding to a desc object.
It can be a quali, quanti or lsmeans desc object so the corresponding graphics are barplot or
lineplot with or without confidence intervals (see examples in report.doc function).

## Issues

* The code has been cleaned (no more eval(parse(text=...)) ) 
* Suppressed some recursive calls





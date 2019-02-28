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


# ClinReport 0.9.1.7

## Enhancement

* Add the font.size option



# ClinReport 0.9.1.8

## Issues

* Corrected the code of report.lsmeans which reported systematically in response values the lsmeans of GLM models

## Enhancement

* Add the add.mad option in report.quanti so the mad is not systematically reported


# ClinReport 0.9.1.9


## Issues

* Correction of nbcol argument when x2=NULL and x1!=NULL in report.quali function

## Enhancement

* function report.quanti: stop using group_by_: replaced by group_by
* function report.quanti: remove the funs_ in summarise_at
* Add a vignette in the package
* Add a footer_row and hline to all report.doc outputs




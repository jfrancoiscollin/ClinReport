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
* Correction of a footenote issue for ls-means reporting of GLM model

## Enhancement

* function report.quanti: stop using group_by_: replaced by group_by
* function report.quanti: remove the funs_ in summarise_at
* Add a vignette in the package
* Add a footer_row and hline to all report.doc outputs



# ClinReport 0.9.1.10


## Issues

* correction of report.lsmeans: always updated the results to type="response"

## Enhancement

* check in report.quanti and report.quali if the variables are factors or numeric and
return a message if they are not (eventually transform character variables into factors)


# ClinReport 0.9.1.11


## Enhancement

* Add the add.stat function to add a specific statistic in report.quanti function
* Improve the regroup function to regroup two quantitative desc object
* Improve the report.quanti function so it's now possible to not systematically have the default statistics.
The user can now implement it's own statistics using argument func.stat and func.stat.name.
* Complete the vignettes

# ClinReport 0.9.1.12


## Issues

* correction of add.stat: when subjid=NULL add.stat didn't work
* correction in report.quanti: if default quantitative stat were integer, the correct number of digits was not homogeneous
* correction in report.quanti: total column was not rounded properly, it was always 2 digits
* correction: spacetable function, reordered lexically the at.row factor: corrected      

## Enhancement

* Can regroup more than 2 desc object with the function regroup
* the vertical lines are splitted in report.doc if the at.row argument is filled
it adds more readibility to the output
* the horizontal line under the colspan value underline only the levels of colspan levels
it adds more readibility to the doc


# ClinReport 0.9.1.13

## Enhancement

* The reporting of LS-Means has been much simplified so now most of the old arguments are deprecated
* The reporting of Cox model hazard ratios has been added to the report.lsmeans function
* The print of the tables has been improved
* Minor bugs have been corrected 



# ClinReport 0.9.1.14

## Enhancement

* Add drop.x1 and drop.x2 arguments in report.quanti and report.quali to drop levels from the results if needed
* Add the transpose argument in report.lsmeans function to show the statistics in columns or in rows
* Add the report.modelinfo function, to export in a table the information relative to a statistical model
(the call to the model, the package name, the function name, the AIC, BIC, number of observation etc...)
* If x2 argument is not missing then at.row argument automatically will space the results by row, no need to fill this
argument systematically anymore
* The title is automatically filled with y.label argument









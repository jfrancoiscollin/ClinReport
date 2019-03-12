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













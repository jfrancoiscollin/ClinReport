ClinReport R package
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

The aim of ClinReport is to give to R users the possibility to create
nice statistical reporting outputs (tables and graphics) in ‘Microsoft
Word’.

## Installation

You can install the released version of ClinReport from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ClinReport")
```

## Basic example code

``` r

library(ClinReport)
library(officer)
library(flextable)

# Load data

data(data)

# Descriptive statistice (Numerical response and 2 explicative variables):

tab1=report.quanti(data=data,y="y_numeric",
        x1="GROUP",x2="TIMEPOINT",at.row="TIMEPOINT",subjid="SUBJID")
tab1
#>    TIMEPOINT Statistics      A (N=30)      B (N=21)      C (N=17)
#> 1         D0          N            30            20            16
#> 2         D0  Mean (SD)   -1.01(1.22)   -1.11(0.93)   -0.51(1.27)
#> 3         D0     Median         -1.06         -0.98         -0.68
#> 4         D0    [Q1;Q3] [-1.72;-0.12] [-1.66;-0.75] [-1.67; 0.12]
#> 5         D0  [Min;Max]  [-3.95;0.85]  [-2.88;0.28]  [-2.50;2.17]
#> 6         D0    Missing             0             0             1
#> 7                                                                
#> 8         D1          N            30            20            16
#> 9         D1  Mean (SD)    1.55(0.65)    3.84(0.83)    4.70(0.95)
#> 10        D1     Median          1.71          3.97          4.62
#> 11        D1    [Q1;Q3] [ 0.87; 1.88] [ 3.10; 4.48] [ 4.16; 5.08]
#> 12        D1  [Min;Max]  [ 0.45;2.70]  [ 2.35;5.16]  [ 2.56;6.68]
#> 13        D1    Missing             0             0             0
#> 14                                                               
#> 15        D2          N            30            20            16
#> 16        D2  Mean (SD)    1.75(0.88)    4.25(1.34)    5.14(1.22)
#> 17        D2     Median          1.74          3.83          4.84
#> 18        D2    [Q1;Q3] [ 1.07; 2.47] [ 3.19; 5.05] [ 4.43; 5.79]
#> 19        D2  [Min;Max]  [-0.10;3.31]  [ 1.93;7.11]  [ 3.35;7.32]
#> 20        D2    Missing             1             0             1
#> 21                                                               
#> 22        D3          N            30            20            16
#> 23        D3  Mean (SD)    1.94(0.90)    4.05(1.16)    5.38(0.88)
#> 24        D3     Median          2.03          3.83          5.35
#> 25        D3    [Q1;Q3] [ 1.44; 2.56] [ 3.27; 4.56] [ 4.61; 6.10]
#> 26        D3  [Min;Max]  [-0.64;3.64]  [ 1.84;5.83]  [ 3.99;6.84]
#> 27        D3    Missing             2             1             0
#> 28                                                               
#> 29        D4          N            30            20            16
#> 30        D4  Mean (SD)    2.07(0.97)    3.98(1.10)    4.94(0.94)
#> 31        D4     Median          2.19          4.17          4.80
#> 32        D4    [Q1;Q3] [ 1.46; 2.64] [ 3.16; 4.47] [ 4.27; 5.27]
#> 33        D4  [Min;Max]  [-0.19;3.80]  [ 1.87;6.73]  [ 3.05;6.66]
#> 34        D4    Missing             1             0             1
#> 35                                                               
#> 36        D5          N            30            20            16
#> 37        D5  Mean (SD)    2.08(0.93)    4.21(1.03)    5.09(0.76)
#> 38        D5     Median          2.18          4.38          5.37
#> 39        D5    [Q1;Q3] [ 1.68; 2.73] [ 3.57; 4.71] [ 4.51; 5.43]
#> 40        D5  [Min;Max]  [-0.09;3.70]  [ 1.74;6.30]  [ 3.38;5.89]
#> 41        D5    Missing             2             0             0

# You can have the correspnding graphic:

gg=plot(tab1,title="Mean response evolution as a function of time by treatment group",
 legend.label="Treatment groups",ylab="Y mean")
gg
```

<img src="man/figures/README-unnamed-chunk-1-1.png" width="90%" />

# Put the results in a docx document with officer

``` r

doc=read_docx()
doc=report.doc(tab1,title="Example of quantitative descriptive table",doc=doc) 

# See the results

file=paste(tempfile(),".docx",sep="")
print(doc, target =file)

#shell.exec(file)
```

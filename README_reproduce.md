# Pareto3

In ANALYSIS/DATA, you can find the following raw data sets:

"CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx": these are the survey data provided by CHIDLREN

"Verbraucherpreisindizes_Deutschland.xlsx": sector-specific consumer price indices provided by the German Statistical Office

"Allgemeiner_Verbraucherpreindex.xlsx": economy-wide consumer price indices provided by the German Statistical Office

"Variable+Names+Comparison" is a correspondence table between the names of the variables in "CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx" (which differ by year) and the aligned variable names we use. 

We work with R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night"

To reproduce our work, open "Pareto3.Proj" in RStudio.

First run this in the console:

install.packages(c(
    'texreg',
    'readxl',
    'tidyverse',
    'Scale',
    'psych',
    'Hmisc', 
    'lattice',
    'survival',
    'Formula',
    'tidyimpute',
    'tidyselect',
    'imputeMissings',
    'zoo',
    'rlang',
    'stargazer',
    'estimatr',
    'ggplot2',
    'stats',
    'cowplot',
    'ggpubr',
    'polycor',
    'lavaan',
    'dplyr',
    'magrittr',
    'tidyselect', 
    'robustbase',
    'sandwich',
    'lmtest',
    'broom',
    'zoo',
    'lfe',
    'Matrix',
    'tseries',
    'graphics',
    'gridGraphics',
    'ggfortify'

))

Then open and run the following R scripts:
ANALYSIS/DATA/MakeGather.R
ANALYSIS/MakeAnalysis.R
ANALYSIS/MakeDiD.R

To reproduce our paper, run the following commands in the (Git Bash) terminal in RStudio:
Rscript -e "library(knitr); knit('paper.Rnw')"
pdflatex paper
biber paper
pdflatex paper
pdflatex paper


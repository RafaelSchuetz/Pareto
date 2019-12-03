library(pdftools)
report2016 <- pdf_text("ANALYSIS/DATA/CHILDREN_Jahresbericht_2016.pdf")
report2017 <- pdf_text("ANALYSIS/DATA/Children_Jahresbericht_2017.pdf")

view(report2016)
view(report2017)
#erstellen einer professionelleren time series

library(dplyr)
library(tidyverse)
library(magrittr)
library(tidyselect)
library(tseries)
library(graphics)

#Time series treat: ts_dayToDaySkills_control, ts_selfworth_treat
#Time series control: ts_dayToDaySkills_treat, ts_selfworth_control


# ###1. DayToDaySkills ----------------------------------------------------


#use the seqplot.ts befehl

seqplot.ts(ts_dayToDaySkills_treat, ts_dayToDaySkills_control, colx = "grey0", coly = "grey40", typex = "o",
           typey = "o", pchx = 2, pchy = 1, ltyx = "solid",
           ltyy = "dotted", oma = c(6, 0, 5, 0), ann = par("ann"),
           xlab = "Year", ylab = ("Day-To-Day-Skills"), main = "Trend of Day-To-Day-Skills")

#hinzufügen einer legende

legend("topleft", legend=c("Treatment", "Control"),
       col=c("grey0", "grey40"), lty=1:2, cex=0.8)

#hinzufügen von raster im hintergrund

grid(nx = NA, ny = NULL, col = "grey70", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)

#hinzufügen von linearen trends
#für dayToDaySkills: linearTrend_fit_treat2,linearTrend_fit_control2
#für selfworth: linearTrend_fit_treat,linearTrend_fit_control

lines(linearTrend_fit_treat2, col = "grey0", lwd = 1)
lines(linearTrend_fit_control2, col = "grey40", lwd = 1)


# ###2. selfworth ---------------------------------------------------------

#use the seqplot.ts befehl

seqplot.ts(ts_selfworth_treat, ts_selfworth_control, colx = "grey0", coly = "grey40", typex = "o",
           typey = "o", pchx = 2, pchy = 1, ltyx = "solid",
           ltyy = "dotted", oma = c(6, 0, 5, 0), ann = par("ann"),
           xlab = "Year", ylab = ("Selfworth"), main = "Trend of Selfworth")

#hinzufügen einer legende

legend("topleft", legend=c("Treatment", "Control"),
       col=c("grey0", "grey40"), lty=1:2, cex=0.8)

#hinzufügen von raster im hintergrund

grid(nx = NA, ny = NULL, col = "grey70", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)

#hinzufügen von linearen trends
#für selfworth: linearTrend_fit_treat,linearTrend_fit_control

lines(linearTrend_fit_treat, col = "grey0", lwd = 1)
lines(linearTrend_fit_control, col = "grey40", lwd = 1)


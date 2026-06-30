## Prepare plots and tables for report

## Before:
## After:

rm(list=ls())

library(icesTAF)
library(rmarkdown)
library(tidyverse)
library(flextable)
library(icesAdvice)
library(FLCore)
library(knitr)
library(FLasher)
library(mixfishtools)
library(ggplot2)
library(ggplotFL)
library(kableExtra)
library(RColorBrewer)
library(cowplot)

mkdir("report")


rmarkdown::render(
  input="report_tables.Rmd",
  output_file = "report/report_tables.docx"
)

rmarkdown::render(
  input="report_plots.Rmd",
  output_file = "report/report_plots.docx"
)

# change in advice
rmarkdown::render(
  input="report_change_in_advice.Rmd",
  output_file = "report/had.27.46a20_change_in_advice.html"
)


# MIXFISH RTA
rmarkdown::render(
  input="report_MIXFISH_RTA.Rmd",
  output_file = "report/had.27.46a20_MIXFISH_RTA.html"
)

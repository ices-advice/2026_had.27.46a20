## Extract results of interest, write TAF output tables

## Before:
## After:

rm(list=ls())
graphics.off()

library(icesTAF)
library(stockassessment)
library(ggplot2)
library(RColorBrewer)
library(FLCore)
library(icesAdvice)
library(FLfse)
library(icesSAG)
library(cowplot)
library(tidyr)
library(dplyr)

# for output_forecast_advice.r
library(ggplotFL)
library(grid)
library(sp)
library(tidyverse)

mkdir("output/input data")
mkdir("output/SAM")
mkdir("output/SURBAR")
mkdir("output/Forecast")
mkdir("output/Change_in_advice")

# Survey indices splom plots ----------------------------------------------------------

# for inclusion in report.Rmd
load("data/init.RData")
sourceDir("boot/software/utilities/")

load("boot/data/Input data - WGNSSK 2026/indices.RData")
output.dir <- "output/SURBAR/"

# splom plot
png(paste0(output.dir,"had.27.46a20 - Q1 survey splom plot.png"),width = 6, height = 6, pointsize=10, res=300, units="in")
survey.idx.splom(x.idx=x.idx[[1]], with.plus.pg = TRUE)
dev.off()
png(paste0(output.dir,"had.27.46a20 - Q3+Q4 survey splom plot.png"),width = 6, height = 6, pointsize=10, res=300, units="in")
survey.idx.splom(x.idx=x.idx[[2]], with.plus.pg = TRUE)
dev.off()


# Run plot scripts ----------------------------------------
source("output_assessment.R") # to add in 2026 - jit and simstudy plots 
source("output_forecast_assumptions.R")
source("output_forecast_advice.R")
source("output_change_in_advice.R") 


#Make WGMIXFISH objects ------------------------------------------------------------------------


rm(list=ls())
graphics.off()

library(icesTAF)
library(stockassessment)
library(FLCore)
library(FLfse)

load("data/init.RData")

# load this year's results
load("model/SAM/NShaddock_WGNSSK2026_Run1/model.RData")
load("data/stockData.RData")

# make model estimate object ------------------------------------------#
stock <- SAM2FLStock(fit, catch_estimate=TRUE)

# desc
stock@desc <- "had.27.46a20 - FLStock created from SAM model fit. catches = model estimates"
stock@name <- "had.27.46a20"

# set units
nmes <- names(units(stock))
un.lst <- as.list(c(rep(c("tonnes","thousands","kg"),4),rep("NA",2),"f",rep("NA",2)))
names(un.lst) <- nmes
units(stock) <- un.lst

# checks
range(stock)
round(harvest(stock)-computeHarvest(stock),5)
round(stock(stock)-computeStock(stock),5)
round(landings(stock)-computeLandings(stock),5)
round(discards(stock)-computeDiscards(stock),5)
round(catch(stock)-computeCatch(stock),5)
round(catch(stock)-landings(stock)-discards(stock),5)
round(computeCatch(stock)-computeLandings(stock)-computeDiscards(stock),5)
round(catch.n(stock)-landings.n(stock)-discards.n(stock),5)

save(stock,file=paste0("output/had_27_46a20_FLStock object_model estimates_",ay,".Rdata"))

# make stock data object -----------------------------------------------#
sw <- stock.data@stock.wt

# combine BMS and ibc into discards
dn_new <- stock.data@discards.n+window(ibcn+bmsn,start=1972,end=ay)
dw_new <- round((stock.data@discards.n*stock.data@discards.wt+
                   window(ibcn*ibcw+bmsn*bmsw,start=1972,end=ay))/
                  (stock.data@discards.n+window(ibcn+bmsn,start=1972,end=ay)),3)
dw_new[is.na(dw_new)] <- 0

# check consistency
sum(round(stock.data@catch.n-(stock.data@landings.n+dn_new),2),na.rm=T)
tmp <-((stock.data@landings.n*stock.data@landings.wt+dn_new*dw_new)/c(stock.data@landings.n+dn_new))
tmp[is.na(tmp)] <- 0
sum(round(stock.data@catch.wt-tmp,2),na.rm=T)

# add to stock data
stock.data@discards.n <- dn_new
stock.data@discards.wt <- dw_new
stock.data@discards <- computeDiscards(stock.data)

# SOP for landing and catch
stock.data@landings <- computeLandings(stock.data)
stock.data@catch <- computeCatch(stock.data)

# plus group
stock.data <- setPlusGroup(stock.data,plusgroup=8)

# overwrite some slots where setPlusGroup does it wrong
stock.data@stock.wt["8",] <- sw["8",] # stock weight is the set the same for ages 8-15


# check range
range(stock.data)
range(stock.data)["minfbar"] <- 2
range(stock.data)["maxfbar"] <- 4

stock.data@desc <- "had.27.46a20 - FLStock created from input data. catches = observations"
stock.data@name <- "had.27.46a20"

stock.data <- window(stock.data,end=(ay-1))

# checks
round(landings(stock.data)-computeLandings(stock.data),5)
round(discards(stock.data)-computeDiscards(stock.data),5)
round(catch(stock.data)-computeCatch(stock.data),5)

save(stock.data,file=paste0("output/had_27_46a20_FLStock object_input data_",ay,".Rdata"))


# Extract intermediate year numbers at age from forecast for WGMIXFISH ------------#

load("model/SAM/NShaddock_WGNSSK2026_Run1/model.RData")
load("model/SAM/forecast.RData")

tmp <- FC[["Fsq, then Fmsy"]]

N <- ntable(fit)
fc <- FC[[1]]
Nfc <- do.call(rbind, lapply(fc, function(x)exp(colMeans(x$sim[,1:ncol(N)]))))
rownames(Nfc) <- lapply(fc, function(x)x$year)
colnames(Nfc) <- colnames(N)

natage_fc <- as.data.frame(Nfc)
natage_fc <- as.data.frame(Nfc)
natage_fc$Year <- (ay-1):(ay+2)
colnames(natage_fc) <- c(0:8,"Year")
natage_fc <- as.data.frame(natage_fc)
natage_intYr <- t(natage_fc[natage_fc$Year ==ay,ac(0:8)])


write.table(natage_intYr,file="output/had_27_46a20 - N_at_age_in_intermediate_year_from_forecast.txt",sep=";",col.names=F)

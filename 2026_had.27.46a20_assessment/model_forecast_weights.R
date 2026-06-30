## Preprocess data, write TAF data tables

## Before:
## After:

# cohort growth modelling - Jaworski et al 2011

rm(list=ls())

# R v 4.2
library(icesTAF)
library(tidyverse)
library(FLCore)
library(RColorBrewer)
options(digits=15)

sourceDir("boot/software/utilities/")

load("data/init.RData")

# directories
input.dir <- paste0("boot/data/Input data - WGNSSK ",ay,"/")
forecast.dir <-"output/Forecast/" # where to save forecast data

years <- 1965:(ay-1)

# landings --------------------------------------------------------------------------
ca.wt <- read.table(paste0(input.dir,"nor_had_cw_lan.txt"),skip = 5, header=F)
ca.n <- read.table(paste0(input.dir,"nor_had_cn_lan.txt"),skip = 5, header=F)
names(ca.wt) <- names(ca.n) <- as.character(ages)
row.names(ca.n) <- row.names(ca.wt) <- years


# calc forecast wts
ca.wt.mod <-jaworski.mod.wts(ca.wt=ca.wt,ages=ages,years=years,type="landings",output.dir=forecast.dir)
lan.n <- ca.n
lan.mod.wt <- ca.wt.mod

#save output
write.table(ca.wt.mod,file=paste0(forecast.dir,"/had.27.46a20 - Jarworski model results - landings-at-age.txt"),sep="\t",quote=F)

# calc plus group
names(ca.n)<-as.character(ages)
row.names(ca.n)<-years

# weight next 3 years (modelled weight) by numbers at age over the last 3 years of data
ca<-ca.n[c((dim(ca.n)[1]-2):(dim(ca.n)[1])),]
wt<-ca.wt.mod[c((dim(ca.wt.mod)[1]-2):(dim(ca.wt.mod)[1])),]
mean.ca <- rbind(colMeans(ca,na.rm=T),colMeans(ca,na.rm=T),colMeans(ca,na.rm=T))
cawt<-mean.ca*wt
wt.plgr<-rowSums(cawt[,as.character(c(pg:colnames(cawt)[dim(cawt)[2]]))])/
  rowSums(mean.ca[,as.character(c(pg:colnames(mean.ca)[dim(mean.ca)[2]]))])

# bind results together
min.age<-min(as.numeric(colnames(ca.wt.mod)))
frcst.wts<-cbind(ca.wt.mod[c((dim(ca.wt.mod)[1]-2):(dim(ca.wt.mod)[1])),as.character(c(min.age:(pg-1)))],wt.plgr)
names(frcst.wts)<-c(c(min.age:(pg-1)),paste(pg,"+",sep=""))

# round to 3dp
frcst.wts<-round(frcst.wts,digits=3)
frcst.wts["0"][is.na(frcst.wts["0"])] <- 0

#save output
write.table(frcst.wts,file=paste0(forecast.dir,"/had.27.46a20 - Forecast weights - landings-at-age.txt"),sep="\t",quote=F)


# discards and bms and ibc --------------------------------------------------------------
ca.wt <- read.table(paste0(input.dir,"nor_had_cw_dis.txt"),skip = 5, header=F)
ca.n <- read.table(paste0(input.dir,"nor_had_cn_dis.txt"),skip = 5, header=F)
ca.wt1 <- read.table(paste0(input.dir,"nor_had_bmsw.txt"),skip = 5, header=F)
ca.n1 <- read.table(paste0(input.dir,"nor_had_bmsn.txt"),skip = 5, header=F)
ca.wt2 <- read.table(paste0(input.dir,"nor_had_byw.txt"),skip = 5, header=F)
ca.n2 <- read.table(paste0(input.dir,"nor_had_byn.txt"),skip = 5, header=F)

# combine dis and bms
ca.wt <- (ca.n*ca.wt + ca.n1*ca.wt1 + ca.n2*ca.wt2)/(ca.n+ca.n1+ca.n2)
ca.wt[is.na(ca.wt)] <- 0
ca.n <- ca.n+ca.n1+ca.n2

names(ca.wt) <- names(ca.n) <- as.character(ages)
row.names(ca.wt) <- row.names(ca.n) <- years

ca.wt.mod <-jaworski.mod.wts(ca.wt=ca.wt,ages=ages,years=years,type="discards",output.dir = forecast.dir)
dis.n <- ca.n
dis.mod.wt <- ca.wt.mod

#save output
write.table(ca.wt.mod,file=paste0(forecast.dir,"/had.27.46a20 - Jarworski model results - discards-at-age incl BMS and IBC.txt"),sep="\t",quote=F)

# calc plus group
names(ca.n)<-as.character(ages)
row.names(ca.n)<-years

# weight next 3 years (modelled weight) by numbers at age over the last 3 years of data
ca<-ca.n[c((dim(ca.n)[1]-2):(dim(ca.n)[1])),]
wt<-ca.wt.mod[c((dim(ca.wt.mod)[1]-2):(dim(ca.wt.mod)[1])),]
mean.ca <- rbind(colMeans(ca,na.rm=T),colMeans(ca,na.rm=T),colMeans(ca,na.rm=T))
cawt<-mean.ca*wt
wt.plgr<-rowSums(cawt[,as.character(c(pg:colnames(cawt)[dim(cawt)[2]]))])/
  rowSums(mean.ca[,as.character(c(pg:colnames(mean.ca)[dim(mean.ca)[2]]))])

# bind results together
min.age<-min(as.numeric(colnames(ca.wt.mod)))
frcst.wts<-cbind(ca.wt.mod[c((dim(ca.wt.mod)[1]-2):(dim(ca.wt.mod)[1])),as.character(c(min.age:(pg-1)))],wt.plgr)
names(frcst.wts)<-c(c(min.age:(pg-1)),paste(pg,"+",sep=""))

# round to 3dp
frcst.wts<-round(frcst.wts,digits=3)

#save output
write.table(frcst.wts,file=paste0(forecast.dir,"/had.27.46a20 - Forecast weights - discards-at-age incl BMS and IBC.txt"),sep="\t",quote=F)


# catch -------------------------------------------------------------------------
ca.n <- read.table(paste0(input.dir,"nor_had_cn_lan.txt"),skip = 5, header=F)
ca.n1 <- read.table(paste0(input.dir,"nor_had_cn_dis.txt"),skip = 5, header=F)
ca.n2 <- read.table(paste0(input.dir,"nor_had_byn.txt"),skip = 5, header=F)
ca.n3 <- read.table(paste0(input.dir,"nor_had_bmsn.txt"),skip = 5, header=F)

# make plus group
ca.n[,9] <- rowSums(ca.n[,9:16],na.rm=T)
ca.n1[,9] <- rowSums(ca.n1[,9:16],na.rm=T)
ca.n2[,9] <- rowSums(ca.n2[,9:16],na.rm=T)
ca.n3[,9] <- rowSums(ca.n3[,9:16],na.rm=T)
ca.n <- ca.n[,1:9]
ca.n1 <- ca.n1[,1:9]
ca.n2 <- ca.n2[,1:9]
ca.n3 <- ca.n3[,1:9]

#calc prop
prop.lan <- colMeans((ca.n/(ca.n+ca.n1+ca.n2+ca.n3))[(nrow(ca.n)-2):nrow(ca.n),],na.rm=T)
prop.disbmsby <- colMeans(((ca.n1+ca.n2+ca.n3)/(ca.n+ca.n1+ca.n2+ca.n3))[(nrow(ca.n)-2):nrow(ca.n),],na.rm=T)

#check
prop.lan+prop.disbmsby # should be 1 for each age
prop.lan <- rbind(prop.lan,prop.lan,prop.lan)
prop.disbmsby <- rbind(prop.disbmsby,prop.disbmsby,prop.disbmsby)

# get wts
lan.frcst.wts <- read.table(file=paste0(forecast.dir,"/had.27.46a20 - Forecast weights - landings-at-age.txt"))
dis.frcst.wts <- read.table(file=paste0(forecast.dir,"/had.27.46a20 - Forecast weights - discards-at-age incl BMS and IBC.txt"))

# check this is only the first age
lan.frcst.wts[is.na(lan.frcst.wts)]<-0
dis.frcst.wts[is.na(dis.frcst.wts)]<-0

cat.frcst.wts <- round((prop.lan*lan.frcst.wts+
                          prop.disbmsby*dis.frcst.wts)/(prop.lan+prop.disbmsby),3)
colnames(cat.frcst.wts) <- c(c(0:(pg-1)),paste(pg,"+",sep=""))

write.table(cat.frcst.wts,file=paste0(forecast.dir,"/had.27.46a20 - Forecast weights - catch-at-age.txt"),sep="\t",quote=F)

#stock wts ------------------------------------------------------------
# apply correction factor
cf <- read.csv("boot/data/had.27.46a20 - Mean_catchwt_to_stockwt_correction_factors.csv")
stk.frcst.wts <- round(rbind(cf,cf,cf) * cat.frcst.wts,3)
colnames(stk.frcst.wts) <- c(c(0:(pg-1)),paste(pg,"+",sep=""))
row.names(stk.frcst.wts) <- (c(last(years):(last(years)+2)))+1

write.table(stk.frcst.wts,file=paste0(forecast.dir,"/had.27.46a20 - Forecast weights - stock-at-age.txt"),sep="\t",quote=F)


## Preprocess data, write TAF data tables

## Before:
## After:

rm(list=ls())

# R v 4.4.2 used 01/04/2026
library(icesTAF)
taf.bootstrap()

#library(tidyverse)
#library(ggplot2)
library(FLCore)
library(RColorBrewer)


mkdir("data")
mkdir("data/SAM")
mkdir("data/SURBAR")

# Set common variables
ay <- 2026  # assessment year
ts_yrs <- 1972:ay
ages <- 0:15
pg <- 8

TAC <- 108301 # in assessment year


#Reference points - updated 2025
Fmsy <- 0.167
Fmsy_lo <- 0.155
Fmsy_hi <- 0.167
Btrig <- Bpa <- 192109
Blim <- 138250
Flim <- 0.31
Fpa <- 0.167
Fp.05 <- 0.167
Bmsy <- 248098

fn.prefix <- paste0("had.27.46a.20 - WGNSSK ",ay)
col.pal <- c(brewer.pal(n = 8, name = "Dark2"),brewer.pal(n=6,name="Set2")[6],brewer.pal(n=6,name="Accent"))
col.pal9 <- c(brewer.pal(n = 8, name = "Dark2"),brewer.pal(n=6,name="Set2")[4])


save(list=c("ay","ts_yrs","ages","pg","TAC","Fmsy","Fmsy_lo","Fmsy_hi","Btrig","Bmsy","Bpa","Blim","Flim","Fpa","Fp.05",
"col.pal","col.pal9","fn.prefix"),file="data/init.RData")

options(digits=15)

sourceDir("boot/software/utilities/")

# directories

input.dir <- paste0("boot/data/Input data - WGNSSK ",ay,"/")
sam.dir <- "data/SAM/" # where to save SAM files
surbar.dir <- "data/SURBAR/" # where to save surbar files

# stock description
st.desc <- paste0("Haddock in the Northern Shelf (had.27.46a20) (WGNSSK ",ay,"): ")

# read in data files ----------------------------------------------------

# surveys
load(paste0(input.dir,"Indices.RData"))

# biologicals
nm <- readVPAFile(paste0(input.dir,"/nor_had_nm.txt"))
mo <- readVPAFile(paste0(input.dir,"/nor_had_mo.txt"))
sw <- readVPAFile(paste0(input.dir,"/nor_had_sw.txt"))

# catch numbers
ln <- readVPAFile(paste0(input.dir,"/nor_had_cn_lan.txt"))
dn <- readVPAFile(paste0(input.dir,"/nor_had_cn_dis.txt"))
cn <- readVPAFile(paste0(input.dir,"/nor_had_cn.txt")) 
bmsn <- readVPAFile(paste0(input.dir,"/nor_had_bmsn.txt"))
ibcn <- readVPAFile(paste0(input.dir,"/nor_had_byn.txt"))

# catch weights
lw <- readVPAFile(paste0(input.dir,"/nor_had_cw_lan.txt"))
dw <- readVPAFile(paste0(input.dir,"/nor_had_cw_dis.txt"))
cw <- readVPAFile(paste0(input.dir,"/nor_had_cw.txt")) 
bmsw <- readVPAFile(paste0(input.dir,"/nor_had_bmsw.txt"))
ibcw <- readVPAFile(paste0(input.dir,"/nor_had_byw.txt"))

# catch tonnage
lt <- readVPAFile(paste0(input.dir,"/nor_had_ca_lan.txt"))
dt <- readVPAFile(paste0(input.dir,"/nor_had_ca_dis.txt"))
ct <- readVPAFile(paste0(input.dir,"/nor_had_ca.txt")) 
bmst <- readVPAFile(paste0(input.dir,"/nor_had_bms.txt"))
ibct <- readVPAFile(paste0(input.dir,"/nor_had_by.txt"))

# check consistency of catch - should be 0
sum(round(cn-(ln+dn+bmsn+ibcn),2))
tmp <-((ln*lw+dn*dw+bmsn*bmsw+ibcn*ibcw)/c(ln+dn+bmsn+ibcn))
tmp[is.na(tmp)] <- 0
sum(round(cw-tmp,2))

# combine discards, bms and ibc together for SAM -----------------------------
dn_new <- dn+ibcn+bmsn
dw_new <- round((dn*dw+ibcn*ibcw+bmsn*bmsw)/(dn+ibcn+bmsn),3)
dw_new[is.na(dw_new)] <- 0

# check consistency - should be 0
sum(round(cn-(ln+dn+ibcn+bmsn),2))
tmp <-((ln*lw+dn_new*dw_new)/c(ln+dn_new))
tmp[is.na(tmp)] <- 0
sum(round(cw-tmp,2))


# Make SAM files ---------------------------------------------------------

# copy across files that don't need editing
file.copy(from = file.path(input.dir,"nor_had_cn.txt"), to = file.path(sam.dir,"cn.dat"),overwrite=T)
file.copy(from = file.path(input.dir,"nor_had_cw.txt"), to = file.path(sam.dir,"cw.dat"),overwrite=T)
file.copy(from = file.path(input.dir,"nor_had_cw_lan.txt"), to = file.path(sam.dir,"lw.dat"),overwrite=T)
file.copy(from = file.path(input.dir,"nor_had_ef_q1_q3q4.txt"), to = file.path(sam.dir,"survey.dat"),overwrite=T)
file.copy(from = file.path(input.dir,"nor_had_nm.txt"), to = file.path(sam.dir,"nm.dat"),overwrite=T)
file.copy(from = file.path(input.dir,"nor_had_mo.txt"), to = file.path(sam.dir,"mo.dat"),overwrite=T)
file.copy(from = file.path(input.dir,"nor_had_sw.txt"), to = file.path(sam.dir,"sw.dat"),overwrite=T)
file.copy(from = file.path(input.dir,"nor_had_pf.txt"), to = file.path(sam.dir,"pf.dat"),overwrite=T)
file.copy(from = file.path(input.dir,"nor_had_pm.txt"), to = file.path(sam.dir,"pm.dat"),overwrite=T)
file.copy(from = file.path(input.dir,"nor_had_cn_lan.txt"), to = file.path(sam.dir,"lf.dat"),overwrite=T)

# copy survey CV files
file.copy(from = file.path(input.dir,"survey-haddock-Q1-1-8plus_CV.dat"), to = file.path(sam.dir,"survey-haddock-Q1-1-8plus_CV.dat"),overwrite=T)
file.copy(from = file.path(input.dir,"survey-haddock-Q3Q4-0-8plus_CV.dat"), to = file.path(sam.dir,"survey-haddock-Q3Q4-0-8plus_CV.dat"),overwrite=T)


# Write out files for combined discards, IBC and BMS
writeVPAFile(FLStock.=NULL,file.=paste0(sam.dir,"dw.dat"),desc.="discards weight-at-age",slot.="discards.wt",
             obj.=dw_new,name.= st.desc) 

writeVPAFile(FLStock.=NULL,file.=paste0(sam.dir,"nor_had_cn_dis.txt"),desc.="discards-at-age",slot.="discards.n",
             obj.=dn_new,name.= st.desc)


# Make SURBAR files ---------------------------------------------------------

# need to trim down to first survey year (1983)

# ages 1-7
#mat
writeVPAFile(FLStock.=NULL, file.=paste0(surbar.dir,"nosh_had_mo.dat"), slot.="mat", 
             obj.=window(trim(mo,age=1:7),start=1983),desc.="maturity-at-age ogive",name.= st.desc) 
#stock weights
writeVPAFile(FLStock.=NULL, file.=paste0(surbar.dir,"nosh_had_sw.dat"), slot.="stock.wt", 
             obj.=window(trim(sw,age=1:7),start=1983),desc.="stock weight-at-age",name.= st.desc) 

# surveys
# ages 1-7
idx.q1 <- x.idx[[1]]
idx.q3q4 <- x.idx[[2]]
x.idx.trim <- FLIndices(trim(idx.q1,age=1:7),trim(idx.q3q4,age=1:7))
x.idx.trim@desc <- paste0(st.desc,"survey indices")
writeIndicesVPA(x.idx.trim,paste0(surbar.dir,"nosh_had_ibts.dat"))


# FLStock object -------------------------------------------------
stock.data <- FLStock(catch=ct,landings=lt,discards=dt,
                     catch.n = cn,landings.n=ln,discards.n=dn,
                     catch.wt=cw,landings.wt=lw,discards.wt=dw)



stock.data <- window(stock.data,end=ay)
stock.data@stock.wt <- sw
stock.data@mat <- mo
stock.data@m <- nm

# check
summary(stock.data)
units(stock.data@landings) <- units(stock.data@discards) <- units(stock.data@catch) <- units(stock.data@stock) <- "tonnes"
units(stock.data@landings.n) <- units(stock.data@discards.n) <- units(stock.data@catch.n) <- units(stock.data@stock.n) <- "thousands"
units(stock.data@landings.wt) <- units(stock.data@discards.wt) <- units(stock.data@catch.wt) <- units(stock.data@stock.wt) <- "kg"
units(stock.data@m) <- units(stock.data@mat) <- units(stock.data@harvest) <- "NA"
range(stock.data)["minfbar"] <- 2
range(stock.data)["maxfbar"] <- 4


# make FLR objects (data only) ---------------------------------------------------------

stock.data65 <- stock.data # full time series back to 1965
stock.data <- window(stock.data,start=1972) # assessment time series starts in 1972

# plus group
stock.data.pg <- setPlusGroup(stock.data,plusgroup=8)
stock.data65.pg <- setPlusGroup(stock.data65,plusgroup=8)

# overwrite some slots
stock.data.pg@stock.wt["8",] <- stock.data@stock.wt["8",] # stock weight is the set the same for ages 8-15
stock.data65.pg@stock.wt["8",] <- stock.data65@stock.wt["8",] # stock weight is the set the same for ages 8-15

# BMS and IBC 
bmsn.pg <- setPlusGroup(bmsn,8)
ibcn.pg <- setPlusGroup(ibcn,8)

bmsw.pg <- trim(bmsw,age=0:8)
bmsw.pg["8",] <- quantSums(bmsn[ac(8:15),]*bmsw[ac(8:15),],na.rm=T)/quantSums(bmsn[ac(8:15),],na.rm=T)
is.na(bmsw.pg["8",]) <- 0
ibcw.pg <- trim(ibcw,age=0:8)
ibcw.pg["8",] <- quantSums(ibcn[ac(8:15),]*ibcw[ac(8:15),],na.rm=T)/quantSums(ibcn[ac(8:15),],na.rm=T)
is.na(ibcw.pg["8",]) <- 0


save(list=c("stock.data65","stock.data65.pg","stock.data","bmsn","bmsw","ibcn","ibcw",
            "stock.data.pg","bmsn.pg","bmsw.pg","ibcn.pg","ibcw.pg"),file="data/stockData.RData")



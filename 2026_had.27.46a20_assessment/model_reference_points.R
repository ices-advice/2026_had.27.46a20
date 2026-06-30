## Run reference points script for two assessments and compare 

## Before:
## After:

### Need to check decision tree. Can I automate any decisions???? ####

##~--------------------------------------------------------------------------
# Code to take the assessment from stockassessment.org (new TMB fits), 
# and run ICES standard EqSim analyses
# D.C.M.Miller
##~--------------------------------------------------------------------------
## Issues:
# Doesn't work on old SAM fits from stockassessment.org. # Could add option to simply load FLStock object (i.e. make more general)
# Currently simply sets MSY Btrigger to Bpa. # This should be changed to follow MSY Btrigger decision tree
# Currently assume 0 discards
# R.3.4.1, run in RGui not RStudio
###-------------------------------------------------------------------------------

# setup ####
rm(list=ls())

set.seed(12345)
load("data/init.RData") # useful stuff. 

runName <- "NShaddock_WGNSSK2025_Run1" 

## Save plots?
savePlots <- T

datayear <- ay-1 # final data year

# Get fit
load(paste0("model/SAM/",runName,"/model.RData")) # loads 'fit' to the workspace 


## Stock and assessment ------------------------------------------------
stockName <- "HAD2746a20"  
SAOAssessment <- runName # = stock name in stockassesssment.org
user <- 259 # User 259 = Harriet?; User 348 = me; User 3 = Guest (ALWAYS GETS THE LATEST COMMITTED VERSION)
ages <- 0:pg
years <- min(ts_yrs):datayear  # all years of data (catch, survey, mortality) # Harriet said 2000 onwards
meanFages <- c(2:4)

## Reference points
refPts <- matrix(NA,nrow=1,ncol=10, dimnames=list("value",c("Btrigger","Bpa","Blim","Bmsy","Fpa","Flim", "Fp05","Fmsy_unconstr","Fmsy","Fmsy_upper","Fmsy_lower")))

# Get Blim
# Type 1, Blim = lowest biomass among high recruitment years (check WD 10)
Rth <- quantile(rectable(fit)[,"Estimate"],0.95) # high recruits defined as 95th percentile
idx <- which(rectable(fit)[,"Estimate"] >=Rth)
cat("High recruits in: ",names(rectable(fit)[idx,"Estimate"]),"\n")
fitBlim <- min(ssbtable(fit)[idx,"Estimate"])
fitBlimYr <- names(which(ssbtable(fit)[idx,"Estimate"] == fitBlim))
print(paste0("lowest SSB with high recruits observed in: ",fitBlimYr))

# set Blim
refPts[,"Blim"] <- round(fitBlim)

## Simulation settings  ----------------------------------------------------------

# Number of sims
noSims <- 2000

# SR models to use
appModels <- c("Segreg","Ricker","Bevholt")

# Which years (SSB years) to exclude from the SRR fits (leave as 'c()' if no years excluded)
#rmSRRYrs <- c(1978:1982) # Can specify here which other years (e.g. early period) should be left out

minYear <- 2000 # specify minimum year here 

# Autocorrelation in recruitment?
rhoRec <- F # default=F # plot from Harriet: autocorrelation at lag 1 and 5. But the plot produced by this script show no significant autocorrelation

## Bio params and selectivity
# Avg. Years
numAvgYrsB <- 5   #Bio 
numAvgYrsS <- 5   #Sel

## Forecast error
cvF  <- 0.212;	phiF <-	0.423 # default values, look in WKMSYREF4 (although confusion with 0.233?)
cvSSB <- 0; phiSSB <- 0

##--------------------------------------------------------------------------#
## Get fit ####

#fit<-reduce(fit,year=c(rmSRRYrs))  # years are removed line 236

# Check model fit
windows()
plot(fit)
dev.off()

## Stock-recruitment plots
df <- data.frame(summary(fit))

ds <- dim(df)
rec <- df$R.age.0./1000 #rec <- df$R.age.0.[2:ds[1]]/1000 # For haddock recruitment is at age 0 (same year as SSB),
ssb <- df$SSB/1000 #ssb <- df$SSB[1:(ds[1]-1)]/1000       # so we don't need the 1 year lag
yr  <- rownames(df) #[1:(ds[1]-1)]

if (savePlots){ 
  x11()
  plot(ssb,rec,type='l',ylim=c(0,1.1*max(rec)),xlim=c(0,1.1*max(ssb)),main=stockName,xlab="SSB",ylab="Recruits at age 0",cex.lab=1.5); text(ssb,rec,yr,cex=.8)
  savePlot(paste0("output/ref_pts/02_",runName,"_SRR.png"),type="png")
  dev.off()
  
  x11()
  plot(yr,log(rec/ssb),type='b',main=stockName,xlab="Year",ylab="ln(Recruits/SSB) ",cex.lab=1.5)
  savePlot(paste0("output/ref_pts/03_",runName,"_SPR.png"),type="png")
  dev.off()
}

## Uncertainty last year
# Get from last assessment year (SAM) unless this is specified as a value

## Get sigmaSSB and sigmaF from the assessment fit - but use default if default is larger?
idx <- names(fit$sdrep$value) == "logssb"
sigmaSSB <- fit$sdrep$sd[idx][fit$data$years==max(years)] # Use last year in status table
#sigmaSSB <- fit$sdrep$sd[idx][fit$data$years==(max(years)-1)] 
sigmaSSB <- ifelse(sigmaSSB>0.2, sigmaSSB,0.2)

idx <- names(fit$sdrep$value) == "logfbar"
#sigmaF <- fit$sdrep$sd[idx][fit$data$years==max(years)]
sigmaF <- fit$sdrep$sd[idx][fit$data$years==(max(years)-1)]  # Use last year in status table
sigmaF <- ifelse(sigmaF>0.2, sigmaF,0.2)

print(paste0("sigmaSSB: ",sigmaSSB))
print(paste0("sigmaF: ",sigmaF))


## Create FLStock object--------------------------------------------------- 

# Get stk using SAM2FLStock so that +group is computed correctly
stk <- FLfse::SAM2FLStock(fit)

# Set units
units(stk)[1:17]    <- as.list(c(rep(c("tonnes","thousands","kg"),4), rep("NA",2),"f",rep("NA",2)))

# Mean F range
range(stk)[c("minfbar","maxfbar")]    <- c(min(meanFages), max(meanFages))

# Last age a plusgroup (should be done already)
stk  <- setPlusGroup(stk,stk@range["max"])

### Read raw data from stockassessment.org
url <- paste("https://www.stockassessment.org/datadisk/stockassessment/userdirs/user",user,"/",SAOAssessment,"/data/", sep="")  
filestoget <- c("cn.dat", "cw.dat", "dw.dat", "lf.dat", "lw.dat",
                "mo.dat", "nm.dat", "pf.dat", "pm.dat", "sw.dat",
                "survey.dat")
d <- lapply(filestoget, function(f)download.file(paste(url,f,sep=""),destfile=paste0("model/SAM/",runName,"/",f),
                                                 method="wininet", extra = "--no-check-certificate"))

# Catches OK, but override landings (FOR SOME REASON SAM2FLSTOCK GIVES WRONG LANDING NUMBERS???)
#catch.n(stk)[,ac(years[1]:(max(years)-1))] <- landings.n(stk)[,ac(years[1]:(max(years)-1))] <- tmpCat; rm(tmpCat)
#tmpCat <- t(read.ices("cn.dat"))
tmpLF <- t(read.ices(paste0("model/SAM/",runName,"/lf.dat")))
dms <- list(intersect(ac(ages),dimnames(tmpLF)[[1]]),intersect(years,dimnames(tmpLF)[[2]]))
#catch.n(stk)[dms[[1]],dms[[2]]] <- tmpCat[dms[[1]],dms[[2]]]
catch.n(stk)[is.na(catch.n(stk))] <- 0
# This next line computes landings with catch.n from SAM2FLStock (with +group) and landings fraction
landings.n(stk)[dms[[1]],dms[[2]]] <- array(catch.n(stk),dim=c(length(dms[[1]]),length(dms[[2]]))) * tmpLF[dms[[1]],dms[[2]]]
landings.n(stk)[is.na(landings.n(stk))] <- 0
discards.n(stk)[] <- catch.n(stk) - landings.n(stk)
rm(dms)

#catch.wt(stk)[,ac(years[1]:(max(years)-1))] <- t(read.ices("cw.dat"))[-1,]
catch.wt(stk)[is.na(catch.wt(stk))] <- 0.001  # Replace NA weights with something low
catch.wt(stk)[catch.wt(stk)==0] <- 0.001  # Replace 0 weights with something low

landings.wt(stk)[is.na(landings.wt(stk))] <- 0.001  # Replace NA weights with something low
landings.wt(stk)[landings.wt(stk)==0] <- 0.001  # Replace 0 weights with something low

discards.wt(stk)[is.na(discards.wt(stk))] <- 0.001  # Replace NA weights with something low
discards.wt(stk)[discards.wt(stk)==0] <- 0.001  # Replace 0 weights with something low

discards(stk) <- computeDiscards(stk)
landings(stk) <- computeLandings(stk)
catch(stk) <- computeCatch(stk)

# add bio
tmp <- t(read.ices(paste0("model/SAM/",runName,"/mo.dat"))); dms <- list(intersect(ac(ages),dimnames(tmp)[[1]]),intersect(years,dimnames(tmp)[[2]]))
mat(stk)[dms[[1]],dms[[2]]] <- tmp[dms[[1]],dms[[2]]]; rm(tmp,dms)
tmp <- t(read.ices(paste0("model/SAM/",runName,"/nm.dat"))); dms <- list(intersect(ac(ages),dimnames(tmp)[[1]]),intersect(years,dimnames(tmp)[[2]]))
m(stk)[dms[[1]],dms[[2]]] <- tmp[dms[[1]],dms[[2]]]; rm(tmp,dms)
tmp <- t(read.ices(paste0("model/SAM/",runName,"/pf.dat"))); dms <- list(intersect(ac(ages),dimnames(tmp)[[1]]),intersect(years,dimnames(tmp)[[2]]))
harvest.spwn(stk)[dms[[1]],dms[[2]]] <- tmp[dms[[1]],dms[[2]]]; rm(tmp,dms)
tmp <- t(read.ices(paste0("model/SAM/",runName,"/pm.dat"))); dms <- list(intersect(ac(ages),dimnames(tmp)[[1]]),intersect(years,dimnames(tmp)[[2]]))
m.spwn(stk)[dms[[1]],dms[[2]]] <- tmp[dms[[1]],dms[[2]]]; rm(tmp,dms)

# Update stock and fisheries from SAM fit
stock.n(stk)[] <- exp(fit$pl$logN)
tmp <- t(read.ices(paste0("model/SAM/",runName,"/sw.dat"))); dms <- list(intersect(ac(ages),dimnames(tmp)[[1]]),intersect(years,dimnames(tmp)[[2]]))
stock.wt(stk)[dms[[1]],dms[[2]]] <- tmp[dms[[1]],dms[[2]]]; rm(tmp,dms)
stock.wt(stk)[is.na(stock.wt(stk))] <- 0.001  # Replace NA weights with something low
stock.wt(stk)[stock.wt(stk)==0] <- 0.001  # Replace 0 weights with something low
stock(stk)[] <- computeStock(stk)
# harvest is unique to the set code (i.e. depends on config)
# check conf. file for which F states are estimated
Fstates <- fit$conf$keyLogFsta[1,]
Fstates_start <- which(Fstates==0)
Fstates_end   <- which(Fstates==max(Fstates))
harvest(stk)[Fstates_start:min(Fstates_end),] <- exp(fit$pl$logF)
harvest(stk)[Fstates_end[-1],] <- harvest(stk)[Fstates_end[1],]
harvest(stk)[Fstates==-1,] <- 0

## Selectivity curves

if(min(ages)==0) {meanFages_ <- meanFages+1} else {meanFages_ <- meanFages}

if (savePlots) {
  x11()
  meanF <- apply(harvest(stk)[meanFages_,],2, "mean")
  sel <- sweep(harvest(stk),2,meanF,"/")
  plot(ages,sel[,ac(max(years)-1)], type="l", ylim=c(0,max(sel)), xlab="Age", ylab="Selectivity", main="Selectivity")
  for (i in ac((datayear-19):datayear)) lines(ages,sel[,i], col=i)
  lines(ages,apply(sel[,ac((datayear-2):datayear)],1,mean), col=1, lwd=5)
  lines(ages,apply(sel[,ac((datayear-4):datayear)],1,mean), col=2, lwd=5)
  lines(ages,apply(sel[,ac((datayear-9):datayear)],1,mean), col=3, lwd=5)
  lines(ages,apply(sel[,ac((datayear-19):datayear)],1,mean), col=4, lwd=5)
  legend("topleft", legend=c("Mean last 3yrs","Mean last 5yrs","Mean last 10yrs","Mean last 20yrs"), lwd=5, col=1:4, bty="n")
  #legend("bottomright", legend=c(1997:2016), lwd=1, col=1:20, bty="n")
  savePlot(paste0("output/ref_pts/00_",runName,"_Selectivity.png"),type="png")
  dev.off()
}

## Weight at age
if (savePlots){
  
  x11()
  plot(ages,stock.wt(stk)[,ac(max(years)-1)], type="l", ylim=c(0,max(stock.wt(stk))), xlab="Age", ylab="Weight (kg)", main="Weight at Age")
  for (i in ac((datayear-19):datayear)) lines(ages,stock.wt(stk)[,i], col=i)
  lines(ages,apply(stock.wt(stk)[,ac((datayear-2):datayear)],1,mean), col=1, lwd=5)
  lines(ages,apply(stock.wt(stk)[,ac((datayear-4):datayear)],1,mean), col=2, lwd=5)
  lines(ages,apply(stock.wt(stk)[,ac((datayear-9):datayear)],1,mean), col=3, lwd=5)
  lines(ages,apply(stock.wt(stk)[,ac((datayear-19):datayear)],1,mean), col=4, lwd=5)
  legend("topleft", legend=c("Mean last 3yrs","Mean last 5yrs","Mean last 10yrs","Mean last 20yrs"), lwd=5, col=1:4, bty="n")
  #legend("bottomright", legend=c(1997:2016), lwd=1, col=1:20, bty="n")
  savePlot(paste0("output/ref_pts/00_",runName,"_WAA.png"),type="png")
  dev.off()
}

if (savePlots){
  
  x11()
  plot(ages,mat(stk)[,ac(max(years)-1)], type="l", ylim=c(0,max(mat(stk))), xlab="Age", ylab="Maturity", main="Maturity at Age")
  for (i in ac((datayear-19):datayear)) lines(ages,mat(stk)[,i], col=i)
  lines(ages,apply(mat(stk)[,ac((datayear-2):datayear)],1,mean), col=1, lwd=5)
  lines(ages,apply(mat(stk)[,ac((datayear-4):datayear)],1,mean), col=2, lwd=5)
  lines(ages,apply(mat(stk)[,ac((datayear-9):datayear)],1,mean), col=3, lwd=5)
  lines(ages,apply(mat(stk)[,ac((datayear-19):datayear)],1,mean), col=4, lwd=5)
  legend("topleft", legend=c("Mean last 3yrs","Mean last 5yrs","Mean last 10yrs","Mean last 20yrs"), lwd=5, col=1:4, bty="n")
  #legend("bottomright", legend=c(1997:2016), lwd=1, col=1:20, bty="n")
  savePlot(paste0("output/ref_pts/00_",runName,"_MAA.png"),type="png")
  dev.off()
}
if (savePlots){
  x11()
  plot(ages,m(stk)[,ac(max(years)-1)], type="l", ylim=c(0,max(m(stk))), xlab="Age", ylab="Natural mortality", main="Natural mortality at Age")
  for (i in ac((datayear-19):datayear)) lines(ages,m(stk)[,i], col=i)
  lines(ages,apply(m(stk)[,ac((datayear-2):datayear)],1,mean), col=1, lwd=5)
  lines(ages,apply(m(stk)[,ac((datayear-4):datayear)],1,mean), col=2, lwd=5)
  lines(ages,apply(m(stk)[,ac((datayear-9):datayear)],1,mean), col=3, lwd=5)
  lines(ages,apply(m(stk)[,ac((datayear-19):datayear)],1,mean), col=4, lwd=5)
  legend("topleft", legend=c("Mean last 3yrs","Mean last 5yrs","Mean last 10yrs","Mean last 20yrs"), lwd=5, col=1:4, bty="n")
  #legend("bottomright", legend=c(1997:2016), lwd=1, col=1:20, bty="n")
  savePlot(paste0("output/ref_pts/00_",runName,"_NMAA.png"),type="png")
  dev.off()
}
### Trim off last year of the stock object (incomplete data for last assessment year)

maxYear <- range(stk)["maxyear"]
origStk <- stk
stk <- window(stk, start=minYear, end=(maxYear-1))

### test for type 1 - spasmodic stock ----------------------------------------------------------
# Functions to determine if a stock is spasmodic
# Original code by Paula Silvar Villamidou

## empirical cumulative distribution function
## could use R's ecdf function but to extract values
## requires defining and applying the function
if(0){
## own ecdf function
ecdf_fn <- function(x){
  sx <- sort(x)
  n <- length(x)
  return(list(x = sx, y = (1:n)/n))
}


## simulate pointwise quantiles of lognormal cdf with given variability
get_bounds <- function(n, sd, alpha = 0.2, m){
  ##-------------------------------------#
  ## simulates cdfs from a scaled lognormal distribution
  ## n is the length of the recruitment timeseries
  ## sd is the standard deviation on the log scale of a lognormal distributions
  ## alpha is the significance level of the bands
  ## m is the number of replicates
  ##-------------------------------------#
  ## set up a container for the results
  res <- data.frame(x = rep(NA, m*n), y = rep(NA, m*n))
  ## simulate scaled cdfs m times and store
  for(j in 1:m){
    x <- rlnorm(n, meanlog = 5, sdlog = sd) ## mean doesn't matter here
    x <- x / max(x)
    ## ecdf
    ecdfj <- ecdf_fn(x)
    ## store the simulated ecdf
    res$y[((j-1)*n + 1):(j*n)] <- ecdfj$y
    ## store the scaled x
    ## rounding here to aggregate subsequently
    ## could change precision and increase m for smoother bounds
    res$x[((j-1)*n + 1):(j*n)] <- round(ecdfj$x, 2)
  }
  
  
  ## get the bounds
  ## lower
  lwr <- aggregate(y ~ x, quantile, p = alpha/2, data = res)
  names(lwr)[names(lwr) == "y"] <- "lwr"
  ## upper
  upr <- aggregate(y ~ x, quantile, p = 1 - alpha/2, data = res)
  names(upr)[names(upr) == "y"] <- "upr"
  bounds <- merge(lwr, upr)
  ##
  return(bounds)
}


ssbrec_df <- as_tibble(as.data.frame(ssb(origStk))[, c('year', 'data')]) %>% mutate(rec = rec(origStk)[drop=T]) %>% 
  filter(year < ay)
names(ssbrec_df)[2] <- 'ssb'

## Raw recruitment, not detrended

## ecdf of recruitment scaled to maximum
ecdf_scaled <- ecdf_fn(ssbrec_df$rec/max(ssbrec_df$rec))

## simulation )takes some time)
bounds <- get_bounds(n = nrow(ssbrec_df), sd = 1, alpha = 0.2, m = 1e4)

## Detrended recruitment
# remove longterm low frequency variability with a loess filter

ssbrec_df$lnR <- log(ssbrec_df$rec)

fit <- loess(lnR ~ year, span = 0.3, data = ssbrec_df)

with(ssbrec_df, plot(year, lnR, bty = "l"))
lines(fit$x, fit$fitted)

## multiplicateve residuals around long term trend
mres <- exp(residuals(fit))

## ecdf of detrended and scaled residuals 
ecdf_detrend <- ecdf_fn(mres/max(mres))

## plot all
taf.png("output/ref_pts/spasmodic_recruitment.png",width=9,height=7,units="in",res=300)
plot(ecdf_scaled, main = "Cumulative distribution functions",
     type = "s", bty = "l", lty = 2,
     xlab = "Scaled recruitment", ylab = "Cumulative probability",
     xlim = c(0, 1), col = "navy", lwd = 1.5)
lines(ecdf_detrend, col = "navy", lwd = 1.5, type = "s")
polygon(c(bounds$x, rev(bounds$x)), c(bounds$lwr, rev(bounds$upr)), col = "#FF7F5060", border = "red")
legend("bottomright", legend = c("Detrended CDF", "Scaled CDF", "'Spasmodic' region"), lty = c(1, 2, NA),
       pch = c(NA, NA, 15),
       lwd = c(1.5, 1.5, NA),
       col = c("navy", "navy", "#FF7F5060"), bty = "n")
dev.off()

}



###-------------------------------------------------------------------------------
### Set SRR Models for the simulations------------------------------------------
#Models: "segreg","ricker", "bevholt"; or specials: "SegregBlim/Bloss" (breakpt. Blim/Bloss)

## SRR years 
# Which years (SSB years) to exclude from the SRR fits
# Keep all except last 2 (poorly estimated rec/selec) for haddock
rmSRRYrs <-NULL #union(rmSRRYrs, c(maxYear-1,maxYear))  # This removes last two years
srYears <- setdiff(c(minYear:(maxYear-1)),rmSRRYrs)

## determine segreg model with Blim breakpoint and (roughly) geomean rec above this
SegregBlim  <- function(ab, ssb) log(ifelse(ssb >= refPts[,"Blim"], ab$a * refPts[,"Blim"], ab$a * ssb))

## determine segreg model with Bloss breakpoint and (roughly) geomean rec above this
SegregBloss  <- function(ab, ssb) log(ifelse(ssb >= min(ssb(stk)), ab$a * min(ssb(stk)), ab$a * ssb))

###~~~~~~~~~~~~~
## autocorrelation
ACFrec <- acf(rec(stk)[,ac(srYears)])
acfRecLag1 <- round(ACFrec$acf[,,][2],2)
if (savePlots){
  x11()
  acf(rec(stk), plot=T, main=paste("Autocor. in Rec, Lag1 =",acfRecLag1,sep=" "))
  savePlot(paste0("output/ref_pts/04_",runName,"_SRautocor.png"),type="png")
  dev.off()
}

## Fit SRRs----------------------------------------------------------------
FIT_segregBlim <- eqsr_fit(stk,nsamp=noSims, models = "SegregBlim", remove.years=rmSRRYrs)
#FIT_segregBloss <- eqsr_fit(stk,nsamp=noSims, models = "SegregBloss", remove.years=rmSRRYrs)
FIT_segreg <- eqsr_fit(stk,nsamp=noSims, models = "Segreg", remove.years=rmSRRYrs)
FIT_All <- eqsr_fit(stk,nsamp=noSims, models = appModels, remove.years=rmSRRYrs)

# save model proportions and parameters:
write.csv(FIT_segregBlim$sr.det, paste0("output/ref_pts/",runName,"_FIT_segregBlim_SRpars.csv",sep=""))
#write.csv(FIT_segregBloss$sr.det, paste(stockName,"_FIT_segregBloss_SRpars.csv",sep=""))
write.csv(FIT_segreg$sr.det, paste0("output/ref_pts/",runName,"_FIT_segreg_SRpars.csv",sep=""))
write.csv(FIT_All$sr.det, paste0("output/ref_pts/",runName,"_FIT_All_SRpars.csv",sep=""))

# Plot raw SRR results - all models freely estimated breakpoint
if (savePlots){
  x11()
  eqsr_plot(FIT_All,n=2e4) 
  savePlot(paste0("output/ref_pts/05a_",runName,"_SRRall.png"),type="png")
  dev.off()
}

# Plot raw SRR results - breakpoint fixed at Blim
if (savePlots){
  x11()
  eqsr_plot(FIT_segregBlim,n=2e4) 
  savePlot(paste0("output/ref_pts/05b_",runName,"_SRRsegregBlim.png"),type="png")
  dev.off()
}

# Plot raw SRR results - segreg freely estimated breakpoint
if (savePlots) {
  x11()
  eqsr_plot(FIT_segreg,n=2e4) 
  savePlot(paste0("output/ref_pts/05c_",runName,"_SRRsegreg.png"),type="png")
  dev.off()
}


## Run simulations ---------------------------------------------------------------

## Calculate Bpa based on sigmaSSB -----#
refPts[,"Bpa"]  <- round(refPts[,"Blim"]*exp(sigmaSSB*1.645)) #40000  # Used as Btrigger
refPts[,"Btrigger"]  <- refPts[,"Bpa"]  # This should be changed to follow MSY Btrigger decision tree


## Simuation 1a - get Flim -------------------------------------------------------
# Flim is derived from Blim by simulating the stock with segmented regression S-R function with the point of inflection at Blim 
# Flim = the F that, in equilibrium, gives a 50% probability of SSB > Blim. 
# Note this simulation should be conducted with:
#  fixed F (i.e. without inclusion of a Btrigger)
#  without inclusion of assessment/advice errors. 

SIM_Flim_segregBlim <- eqsim_run(FIT_segregBlim,  bio.years = c(maxYear-numAvgYrsB, maxYear-1), bio.const = TRUE, # FIT_All
                                 sel.years = c(maxYear-numAvgYrsS, maxYear-1), sel.const = TRUE,
                                 Fcv=0, Fphi=0, SSBcv=0,
                                 rhologRec=rhoRec,
                                 Btrigger = 0, Blim=refPts[,"Blim"],Bpa=refPts[,"Bpa"],
                                 Nrun=200, Fscan = seq(0,1.0,len=101),verbose=T)


# save MSY and lim values
tmp1 <- t(SIM_Flim_segregBlim$Refs2)
write.csv(tmp1, paste("output/ref_pts/EqSim_",runName,"_Flim_SegregBlim_eqRes.csv",sep=""))
refPts[,"Flim"] <- tmp1["F50","catF"]
refPts[,"Flim"] <- round(refPts[,"Flim"],3)

print(paste0("Flim = ",round(refPts[,"Flim"],3)))

# IF 5th PERCENTILE OF Bfmsy NEEDS TO BE CALCULATED (FOLLOW DECISION TREE FROM THE GUIDELINES),
# THEN UNCOMMENT AND EXECUTE THE CHUNK OF CODE BELOW

##################################################################################-
##################################################################################-
##################################################################################-

## Simuation 2a - get initial Fmsy -----------------------------------------------------------
# FMSY should initially be calculated based on:
#     a constant F evaluation 
#     with the inclusion of stochasticity in population and exploitation 
#     as well as assessment/advice error. 
#     Appropriate SRRs should be specified (here using all 3)

SIM_Fmsy_segregBlim_noTrig <- eqsim_run(FIT_segregBlim,  bio.years = c(maxYear-numAvgYrsB, maxYear-1), bio.const = FALSE,
                                        sel.years = c(maxYear-numAvgYrsS, maxYear-1), sel.const = FALSE,
                                        Fcv=cvF, Fphi=phiF, SSBcv=cvSSB,
                                        rhologRec=rhoRec,
                                        Btrigger = 0, Blim=refPts[,"Blim"],Bpa=refPts[,"Bpa"],
                                        Nrun=200, Fscan = seq(0,1.0,len=101),verbose=T)

# save MSY values
tmp2 <- t(SIM_Fmsy_segregBlim_noTrig$Refs2)
write.csv(tmp2, paste("output/ref_pts/EqSim_",runName,"_Fmsy_SegregBlim_noTrig_eqRes.csv",sep=""))
Fmsy_tmp <- tmp2["medianMSY","lanF"]
refPts[,"Fmsy_unconstr"] <- Fmsy_tmp

# save Equilibrium plots
if (savePlots) {
  x11()
  eqsim_plot(SIM_Fmsy_segregBlim_noTrig,catch=TRUE)  
  savePlot(paste("output/ref_pts/06_",runName,"_Fmsy_SegregBlim_noTrig_eqRes.png"),type="png")
  dev.off()
}

# check Fmsy unconstrain against the assessment results
if (savePlots) {
  fbar_10yrs <- fbartable(fit)[as.character((max(years)-9):max(years)),"Estimate"]
  fbarL_10yrs <- fbartable(fit)[as.character((max(years)-9):max(years)),"Low"]
  fbarH_10yrs <- fbartable(fit)[as.character((max(years)-9):max(years)),"High"]
  windows(height=7,width=11)
  plot(names(fbar_10yrs),fbar_10yrs,col="black",xlab="",ylab="Fbar",type="o")
  lines(names(fbar_10yrs),rep(Fmsy_tmp,10),col="blue")
  lines(names(fbarL_10yrs),fbarL_10yrs,col="black",lty="dotted")
  lines(names(fbarH_10yrs),fbarH_10yrs,col="black",lty="dotted")
  legend("topright",inset=0.02,legend=c(runName,"Fmsy_unconstr","5th/95th CI"),pch=c(1,NA,NA),col=c("black","blue","black"),lty=c("solid","solid","dotted"))
  savePlot(paste("output/ref_pts/06a_",runName,"_check_Fmsy_unconstr_vs_Fbar.png"),type="png")
  dev.off()
}

round(fbar_10yrs,2)


# Pause here to check the decision tree ----------------


## Simulation 1b to derive MSY Btrigger ---------------------------------------------------------------
if (1){
#flowchart - has stock been fished at or below Fmsy unconst for 5 yrs or more?

SIM_MSYBtrigger_segregBlim <- eqsim_run(FIT_segregBlim,  bio.years = c(maxYear-numAvgYrsB, maxYear-1), bio.const = TRUE,
                                    sel.years = c(maxYear-numAvgYrsS, maxYear-1), sel.const = TRUE,
                                    Fcv=0, Fphi=0, SSBcv=0,
                                    rhologRec=rhoRec,
                                    Btrigger = 0, Blim=refPts[,"Blim"], Bpa=refPts[,"Bpa"],
                                    Nrun=200, Fscan = seq(0,1.0,len=101), verbose=T)

if (savePlots) {x11()
  eqsim_plot(SIM_MSYBtrigger_segregBlim,catch=TRUE)
  savePlot(paste("output/ref_pts/07_",runName,"_MSYBtrigger_SegregBlim_eqMSYplots.png"),type="png")
  dev.off()
}

# Check Bmsy 5th percetnile
dbEq_Fmsy <- SIM_MSYBtrigger_segregBlim$rbp
Fs     <- dbEq_Fmsy[dbEq_Fmsy$variable == "Spawning stock biomass", ]$Ftarget
ssb.05 <- dbEq_Fmsy[dbEq_Fmsy$variable == "Spawning stock biomass", ]$p05
ssb.50 <- dbEq_Fmsy[dbEq_Fmsy$variable == "Spawning stock biomass", ]$p50

# 5th percentile
if (savePlots) {
  windows(height=7,width=11)
  plot(ssb.05~Fs, ylab="tonnes", xlab="F", main = '5% percentile of SSB versus F')
  abline(v=Fmsy_tmp) # unconstrained Fmsy
  i <- which(Fs<Flim)
  b.lm <- loess(ssb.05[i] ~ Fs[i],span=0.3)
  lines(Fs[i],c(predict(b.lm)),type='l')
  
  MSYBtrigger_temp <- round(predict(b.lm,Fmsy_tmp))
  abline(h=MSYBtrigger_temp, col = 2, lwd = 2)
  text(0.1,MSYBtrigger_temp*1.1, expression(MSYB[trigger_tmp]) )
  abline(h=Bpa, col = "lightgreen", lwd = 2)
  text(0.1,Bpa*1.1, expression( B[pa]) )
  savePlot(paste("output/ref_pts/07a_",runName,"_check_MSYBtrigger_Bpa.png"),type="png")
  dev.off()
}

# 50th percentile - Bmsy (?)
if (savePlots) {
  windows(height=7,width=11)
  plot(ssb.50~Fs, ylab="tonnes", xlab="F", main = '50% percentile of SSB versus F')
  abline(v=Fmsy_tmp) # unconstrained Fmsy
  i <- which(Fs<Flim)
  b.lm <- loess(ssb.50[i] ~ Fs[i],span=0.3)
  lines(Fs[i],c(predict(b.lm)),type='l')
  
  BMSY <- round(predict(b.lm,Fmsy_tmp))
  abline(h=BMSY, col = 2, lwd = 2)
  text(0.1,BMSY*1.1, expression(B[MSY]) )
 
  savePlot(paste("output/ref_pts/07a_",runName,"_check_BMSY.png"),type="png")
  dev.off()
}

tmp1 <- t(SIM_MSYBtrigger_segregBlim$Refs2)
write.csv(tmp1, paste("output/ref_pts/EqSim_",runName,"_MSYBtrigger_SegregBlim_eqRes.csv",sep=""))

fifthpercBfmsy<-SIM_MSYBtrigger_segregBlim$rbp[SIM_MSYBtrigger_segregBlim$rbp$variable =="Spawning stock biomass" & SIM_MSYBtrigger_segregBlim$rbp$Ftarget==round(Fmsy_tmp,2) , "p05"] # CHECK VALUE!
print(fifthpercBfmsy)

Btrig_tmp <- NULL
# decisiont ree - note that the logical statements here are inverted compared to the tree
Btrig_tmp <- ifelse(fifthpercBfmsy < refPts[,"Bpa"],refPts[,"Bpa"], # is 5th percentile of Bmsy < Bpa
                    ifelse(fifthpercBfmsy < Btrig, Btrig, # is 5th percentile of Bmsy < current MSY Btrigger
                           ifelse(fifthpercBfmsy < ssbtable(fit)[ac(ay),"Low"],round(fifthpercBfmsy), # is 5th percentile of Bmsy < 5th percentile of current (ay) SSB
                                  max(c(Btrig,fifthpercBfmsy))))) # if all are no then MSY Btrigger is max of current Btrig and 5th percentile of Bmsy


refPts[,"Btrigger"] <- round(Btrig_tmp)
}
##################################################################################-
##################################################################################-
##################################################################################-

## Simuation 2b - get final Fmsy ---------------------------------------------------------
# MSY Btrigger should be selected to safeguard against an undesirable or unexpected low SSB when fishing at FMSY
# The ICES MSY AR should be evaluated to check that the FMSY and MSY Btrigger combination adheres to precautionary considerations: 
#      in the long term, P(SSB<Blim)<5%
# The evaluation must include:
#      realistic assessment/advice error
#      stochasticity in population biology and fishery exploitation.
#      Appropriate SRRs should be specified


SIM_Fp05_segregBlim_Trig <- eqsim_run(FIT_segregBlim,  bio.years = c(maxYear-numAvgYrsB, maxYear-1), bio.const = FALSE, #FIT_segregBlim
                                      sel.years = c(maxYear-numAvgYrsS, maxYear-1), sel.const = FALSE,
                                      Fcv=cvF, Fphi=phiF, SSBcv=cvSSB,
                                      rhologRec=rhoRec,
                                      Btrigger = refPts[,"Btrigger"], Blim=refPts[,"Blim"],Bpa=refPts[,"Bpa"],
                                      Nrun=200, Fscan = seq(0,1.0,len=101),verbose=T)


# save MSY and lim values
tmp3 <- t(SIM_Fp05_segregBlim_Trig$Refs2)
write.csv(tmp3, paste("output/ref_pts/EqSim_",runName,"_Fp05_segregBlim_Trig_eqRes.csv",sep=""))
refPts[,"Fp05"] <- round(tmp3["F05","catF"],3)

# save Equilibrium plots
if (savePlots) {
  x11()
  eqsim_plot(SIM_Fp05_segregBlim_Trig,catch=TRUE)  
  savePlot(paste("output/ref_pts/07b_",runName,"_Fp05_segregBlim_Trig_eqMSYplots.png"),type="png")
  dev.off()
}

# To ensure consistency between the precautionary and MSY frameworks, FMSY is not allowed to be above Fp05
refPts[,"Fpa"] <- refPts[,"Fp05"]
if (Fmsy_tmp > refPts[,"Fpa"]) {
  print("WHOAAA, Fmsy > Fpa!") 
  refPts[,"Fmsy"] <- refPts[,"Fpa"] 
} else {
  refPts[,"Fmsy"] <- Fmsy_tmp
}

# If the precautionary criterion (FMSY < Fp.05) evaluated is not met, then FMSY upper should be reduced to  Fp.05. 
# then recalc Fmsy lower from Fp.05
if (Fmsy_tmp > refPts[,"Fp05"]) {
  print("WHOAAA, Fmsy > Fp05!") 
  refPts[,"Fmsy_upper"] <- round(refPts[,"Fp05"],3) # If Fmsy > Fp05, Fmsy = Fp05
  
  # Recalculate Fmsy lower as Fmsy is capped. May need to adjust Fscan range
  SIM_Fmsy_lower_segregBlim_Trig <- eqsim_run(FIT_segregBlim,  bio.years = c(maxYear-numAvgYrsB, maxYear-1), bio.const = FALSE,
                                              sel.years = c(maxYear-numAvgYrsS, maxYear-1), sel.const = FALSE,
                                              Fcv=cvF, Fphi=phiF, SSBcv=cvSSB,
                                              rhologRec=rhoRec,
                                              Btrigger = 0, Blim=refPts[,"Blim"],Bpa=refPts[,"Bpa"],
                                        Nrun=200, Fscan = seq(0,0.2,len=201),verbose=T)

  # Landings for Fmsy
  #library(data.table)

  lan <-   data.table(SIM_Fmsy_lower_segregBlim_Trig$rbp)[abs(Ftarget - refPts[,"Fmsy"]) == 
                                                            min(abs(Ftarget - refPts[,"Fmsy"])) & 
                                                            variable == 'Landings', p50]
  Flow_newref  <- data.table(SIM_Fmsy_lower_segregBlim_Trig$rbp)[variable == 'Landings' & p50 >= lan * 0.95, 
                                                                 Ftarget][1]
  
  
  data.table(SIM_Fmsy_lower_segregBlim_Trig$rbp)[abs(Ftarget - refPts[,"Fmsy"]) == 
                                                   min(abs(Ftarget - refPts[,"Fmsy"])) & 
                                                   variable == 'Landings',]
  
  refPts[,"Fmsy_lower"] <- round(Flow_newref ,3)
  
} 

refPts[,"Fp05"] <- round(refPts[,"Fp05"],3) 
refPts[,"Fmsy_unconstr"] <- round(refPts[,"Fmsy_unconstr"],3) 

# New guidelines, Fpa is no longer derived from Flim as below, but instead is Fp05 
# refPts[,"Fpa"] <- round(refPts[,"Flim"] * exp(-sigmaF * 1.645) , 3)
new_Fmsy <- refPts[,"Fmsy"] # Fp 0.5 is < Fmsy so Fmsy is capped it


##############################################-
# Extract yield data (landings) - mean version ------------------------------------------------
# Required input: x1.sim (output from eqsim_run)

if(TRUE){
  pdf.plots <-TRUE
  
  if (pdf.plots) pdf(file = paste0("output/ref_pts/",runName," - HAD2746a20 EqSim_ref point.pdf"), onefile = TRUE, width = 10, height = 7)
  
  
  x1.sim<-SIM_Fmsy_segregBlim_noTrig
  data.95 <- x1.sim$rbp
  x.95 <- data.95[data.95$variable == "Landings",]$Ftarget
  y.95 <- data.95[data.95$variable == "Landings",]$Mean
  x.95 <- x.95[2:length(x.95)]
  y.95 <- y.95[2:length(y.95)]
  
  # Plot curve with 95% line
  if (!pdf.plots) windows(width = 10, height = 7)
  par(mfrow = c(1,1), mar = c(5,4,2,1), mgp = c(3,1,0))
  plot(x.95, y.95, ylim = c(0, max(y.95, na.rm = TRUE)),
       xlab = "Total catch F", ylab = "Mean landings")
  yield.p95 <- 0.95 * max(y.95, na.rm = TRUE)
  abline(h = yield.p95, col = "blue", lty = 1)
  
  # Fit loess smoother to curve
  x.lm <- loess(y.95 ~ x.95, span = 0.2)
  lm.pred <- data.frame(x = seq(min(x.95), max(x.95), length = 1000),
                        y = rep(NA, 1000))
  lm.pred$y <- predict(x.lm, newdata = lm.pred$x) 
  lines(lm.pred$x, lm.pred$y, lty = 1, col = "red")
  points(x = x1.sim$Refs["lanF","meanMSY"], 
         y = predict(x.lm, newdata = x1.sim$Refs["lanF","meanMSY"]),
         pch = 16, col = "blue")
  
  # Limit fitted curve to values greater than the 95% cutoff
  lm.pred.95 <- lm.pred[lm.pred$y >= yield.p95,]
  fmsy.lower <- min(lm.pred.95$x)
  fmsy.upper <- max(lm.pred.95$x)
  abline(v = c(fmsy.lower, fmsy.upper), lty = 8, col = "blue")
  abline(v = x1.sim$Refs["lanF","meanMSY"], lty = 1, col = "blue")
  legend(x = "topright", bty = "n", cex = 1.0, 
         title = "F(msy)", title.col = "blue",
         legend = c(paste0("lower = ", round(fmsy.lower,5)),
                    paste0("mean = ", round(x1.sim$Refs["lanF","meanMSY"],5)), 
                    paste0("upper = ", round(fmsy.upper,5))))
  
  fmsy.lower.mean <- fmsy.lower
  fmsy.upper.mean <- fmsy.upper
  landings.lower.mean <- lm.pred.95[lm.pred.95$x == fmsy.lower.mean,]$y	
  landings.upper.mean <- lm.pred.95[lm.pred.95$x == fmsy.upper.mean,]$y
  
  
  # Repeat for 95% of yield at F(05):
  f05 <- x1.sim$Refs["catF","F05"]
  yield.f05 <- predict(x.lm, newdata = f05)
  points(f05, yield.f05, pch = 16, col = "green")
  yield.f05.95 <- 0.95 * yield.f05
  abline(h = yield.f05.95, col = "green")
  lm.pred.f05.95 <- lm.pred[lm.pred$y >= yield.f05.95,]
  f05.lower <- min(lm.pred.f05.95$x)
  f05.upper <- max(lm.pred.f05.95$x)
  abline(v = c(f05.lower,f05.upper), lty = 8, col = "green")
  abline(v = f05, lty = 1, col = "green")
  legend(x = "right", bty = "n", cex = 1.0, 
         title = "F(5%)", title.col = "green",
         legend = c(paste0("lower = ", round(f05.lower,5)),
                    paste0("estimate = ", round(f05,5)),
                    paste0("upper = ", round(f05.upper,5))))
  
  
  ################################################
  # Extract yield data (landings) - median version ------------------------------------------------------------
  # Required input: x1.sim (output from eqsim_run)
  
  x1.sim<-SIM_Fmsy_segregBlim_noTrig
  data.95 <- x1.sim$rbp
  x.95 <- data.95[data.95$variable == "Landings",]$Ftarget
  y.95 <- data.95[data.95$variable == "Landings",]$p50
  
  # Plot curve with 95% line
  if (!pdf.plots) windows(width = 10, height = 7)
  par(mfrow = c(1,1), mar = c(5,4,2,1), mgp = c(3,1,0))
  plot(x.95, y.95, ylim = c(0, max(y.95, na.rm = TRUE)),
       xlab = "Total catch F", ylab = "Median landings")
  yield.p95 <- 0.95 * max(y.95, na.rm = TRUE)
  abline(h = yield.p95, col = "blue", lty = 1)
  
  # Fit loess smoother to curve
  x.lm <- loess(y.95 ~ x.95, span = 0.2)
  lm.pred <- data.frame(x = seq(min(x.95), max(x.95), length = 1000),
                        y = rep(NA, 1000))
  lm.pred$y <- predict(x.lm, newdata = lm.pred$x) 
  lines(lm.pred$x, lm.pred$y, lty = 1, col = "red")
  
  # Find maximum of fitted curve - this will be the new median (F(msy)
  Fmsymed <- lm.pred[which.max(lm.pred$y),]$x
  Fmsymed.landings <- lm.pred[which.max(lm.pred$y),]$y
  
  # Overwrite Refs table
  x1.sim$Refs[,"medianMSY"] <- NA
  x1.sim$Refs["lanF","medianMSY"] <- Fmsymed
  x1.sim$Refs["landings","medianMSY"] <- Fmsymed.landings
  
  # Add maximum of medians to plot
  points(x = x1.sim$Refs["lanF","medianMSY"], 
         y = predict(x.lm, newdata = x1.sim$Refs["lanF","medianMSY"]),
         pch = 16, col = "blue")
  
  # Limit fitted curve to values greater than the 95% cutoff
  lm.pred.95 <- lm.pred[lm.pred$y >= yield.p95,]
  fmsy.lower <- min(lm.pred.95$x)
  fmsy.upper <- max(lm.pred.95$x)
  abline(v = c(fmsy.lower, fmsy.upper), lty = 8, col = "blue")
  abline(v = x1.sim$Refs["lanF","medianMSY"], lty = 1, col = "blue")
  legend(x = "topright", bty = "n", cex = 1.0, 
         title = "F(MSY) NAR", title.col = "blue",
         legend = c(paste0("lower = ", round(fmsy.lower,5)),
                    paste0("median = ", round(x1.sim$Refs["lanF","medianMSY"],5)), 
                    paste0("upper = ", round(fmsy.upper,5))))
  
  fmsy.lower.median <- fmsy.lower
  fmsy.upper.median <- fmsy.upper
  landings.lower.median <- lm.pred.95[lm.pred.95$x == fmsy.lower.median,]$y
  landings.upper.median <- lm.pred.95[lm.pred.95$x == fmsy.upper.median,]$y
  
  
  # Repeat for 95% of yield at F(05):
  f05 <- x1.sim$Refs["catF","F05"]
  yield.f05 <- predict(x.lm, newdata = f05)
  points(f05, yield.f05, pch = 16, col = "green")
  yield.f05.95 <- 0.95 * yield.f05
  abline(h = yield.f05.95, col = "green")
  lm.pred.f05.95 <- lm.pred[lm.pred$y >= yield.f05.95,]
  f05.lower <- min(lm.pred.f05.95$x)
  f05.upper <- max(lm.pred.f05.95$x)
  abline(v = c(f05.lower,f05.upper), lty = 8, col = "green")
  abline(v = f05, lty = 1, col = "green")
  legend(x = "right", bty = "n", cex = 1.0, 
         title = "F(5%) NAR", title.col = "green",
         legend = c(paste0("lower = ", round(f05.lower,5)),
                    paste0("estimate = ", round(f05,5)),
                    paste0("upper = ", round(f05.upper,5))))
  
  
  ##### SSB plot
  x1.sim<-SIM_Fmsy_segregBlim_noTrig
  data.95 <- x1.sim$rbp
  
  x.95 <- data.95[data.95$variable == "Spawning stock biomass",]$Ftarget
  b.95 <- data.95[data.95$variable == "Spawning stock biomass",]$p50
  
  # Plot curve with 95% line
  if (!pdf.plots) windows(width = 10, height = 7)
  par(mfrow = c(1,1), mar = c(5,4,2,1), mgp = c(3,1,0))
  plot(x.95, b.95, ylim = c(0, max(b.95, na.rm = TRUE)),
       xlab = "Total catch F", ylab = "Median SSB")
  
  # Fit loess smoother to curve
  b.lm <- loess(b.95 ~ x.95, span = 0.2)
  b.lm.pred <- data.frame(x = seq(min(x.95), max(x.95), length = 1000),
                          y = rep(NA, 1000))
  b.lm.pred$y <- predict(b.lm, newdata = b.lm.pred$x) 
  lines(b.lm.pred$x, b.lm.pred$y, lty = 1, col = "red")
  
  # Estimate SSB for median F(msy) and range
  b.msymed <- predict(b.lm, newdata = Fmsymed)
  b.medlower <- predict(b.lm, newdata = fmsy.lower.median)
  b.medupper <- predict(b.lm, newdata = fmsy.upper.median)
  abline(v = c(fmsy.lower.median, Fmsymed, fmsy.upper.median), col = "blue", lty = c(8,1,8))
  points(x = c(fmsy.lower.median, Fmsymed, fmsy.upper.median), 
         y = c(b.medlower, b.msymed, b.medupper), col = "blue", pch = 16)
  legend(x = "topright", bty = "n", cex = 1.0, 
         title = "F(msy)", title.col = "blue",
         legend = c(paste0("lower = ", round(b.medlower,0)),
                    paste0("median = ", round(b.msymed,0)),
                    paste0("upper = ", round(b.medupper,0))))
  
  

 #### Only run this next bit if Fmsy is constrained --------------------------------------------
  
  
  x1.sim<-SIM_Fp05_segregBlim_Trig
  data.95 <- x1.sim$rbp
  x.95 <- data.95[data.95$variable == "Landings",]$Ftarget
  y.95 <- data.95[data.95$variable == "Landings",]$p50
  
  # Plot curve with 95% line
  if (!pdf.plots) windows(width = 10, height = 7)
  par(mfrow = c(1,1), mar = c(5,4,2,1), mgp = c(3,1,0))
  plot(x.95, y.95, ylim = c(0, max(y.95, na.rm = TRUE)),
       xlab = "Total catch F", ylab = "Median landings")
  yield.p95 <- 0.95 * max(y.95, na.rm = TRUE)
  abline(h = yield.p95, col = "blue", lty = 1)
  
  # Fit loess smoother to curve
  x.lm <- loess(y.95 ~ x.95, span = 0.2)
  lm.pred <- data.frame(x = seq(min(x.95), max(x.95), length = 1000),
                        y = rep(NA, 1000))
  lm.pred$y <- predict(x.lm, newdata = lm.pred$x) 
  lines(lm.pred$x, lm.pred$y, lty = 1, col = "red")
  
  # Find maximum of fitted curve - this will be the new median (F(msy)
  Fmsymed <- lm.pred[which.max(lm.pred$y),]$x
  Fmsymed.landings <- lm.pred[which.max(lm.pred$y),]$y
  
  # Overwrite Refs table
  x1.sim$Refs[,"medianMSY"] <- NA
  x1.sim$Refs["lanF","medianMSY"] <- Fmsymed
  x1.sim$Refs["landings","medianMSY"] <- Fmsymed.landings
  
  # Add maximum of medians to plot
  points(x = x1.sim$Refs["lanF","medianMSY"],
         y = predict(x.lm, newdata = x1.sim$Refs["lanF","medianMSY"]),
         pch = 16, col = "blue")

  # # Limit fitted curve to values greater than the 95% cutoff
  lm.pred.95 <- lm.pred[lm.pred$y >= yield.p95,]
  fmsy.lower <- min(lm.pred.95$x)
  fmsy.upper <- max(lm.pred.95$x)
  abline(v = c(fmsy.lower, fmsy.upper), lty = 8, col = "blue")
  abline(v = x1.sim$Refs["lanF","medianMSY"], lty = 1, col = "blue")
  legend(x = "topright", bty = "n", cex = 1.0,
         title = "F(MSY) AR", title.col = "blue",
         legend = c(paste0("lower = ", round(fmsy.lower,5)),
                    paste0("median = ", round(x1.sim$Refs["lanF","medianMSY"],5)),
                    paste0("upper = ", round(fmsy.upper,5))))


  
  f05a <- new_Fmsy # Current estimate of F(msy)
  yield.f05a <- predict(x.lm, newdata = f05a)
  points(f05a, yield.f05a, pch = 16, col = "red")
  yield.f05a.95 <- 0.95 * yield.f05a
  abline(h = yield.f05a.95, col = "red")
  lm.pred.f05a.95 <- lm.pred[lm.pred$y >= yield.f05a.95,]
  f05a.lower <-min(lm.pred.f05a.95$x) ############### THIS ONE!
  f05a.upper <-max(lm.pred.f05a.95$x)
  abline(v = c(f05a.lower,f05a.upper), lty = 8, col = "red")
  abline(v = f05a, lty = 1, col = "red")
  #abline(v = refPts[,"Fpa"], lty = 1, col = "green")  #fpa for plot
  legend(x = "right", bty = "n", cex = 1.0,
         title = "F(5%) AR", title.col = "red", # using AR and error
         legend = c(paste0("lower = ", round(f05a.lower,5)),
                    paste0("estimate = ", round(f05a,5)),
                    paste0("upper = ", round(f05a.upper,5))))
  # 	 legend(0.85,5000, bty = "n", cex = 1.0,
  #          title = "F(pa)",legend =paste("estimate =", refPts[,"Fpa"]), title.col = "green"
  #          )
  
f05a
yield.f05a
yield.f05a.95
  
  
  # Update summary table with John's format
  
  x1.sim$Refs <- x1.sim$Refs[,!(colnames(x1.sim$Refs) %in% c("FCrash05","FCrash50"))]
  x1.sim$Refs <- cbind(x1.sim$Refs, Medlower = rep(NA,6), Meanlower = rep(NA,6), 
                       Medupper = rep(NA,6), Meanupper = rep(NA,6))
  
  x1.sim$Refs["lanF","Medlower"] <- fmsy.lower.median
  x1.sim$Refs["lanF","Medupper"] <- fmsy.upper.median
  x1.sim$Refs["lanF","Meanlower"] <- fmsy.lower.mean
  x1.sim$Refs["lanF","Meanupper"] <- fmsy.upper.mean
  
  x1.sim$Refs["landings","Medlower"] <- landings.lower.median
  x1.sim$Refs["landings","Medupper"] <- landings.upper.median
  x1.sim$Refs["landings","Meanlower"] <- landings.lower.mean
  x1.sim$Refs["landings","Meanupper"] <- landings.upper.mean
  
  x1.sim$Refs["lanB","medianMSY"] <- b.msymed
  x1.sim$Refs["lanB","Medlower"] <- b.medlower
  x1.sim$Refs["lanB","Medupper"] <- b.medupper
  
  #library(gplots)
  # Reference point estimates
  cat("\nReference point estimates:\n")
  print(round(x1.sim$Refs,5))
  par(mfrow = c(1,1), mar = c(5,4,2,1), mgp = c(3,1,0))
  textplot(round(x1.sim$Refs,5), rmar = 1.0, cex = 0.60)
  
  if (pdf.plots) dev.off()				
}

# Calculate Bmsy -----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 7.  Equilibrium analysis to calculate ** Bmsy **  ----#
# *** ICES guidelines ***^
# Bmsy corresponding to Fmsy in the equilibrium analysis but without
# introducing assessment error in the simulation. (i.e eqPop_Flim)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Fscan <- seq(0,1.5,length = 200)  
Fscan <- seq(0,1.0,len=101)

SSBFscan_p50 <- SIM_Flim_segregBlim$rbp$p50[SIM_Flim_segregBlim$rbp$variable=="Spawning stock biomass"]
## Interpolate to get SSB for more F values, percentile 50 of SSB for (SSB,F) pairs. 
SSBF_p50 <- as.data.frame(approx(Fscan,                             # The F-s for which we have F
                                 SSBFscan_p50,                      # The p50 SSB-s corresponding to Fscan
                                 xout = seq(min(Fscan), max(Fscan),length=1000))) # F values over we want to interpolate
names(SSBF_p50) <- c('F', 'SSB')

# BMSY: The SSB corresponding to the F closest to Fmsy.
Bmsy <- SSBF_p50$SSB[which.min(abs(SSBF_p50$'F' - refPts[,"Fmsy"]))]
refPts[,"Bmsy"] <- round(Bmsy)

plot(SSBF_p50$'F',SSBF_p50$SSB)
points(refPts[,"Fmsy"],Bmsy,col="red")

## Save reference points --------------------------------------------------------
write.csv(refPts, paste("output/ref_pts/",runName,"_RefPts.csv",sep=""))


## Save run settings -----------------------------------------------------------------------
SRused <- "SegregBlim" #appModels[1]
if (length(appModels)>1) for (i in 2:length(appModels)) SRused <- paste(SRused,appModels[i],sep="_")
SRyears_min <- min(srYears); SRyears_max <- max(srYears)
setList <- c("stockName", "runName", "SAOAssessment", "sigmaF", "sigmaSSB", "noSims", "SRused", "SRyears_min", 
             "SRyears_max", "acfRecLag1","rhoRec", "numAvgYrsB", "numAvgYrsS", "cvF", "phiF", "cvSSB", "phiSSB")
runSet <- matrix(NA,ncol=1, nrow=length(setList), dimnames=list(setList,c("Value")))
for (i in setList) runSet[which(setList==i),] <- eval(parse(text = i))

write.csv(runSet, paste("output/ref_pts/",runName,"_RunSettings.csv",sep=""))


## Save workspace -------------------------------------------------------------------
save.image(file=paste("output/ref_pts/",runName,"_",maxYear,"_EqSim_Workspace.Rdata",sep=""))
#load(paste("output/ref_pts/",runName,"_",maxYear,"_EqSim_Workspace.Rdata",sep=""))


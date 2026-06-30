## Run analysis, write model results

## Before:
## After:

#--------------------------------------------------------------------##
# Forecast setup ####

rm(list=ls())
sourceDir("boot/software/utilities/")

load("data/init.RData")

## Stochastic single fleet SAM forecast #
#The forecast is conducted via the standard forecast function in SAM. In the
#assessment only the Quarter 1 survey is observed in the last assessment
#year , so the forcast uses the year before as the base for
#projecting forward 3 years 
#
#The last state estimates are used (logN and logF in data year) are sampled 10000
#times to capture the estimation uncertainty. 
#
#Age-specific averages of the last 3 years are used for
#some biological parameters (mo, nm, lf) in the forecast years, but the weights
#(cw, sw, lw, dw) in the forecast period are estimated via the Jaworski method. 
#
#Recruitment in the forecast period is sampled with replacement from the
#recruitment estimates in the period 2000-data year. 
#
#The selectivity used in the forecast period is the average selectivity of the
#last 3 years 

assess_year <- ay # the intermediate year when assessment is being conducted
advice_year <- ay+1 # the year for TAC advice
data_yrs <- 1972:(ay-1)

## Forecast parameters:
NsimForecast <- 10000

Ay <- (ay-3):(ay-1) # for biols
Sy <- (ay-3):(ay-1)  # for sel
Ry <- 2000:(ay-1) # for rec 
length(Ry)

# load run
runName <- "NShaddock_WGNSSK2026_Run1"
load(paste0("model/SAM/",runName,"/model.RData"))

# Fsq reference - Fbar from final year of catch data or 3 year mean? Is there a trend?
#Fsq <- round(fbartable(fit)[as.character(assess_year-1),1],3)
Fsq <- round(mean(fbartable(fit)[as.character(c(assess_year-3):(assess_year-1)),1]),3)

# update forcast weights - Jaworski method
# replace forecast weights with cohort modelled weights
ca.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - catch-at-age.txt")
st.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - stock-at-age.txt")
lan.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - landings-at-age.txt")
dis.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - discards-at-age incl BMS and IBC.txt")

colnames(ca.frct.wt) <- colnames(st.frct.wt) <- colnames(lan.frct.wt) <- colnames(dis.frct.wt) <- colnames(fit$data$catchMeanWeight)

ac<-as.character

fit$data$catchMeanWeight <- rbind(fit$data$catchMeanWeight[ac(data_yrs),,"Residual catch"],ca.frct.wt)
fit$data$stockMeanWeight <- rbind(fit$data$stockMeanWeight[ac(data_yrs),],st.frct.wt) 
fit$data$landMeanWeight <- rbind(fit$data$landMeanWeight[ac(data_yrs),,"Residual catch"],lan.frct.wt)
fit$data$disMeanWeight <- rbind(fit$data$disMeanWeight[ac(data_yrs),,"Residual catch"],dis.frct.wt) 

ca.frct.wt$Year <- st.frct.wt$Year <- lan.frct.wt$Year <- dis.frct.wt$Year <- row.names(ca.frct.wt)
ca.frct.wt$Cat <- "catch"
st.frct.wt$Cat <- "stock"
lan.frct.wt$Cat <- "landings"
dis.frct.wt$Cat <- "discards"

# Data are provided for ay (Q1 survey) - Mat and NM values are provided but we want to use 3 year mean!
# adjust ay values for Mat and NM so the 3 year mean value is used
# perhaps there is a better way to do this??
fit$data$propMat[ac(ay),] <- round(colMeans(fit$data$propMat[ac(Ay),]),3)
fit$data$natMor[ac(ay),] <- round(colMeans(fit$data$natMor[ac(Ay),]),3)

# change some dimensions
fit$data$landFrac <- fit$data$landFrac[,,"Residual catch"]
fit$data$propF <- fit$data$propF[,,"Residual catch"]

# projection: Fsq
set.seed(211988)
tmp1 <- forecast(fit = fit, ave.years = Ay, rec.years = Ry,
                 nosim = NsimForecast,year.base=assess_year-1, 
                 fval = c(Fsq, Fsq, Fmsy, Fmsy),
                 #  catchval = c(NA,NA,NA,NA),
                 overwriteSelYears=Sy,processNoiseF=FALSE,
                 label = "Fsq, Fmsy", splitLD = TRUE,savesim = TRUE)

SSB_advice_year <- attr(tmp1, "tab")[as.character(advice_year), "ssb:median"]
catch_int_year <- attr(tmp1, "tab")[as.character(assess_year), "catch:median"]

# is a TAC constraint needed?
if(catch_int_year > TAC){
  print("Fsq catch in intermediate year exceeds TAC. TAC constraint needed")



# projection: with TAC constraint
set.seed(211988)
tmp2 <- forecast(fit = fit, ave.years = Ay, rec.years = Ry,
                 nosim = NsimForecast,year.base=assess_year-1,
                 fval = c(Fsq, NA, Fmsy, Fmsy),
                 catchval = c(NA,TAC,NA,NA),
                 overwriteSelYears=Sy,processNoiseF=FALSE,
                 label = "TACcont, Fmsy", splitLD = TRUE,savesim = TRUE)

SSB_advice_year_TAC <- attr(tmp2, "tab")[as.character(advice_year), "ssb:median"]
catch_int_year_TAC <- attr(tmp2, "tab")[as.character(assess_year), "catch:median"]
Fbar_int_year_TAC <- attr(tmp2, "tab")[as.character(assess_year), "fbar:median"]

Fintyr <- Fbar_int_year_TAC
save(list(tmp1,tmp2), file="model/SAM/temp_forecast.RData")

}else{
  Fintyr <- Fsq
  save(tmp1, file="model/SAM/temp_forecast.RData")
  
}
print(Fintyr)

## main scenarios:
scen1 <- list()

scen1[["Fsq, then Fmsy"]] <- list(fval = c(Fsq, Fsq, Fmsy, Fmsy),
                                      catchval = c(NA,NA,NA,NA))

scen1[["Fsq, then FmsyLower"]] <- list(fval = c(Fsq, Fsq, Fmsy_lo, Fmsy_lo),
                                           catchval = c(NA,NA,NA,NA))

scen1[["Fsq, then FmsyHigher"]] <- list(fval = c(Fsq, Fsq, Fmsy_hi, Fmsy_hi),
                                            catchval = c(NA,NA,NA,NA))

scen1[["Fsq, then 0"]] <- list(fval = c(Fsq, Fsq, 0.00000001, 0.00000001),  
                                   catchval = c(NA,NA,NA,NA))

scen1[["Fsq, then Fpa"]] <- list(fval = c(Fsq, Fsq, Fpa, Fpa), 
                                     catchval = c(NA,NA,NA,NA))

scen1[["Fsq, then Fp.05"]] <- list(fval = c(Fsq, Fsq, Fp.05, Fp.05), 
                                       catchval = c(NA,NA,NA,NA))

scen1[["Fsq, then Flim"]] <- list(fval = c(Fsq, Fsq, Flim, Flim), 
                                      catchval = c(NA,NA,NA,NA))

scen1[[paste0("Fsq, then SSB(", advice_year+1, ") = Blim")]] <-
  list(fval = c(Fsq,Fsq, NA, NA),
       catchval=c(NA,NA,NA,NA),
       nextssb = c(NA, NA, Blim,Blim))

scen1[[paste0("Fsq, then SSB(", advice_year+1, ") = Bpa = MSY Btrigger")]] <-
  list( fval = c(Fsq,Fsq, NA, NA),
        catchval=c(NA,NA,NA,NA),
        nextssb = c(NA, NA, Btrig,Btrig))

scen1[[paste0("Fsq, then F",ay)]] <- list(fval = c(Fsq,Fsq,Fintyr,Fintyr),
                                              catchval = c(NA,NA,NA,NA))

scen1[["Fsq, then MSYrule"]] <- list(fval = c(Fsq,Fsq,rep(Fmsy * min(c(1, SSB_advice_year / Btrig)), 2)),
                                         catchval = c(NA,NA,NA,NA))
#scen1[["Fsq, then MSYrule"]] <- list(fval = c(Fsq,Fsq,rep(Fmsy * min(c(1, SSB_advice_year_TAC / Btrig)), 2)),
 #                                    catchval = c(NA,NA,NA,NA))

scen1[["Fsq, then rolloverTAC"]] <- list(fval = c(Fsq,Fsq,NA,NA),
                                             catchval = c(NA,NA,TAC,TAC))

## F-step scenarios (0.01 steps between 0 and Fupper)
FMSYs <- seq(0.01, round(Fmsy_hi, 2), 0.01)
scen2 <- vector("list", length(FMSYs))
names(scen2) <- paste("Fsq, then FMSY =", sprintf(fmt = "%.2f", FMSYs) )
for(i in seq(scen2)){
  scen2[[i]] <- list(fval = c(Fsq,Fsq,rep(FMSYs[i],2)),
                     catchval = c(NA,NA,NA,NA))
}

reopening <- FALSE
## Combine all scenarios, and give year names for clarity
scen <- c(scen1, scen2)
names(scen)
argNames <- if(reopening){
  argNames <- assess_year + (0:3)
} else {
  argNames <- assess_year + (-1:2)
}
for(i in seq(scen)){
  for(j in seq(scen[[i]])){
    names(scen[[i]][[j]]) <- argNames
  }
}

##-----------------------------------------------------------------------------##
## perform forecasts ####

FC <- vector("list", length(scen))
names(FC) <- names(scen)
for(i in seq(scen)){
  set.seed(211988)
  
  ARGS <- scen[[i]]
  ARGS <- c(ARGS,
            list(fit = fit, ave.years = Ay, rec.years = Ry, nosim = NsimForecast,year.base=assess_year-1,
                 label = names(scen)[i], overwriteSelYears = Sy, processNoiseF=FALSE, splitLD = TRUE, savesim = TRUE))
  
  FC[[i]] <- do.call(forecast, ARGS)
 
  print(paste0("forecast : ", "'", names(scen)[i], "'", " is complete"))
}


{ # Optimization to reach precise SSB targets:
  timeSaved <-  Sys.time()
  message("## starting optimization at ", timeSaved)
  
  ## Additional solver for more precise matching of median value for SSB targets
  scen_num <- which(grepl(paste0("then SSB(", advice_year+1, ") = "),
                          names(scen), fixed = TRUE))
  
  FC2 <- vector("list", length(scen_num))
  names(FC2) <- names(scen)[scen_num]
  for(i in seq(FC2)){
    
    ARGS <- scen[[scen_num[i]]]
    ARGS <- c(ARGS,
              list(fit = fit, ave.years = Ay, rec.years = Ry, nosim = NsimForecast,year.base=assess_year-1,
                   label = names(scen)[scen_num[i]],processNoiseF=FALSE, overwriteSelYears = Sy, splitLD = TRUE,savesim=T))
    
    fun <- function(fval = 0.35, ARGS){
      set.seed(211988)
      ARGS2 <- ARGS
      ARGS2$nextssb <- c(NA, NA, NA, NA)
      ARGS2$fval[which(as.numeric(names(ARGS2$fval)) > (assess_year))] <- fval
      
      fc <- do.call(forecast, ARGS2)
      fc_tab <- attr(fc, "tab")
      ssbmed <- fc_tab[rownames(fc_tab) == as.character(assess_year+2),
                       colnames(fc_tab) == "ssb:median"]
      fitness <- sqrt((ssbmed - ARGS$nextssb[4])^2)
      return(fitness)
    }
    
    message("\n## Optimization for scenario \"",
            names(scen)[scen_num][i], "\"...")
    ## Non optimized scenario should constitute a safe starting point:
    Fstart <- attr(FC[[scen_num[i]]],
                   "tab")[as.character(assess_year+1) ,
                          "fbar:median"]
    
    Frange <- Fstart * c(0.5, 1.2) # -/+ 10%
    
    system.time(
      res <- optim(par = Fstart, fn = fun, ARGS = ARGS,
                   lower = Frange[1], upper = Frange[2],
                   method = "Brent",
                   control = list(## trace = 4,
                    #  factr = 1e-5,
                     abstol = 0.049
                   )))
    
    set.seed(211988)
    ARGS2 <- ARGS
    ARGS2$nextssb <- c(NA, NA, NA, NA)
    ARGS2$fval[which(as.numeric(names(ARGS2$fval)) > (assess_year))] <- res$par
    
    FC2[[i]] <- do.call(forecast, ARGS2)
    attr(FC2[[i]], "tab")
    
  }

  timeEnd <-  Sys.time()
  timeDiff <- timeEnd - timeSaved
  
  message("## Finishing optimization at ", timeEnd,
          "\n## Elapsed time: ", round(timeDiff, 1), " ", attr(timeDiff, "unit"))
}


#save(tmp1,tmp2, FC, FC2, file="model/SAM/forecast.RData")
save(FC, FC2, file="model/SAM/forecast.RData")


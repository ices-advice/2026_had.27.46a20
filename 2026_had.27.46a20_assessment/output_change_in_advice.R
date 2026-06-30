
# Make csv outputs for chagne in advice Rmarkdown
rm(list=ls())
graphics.off()

# for inclusion in report.Rmd
load("data/init.RData")
sourceDir("boot/software/utilities/")

# Inputs:
# Forecast_assumptions.csv - table of recruitment, SSB, Fbar and total catch from this year's WG and last year's WG for the data year, intermediate year and advice year.
# Forecast_stockwts.csv - table of stock weights at age used in the forecast from this year's WG and last year's WG for the data year, intermediate year and advice year.
# Forecast_selectivity.csv - table of selectivity at age used in the forecast from this year's WG and last year's WG for the data year, intermediate year and advice year.
# Forecast_N_at_age.csv - table of stock numbers at age used in the forecast from this year's WG and last year's WG for the data year, intermediate year and advice year.
# Compare_forecast_B_at_age.csv - table of stock biomass at age used in the forecast from this year's WG and last year's WG for the data year, intermediate year and advice year.
# Now_assessment_N_at_age.csv - table of stock numbers at age from THIS year's assessment. Usually a standard output for the WG report.
# Now_assessment_B_at_age.csv - table of stock biomass at age from THIS year's assessment. Multiply "now_assessment_N_at_age.csv" by stock weights at age
# Prev_assessment_N_at_age.csv - table of stock numbers at age from LAST year's assessment. Usually a standard output for the WG report.
# Prev_assessment_B_at_age.csv - table of stock biomass at age from LAST year's assessment. Multiply "now_assessment_N_at_age.csv" by stock weights at age


# Need to think about if I should use median value for Rec or sub in geometric mean value??

# settings ----------------------------------------
output.dir <- "output/Change_in_advice/"

data_yrs <- 1972:(ay-1)
advice_year <- ay+1

## Forecast parameters:
Ay <- (ay-3):(ay-1) # for biols
Sy <- (ay-3):(ay-1)  # for sel
Ry <- 2000:(ay-1) # for rec

ac<-as.character

# load assessments and forecasts -------------------------------------
load("model/SAM/NShaddock_WGNSSK2025_Run1/model.RData") # last year's
prev_ass <- fit
load("boot/data/forecast - WGNSSK 2025.RData")
prev_FC1 <- FC1
prev_frcst_fit <- attr(prev_FC1, "fit")

# load this year's assessment and forecast
load("model/SAM/NShaddock_WGNSSK2026_Run1/model.RData")

load("model/SAM/forecast.RData")

# Forecast_assumptions.csv  -----------------------------------------------

# this year
tab1 <- data.frame(WG =paste0("WGNSSK ",ay),Variable= sort(rep(c("SSB","Recruitment","Fbar","Total catch"),3)), 
                   Year = rep((ay-1):(ay+1),4), Type=NA, Value= NA, Source = "Forecast")

tab1$Type[tab1$Year %in% (ay-1)] <- "Data year"
tab1$Type[tab1$Year %in% (ay)] <- "Intermediate year"
tab1$Type[tab1$Year %in% (ay+1)] <- "Advice year"

tab1$Source[tab1$Year %in% (ay-1)] <- "Assessment"

astab <- as.data.frame(summary(fit))
astab$Year <- rownames(astab)
tmp_tab <- catchtable(fit)
fctab <- attr(FC[[1]],"tab")

# data year values
idx <- which(astab$Year %in% c(ay-1)) # data yr
tab1$Value[tab1$Year %in% (ay-1) & tab1$Variable %in% "Recruitment"] <- astab$"R(age 0)"[idx] # data year
tab1$Value[tab1$Year %in% (ay-1) & tab1$Variable %in% "SSB"] <- astab$SSB[idx] # data year
tab1$Value[tab1$Year %in% (ay-1) & tab1$Variable %in% "Fbar"] <- astab$"Fbar(2-4)"[idx] # data year
tab1$Value[tab1$Year %in% (ay-1) & tab1$Variable %in% "Total catch"] <- catchtable(fit)[as.character(ay-1),"Estimate"] # data year

# int year values
tab1$Value[tab1$Year %in% (ay) & tab1$Variable %in% "SSB"] <- fctab[as.character(ay),"ssb:median"]
tab1$Value[tab1$Year %in% (ay) & tab1$Variable %in% "Fbar"] <- fctab[as.character(ay),"fbar:median"]
tab1$Value[tab1$Year %in% (ay) & tab1$Variable %in% "Total catch"] <- fctab[as.character(ay),"catch:median"]
tab1$Value[tab1$Year %in% (ay) & tab1$Variable %in% "Recruitment"] <- fctab[as.character(ay),"rec:median"]

# advice year values
tab1$Value[tab1$Year %in% (ay+1) & tab1$Variable %in% "SSB"] <- fctab[as.character(ay+1),"ssb:median"]
tab1$Value[tab1$Year %in% (ay+1) & tab1$Variable %in% "Fbar"] <- fctab[as.character(ay+1),"fbar:median"]
tab1$Value[tab1$Year %in% (ay+1) & tab1$Variable %in% "Total catch"] <- fctab[as.character(ay+1),"catch:median"]
tab1$Value[tab1$Year %in% (ay+1) & tab1$Variable %in% "Recruitment"] <- fctab[as.character(ay+1),"rec:median"]

# Replace recruitment with geometric mean value
Ry <- 2000:(ay-1)
R <- rectable(fit)[,1]
R <- R[as.character(1972:(ay-1))]
R_geoMean <- exp(mean(log(R[ac(Ry)]))) # better summary stat for tables and plots when length(Ry) is even

tab1$Value[tab1$WG %in% paste0("WGNSSK ",ay) & tab1$Variable %in% "Recruitment" & !tab1$Source %in% "Assessment"] <- R_geoMean


# last year
tab2 <- data.frame(WG =paste0("WGNSSK ",ay-1),Variable= sort(rep(c("SSB","Recruitment","Fbar","Total catch"),3)), 
                   Year = rep((ay-2):(ay),4), Type=NA, Value= NA, Source = "Forecast")

tab2$Type[tab2$Year %in% (ay-2)] <- "Data year"
tab2$Type[tab2$Year %in% (ay-1)] <- "Intermediate year"
tab2$Type[tab2$Year %in% (ay)] <- "Advice year"

tab2$Source[tab2$Year %in% (ay-2)] <- "Assessment"

# Get values from SAG (inlcuding geometric mean for rec) and previous forecast
astab <- getSAG(stock = "had.27.46a20",year=(ay-1),purpose="Advice")
fctab <- attr(prev_FC1,"tab")
astab$recruitment[astab$Year == ay-1]

# data year values
idx <- which(astab$Year %in% c(ay-2)) # data yr
tab2$Value[tab2$Year %in% (ay-2) & tab2$Variable %in% "Recruitment"] <- astab$recruitment[idx] # data year
tab2$Value[tab2$Year %in% (ay-2) & tab2$Variable %in% "SSB"] <- astab$SSB[idx] # data year
tab2$Value[tab2$Year %in% (ay-2) & tab2$Variable %in% "Fbar"] <- astab$"F"[idx] # data year
tab2$Value[tab2$Year %in% (ay-2) & tab2$Variable %in% "Total catch"] <- astab$catches[idx] # data year

# int year values
tab2$Value[tab2$Year %in% (ay-1) & tab2$Variable %in% "SSB"] <- fctab[as.character(ay-1),"ssb:median"]
tab2$Value[tab2$Year %in% (ay-1) & tab2$Variable %in% "Fbar"] <- fctab[as.character(ay-1),"fbar:median"]
tab2$Value[tab2$Year %in% (ay-1) & tab2$Variable %in% "Total catch"] <- fctab[as.character(ay-1),"catch:median"]
tab2$Value[tab2$Year %in% (ay-1) & tab2$Variable %in% "Recruitment"] <- astab$recruitment[astab$Year == ay-1]

# advice year values
tab2$Value[tab2$Year %in% (ay) & tab2$Variable %in% "SSB"] <- fctab[as.character(ay),"ssb:median"]
tab2$Value[tab2$Year %in% (ay) & tab2$Variable %in% "Fbar"] <- fctab[as.character(ay),"fbar:median"]
tab2$Value[tab2$Year %in% (ay) & tab2$Variable %in% "Total catch"] <- fctab[as.character(ay),"catch:median"]
tab2$Value[tab2$Year %in% (ay) & tab2$Variable %in% "Recruitment"] <- astab$recruitment[astab$Year == ay-1]


# combine tables and export
tab <- rbind(tab1,tab2)

write.csv(tab,paste0(output.dir,"Forecast_assumptions.csv"),row.names=F)

#  format forcast assumptions
dat <- tab
dat <- dat[dat$Year >(ay-2),]
dat$Type <- factor(dat$Type,levels=c("Data year","Intermediate year","Advice year"))
dat$Variable <- factor(dat$Variable,levels=c("SSB","Fbar","Total catch","Recruitment"))

# plot
png(paste0(output.dir,"Change in advice - forecast assumptions.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(dat,aes(x=Year,y=Value,colour=WG,shape=Type))+geom_point(size=3)+facet_wrap(~Variable,scales="free_y")+
  theme_bw()+labs(x="",y="",colour="",shape="")+ scale_shape_manual(values=c(16, 2, 0))#+ylim(0,NA)
print(p1)
dev.off()

# Forecast_stockwts.csv ---------------------------------------------------

# this year
ca.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - catch-at-age.txt")
st.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - stock-at-age.txt")
lan.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - landings-at-age.txt")
dis.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - discards-at-age incl BMS and IBC.txt")

colnames(ca.frct.wt) <- colnames(st.frct.wt) <- colnames(lan.frct.wt) <- colnames(dis.frct.wt) <- colnames(fit$data$catchMeanWeight)


fit$data$catchMeanWeight[,,"Residual catch"] <- rbind(fit$data$catchMeanWeight[ac(data_yrs),,"Residual catch"],ca.frct.wt)
fit$data$stockMeanWeight <- rbind(fit$data$stockMeanWeight[ac(data_yrs),],st.frct.wt) 
fit$data$landMeanWeight[,,"Residual catch"] <- rbind(fit$data$landMeanWeight[ac(data_yrs),,"Residual catch"],lan.frct.wt)
fit$data$disMeanWeight[,,"Residual catch"] <- rbind(fit$data$disMeanWeight[ac(data_yrs),,"Residual catch"],dis.frct.wt) 

ca.frct.wt$Year <- st.frct.wt$Year <- lan.frct.wt$Year <- dis.frct.wt$Year <- row.names(ca.frct.wt)
ca.frct.wt$Cat <- "catch"
st.frct.wt$Cat <- "stock"
lan.frct.wt$Cat <- "landings"
dis.frct.wt$Cat <- "discards"

dat <- reshape2::melt(rbind(ca.frct.wt,st.frct.wt,lan.frct.wt,dis.frct.wt),id.vars=c("Year","Cat"))
names(dat) <- c("Year","Cat","age","wt")

# age 0 - no observations - set to NA
dat[dat$age==0 & dat$wt==0,"wt"]<-NA

dat <- dat[dat$Cat %in% "stock",]
dat$Source <- paste0("WGNSSK ",ay)

# compare to last year's stock weights
prev_frcst.wt <- prev_frcst_fit$data$stockMeanWeight
prev_frcst.wt$Year <- rownames(prev_frcst.wt)
prev_frcst.wt$Cat <- "stock"
prev_frcst.wt$Source <- paste0("WGNSSK ",(ay-1))
dat.prev <- reshape2::melt(prev_frcst.wt,id.vars=c("Source","Cat","Year"))
names(dat.prev) <- c("Source","Cat","Year","age","wt")
dat.prev <- dat.prev[dat.prev$Year %in% ((ay-1):(ay+1)),]

dat <- rbind(dat,dat.prev)

png(paste0(output.dir,"Change in advice - stock mean weights.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=dat, aes(x=age, y=wt,colour=Source,group=Source)) + 
  facet_wrap(~Year,nrow = 2)+ geom_point()+
  geom_line() + theme_bw()+ labs(colour="",x="",y="mean weight (kg)") +
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)
dev.off()

# save out
colnames(dat) <- c("Year","Cat","Age","Weight","Source")

write.csv(dat[,c("Year","Age","Weight","Source")],file = paste0(output.dir,"Forecast_stockwts.csv"),row.names=F)

# Forecast_selectivity.csv --------------------------------------------------

Fsel <- faytable(fit)/rowSums(faytable(fit))
Fsel.frcst <- colMeans(Fsel[ac(Sy),])

# last year's sel
Fsel <- faytable(prev_ass)/rowSums(faytable(prev_ass))
Fsel.prev <- colMeans(Fsel[ac(Sy-1),])

# write out
dat1 <- reshape2::melt(Fsel.frcst)
dat1$Age <- rownames(dat1)
dat1$Source <- paste0("WGNSSK ",ay)

dat2 <- reshape2::melt(Fsel.prev)
dat2$Age <- rownames(dat2)
dat2$Source <- paste0("WGNSSK ",(ay-1))

dat <- rbind(dat1,dat2)
colnames(dat) <- gsub("value","Selectivity",colnames(dat))

write.csv(dat,file = paste0(output.dir,"Forecast_selectivity.csv"),row.names=F)

png(paste0(output.dir,"Change in advice - selectivity.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=dat, aes(x=Age, y=Selectivity,colour=Source,group=Source)) + 
  geom_point()+
  geom_line() + theme_bw()+ labs(colour="",x="",y="Selectivity") +
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)

# N at age compared to previous forecast ------------------------------------------

# get forecast n at age tables
#natage_now <- attr(FC[[1]],"naytable")
#natage_prev <- attr(prev_FC1,"naytable")

# numbers - stock - now
N <- ntable(fit)
fc <- FC[[1]]
Nfc <- do.call(rbind, lapply(fc, function(x)exp(colMeans(x$sim[,1:ncol(N)]))))
rownames(Nfc) <- lapply(fc, function(x)x$year)
colnames(Nfc) <- colnames(N)

natage_fc <- as.data.frame(Nfc)
natage_fc$Year <- (ay-1):(ay+2)
colnames(natage_fc) <- c(0:8,"Year")
natage_now <- natage_fc[natage_fc$Year >=ay,]


# numbers - stock - prev
N <- ntable(prev_ass)
fc <- prev_FC1
Nfc <- do.call(rbind, lapply(fc, function(x)exp(colMeans(x$sim[,1:ncol(N)]))))
rownames(Nfc) <- lapply(fc, function(x)x$year)
colnames(Nfc) <- colnames(N)

natage_fc <- as.data.frame(Nfc)
natage_fc$Year <- row.names(natage_fc)
colnames(natage_fc) <- c(0:8,"Year")
natage_prev <- natage_fc[natage_fc$Year >=(ay-1),]

# n at age in fit
n_now <- as.data.frame(cbind(c(1972:(ay)),ntable(fit)))
colnames(n_now) <- c("Year",0:8)

n_prev <- as.data.frame(cbind(c(1972:(ay-1)),ntable(prev_ass)))
colnames(n_prev) <- c("Year",0:8)

n_now <- reshape2::melt(n_now,id.vars="Year")
n_prev <- reshape2::melt(n_prev,id.vars="Year")

dat0 <- n_now[n_now$Year == (ay-2),]
dat0$WG <- paste0("WGNSSK ",ay)
dat0$Type="Data"
dat1 <- n_now[n_now$Year == (ay-1),]
dat1$WG <- paste0("WGNSSK ",ay)
dat1$Type="Data"
dat2 <- n_prev[n_prev$Year == (ay-2),]
dat2$WG <- paste0("WGNSSK ",(ay-1))
dat2$Type="Data"

dat3 <- data.frame(value=t(natage_now[1,ac(0:8)]),Year=ay,variable=0:8) # int yr
colnames(dat3)[1] <- "value"
dat3$WG <- paste0("WGNSSK ",ay)
dat3$Type="Intermediate year"
dat4 <- data.frame(value=t(natage_prev[1,ac(0:8)]),Year=ay-1,variable=0:8) # int yr
colnames(dat4)[1] <- "value"
dat4$WG <- paste0("WGNSSK ",(ay-1))
dat4$Type="Intermediate year"

dat5 <- data.frame(value=t(natage_now[2,ac(0:8)]),Year=ay+1,variable=0:8) #  advice yr
colnames(dat5)[1] <- "value"
dat5$WG <- paste0("WGNSSK ",ay)
dat5$Type="Advice year"
dat6 <-  data.frame(value=t(natage_prev[2,ac(0:8)]),Year=ay,variable=0:8) #  advice yr
colnames(dat6)[1] <- "value"
dat6$WG <- paste0("WGNSSK ",(ay-1))
dat6$Type="Advice year"


dat <- rbind(dat0,dat1,dat2,dat3,dat4,dat5,dat6)
colnames(dat) <- c("Year","Age","N","WG","Type")
dat$Type <- factor(dat$Type,levels=c("Data","Intermediate year","Advice year"))

# correct Rec from WGNSSK ay to account for geometric mean
Ry <- 2000:(ay-1)
R <- rectable(fit)[,1]
R <- R[as.character(1972:(ay-1))]
R_geoMean <- exp(mean(log(R[ac(Ry)]))) # better summary stat for tables and plots when length(Ry) is even

dat$N[dat$WG %in% paste0("WGNSSK ",ay) & dat$Age == 0 & !dat$Type %in% "Data"] <- R_geoMean

# replace recruitment in previous forecast with geomean from SAG
dat$N[dat$WG %in% paste0("WGNSSK ",ay-1) & dat$Age == 0 & !dat$Type %in% "Data"] <- astab$recruitment[astab$Year ==(ay-1)]

write.csv(dat,file=paste0(output.dir,"Forecast_N_at_age.csv"),row.names=F)

png(paste0(output.dir,"Change in advice - Stock numbers-at-age.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(dat,aes(x=Age,y=N,group=interaction(Type,WG),colour=WG,shape=Type))+ geom_line()+geom_point(size=3)+labs(colour="",y="Abundance (thousands)",shape="")+
  facet_wrap(~Year,nrow=2)+theme_bw()+scale_shape_manual(values=c(16, 2, 0))

print(p1)
dev.off()


#res.n <- res.bm <- vector(mode="list",length=length(ts_yrs))
#names(res.n) <- names(res.bm) <- ts_yrs

# compare N at age table ----------------------------------

# this year's assessment and forecast
# n at age in fit
n_now <- as.data.frame(cbind(c(1972:(ay)),ntable(fit)))
colnames(n_now) <- c("Year",0:8)

# add geometric mean for Rec
natage_now[,"0"] <- R_geoMean
n_now <- rbind(n_now[n_now$Year %in% 1972:(ay-1),],natage_now)

# last year's assessment and forecast
n_prev <- as.data.frame(cbind(c(1972:(ay-1)),ntable(prev_ass)))
colnames(n_prev) <- c("Year",0:8)
# add geometric mean for Rec
natage_prev[,"0"] <- astab$recruitment[astab$Year == (ay-1)]
n_prev <- rbind(n_prev[n_prev$Year %in% 1972:(ay-2),],natage_prev)

# save results
write.csv(n_now,file=paste0(output.dir,"Now_assessment_N_at_age.csv"),row.names=F)
write.csv(n_prev,file=paste0(output.dir,"Prev_assessment_N_at_age.csv"),row.names=F)


# biomass at age compared to previous forecast -----------------------------------------

# get forecast n at age tables
wt_now <- as.data.frame(attr(FC[[1]],"fit")$data$stockMeanWeight)
b_now <- n_now[,-1]*wt_now
b_now$Year <- n_now$Year

# prev ass/forecast 
wt_prev <- as.data.frame(prev_frcst_fit$data$stockMeanWeight)
b_prev <- n_prev[,-1]*wt_prev
b_prev$Year <- n_prev$Year 

b_now <- b_now[,c(10,1:9)]
b_prev <- b_prev[,c(10,1:9)]

# save results
write.csv(b_now,file=paste0(output.dir,"Now_assessment_B_at_age.csv"),row.names=F)
write.csv(b_prev,file=paste0(output.dir,"Prev_assessment_B_at_age.csv"),row.names=F)

# reshape
b_now <- reshape2::melt(b_now,id.vars="Year")
b_prev <- reshape2::melt(b_prev,id.vars="Year")

# Data years
dat0 <- b_now[b_now$Year == (ay-2),]
dat0$WG <- paste0("WGNSSK ",ay)
dat0$Type="Data"
dat1 <- b_now[b_now$Year == (ay-1),]
dat1$WG <- paste0("WGNSSK ",ay)
dat1$Type="Data"
dat2 <- b_prev[b_prev$Year == (ay-2),]
dat2$WG <- paste0("WGNSSK ",(ay-1))
dat2$Type="Data"

# Intermediate years
dat3 <- b_now[b_now$Year == ay,]
dat3$WG <- paste0("WGNSSK ",ay)
dat3$Type="Intermediate year"
dat4 <- b_prev[b_prev$Year==ay-1,]
dat4$WG <- paste0("WGNSSK ",(ay-1))
dat4$Type="Intermediate year"

#Advice year
dat5 <- b_now[b_now$Year ==ay+1,]
dat5$WG <- paste0("WGNSSK ",ay)
dat5$Type="Advice year"
dat6 <-  b_prev[b_prev$Year ==ay,]
dat6$WG <- paste0("WGNSSK ",(ay-1))
dat6$Type="Advice year"

dat <- rbind(dat0,dat1,dat2,dat3,dat4,dat5,dat6)
colnames(dat) <- c("Year","Age","B","WG","Type")
dat$Type <- factor(dat$Type,levels=c("Data","Intermediate year","Advice year"))


write.csv(dat,file=paste0(output.dir,"Forecast_B_at_age.csv"),row.names=F)


png(paste0(output.dir,"Change in advice - Stock biomass-at-age.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(dat,aes(x=Age,y=B,group=interaction(Type,WG),colour=WG,shape=Type))+ geom_line()+geom_point(size=3)+labs(colour="",y="Biomass (t)",shape="")+
  facet_wrap(~Year,nrow=2)+theme_bw()+scale_shape_manual(values=c(16, 2, 0))

print(p1)

dev.off()




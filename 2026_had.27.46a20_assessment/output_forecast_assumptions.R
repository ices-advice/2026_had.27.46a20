## Extract results of interest, write TAF output tables

## Before:
## After:
rm(list=ls())
graphics.off()

load("data/init.RData")
sourceDir("boot/software/utilities/")

load("data/stockData.RData")

advice_year <- ay+1

# load last year's assessment and forecast
load("model/SAM/NShaddock_WGNSSK2025_Run1/model.RData") # last year's
prev_ass <- fit
load("boot/data/forecast - WGNSSK 2025.RData")
prev_FC1 <- FC1
prev_frcst_fit <- attr(FC1, "fit")

# load this year's model again
load("model/SAM/NShaddock_WGNSSK2026_Run1/model.RData")

load("model/SAM/temp_forecast.RData")
tmp_FC <- tmp1

###---------------------------------------------------------------------###

# Forecast input and comparison plots
#assess_year <- ay # the intermediate year when assessment is being conducted
#advice_year <- ay+1 # the year for TAC advice
data_yrs <- 1972:(ay-1)

## Forecast parameters:
Ay <- (ay-3):(ay-1) # for biols
Sy <- (ay-3):(ay-1)  # for sel
Ry <- 2000:(ay-1) # for rec

tab.inputs <- data.frame(age=0:8,Mat=NA,NM=NA,Sel=NA,LF=NA)
tab.inputs.prev <- tab.inputs

# mean weights - compare to last year forecast ----------------------------------------------
if(0){ # this is complicated because data from 2021-2023 were revised in WG 2025. 
## observed
load("N:/Stock_assessment/WGNSSK/2025/2025_had.27.46a20_assessment/data/stockData.RData")
tmp1 <- as.data.frame(stock.data.pg@catch.wt)
tmp1$type <- "catch.wt"
tmp2 <- as.data.frame(stock.data.pg@landings.wt) 
tmp2$type <- "landings.wt"
tmp3<- as.data.frame(stock.data.pg@discards.wt)
tmp3$type <- "discards.wt"
tmp4 <- as.data.frame(stock.data.pg@stock.wt)
tmp4$type <- "stock.wt"

dat0 <- bind_rows(bind_rows(bind_rows(tmp1,tmp2),tmp3),tmp4)
dat0$label <- "observed"
dat0$WG <- "WGNSSK 2025"
dat0 <- dat0[c("year","age","data","type","label","WG")]

# observed
load("data/stockData.RData")
tmp1 <- as.data.frame(stock.data.pg@catch.wt)
tmp1$type <- "catch.wt"
tmp2 <- as.data.frame(stock.data.pg@landings.wt) 
tmp2$type <- "landings.wt"
tmp3<- as.data.frame(stock.data.pg@discards.wt)
tmp3$type <- "discards.wt"
tmp4 <- as.data.frame(stock.data.pg@stock.wt)
tmp4$type <- "stock.wt"

dat1 <- bind_rows(bind_rows(bind_rows(tmp1,tmp2),tmp3),tmp4)
dat1$label <- "observed"
dat1$WG <- "WGNSSK 2026"
dat1 <- dat1[c("year","age","data","type","label","WG")]


# last year's
tmp1 <- as.data.frame(prev_frcst_fit$data$catchMeanWeight)
tmp1$type <- "catch.wt"
tmp2 <- as.data.frame(prev_frcst_fit$data$landMeanWeight) 
tmp2$type <- "landings.wt"
tmp3<- as.data.frame(prev_frcst_fit$data$disMeanWeight)
tmp3$type <- "discards.wt"
tmp4 <- as.data.frame(prev_frcst_fit$data$stockMeanWeight)
tmp4$type <- "stock.wt"

dat2 <- bind_rows(bind_rows(bind_rows(tmp1,tmp2),tmp3),tmp4)
dat2$label <- "forecast"
dat2$WG <- "WGNSSK 2025"
dat2$year <- 1972:2027
dat2 <- pivot_longer(dat2,cols=as.character(0:8),names_to="age",values_to="data")
dat2 <- filter(dat2,year>2024)
dat2$age <- as.numeric(dat2$age)

# this year's
ca.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - catch-at-age.txt")
st.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - stock-at-age.txt")
lan.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - landings-at-age.txt")
dis.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - discards-at-age incl BMS and IBC.txt")

colnames(ca.frct.wt) <- colnames(st.frct.wt) <- colnames(lan.frct.wt) <- colnames(dis.frct.wt) <- 0:8

ac<-as.character

ca.frct.wt$year <- st.frct.wt$year <- lan.frct.wt$year <- dis.frct.wt$year <- row.names(ca.frct.wt)
ca.frct.wt$type <- "catch.wt"
st.frct.wt$type <- "stock.wt"
lan.frct.wt$type <- "landings.wt"
dis.frct.wt$type <- "discards.wt"

dat3 <- bind_rows(bind_rows(bind_rows(ca.frct.wt,lan.frct.wt),dis.frct.wt),st.frct.wt)
dat3 <- pivot_longer(dat3,cols=as.character(0:8),names_to="age",values_to="data")
dat3$year <- as.numeric(dat3$year)
dat3$age <- as.numeric(dat3$age)
dat3$label <- "forecast"
dat3$WG <- "WGNSSK 2026"

# combine
dat <- bind_rows(dat0,dat1,dat2,dat3)

# age 0 - no observations - set to NA
dat$data[dat$age==0 & dat$data==0]<-NA

dat$cohort <- dat$year-dat$age

# plot
for (kk in c("landings.wt","discards.wt","catch.wt","stock.wt")){
png(paste0("output/Forecast/Forecast inputs - mean weights ",kk," by cohort.png"),width = 11, height = 7, units = "in", res = 600)

toplot <- filter(dat,type %in% kk,cohort>2018 & cohort<2028, age<8)
p1 <- ggplot(data=toplot, aes(x=age, y=data,colour=WG,shape=label)) + 
  facet_wrap(~cohort,ncol=2)+ geom_point()+ scale_shape_manual(values=c(1,16))+
  geom_line() + theme_bw()+ labs(y="mean weight (kg)",colour="",x="",shape="") +
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) + scale_colour_manual(values=col.pal9)
print(p1)
dev.off()

png(paste0("output/Forecast/Forecast inputs - mean weights ",kk," by year.png"),width = 11, height = 7, units = "in", res = 600)

toplot <- filter(dat,type %in% kk)
p1 <- ggplot(data=toplot, aes(x=year, y=data,colour=WG,shape=label)) + 
  facet_wrap(~age,ncol=3,scales="free_y")+ geom_point()+  scale_shape_manual(values=c(1,16))+
  geom_line() + theme_bw()+ labs(y="mean weight (kg)",colour="",x="",shape="") +
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) + scale_colour_manual(values=col.pal9)
print(p1)
dev.off()
}
}

# mean weights - plot this year's assumptions -----------------------------------------
ca.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - catch-at-age.txt")
st.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - stock-at-age.txt")
lan.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - landings-at-age.txt")
dis.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - discards-at-age incl BMS and IBC.txt")

colnames(ca.frct.wt) <- colnames(st.frct.wt) <- colnames(lan.frct.wt) <- colnames(dis.frct.wt) <- colnames(fit$data$catchMeanWeight)

ac<-as.character

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

png(paste0("output/Forecast/Forecast inputs - mean weights.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=dat, aes(x=age, y=wt,colour=Cat,group=Cat)) + 
  facet_wrap(~Year,ncol=2)+ geom_point()+
  geom_line() + theme_bw()+ labs(y="mean weight (kg)",colour="",x="") +
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) + scale_colour_manual(values=col.pal9)
print(p1)
dev.off()

png(paste0("output/Forecast/Forecast inputs - mean weights v2.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=dat, aes(x=age, y=wt,colour=Year,group=Year)) + 
  facet_wrap(~Cat)+ geom_point()+
  geom_line() + theme_bw()+ labs(y="mean weight (kg)",colour="",x="") +
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) + scale_colour_manual(values=col.pal9)
print(p1)
dev.off()

# plot maturity and natural mortality -----------------------------------------

# last years
tab.inputs.prev$Mat <- round(colMeans(fit$data$propMat[ac(Ay-1),]),3)
tab.inputs.prev$natMor <- round(colMeans(fit$data$natMor[ac(Ay-1),]),3)

# this yr
fit$data$propMat[ac(ay),] <- round(colMeans(fit$data$propMat[ac(Ay),]),3)
fit$data$natMor[ac(ay),] <- round(colMeans(fit$data$natMor[ac(Ay),]),3)

tab.inputs$Mat <- fit$data$propMat[ac(ay),]
tab.inputs$NM <- fit$data$natMor[ac(ay),]


dat <- fit$data$propMat[ac(1972:(ay-1)),] #data years only
dat.frcst <- round(colMeans(fit$data$propMat[ac(Ay),]),3)
dat.frcst <- rbind(dat.frcst,dat.frcst,dat.frcst)
rownames(dat.frcst) <- (ay-1):(ay+1)

# maturity
png(paste0("output/Forecast/Forecast inputs - maturity.png"),width = 11, height = 7, units = "in", res = 600)

par(mar=c(5, 4, 4, 10), xpd=TRUE)
for (a in 0:8){
  if(a==0){
    plot(1972:(ay-1),dat[,ac(a)],ylim=c(0,1),xlim=c(1972,ay+2),type="l",col=col.pal9[(a+1)],ylab="proportion mature",xlab="")
    points(ay:(ay+2),dat.frcst[,ac(a)],pch=16,col=col.pal9[(a+1)])
    points((ay:(ay+2)-1),rep(tab.inputs.prev$Mat[tab.inputs.prev$age %in% ac(a)],3),pch=1,col=col.pal9[(a+1)])
  }else{
    lines(1972:(ay-1),dat[,ac(a)],col=col.pal9[(a+1)])
    points(ay:(ay+2),dat.frcst[,ac(a)],pch=16,col=col.pal9[(a+1)])
    points((ay:(ay+2)-1),rep(tab.inputs.prev$Mat[tab.inputs.prev$age %in% ac(a)],3),pch=1,col=col.pal9[(a+1)])
  }
}
legend("topright", inset=c(-0.2, 0),legend=0:8,col=col.pal9,lty="solid",pch=16)
dev.off()

#natural mortality
dat <- fit$data$natMor[ac(1972:(ay-1)),] #data years only
dat.frcst <- round(colMeans(fit$data$natMor[ac(Ay),]),3)
dat.frcst <- rbind(dat.frcst,dat.frcst,dat.frcst)
rownames(dat.frcst) <- (ay-1):(ay+1)


png(paste0("output/Forecast/Forecast inputs - natural mortality.png"),width = 11, height = 7, units = "in", res = 600)

col.pal9 <- c(brewer.pal(n = 8, name = "Dark2"),brewer.pal(n=6,name="Set2")[4])
par(mar=c(5, 4, 4, 10), xpd=TRUE)
for (a in 0:8){
  if(a==0){
    plot(1972:(ay-1),dat[,ac(a)],ylim=c(0,1.8),xlim=c(1972,ay+2),type="l",col=col.pal9[(a+1)],ylab="natural mortality",xlab="")
    points(ay:(ay+2),dat.frcst[,ac(a)],pch=16,col=col.pal9[(a+1)])
    points((ay:(ay+2)-1),rep(tab.inputs.prev$natMor[tab.inputs.prev$age %in% ac(a)],3),pch=1,col=col.pal9[(a+1)])
    
  }else{
    lines(1972:(ay-1),dat[,ac(a)],col=col.pal9[(a+1)])
    points(ay:(ay+2),dat.frcst[,ac(a)],pch=16,col=col.pal9[(a+1)])
    points((ay:(ay+2)-1),rep(tab.inputs.prev$natMor[tab.inputs.prev$age %in% ac(a)],3),pch=1,col=col.pal9[(a+1)])
    
  }
}
legend("topright", inset=c(-0.2, 0),legend=0:8,col=col.pal9,lty="solid",pch=16)
dev.off()


# selectivity and catch proportions -----------------------------------------

# last year's
Fsel <- faytable(prev_frcst_fit)/rowSums(faytable(prev_frcst_fit))
Fsel.tmp <- colMeans(Fsel[ac(Sy-1),])

tab.inputs.prev$Sel <- round(colMeans(Fsel[ac(Sy-1),]),3)

# this years
Fsel <- faytable(fit)/rowSums(faytable(fit))
Fsel.frcst <- colMeans(Fsel[ac(Sy),])

tab.inputs$Sel <- round(colMeans(Fsel[ac(Sy),]),3)

png(paste0("output/Forecast/Forecast inputs - selectivity.png"),width = 11, height = 7, units = "in", res = 600)

col.pal9 <- c(brewer.pal(n = 8, name = "Dark2"),brewer.pal(n=6,name="Set2")[4])
for (y in Sy){
  if(y==Sy[1]){
    plot(0:8,Fsel[ac(y),],ylim=c(0,max(Fsel)),type="l",col=col.pal9[(y-Sy[1])+1],ylab="Selectivity",xlab="",lwd=2)
  }else{
    lines(0:8,Fsel[ac(y),],col=col.pal9[(y-Sy[1])+1],lwd=2)
  }
}
points(0:8,Fsel.frcst,pch=16,col=col.pal9[4])
lines(0:8,Fsel.frcst,col=col.pal9[4],lwd=2,lty=3)
#points(0:8,tab.inputs.prev$Sel,pch=1,col=col.pal9[5])
#lines(0:8,tab.inputs.prev$Sel,col=col.pal9[5],lwd=1,lty=2)
legend("topright", inset=0.02,legend=c(Sy,"Forecast"),col=col.pal9[1:4],lty=c(1,1,1,3),lwd=c(2,2,2,2),pch=c(NA,NA,NA,16))
dev.off()

# Catch proportions 
lf <- fit$data$landFrac[ac(1972:(ay-1)),,"Residual catch"] #data years only
lf.frcst <- round(colMeans(lf[ac(Sy),]),3)
df.frcst <- 1-lf.frcst
df <-(1-lf)

tab.inputs$LF <- lf.frcst
tab.inputs$DF <- df.frcst

png(paste0("output/Forecast/Forecast inputs - Catch proportions by age.png"),width = 11, height = 7, units = "in", res = 600)
par(mar=c(5, 4, 4, 10), xpd=TRUE)
for (y in Sy){
  if(y==Sy[1]){
    plot(0:8,lf[ac(y),],type="l",col=col.pal9[(y-Sy[1]+1)],ylim=c(0,1),ylab="Catch proportions",xlab="")
    lines(0:8,df[ac(y),],col=col.pal9[((y-Sy[1]+1))],lty=2)
  }else{
    lines(0:8,lf[ac(y),],col=col.pal9[((y-Sy[1]+1))])
    lines(0:8,df[ac(y),],col=col.pal9[((y-Sy[1]+1))],lty=2)
  }
}
points(0:8,lf.frcst,pch=16,col=col.pal9[4],type="o")
points(0:8,df.frcst,pch=16,col=col.pal9[4],type="o",lty=2)
legend("bottomright", inset=c(-0.2, 0),legend=c(Sy,"Forecast","Landings","Discards+BMS+IBC"),col=c(col.pal9[1:4],"black","black"),
       lty=c(rep("solid",4),"solid","dashed"),pch=c(NA,NA,NA,16,NA,NA),cex=0.75)
dev.off()


# plot Recruitment -----------------------------------------

png(paste0("output/Forecast/Forecast inputs - recruitment.png"),width = 11, height = 7, units = "in", res = 600)

R <- rectable(fit)[,1]
R <- R[as.character(1972:(ay-1))]
R_datayr <- attr(tmp_FC, "tab")[as.character(advice_year-2), "rec:median"]
R_ay <- attr(tmp_FC, "tab")[as.character(advice_year-1), "rec:median"]
R_TACyr <- attr(tmp_FC, "tab")[as.character(advice_year), "rec:median"] 
R_geoMean <- exp(mean(log(R[ac(Ry)]))) # geomean is reported in advice sheet

tab.inputs$Rec_datayr <- NA
tab.inputs$Rec_median_ay <- NA
tab.inputs$Rec_median_TACyr <- NA
tab.inputs$Rec_gmean <- NA
tab.inputs$Rec_datayr[1] <- R_datayr
tab.inputs$Rec_median_ay[1] <- R_ay
tab.inputs$Rec_median_TACyr[1] <- R_TACyr
tab.inputs$Rec_gmean[1] <- R_geoMean

plot(names(R),R,xlab="",ylab="Recruitment age 0 (thousands)",pch=16,type="o")
lines(Ry,rep(R_datayr,length(Ry)),lty=2,col="lightseagreen",lwd=2)
lines(Ry,rep(R_TACyr,length(Ry)),lty=4,col="magenta",lwd=2)


lines(Ry,rep(R_geoMean,length(Ry)),lty=1,col="gold",lwd=2)
legend("topright", inset=0.02,legend=c(paste0("Rec ",ay-1),paste0("resampled ",ay,"-",ay+1),"geometric mean"),
       col=c("lightseagreen","magenta","gold"),
       lty=c(2,4,1),lwd=2)

dev.off()

# save Forecast inputs table -----------------------------------------------------------------

write.taf(tab.inputs,file="output/Forecast/Forecast inputs table.csv")




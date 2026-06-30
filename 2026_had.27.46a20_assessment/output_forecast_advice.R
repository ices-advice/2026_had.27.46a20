## Extract results of interest, write TAF output tables

## Before:
## After:

rm(list=ls())
graphics.off()

load("data/init.RData")
sourceDir("boot/software/utilities/")

advice_year <- ay+1

load("model/SAM/forecast.RData")

# save out headline advice forecast for next year
FC1 <- FC[[1]]
save(FC1,file=paste0("output/forecast - WGNSSK ",ay,".RData"))

# Settings
#assess_year <- ay # the intermediate year when assessment is being conducted
#advice_year <- ay+1 # the year for TAC advice
data_yrs <- 1972:(ay-1)

## Forecast parameters:
Ay <- (ay-3):(ay-1) # for biols
Sy <- (ay-3):(ay-1)  # for sel
Ry <- 2000:(ay-1) # for rec


# Forecast results tables and plots ----------------------------------------------------------------------

#plot FMSY option

frcst.Fmsy <- FC[["Fsq, then Fmsy"]] 
label <-attr(frcst.Fmsy,"label")
attr(frcst.Fmsy,"estimateLabel") <- "median"
png(paste0("output/Forecast/Forecast results - ",label,".png"),width = 11, height = 7, units = "in", res = 600)
plot(frcst.Fmsy,main=label,xlab="")
dev.off()

# intermediate year numbers
tab <- attr(frcst.Fmsy,"tab")
tab[ac(ay),c("fbar:median","rec:median","ssb:median","catch:median","Land:median","Discard:median")]
tab[ac(ay+1),c("rec:median","ssb:median")]

# get data from forecast fit
frcst.fit <- attr(FC[[1]], "fit")
wts <- as.data.frame(frcst.fit$data$stockMeanWeight)
wts$Year <- row.names(wts)
wts <- pivot_longer(wts,cols=c(as.character(0:8)),names_to="age",values_to="wt")

mat <- as.data.frame(frcst.fit$data$propMat)
mat$Year <- row.names(mat)
tmp <- mat[mat$Year %in% (ay-3):(ay-1),]
mm <- colMeans(tmp[,c(as.character(0:8))])
tmp[1,c(as.character(0:8))] <-  mm
tmp[2,c(as.character(0:8))] <- mm
tmp[3,c(as.character(0:8))] <- mm
tmp$Year <- ay:(ay+2)
mat <- rbind(mat[mat$Year<ay,],tmp)
mat <- pivot_longer(mat,cols=c(as.character(0:8)),names_to="age",values_to="mat")

c.wts <- as.data.frame(frcst.fit$data$catchMeanWeight)
c.wts$Year <- row.names(c.wts)
c.wts <- pivot_longer(c.wts,cols=c(as.character(0:8)),names_to="age",values_to="cwt")

# numbers - stock
natage_fc <- attr(FC[[1]],"naytable")
idx <- which(row.names(natage_fc) %in% "median.50%")
natage_fc <- as.data.frame(natage_fc[idx,])
natage_fc$Year <- (ay-1):(ay+2)
colnames(natage_fc) <- c(0:8,"Year")
natage_fc <- natage_fc[natage_fc$Year >=ay,]

n_now <- as.data.frame(cbind(c(1972:(ay)),ntable(frcst.fit)))
colnames(n_now) <- c("Year",0:8)
n_now <- n_now[n_now$Year < ay,]

nums <- rbind(n_now,natage_fc)

#numbers - catch
catage_fc <- attr(FC[[1]],"caytable")
c_now <- as.data.frame(cbind(c(1972:(ay)),caytable(frcst.fit))) # is this data or estimate?
colnames(c_now) <- c("Year",0:8)
c_now <- reshape2::melt(c_now,id.vars="Year")

catage_fc <- attr(FC[[1]],"caytable")
idx <- which(row.names(catage_fc) %in% "median.50%")
catage_fc <- as.data.frame(catage_fc[idx,])
catage_fc$Year <- (ay-1):(ay+2)
colnames(catage_fc) <- c(0:8,"Year")
catage_fc <- catage_fc[catage_fc$Year >=ay,]

c_now <- as.data.frame(cbind(c(1972:(ay)),caytable(frcst.fit,fleet=1)))
colnames(c_now) <- c("Year",0:8)
c_now <- c_now[c_now$Year < ay,]

c_nums <- rbind(c_now,catage_fc)

# reshape
nums <- pivot_longer(nums,cols=c(as.character(0:8)),names_to="age",values_to="num")
c_nums <- pivot_longer(c_nums,cols=c(as.character(0:8)),names_to="age",values_to="num")

nums$age <- as.numeric(nums$age)
wts$Year <- as.numeric(wts$Year)
wts$age <- as.numeric(wts$age)
mat$Year <- as.numeric(mat$Year)
mat$age <- as.numeric(mat$age)
c_nums$age <- as.numeric(c_nums$age)
c.wts$Year <- as.numeric(c.wts$Year)
c.wts$age <- as.numeric(c.wts$age)

st <- left_join(left_join(nums,wts,by=c("Year","age")),mat,by=c("Year","age"))
ct <- left_join(c_nums,c.wts,by=c("Year","age"))

# Proportion of each age in SSB
st <- st %>% group_by(Year,age) %>% mutate(ssb=num*wt*mat)
st$age <- factor(st$age,levels=rev(0:8))

png(filename="output/Forecast/Age distribution in SSB.png", height=7, width=11, , units = "in", res = 300)

p1 <- ggplot(st,aes(x=Year,y=ssb,fill=age))+geom_col()+theme_bw()+
  scale_fill_manual(values=col.pal9)+labs(x="",y="SSB (t)",fill="")

print(p1)
dev.off()

png(filename="output/Forecast/Age proportion in SSB.png", height=7, width=11, , units = "in", res = 300)

p1 <- ggplot(st,aes(x=Year,y=ssb,fill=age))+geom_col(position="fill")+theme_bw()+
  scale_fill_manual(values=col.pal9)+labs(x="",y="SSB (t)",fill="")

print(p1)
dev.off()

# Proportion of each age in catch
ct <- ct %>% group_by(Year,age) %>% mutate(cWt=num*cwt)
ct$age <- factor(ct$age,levels=rev(0:8))

png(filename="output/Forecast/Age distribution in catch.png", height=7, width=11, , units = "in", res = 300)

p1 <- ggplot(filter(ct,Year<=advice_year),aes(x=Year,y=cWt,fill=age))+geom_col()+theme_bw()+
  scale_fill_manual(values=col.pal9)+labs(x="",y="catch weight (t)",fill="")

print(p1)
dev.off()

png(filename="output/Forecast/Age proportion in catch.png", height=7, width=11, , units = "in", res = 300)

p1 <- ggplot(filter(ct,Year<=advice_year),aes(x=Year,y=cWt,fill=age))+geom_col(position="fill")+theme_bw()+
  scale_fill_manual(values=col.pal9)+labs(x="",y="catch weight (t)",fill="")

print(p1)
dev.off()

# make advice table -----------------------------------------

# forecasts table 
# update scenarios that hit SSB refs
scen_num <- which(grepl(paste0("then SSB(", advice_year+1, ") = "),
                        names(FC), fixed = TRUE))
FC[scen_num] <- FC2


# prob of falling below Blim

tmp <- lapply(names(FC),function(x){
  pr.intyr <- sum(FC[[x]][[2]]$ssb < Blim)/length(FC[[x]][[2]]$ssb)
  pr.tacyr <- sum(FC[[x]][[3]]$ssb < Blim)/length(FC[[x]][[3]]$ssb)
  pr.ssbyr <- sum(FC[[x]][[4]]$ssb < Blim)/length(FC[[x]][[4]]$ssb)
  
  df<-data.frame(year=ay:(ay+2),prob=c(pr.intyr,pr.tacyr,pr.ssbyr))
})
names(tmp) <- names(FC)

ssb_blim_prob <- dplyr::bind_rows(tmp, .id = "column_label")

toplot <- ssb_blim_prob[-grep("FMSY = ",ssb_blim_prob$column_label),]
ggplot(toplot,aes(x=year,y=prob,colour=column_label))+geom_line()+theme_bw()+ylim(c(0,1))

ssb_blim_prob[ssb_blim_prob$prob != 0,]

writeLines("", con = "output/Forecast/tab_forecasts.txt", sep = "\t")
FC_df <- vector("list", length(FC))
for(i in seq(FC)){
  f <- FC[[i]]
  fc_tab <- attr(f, "tab")
  fc_lab <- attr(f, "label")
  tmp <- as.data.frame(fc_tab)
  tmp <- cbind(data.frame("scenario" = fc_lab), tmp)
  tmp <- xtab2taf(tmp)
  FC_df[[i]] <- tmp
  
  fc_tab <- xtab2taf(fc_tab)
  fc_lab <- gsub(pattern = '*', replacement = "star", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = ",", replacement = "", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = "+", replacement = "plus", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = "-", replacement = "minus", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = "=", replacement = "equals", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = "%", replacement = "perc", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = " ", replacement = "_", x = fc_lab, fixed = TRUE)
  fname <- paste0("tab_fc_", fc_lab, ".csv")
  
  write.taf(fc_tab, file.path("output/Forecast", fname))
  
  write.table(x = paste("\n", attr(f,"label")), file = "output/Forecast/tab_forecasts.txt", append = TRUE,
              row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  write.table(x = fc_tab, file="output/Forecast/tab_forecasts.txt", append = TRUE,
              row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
}

FC_df <- do.call("rbind", FC_df)
save(FC_df, file = "output/Forecast/FC_df.Rdata")

## Export forecast results for MAP with step 0.1:
mapIdx <- grepl(pattern = "^TACcont, then FMSY = [.[:digit:]]+$",
                names(FC))

res <- lapply(FC[mapIdx],
              function(x)
              {
                fc_tab <- attr(x, "tab")
                fc_lab <- attr(x, "label")
                tmp <- as.data.frame(fc_tab)
                tmp <- cbind(data.frame("scenario" = fc_lab),
                             year = row.names(tmp),
                             tmp)
                row.names(tmp) <- NULL
                return(tmp)
              })#, simplify = FALSE, USE.NAMES = FALSE)
resFmap <- do.call(rbind, res)

row.names(resFmap) <- NULL

write.csv(resFmap,
          file = file.path("output/Forecast", "Large_F_range_forecast_all_years.csv"))

write.csv(resFmap[resFmap$year %in% advice_year, ],
          file = file.path("output/Forecast", paste0("Large_F_range_forecast_", advice_year, ".csv")))



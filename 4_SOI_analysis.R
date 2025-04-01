##### Notes: This code is about analyzing and visualizing trend in location 
#####       parameter with AO as covariate respectively.

##### Loads all necessary libraries
library(extRemes)
library(distillery)
library(Lmoments)
library(dplyr)

### remote path
library(distillery,lib.loc = '/home/dzhai/R/x86_64-pc-linux-gnu-library/4.1')
library(Lmoments,lib.loc = '/home/dzhai/R/x86_64-pc-linux-gnu-library/4.1')
library(extRemes,lib.loc = '/home/dzhai/R/x86_64-pc-linux-gnu-library/4.1')
library(dplyr,lib.loc = '/home/dzhai/R/x86_64-pc-linux-gnu-library/4.1')

##### Input SST array (set the working directory before)
## Local pathway
rm(list=ls())
setwd("/Volumes/Doris/SSTmax")

## Remote pathway
rm(list=ls())
setwd("/home/dzhai/SSTmax")

############################ Analysis (Summer) ############################
soi <- read.table("SOI_index.txt",skip=1)
# colnames(ao) <- c("year","Jan","Feb","Mar","Apr","May","Jun",
#                   "Jul","Aug","Sep","Oct","Nov","Dec")
soi <- soi[-c(1:31,72),] # start from 1982, end with 2021
soi_summer <- rowMeans(soi[,c(7:9)],na.rm=T)
soi_summer <- as.vector(soi_summer)

load("sst_annual_max_1982_2021.rdata")
load("three_parameters.Rdata")
l.soi <- matrix(data=NA,nrow=nlon,ncol=nlat)  # SOI covariate
s.soi <- matrix(data=NA,nrow=nlon,ncol=nlat)  # SOI covariate
l.tr_soi <- matrix(data=NA,nrow=nlon,ncol=nlat)  # trend with SOI covariate
s.tr_soi <- matrix(data=NA,nrow=nlon,ncol=nlat)  # trend with SOI covariate

lr_p_l.soi<- matrix(data=NA,nrow=nlon,ncol=nlat) # p-value of the soi
lr_p_s.soi<- matrix(data=NA,nrow=nlon,ncol=nlat) # p-value of the soi
lr_p_l.tr_soi <- matrix(data=NA,nrow=nlon,ncol=nlat) # p-value of the trend & soi
lr_p_s.tr_soi <- matrix(data=NA,nrow=nlon,ncol=nlat) # p-value of the trend & soi
lr_p_l.stat.tr_soi <- matrix(data=NA,nrow=nlon,ncol=nlat)
lr_p_s.stat.tr_soi <- matrix(data=NA,nrow=nlon,ncol=nlat)

sig_or_not_l.soi <- matrix(data=NA,nrow=nlon,ncol=nlat) # soi significance
sig_or_not_s.soi <- matrix(data=NA,nrow=nlon,ncol=nlat) # soi significance
sig_or_not_l.tr_soi <- matrix(data=NA,nrow=nlon,ncol=nlat) # trend & soi significance
sig_or_not_s.tr_soi <- matrix(data=NA,nrow=nlon,ncol=nlat) # trend & soi significance
sig_or_not_l.stat.tr_soi <- matrix(data=NA,nrow=nlon,ncol=nlat)
sig_or_not_s.stat.tr_soi <- matrix(data=NA,nrow=nlon,ncol=nlat)

# For loop to get all the values with the trend fitted
lintr <- 1:n
ind <- 0
for (i in 1:nlon) {
  for(j in 1:nlat) {
    ind <- ind+1
    print(ind)
    
    x = sstmax_array[i,j,]
    
    if ( sum(is.na(x))  == 0 && is.na(loc[i,j]) == F && is.na(sca[i,j]) == F && is.na(sha[i,j]) == F) {
      if (sd(x) >0){
        
        out_stat = fevd(x, units = "deg C") # stat
        #fits the linear trend with SOI covariate
        out_l.soi = fevd(x,units = "deg C",location.fun = ~soi_summer) # L-soi
        out_s.soi = fevd(x,units = "deg C",scale.fun = ~soi_summer) # S-soi
        out_l.trend = fevd(x,units = "deg C",location.fun = ~lintr) # L-trend
        out_s.trend = fevd(x,units = "deg C",scale.fun = ~lintr) # S-trend
        out_l.trend_soi = fevd(x,units = "deg C",location.fun = ~lintr + soi_summer) # L-trend-soi
        out_s.trend_soi = fevd(x,units = "deg C",scale.fun = ~lintr + soi_summer) # S-trend-soi
        
        l.soi[i,j] = out_l.soi$results$par[3]
        s.soi[i,j] = out_s.soi$results$par[3]
        l.tr_soi[i,j] = out_l.trend_soi$results$par[3]
        s.tr_soi[i,j] = out_s.trend_soi$results$par[3]
        
        lr_l.soi = lr.test(out_stat,out_l.soi)
        lr_s.soi = lr.test(out_stat,out_s.soi)
        lr_l.trend_soi = lr.test(out_l.trend,out_l.trend_soi)
        lr_s.trend_soi = lr.test(out_s.trend,out_s.trend_soi)
        lr_l.stat.trend_soi = lr.test(out_stat,out_l.trend_soi)
        lr_s.stat.trend_soi = lr.test(out_stat,out_s.trend_soi)
        
        lr_p_l.soi[i,j] = lr_l.soi$p.value
        lr_p_s.soi[i,j] = lr_s.soi$p.value
        lr_p_l.tr_soi[i,j] = lr_l.trend_soi$p.value
        lr_p_s.tr_soi[i,j] = lr_s.trend_soi$p.value
        lr_p_l.stat.tr_soi[i,j] = lr_l.stat.trend_soi$p.value
        lr_p_s.stat.tr_soi[i,j] = lr_s.stat.trend_soi$p.value
      }
    } 
  }
}

# L-STAT vs L-SOI
ind_p = which(lr_p_l.soi < 0.05) #If the P-value is less than 0.05 than the test is significant (non-stationary)
ind_not_p = which(lr_p_l.soi >= 0.05) #if greater than it is not, doesn't change (stationary)
sig_or_not_l.soi[ind_p] = 1 #points that are signficant
sig_or_not_l.soi[ind_not_p] = 0

ind_p = which(lr_p_s.soi < 0.05) #If the P-value is less than 0.05 than the test is significant (non-stationary)
ind_not_p = which(lr_p_s.soi >= 0.05) #if greater than it is not, doesn't change (stationary)
sig_or_not_s.soi[ind_p] = 1 #points that are signficant
sig_or_not_s.soi[ind_not_p] = 0

# L-TREND vs L-TREND-SOI
ind_p = which(lr_p_l.tr_soi < 0.05)
ind_not_p = which(lr_p_l.tr_soi >= 0.05)
sig_or_not_l.tr_soi[ind_p] = 1 #points that are signficant
sig_or_not_l.tr_soi[ind_not_p] = 0

ind_p = which(lr_p_s.tr_soi < 0.05)
ind_not_p = which(lr_p_s.tr_soi >= 0.05)
sig_or_not_s.tr_soi[ind_p] = 1 #points that are signficant
sig_or_not_s.tr_soi[ind_not_p] = 0

# L-STAT vs L-TREND-SOI
ind_p = which(lr_p_l.stat.tr_soi < 0.05)
ind_not_p = which(lr_p_l.stat.tr_soi >= 0.05)
sig_or_not_l.stat.tr_soi[ind_p] = 1 #points that are signficant
sig_or_not_l.stat.tr_soi[ind_not_p] = 0

ind_p = which(lr_p_s.stat.tr_soi < 0.05)
ind_not_p = which(lr_p_s.stat.tr_soi >= 0.05)
sig_or_not_s.stat.tr_soi[ind_p] = 1 #points that are signficant
sig_or_not_s.stat.tr_soi[ind_not_p] = 0

save(l.soi,s.soi,l.tr_soi,s.tr_soi,lr_p_l.soi,lr_p_s.soi,lr_p_l.tr_soi,
     lr_p_s.tr_soi,lr_p_l.stat.tr_soi,lr_p_s.stat.tr_soi,sig_or_not_l.soi,
     sig_or_not_s.soi,sig_or_not_l.tr_soi,sig_or_not_s.tr_soi,
     sig_or_not_l.stat.tr_soi,sig_or_not_s.stat.tr_soi,
     lon,lat,file = "SOI_summer_analysis.Rdata")

##############################################################################
############################ Analysis (Winter) ############################
soi <- read.table("SOI_index.txt",skip=1)
# colnames(ao) <- c("year","Jan","Feb","Mar","Apr","May","Jun",
#                   "Jul","Aug","Sep","Oct","Nov","Dec")
soi <- soi[-c(1:31,72),] # start from 1982, end with 2021
soi_winter <- rowMeans(soi[,c(2,3,13)],na.rm=T)
soi_winter <- as.vector(soi_winter)

load("sst_annual_max_1982_2021.rdata")
load("three_parameters.Rdata")
l.soi <- matrix(data=NA,nrow=nlon,ncol=nlat)  # SOI covariate
s.soi <- matrix(data=NA,nrow=nlon,ncol=nlat)  # SOI covariate
l.tr_soi <- matrix(data=NA,nrow=nlon,ncol=nlat)  # trend with SOI covariate
s.tr_soi <- matrix(data=NA,nrow=nlon,ncol=nlat)  # trend with SOI covariate

lr_p_l.soi<- matrix(data=NA,nrow=nlon,ncol=nlat) # p-value of the soi
lr_p_s.soi<- matrix(data=NA,nrow=nlon,ncol=nlat) # p-value of the soi
lr_p_l.tr_soi <- matrix(data=NA,nrow=nlon,ncol=nlat) # p-value of the trend & soi
lr_p_s.tr_soi <- matrix(data=NA,nrow=nlon,ncol=nlat) # p-value of the trend & soi
lr_p_l.stat.tr_soi <- matrix(data=NA,nrow=nlon,ncol=nlat)
lr_p_s.stat.tr_soi <- matrix(data=NA,nrow=nlon,ncol=nlat)

sig_or_not_l.soi <- matrix(data=NA,nrow=nlon,ncol=nlat) # soi significance
sig_or_not_s.soi <- matrix(data=NA,nrow=nlon,ncol=nlat) # soi significance
sig_or_not_l.tr_soi <- matrix(data=NA,nrow=nlon,ncol=nlat) # trend & soi significance
sig_or_not_s.tr_soi <- matrix(data=NA,nrow=nlon,ncol=nlat) # trend & soi significance
sig_or_not_l.stat.tr_soi <- matrix(data=NA,nrow=nlon,ncol=nlat)
sig_or_not_s.stat.tr_soi <- matrix(data=NA,nrow=nlon,ncol=nlat)

# For loop to get all the values with the trend fitted
lintr <- 1:n
ind <- 0
for (i in 1:nlon) {
  for(j in 1:nlat) {
    ind <- ind+1
    print(ind)
    
    x = sstmax_array[i,j,]
    
    if ( sum(is.na(x))  == 0 && is.na(loc[i,j]) == F && is.na(sca[i,j]) == F && is.na(sha[i,j]) == F) {
      if (sd(x) >0){
        
        out_stat = fevd(x, units = "deg C") # stat
        #fits the linear trend with SOI covariate
        out_l.soi = fevd(x,units = "deg C",location.fun = ~soi_winter) # L-soi
        out_s.soi = fevd(x,units = "deg C",scale.fun = ~soi_winter) # S-soi
        out_l.trend = fevd(x,units = "deg C",location.fun = ~lintr) # L-trend
        out_s.trend = fevd(x,units = "deg C",scale.fun = ~lintr) # S-trend
        out_l.trend_soi = fevd(x,units = "deg C",location.fun = ~lintr + soi_winter) # L-trend-soi
        out_s.trend_soi = fevd(x,units = "deg C",scale.fun = ~lintr + soi_winter) # S-trend-soi
        
        l.soi[i,j] = out_l.soi$results$par[3]
        s.soi[i,j] = out_s.soi$results$par[3]
        l.tr_soi[i,j] = out_l.trend_soi$results$par[3]
        s.tr_soi[i,j] = out_s.trend_soi$results$par[3]
        
        lr_l.soi = lr.test(out_stat,out_l.soi)
        lr_s.soi = lr.test(out_stat,out_s.soi)
        lr_l.trend_soi = lr.test(out_l.trend,out_l.trend_soi)
        lr_s.trend_soi = lr.test(out_s.trend,out_s.trend_soi)
        lr_l.stat.trend_soi = lr.test(out_stat,out_l.trend_soi)
        lr_s.stat.trend_soi = lr.test(out_stat,out_s.trend_soi)
        
        lr_p_l.soi[i,j] = lr_l.soi$p.value
        lr_p_s.soi[i,j] = lr_s.soi$p.value
        lr_p_l.tr_soi[i,j] = lr_l.trend_soi$p.value
        lr_p_s.tr_soi[i,j] = lr_s.trend_soi$p.value
        lr_p_l.stat.tr_soi[i,j] = lr_l.stat.trend_soi$p.value
        lr_p_s.stat.tr_soi[i,j] = lr_s.stat.trend_soi$p.value
      }
    } 
  }
}

# L-STAT vs L-SOI
ind_p = which(lr_p_l.soi < 0.05) #If the P-value is less than 0.05 than the test is significant (non-stationary)
ind_not_p = which(lr_p_l.soi >= 0.05) #if greater than it is not, doesn't change (stationary)
sig_or_not_l.soi[ind_p] = 1 #points that are signficant
sig_or_not_l.soi[ind_not_p] = 0

ind_p = which(lr_p_s.soi < 0.05) #If the P-value is less than 0.05 than the test is significant (non-stationary)
ind_not_p = which(lr_p_s.soi >= 0.05) #if greater than it is not, doesn't change (stationary)
sig_or_not_s.soi[ind_p] = 1 #points that are signficant
sig_or_not_s.soi[ind_not_p] = 0

# L-TREND vs L-TREND-SOI
ind_p = which(lr_p_l.tr_soi < 0.05)
ind_not_p = which(lr_p_l.tr_soi >= 0.05)
sig_or_not_l.tr_soi[ind_p] = 1 #points that are signficant
sig_or_not_l.tr_soi[ind_not_p] = 0

ind_p = which(lr_p_s.tr_soi < 0.05)
ind_not_p = which(lr_p_s.tr_soi >= 0.05)
sig_or_not_s.tr_soi[ind_p] = 1 #points that are signficant
sig_or_not_s.tr_soi[ind_not_p] = 0

# L-STAT vs L-TREND-SOI
ind_p = which(lr_p_l.stat.tr_soi < 0.05)
ind_not_p = which(lr_p_l.stat.tr_soi >= 0.05)
sig_or_not_l.stat.tr_soi[ind_p] = 1 #points that are signficant
sig_or_not_l.stat.tr_soi[ind_not_p] = 0

ind_p = which(lr_p_s.stat.tr_soi < 0.05)
ind_not_p = which(lr_p_s.stat.tr_soi >= 0.05)
sig_or_not_s.stat.tr_soi[ind_p] = 1 #points that are signficant
sig_or_not_s.stat.tr_soi[ind_not_p] = 0

save(l.soi,s.soi,l.tr_soi,s.tr_soi,lr_p_l.soi,lr_p_s.soi,lr_p_l.tr_soi,
     lr_p_s.tr_soi,lr_p_l.stat.tr_soi,lr_p_s.stat.tr_soi,sig_or_not_l.soi,
     sig_or_not_s.soi,sig_or_not_l.tr_soi,sig_or_not_s.tr_soi,
     sig_or_not_l.stat.tr_soi,sig_or_not_s.stat.tr_soi,
     lon,lat,file = "SOI_winter_analysis.Rdata")

##### Notes: This code is about analyzing and visualizing trend in 
##### location (L-trend)
##### Scale (S-trend)
##### both location and scale (LS-trend)
#####       parameter showing increase in SST maximums due to distribution shifting.
#####       Updated date: Jul.02.2023

##### Loads all necessary libraries
library(ggplot2)
library(extRemes)
library(distillery)
library(Lmoments)
library(dplyr)
library(raster)
library(rgdal)

##### Input SST array (set the working directory before)
## Local pathway
rm(list=ls())
setwd("/Volumes/Doris/SSTmax")

## Remote pathway
rm(list=ls())
setwd("/home/dzhai/SSTmax")

##### Set initial parameters
load("sst_annual_max_1982_2021.rdata")
load("three_parameters.Rdata")
loc_tr <- matrix(data=NA,nrow=nlon,ncol=nlat)  #trend in location
sca_tr <- matrix(data=NA,nrow=nlon,ncol=nlat)  #trend in scale
sha_tr <- matrix(data=NA,nrow=nlon,ncol=nlat)  #trend in shape
loc_tr_p <- matrix(data=NA,nrow=nlon,ncol=nlat) #p-value of the trend in location
sca_tr_p <- matrix(data=NA,nrow=nlon,ncol=nlat) #p-value of the trend in scale
sha_tr_p <- matrix(data=NA,nrow=nlon,ncol=nlat) #p-value of the trend in shape
loc_sig <- matrix(data=NA,nrow=nlon,ncol=nlat) #trend significance in location
sca_sig <- matrix(data=NA,nrow=nlon,ncol=nlat) #trend significance in scale
sha_sig <- matrix(data=NA,nrow=nlon,ncol=nlat) #trend significance in shape

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
        
        out = fevd(x, units = "deg C") # STAT
        #fits the linear trend in location
        out_loc_tr = fevd(x,units = "deg C",location.fun = ~lintr) # L-trend
        loc_tr[i,j] = out_loc_tr$results$par[2]
        
        lr_test_loc = lr.test(out,out_loc_tr)
        loc_tr_p[i,j] = lr_test_loc$p.value
        
        #fits the linear trend in scale
        out_sca_tr = fevd(x,units = "deg C",scale.fun = ~lintr) # S-trend
        sca_tr[i,j] = out_sca_tr$results$par[3]
        
        lr_test_sca = lr.test(out,out_sca_tr)
        sca_tr_p[i,j] = lr_test_sca$p.value
        
        #fits the linear trend in shape
        out_sha_tr = fevd(x,units = "deg C",shape.fun = ~lintr) # Shape-trend
        sha_tr[i,j] = out_sha_tr$results$par[3]
        
        lr_test_sha = lr.test(out,out_sha_tr)
        sha_tr_p[i,j] = lr_test_sha$p.value
        
      }
    } 
  }
}

##### significance about trend in location
ind_p_loc = which(loc_tr_p < 0.05) #If the P-value is less than 0.05 than the test is significant (non-stationary)
ind_not_p_loc = which(loc_tr_p >= 0.05) #if greater than it is not, doesn't change (stationary)
loc_sig[ind_p_loc] = 1 #points that are significant
loc_sig[ind_not_p_loc] = 0

##### significance about trend in scale
ind_p_sca = which(sca_tr_p < 0.05) #If the P-value is less than 0.05 than the test is significant (non-stationary)
ind_not_p_sca = which(sca_tr_p >= 0.05) #if greater than it is not, doesn't change (stationary)
sca_sig[ind_p_sca] = 1 #points that are significant
sca_sig[ind_not_p_sca] = 0

##### significance about trend in shape
ind_p_sha = which(sha_tr_p < 0.05) #If the P-value is less than 0.05 than the test is significant (non-stationary)
ind_not_p_sha = which(sha_tr_p >= 0.05) #if greater than it is not, doesn't change (stationary)
sha_sig[ind_p_sha] = 1 #points that are significant
sha_sig[ind_not_p_sha] = 0

## Jul.02 stop in sst server

######## Trend in both loc and sca significant
loc_sca_tr <- matrix(data=NA,nrow=nlon,ncol=nlat)  #trend in location and scale
loc_sca_tr_p <- matrix(data=NA,nrow=nlon,ncol=nlat) #p-value of the trend in location and scale
loc_sca_sig <- matrix(data=NA,nrow=nlon,ncol=nlat) #trend significance in location and scale

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
        
        out = fevd(x, units = "deg C") # STAT
        #fits the linear trend in location and scale
        out_loc_sca_tr = fevd(x,units = "deg C",location.fun = ~lintr,scale.fun = ~lintr) # LS-trend
        loc_sca_tr[i,j] = out_loc_sca_tr$results$hessian[2,1]
        
        lr_test_both = lr.test(out,out_loc_sca_tr)
        loc_sca_tr_p[i,j] = lr_test_both$p.value
        
        
      }
    } 
  }
}

##### significance about trend in location and scale
ind_p_both = which(loc_sca_tr_p < 0.05) #If the P-value is less than 0.05 than the test is significant (non-stationary)
ind_not_p_both = which(loc_sca_tr_p >= 0.05) #if greater than it is not, doesn't change (stationary)
loc_sca_sig[ind_p_both] = 1 #points that are significant
loc_sca_sig[ind_not_p_both] = 0


##### Create data frame contain loc_tr, sca_tr, and sha_tr
lonlat <- as.matrix(expand.grid(lon, lat)) # makes the lat and lon into columns
lonlatdata <- data.frame(lonlat) # then converts them into a data frame so we can add info
names(lonlatdata) <- c("lon", "lat") #changes the name

loc_trvec <- as.vector(loc_tr)
sca_trvec <- as.vector(sca_tr)
sha_trvec <- as.vector(sha_tr)
loc_sca_trvec <- as.vector(loc_sca_tr)
tr_df <- data.frame(cbind(lonlatdata,loc_trvec,sca_trvec,sha_trvec,loc_sca_trvec))
names(tr_df) <- c("Longitude", "Latitude", "Location_trend", "Scale_trend", "Shape_trend","Loc_sca_trend")

# There were like 4 weird trend values, so I used this to take them out
tr_ind <- which(tr_df$Location_trend < -10)
tr_df[tr_ind,c(3:6)] <- NA
tr_ind <- which(tr_df$sca_tr < -10)
tr_df[tr_ind,c(3:6)] <- NA
tr_ind <- which(tr_df$Loc_sca_trend < -10)
tr_df[tr_ind,c(3:6)] <- NA

save(tr_df,loc_tr,sca_tr,sha_tr,loc_sca_tr,
     loc_tr_p,sca_tr_p,sha_tr_p,loc_sca_tr_p,
     loc_sig,sca_sig,sha_sig,loc_sca_sig,
     lon,lat,file = "trend_in_parameters.Rdata")




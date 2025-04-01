##### Notes: This code is about visualizing GEV parameters, location, scale, and
#####       shape, respectively.
#####       Updated date: Jul.02.2023

##### Loads all necessary libraries
library(ggplot2)
library(extRemes)
library(distillery)
library(Lmoments)
library(dplyr)
library(raster)
library(oceanmap)
library(RColorBrewer)
library(scales)

### remote path
library(distillery,lib.loc = '/home/dzhai/R/x86_64-pc-linux-gnu-library/4.1')
library(Lmoments,lib.loc = '/home/dzhai/R/x86_64-pc-linux-gnu-library/4.1')
library(extRemes,lib.loc = '/home/dzhai/R/x86_64-pc-linux-gnu-library/4.1')
library(dplyr,lib.loc = '/home/dzhai/R/x86_64-pc-linux-gnu-library/4.1')
library(ggplot2)
library(raster)
library(oceanmap)
library(RColorBrewer)
library(scales)

##### Input SST array (set the working directory before)
## Local pathway
rm(list=ls())
setwd("/Volumes/Doris/SSTmax")
load("/Volumes/Doris/SSTmax/sst_annual_max_1982_2021.rdata")

## Remote pathway
rm(list=ls())
setwd("/home/dzhai/SSTmax")
load("/home/dzhai/SSTmax/sst_annual_max_1982_2021.rdata")
# 40 year

##### Set initial parameters
loc <- matrix(data=NA,nrow=nlon,ncol=nlat) #location parameter
sca <- matrix(data=NA,nrow=nlon,ncol=nlat)  #scale parameter
sha <- matrix(data=NA,nrow=nlon,ncol=nlat)  #shape parameter

##### Finds the location,scale and shape using maximum likelihood estimation
ind <- 0
for (i in 1:nlon) {
  for(j in 1:nlat) {
    ind <- ind + 1
    print(ind)
    x = sstmax_array[i,j,]
    y = abs(max(x) - min(x))
    
    if (sum(is.na(x))  == 0 & ( y != 0) ) {
      out <- fevd(x,  units = "deg C")
      loc[i,j] <- out$results$par[1]
      sca[i,j] <- out$results$par[2]
      sha[i,j] <- out$results$par[3]
      
    }else{
      
      loc[i,j] <- NA
      sca[i,j] <- NA
      sha[i,j] <- NA
    }
  }
}

# Take out the weird values (probably didn't converge")
# Maybe a problem from fitting using the MLE estimation

ind = which(loc < -10) # the regions with large parameters are locally consistent
loc[ind]= NA
sca[ind]= NA
sha[ind]= NA

ind1=which(sca > 100) 
loc[ind1]=NA
sca[ind1]=NA
sha[ind1]=NA

ind2=which(sha > 100) 
loc[ind2]=NA
sca[ind2]=NA
sha[ind2]=NA

lonlat <- as.matrix(expand.grid(lon, lat)) # makes the lat and lon into columns
lonlatdata <- data.frame(lonlat) # then converts them into a data frame so we can add info
names(lonlatdata) <- c("lon", "lat") #changes the name

# Makes them into a 1 column vector each so I can put into an excel file and then plot
locvec <- as.vector(loc)
scavec <- as.vector(sca)
shavec <- as.vector(sha)

# A new dataframe of the values up above
params_df <- data.frame(cbind(lonlatdata, locvec, scavec, shavec))
names(params_df) <- c("Longitude", "Latitude", "Location", "Scale", "Shape")
save(loc,sca,sha,params_df,lon,lat,file = "three_parameters.Rdata")

range(params_df$Location,na.rm=T)
# -4.119233 34.706333
range(params_df$Scale,na.rm=T)
# -2.518158e-16  8.466573e+01
range(params_df$Shape,na.rm=T)
# -19.23424  60.29342

####################################################
##### Figures plotting with ggplot package
rm(list=ls())
# load parameters
load("/Volumes/Doris/SSTmax/three_parameters.Rdata")

### Location
location_pallette <- rev(brewer.pal(10, "RdBu"))

p <- ggplot(params_df,aes(x=Longitude,y=Latitude,fill=Location)) +
  geom_tile() +
  scale_fill_gradientn(colours=location_pallette,na.value="#353b48",limits=c(-10,40)) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  labs(title = "", x = "Longitude", y = "Latitude") +
  guides(fill=guide_colorbar(title=paste0("°C"))) +
  theme(legend.position = 'right')
savename <- paste0("location.png")
ggsave(savename,width=8.27, height=3.44, dpi=300)

### Shape
# range: -20,100
shape_pallette <- rev(brewer.pal(10, "YlGnBu"))

p <- ggplot(params_df,aes(x=Longitude,y=Latitude,fill=Shape)) +
  geom_tile() +
  scale_fill_gradientn(colours=shape_pallette,na.value="#353b48",limits = c(-1,1),oob=squish) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  labs(title = "", x = "Longitude", y = "Latitude") +
  guides(fill=guide_colorbar(title=paste0("°C"))) +
  theme(legend.position = 'right')
savename <- paste0("shape.png")
ggsave(savename,width=8.27, height=3.44, dpi=300)


### Scale
#range:0,68
scale_pallette <- rev(brewer.pal(10, "Spectral"))

p <- ggplot(params_df,aes(x=Longitude,y=Latitude,fill=Scale)) +
  geom_tile() +
  scale_fill_gradientn(colours=scale_pallette,na.value="#353b48",limits=c(-0.1,2),oob=squish) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  labs(title = "", x = "Longitude", y = "Latitude") +
  guides(fill=guide_colorbar(title=paste0("°C"))) +
  theme(legend.position = 'right')
savename <- paste0("scale.png")
ggsave(savename,width=8.27, height=3.44, dpi=300)














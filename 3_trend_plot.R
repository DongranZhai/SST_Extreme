##### Notes: This code is about visualizing
#####        1) Trend in location parameter
#####        2) Trend in scale parameter
#####        3) Trend in scale parameter
#####        4) Trend in both location and scale parameters
#####       Updated date: Jul.02.2023

##### Loads all necessary libraries
# install.packages("remotes") # hatchfill
# remotes::install_github("coolbutuseless/ggpattern")
library(dplyr)
library(ggplot2)
library(raster)
library(oceanmap)
library(RColorBrewer)
library(scales)
library(ggpattern)
library(gridExtra)

###### I. Figures: Trend in location parameter ######
##### Input and set parameters
rm(list=ls())
setwd("/Volumes/Doris/SSTmax")
load("/Volumes/Doris/SSTmax/trend_in_parameters.Rdata")
tr_df_sub <- tr_df[,c(1:3)]

temp <- loc_sig[,ncol(loc_sig):1]
#temp <- temp[nrow(temp):1,]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","Sig")
tr_df_sub$Sig <- temp$Sig
ind <- which(tr_df_sub$Sig == 0)
tr_df_sub$Sig[ind] <- "No"
ind <- which(tr_df_sub$Sig == 1)
tr_df_sub$Sig[ind] <- "Yes"
ind <- which(is.na(tr_df_sub$Sig))
tr_df_sub$Sig[ind] <- "No" # NA

trend_pallette <- rev(brewer.pal(10, "RdBu"))
## method 2: points
p <- ggplot() +
  geom_raster(data = trend_loc, aes(x = Longitude, y = Latitude,fill = tr_loc )) +
  scale_fill_gradientn(colours=trend_pallette,na.value="#353b48",limits=c(-0.25,0.25),oob=squish) +
  geom_point(data = trend_loc, aes(x = Longitude, y = Latitude,color = Sig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(Yes = alpha("white", 0),No = alpha("#353b48", 0.05))) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  xlab("Longitude") +
  ylab("Latitude") +
  # ggtitle("Significant Trend in location of Global Ocean Extremes from 1982-2018") +
  guides(fill=guide_colorbar(title=expression(paste("°C·yr"^" -1")))) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme() 
savename <- paste0("location_trend.png")
ggsave(savename,width=8.27, height=3.44, dpi=300)

# # Without sig or not
# p <- ggplot(data = tr_df_sub , aes(x = Longitude, y = Latitude,fill = tr_loc )) +
#   geom_tile() +
#   scale_fill_gradientn(colours=trend_pallette,na.value="#353b48",limits=c(-0.25,0.25),oob=squish) +
#   coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
#   xlab("Longitude") +
#   ylab("Latitude") +
#   # ggtitle("Trend in location of Global Ocean Extremes from 1982-2018") +
#   guides(fill=guide_colorbar(title=expression(paste("°C·yr"^" -1")))) +
#   theme_update(plot.title = element_text(hjust = 0.5)) +
#   theme() 
# savename <- paste0("location_trend.png")
# ggsave(savename,width=8.27, height=3.44, dpi=300)

#------------------------------------------------------------------------
# # sig points
# p <- ggplot(data = trend_data, aes(x = Lon, y = Lat,
#                                    fill = trend,pattern = Sig)) +
#   geom_col_pattern(pattern_color = NA,
#                     pattern_fill = "black",
#                     pattern_angle = 45,
#                     pattern_density = 1,
#                     pattern_spacing = 0)+
#   scale_pattern_manual(values = c(Yes = "circle", No = "none"),na.omit=T) +
#   scale_fill_gradientn(colours=trend_pallette,na.value="#353b48",limits=c(-0.25,0.25),oob=squish) +
#   coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
#   xlab("Longitude") +
#   ylab("Latitude") +
#   # ggtitle("Trend in location of Global Ocean Extremes from 1982-2018") +
#   guides(fill=guide_colorbar(title=expression(paste("Trend  ", "(%yr"^" -1",")")))) +
#   theme_update(plot.title = element_text(hjust = 0.5)) +
#   theme() 
# savename <- paste0("trend_sig_location.png")
# ggsave(savename,width=8.27, height=3.44, dpi=300)
#------------------------------------------------------------------------

###### II. Figures: Trend in scale parameter ######
rm(list=ls())
setwd("/Volumes/Doris/SSTmax")
load("/Volumes/Doris/SSTmax/trend_in_parameters.Rdata")
tr_df_sub <- tr_df[,c(1:2,4)]

# ### Create data frame
# temp <- matrix2raster(tr_sca,x = lon, y = lat,layer = 1)
# sca_df <- as.data.frame(temp,xy = T)
# colnames(sca_df) <-c('Lon','Lat','trend')
# rm(list=c('temp'))
# 
# # sig_or_not <- matrix(data=NA,nrow=nlon,ncol=nlat)
# # ind_p <- which(lr_p_value < 0.05)
# # ind_not_p <- which(lr_p_value >= 0.05) 
# # sig_or_not[ind_p] <- 1
# # sig_or_not[ind_not_p] <- 0
# 
# temp <- matrix2raster(sca_sig,x = lon, y = lat,layer = 1)
# temp <- as.data.frame(temp,xy = T)
# colnames(temp) <-c('Lon','Lat','Sig')
# sca_df$Sig <- temp$Sig
# rm(list=c('temp'))
# ind <- which(sca_df$Sig ==1)
# sca_df$Sig[ind] <- "Yes"
# ind <- which(sca_df$Sig ==0)
# sca_df$Sig[ind] <- "No"
# ind <- which(is.na(sca_df$Sig))
# sca_df$Sig[ind] <- "No"

temp <- sca_sig[,ncol(sca_sig):1]
#temp <- temp[nrow(temp):1,]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","Sig")
tr_df_sub$Sig <- temp$Sig
ind <- which(tr_df_sub$Sig == 0)
tr_df_sub$Sig[ind] <- "No"
ind <- which(tr_df_sub$Sig == 1)
tr_df_sub$Sig[ind] <- "Yes"
ind <- which(is.na(tr_df_sub$Sig))
tr_df_sub$Sig[ind] <- "No" # NA

# scale_pallette <- rev(brewer.pal(10, "Spectral"))
scale_pallette <- rev(brewer.pal(10, "RdBu"))
p <- ggplot() +
  geom_raster(data = tr_df_sub, aes(x = Longitude, y = Latitude,fill = tr_sca )) +
  scale_fill_gradientn(colours=scale_pallette,na.value="#353b48",limits=c(-0.25,0.25),oob=squish) +
  geom_point(data = tr_df_sub, aes(x = Longitude, y = Latitude,color = Sig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(No =alpha("#353b48", 0.05),Yes = alpha("white", 0))) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  xlab("Longitude") +
  ylab("Latitude") +
  # ggtitle("Significant Trend in scale of Global Ocean Extremes from 1982-2018") +
  guides(fill=guide_colorbar(title=expression(paste("°C·yr"^" -1")))) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme() 
savename <- paste0("scale_trend.png")
ggsave(savename,width=8.27, height=3.44, dpi=300)

# # Without sig or not
# p <- ggplot(data = tr_df_sub, aes(x = Longitude, y = Latitude,fill = tr_sca )) +
#   geom_tile() +
#   scale_fill_gradientn(colours=scale_pallette,na.value="#353b48",limits=c(-0.25,0.25),oob=squish) +
#   coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
#   xlab("Longitude") +
#   ylab("Latitude") +
#   # ggtitle("Trend in scale of Global Ocean Extremes from 1982-2018") +
#   guides(fill=guide_colorbar(title=expression(paste("°C·yr"^" -1")))) +
#  theme_update(plot.title = element_text(hjust = 0.5)) +
#   theme() 
# savename <- paste0("trend_scale.png")
# ggsave(savename,width=8.27, height=3.44, dpi=300)

###### III. Figures: Trend in shape parameter ######
rm(list=ls())
setwd("/Volumes/Doris/SSTmax")
load("/Volumes/Doris/SSTmax/trend_in_parameters.Rdata")
tr_df_sub <- tr_df[,c(1:2,5)]

temp <- sha_sig[,ncol(sha_sig):1]
#temp <- temp[nrow(temp):1,]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","Sig")
tr_df_sub$Sig <- temp$Sig
ind <- which(tr_df_sub$Sig == 0)
tr_df_sub$Sig[ind] <- "No"
ind <- which(tr_df_sub$Sig == 1)
tr_df_sub$Sig[ind] <- "Yes"
ind <- which(is.na(tr_df_sub$Sig))
tr_df_sub$Sig[ind] <- "No" # NA

# shape_pallette <- rev(brewer.pal(10, "Spectral"))
shape_pallette <- rev(brewer.pal(10, "RdBu"))
p <- ggplot() +
  geom_raster(data = tr_df_sub, aes(x = Longitude, y = Latitude,fill = Shape_trend )) +
  scale_fill_gradientn(colours=shape_pallette,na.value="#353b48",limits=c(-0.25,0.25),oob=squish) +
  geom_point(data = tr_df_sub, aes(x = Longitude, y = Latitude,color = Sig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(No =alpha("#353b48", 0.05),Yes = alpha("white", 0))) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  xlab("Longitude") +
  ylab("Latitude") +
  # ggtitle("Significant Trend in shape of Global Ocean Extremes from 1982-2018") +
  guides(fill=guide_colorbar(title=expression(paste("°C·yr"^" -1")))) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme() 
savename <- paste0("shape_trend.png")
ggsave(savename,width=8.27, height=3.44, dpi=300)

##### IV. Figures: Trend in both location and scale parameters #####
########### Method I
### loc_sca_sig points in location
rm(list=ls())
setwd("/Volumes/Doris/SSTmax")
load("/Volumes/Doris/SSTmax/trend_in_parameters.Rdata")
tr_df_sub <- tr_df[,c(1:3)]

### Create data frame
# both_df <- matrix2raster(tr_both,x = lon, y = lat,layer = 1)
# both_df <- as.data.frame(both_df,xy = T)
# colnames(both_df)  <- c('Lon','Lat','both_trend')
temp <- loc_sca_sig[,ncol(loc_sca_sig):1]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","Sig")

tr_df_sub$Sig <- temp$Sig
ind <- which(tr_df_sub$Sig == 0)
tr_df_sub$Sig[ind] <- "No"
ind <- which(tr_df_sub$Sig == 1)
tr_df_sub$Sig[ind] <- "Yes"
ind <- which(is.na(tr_df_sub$Sig))
tr_df_sub$Sig[ind] <- "No" # NA

trend_pallette <- rev(brewer.pal(10, "RdBu"))

p <- ggplot() +
  geom_raster(data = tr_df_sub, aes(x = Longitude, y = Latitude,fill = tr_loc )) +
  scale_fill_gradientn(colours=trend_pallette,na.value="#353b48",limits=c(-0.25,0.25),oob=squish) +
  geom_point(data =tr_df_sub, aes(x = Longitude, y = Latitude,color = Sig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(Yes = alpha("white", 0),No = alpha("#353b48", 0.05))) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  xlab("Longitude") +
  ylab("Latitude") +
  # ggtitle("Significant Trend in both location and scale of Global Ocean Extremes from 1982-2018") +
  guides(fill=guide_colorbar(title=expression(paste("°C·yr"^" -1")))) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme()
savename <- paste0("trend_loc_both_sig.png")
ggsave(savename,width=8.27, height=3.44, dpi=300)

### loc_sca_sig points in scale
rm(list=ls())
setwd("/Volumes/Doris/SSTmax")
load("/Volumes/Doris/SSTmax/trend_in_parameters.Rdata")
tr_df_sub <- tr_df[,c(1:2,4)]

temp <- loc_sca_sig[,ncol(loc_sca_sig):1]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","Sig")

tr_df_sub$Sig <- temp$Sig
ind <- which(tr_df_sub$Sig == 0)
tr_df_sub$Sig[ind] <- "No"
ind <- which(tr_df_sub$Sig == 1)
tr_df_sub$Sig[ind] <- "Yes"
ind <- which(is.na(tr_df_sub$Sig))
tr_df_sub$Sig[ind] <- "No" # NA

trend_pallette <- rev(brewer.pal(10, "RdBu"))

p <- ggplot() +
  geom_raster(data = tr_df_sub, aes(x = Longitude, y = Latitude,fill = tr_sca )) +
  scale_fill_gradientn(colours=trend_pallette,na.value="#353b48",limits=c(-0.25,0.25),oob=squish) +
  geom_point(data =tr_df_sub, aes(x = Longitude, y = Latitude,color = Sig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(Yes = alpha("white", 0),No = alpha("#353b48", 0.05))) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  xlab("Longitude") +
  ylab("Latitude") +
  # ggtitle("Significant Trend in both location and scale of Global Ocean Extremes from 1982-2018") +
  guides(fill=guide_colorbar(title=expression(paste("°C·yr"^" -1")))) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme()
savename <- paste0("trend_sca_both_sig.png")
ggsave(savename,width=8.27, height=3.44, dpi=300)

######### Method II
rm(list=ls())
setwd("/Volumes/Doris/SSTmax")
load("/Volumes/Doris/SSTmax/trend_in_parameters.Rdata")
tr_df_sub <- tr_df[,c(1:4)]

temp <- loc_sig[,ncol(loc_sig):1]
#temp <- temp[nrow(temp):1,]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","Sig")
trend_loc$Sig <- temp$Sig
ind <- which(trend_loc$Sig == 0)
trend_loc$Sig[ind] <- "No"
ind <- which(trend_loc$Sig == 1)
trend_loc$Sig[ind] <- "Yes"
ind <- which(is.na(trend_loc$Sig))
trend_loc$Sig[ind] <- "No" # NA

temp <- sca_sig[,ncol(sca_sig):1]
#temp <- temp[nrow(temp):1,]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","Sig")
tr_df_sub$Sig <- temp$Sig
ind <- which(tr_df_sub$Sig == 0)
tr_df_sub$Sig[ind] <- "No"
ind <- which(tr_df_sub$Sig == 1)
tr_df_sub$Sig[ind] <- "Yes"
ind <- which(is.na(tr_df_sub$Sig))
tr_df_sub$Sig[ind] <- "No" # NA

trend_pallette <- rev(brewer.pal(10, "RdBu"))
scale_pallette <- rev(brewer.pal(10, "RdBu"))

p1 <- ggplot() +
  geom_raster(data = trend_loc, aes(x = Longitude, y = Latitude,fill = tr_loc )) +
  scale_fill_gradientn(colours=trend_pallette,na.value="#353b48",limits=c(-0.25,0.25),oob=squish) +
  geom_point(data = trend_loc, aes(x = Longitude, y = Latitude,color = Sig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(Yes = alpha("white", 0),No = alpha("#353b48", 0.05))) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  xlab("Longitude") +
  ylab("Latitude") +
  # ggtitle("Significant Trend in location of Global Ocean Extremes from 1982-2018") +
  # guides(fill=guide_colorbar(title=expression(paste("°C·yr"^" -1")))) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = 'none')
# plot.margin = margin(t = .3, r = .5, b = .3, l = .3, unit = "cm")

p2 <- ggplot() +
  geom_raster(data = tr_df_sub, aes(x = Longitude, y = Latitude,fill = tr_sca )) +
  scale_fill_gradientn(colours=scale_pallette,na.value="#353b48",limits=c(-0.25,0.25),oob=squish) +
  geom_point(data = tr_df_sub, aes(x = Longitude, y = Latitude,color = Sig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(No =alpha("#353b48", 0.05),Yes = alpha("white", 0))) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  xlab("Longitude") +
  ylab("Latitude") +
  # ggtitle("Significant Trend in scale of Global Ocean Extremes from 1982-2018") +
  guides(fill=guide_colorbar(title=expression(paste("°C·yr"^" -1")))) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = 'bottom',legend.key.width = unit(3, 'cm'))


grid.arrange(p1, p2, nrow = 2)
savename <- paste0("trend_location_scale_sig.png")
ggsave(savename,width=8.27, height=7, dpi=300)

######### Method III (Jul.02)
rm(list=ls())
setwd("/Volumes/Doris/SSTmax")
load("/Volumes/Doris/SSTmax/trend_in_parameters.Rdata")
tr_df_sub <- tr_df[,c(1:2,6)]

# range(tr_df$Location_trend,na.rm=T)
# -5.7800944  0.7115769
# range(tr_df$Scale_trend,na.rm=T)
# -1.727075 200.073918
# range(tr_df$Shape_trend,na.rm=T)
# -10.773599   6.652686
# range(tr_df$Loc_sca_trend,na.rm=T)
# -1.742852e-04  9.177070e+294

temp <- loc_sca_sig[,ncol(loc_sca_sig):1]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","Sig")

tr_df_sub$Sig <- temp$Sig
ind <- which(tr_df_sub$Sig == 0)
tr_df_sub$Sig[ind] <- "No"
ind <- which(tr_df_sub$Sig == 1)
tr_df_sub$Sig[ind] <- "Yes"
ind <- which(is.na(tr_df_sub$Sig))
tr_df_sub$Sig[ind] <- "No" # NA

trend_pallette <- rev(brewer.pal(10, "RdBu"))
p <- ggplot() +
  geom_raster(data = tr_df_sub, aes(x = Longitude, y = Latitude,fill = Loc_sca_trend )) +
  scale_fill_gradientn(colours=trend_pallette,na.value="#353b48",limits=c(-0.25,0.25),oob=squish) +
  geom_point(data =tr_df_sub, aes(x = Longitude, y = Latitude,color = Sig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(Yes = alpha("white", 0),No = alpha("#353b48", 0.05))) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  xlab("Longitude") +
  ylab("Latitude") +
  # ggtitle("Significant Trend in both location and scale of Global Ocean Extremes from 1982-2018") +
  guides(fill=guide_colorbar(title=expression(paste("°C·yr"^" -1")))) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme()
savename <- paste0("location_scale_trend.png")
ggsave(savename,width=8.27, height=3.44, dpi=300)







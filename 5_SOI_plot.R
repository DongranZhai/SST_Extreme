##### Loads all necessary libraries
library(ggplot2)
library(raster)
library(rgdal)
library(oceanmap)
library(RColorBrewer)
library(scales)

############################ Figure plot (Summer) ############################
### I. SOI in Location and Scale
rm(list=ls())
setwd("/Volumes/Doris/SSTmax")
load("/Volumes/Doris/SSTmax/trend_location.Rdata")
param_data <- trend_loc
param_data <- param_data[,c(-6)]
load("/Volumes/Doris/SSTmax/SOI_summer_analysis.Rdata")

# l
temp <- l.soi[,ncol(l.soi):1]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","lsoi")
param_data$lsoi <- temp$lsoi

temp <- sig_or_not_l.soi[,ncol(sig_or_not_l.soi):1]
#temp <- temp[nrow(temp):1,]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","lsig")
param_data$lsig <- temp$lsig

ind <- which(param_data$lsig == 0)
param_data$lsig[ind] <- "No"
ind <- which(param_data$lsig == 1)
param_data$lsig[ind] <- "Yes"
ind <- which(is.na(param_data$lsig))
param_data$lsig[ind] <- "No" #NA

# s
temp <- s.soi[,ncol(s.soi):1]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","ssoi")
param_data$ssoi <- temp$ssoi

temp <- sig_or_not_s.soi[,ncol(sig_or_not_s.soi):1]
#temp <- temp[nrow(temp):1,]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","ssig")
param_data$ssig <- temp$ssig

ind <- which(param_data$ssig == 0)
param_data$ssig[ind] <- "No"
ind <- which(param_data$ssig == 1)
param_data$ssig[ind] <- "Yes"
ind <- which(is.na(param_data$ssig))
param_data$ssig[ind] <- "No" #NA

trend_pallette <- brewer.pal(9, "Purples")

# l sig points
p <- ggplot() +
  geom_raster(data = param_data, aes(x = Longitude, y = Latitude,fill = lsoi)) +
  scale_fill_gradientn(colours=trend_pallette,na.value="#353b48",limits=c(0,1),oob=squish) +
  geom_point(data = param_data, aes(x = Longitude, y = Latitude,color = lsig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(Yes = alpha("white", 0),No = alpha("#353b48", 0.07))) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  xlab("Longitude") +
  ylab("Latitude") +
  # ggtitle("Significant SOI in location of Global Ocean Extremes from 1982-2020") +
  # guides(fill=guide_colorbar(title=expression(paste("°C·yr"^" -1")))) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme() 
savename <- paste0("l.soi.smr.png")
ggsave(savename,width=8.27, height=3.44, dpi=300)

# s sig points
p <- ggplot() +
  geom_raster(data = param_data, aes(x = Longitude, y = Latitude,fill = ssoi)) +
  scale_fill_gradientn(colours=trend_pallette,na.value="#353b48",limits=c(0,1),oob=squish) +
  geom_point(data = param_data, aes(x = Longitude, y = Latitude,color = ssig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(Yes = alpha("white", 0),No = alpha("#353b48", 0.07))) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  xlab("Longitude") +
  ylab("Latitude") +
  # ggtitle("Significant SOI in location of Global Ocean Extremes from 1982-2020") +
  # guides(fill=guide_colorbar(title=expression(paste("°C·yr"^" -1")))) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme() 
savename <- paste0("s.soi.smr.png")
ggsave(savename,width=8.27, height=3.44, dpi=300)

### II. TREND & SOI in Location and Scale
# l
temp <- l.tr_soi[,ncol(l.tr_soi):1]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","ltrsoi")
param_data$ltrsoi <- temp$ltrsoi

temp <- sig_or_not_l.tr_soi[,ncol(sig_or_not_l.tr_soi):1]
#temp <- temp[nrow(temp):1,]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","ltrsig")
param_data$ltrsig <- temp$ltrsig

ind <- which(param_data$ltrsig == 0)
param_data$ltrsig[ind] <- "No"
ind <- which(param_data$ltrsig == 1)
param_data$ltrsig[ind] <- "Yes"
ind <- which(is.na(param_data$ltrsig))
param_data$ltrsig[ind] <- "No" #NA

# s
temp <- s.tr_soi[,ncol(s.tr_soi):1]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","strsoi")
param_data$strsoi <- temp$strsoi

temp <- sig_or_not_s.tr_soi[,ncol(sig_or_not_s.tr_soi):1]
#temp <- temp[nrow(temp):1,]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","strsig")
param_data$strsig <- temp$strsig

ind <- which(param_data$strsig == 0)
param_data$strsig[ind] <- "No"
ind <- which(param_data$strsig == 1)
param_data$strsig[ind] <- "Yes"
ind <- which(is.na(param_data$strsig))
param_data$strsig[ind] <- "No" #NA

trend_pallette <- rev(brewer.pal(10, "RdBu"))
# l sig points
p <- ggplot() +
  geom_raster(data = param_data, aes(x = Longitude, y = Latitude,fill = ltrsoi)) +
  scale_fill_gradientn(colours=trend_pallette,na.value="#353b48",limits=c(-0.25,0.25),oob=squish) +
  geom_point(data = param_data, aes(x = Longitude, y = Latitude,color = ltrsig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(Yes = alpha("white", 0),No = alpha("#bdc3c7", 0.1))) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  xlab("Longitude") +
  ylab("Latitude") +
  # ggtitle("Significant Trend & SOI in location of Global Ocean Extremes from 1982-2020") +
  guides(fill=guide_colorbar(title=expression(paste("°C·yr"^" -1")))) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme() 
savename <- paste0("l.tr_soi.smr_1220.png")
ggsave(savename,width=8.27, height=3.44, dpi=300)

# s sig points
p <- ggplot() +
  geom_raster(data = param_data, aes(x = Longitude, y = Latitude,fill = strsoi)) +
  scale_fill_gradientn(colours=trend_pallette,na.value="#353b48",limits=c(-0.25,0.25),oob=squish) +
  geom_point(data = param_data, aes(x = Longitude, y = Latitude,color = strsig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(Yes = alpha("white", 0),No = alpha("#353b48", 0.07))) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  xlab("Longitude") +
  ylab("Latitude") +
  # ggtitle("Significant Trend & SOI in location of Global Ocean Extremes from 1982-2020") +
  guides(fill=guide_colorbar(title=expression(paste("°C·yr"^" -1")))) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme() 
savename <- paste0("s.tr_soi.smr.png")
ggsave(savename,width=8.27, height=3.44, dpi=300)

### III. TREND & SOI vs STAT
# l
temp <- sig_or_not_l.stat.tr_soi[,ncol(sig_or_not_l.stat.tr_soi):1]
#temp <- temp[nrow(temp):1,]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","lstat.trsig")
param_data$lstat.trsig <- temp$lstat.trsig

ind <- which(param_data$lstat.trsig == 0)
param_data$lstat.trsig[ind] <- "No"
ind <- which(param_data$lstat.trsig == 1)
param_data$lstat.trsig[ind] <- "Yes"
ind <- which(is.na(param_data$lstat.trsig))
param_data$lstat.trsig[ind] <- "No" #NA

# s
temp <- sig_or_not_s.stat.tr_soi[,ncol(sig_or_not_s.stat.tr_soi):1]
#temp <- temp[nrow(temp):1,]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","sstat.trsig")
param_data$sstat.trsig <- temp$sstat.trsig

ind <- which(param_data$sstat.trsig == 0)
param_data$sstat.trsig[ind] <- "No"
ind <- which(param_data$sstat.trsig == 1)
param_data$sstat.trsig[ind] <- "Yes"
ind <- which(is.na(param_data$sstat.trsig))
param_data$sstat.trsig[ind] <- "No" #NA

trend_pallette <- rev(brewer.pal(10, "RdBu"))

# l sig points
p <- ggplot() +
  geom_raster(data = param_data, aes(x = Longitude, y = Latitude,fill = ltrsoi)) +
  scale_fill_gradientn(colours=trend_pallette,na.value="#353b48",limits=c(-0.25,0.25),oob=squish) +
  geom_point(data = param_data, aes(x = Longitude, y = Latitude,color = lstat.trsig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(Yes = alpha("white", 0),No = alpha("#353b48", 0.07))) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  xlab("Longitude") +
  ylab("Latitude") +
  # ggtitle("Significant Trend & SOI in location of Global Ocean Extremes from 1982-2020") +
  guides(fill=guide_colorbar(title=expression(paste("°C·yr"^" -1")))) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme() 
savename <- paste0("l.tr_soi.stat.smr.png")
ggsave(savename,width=8.27, height=3.44, dpi=300)

# s sig points
p <- ggplot() +
  geom_raster(data = param_data, aes(x = Longitude, y = Latitude,fill = strsoi)) +
  scale_fill_gradientn(colours=trend_pallette,na.value="#353b48",limits=c(-0.25,0.25),oob=squish) +
  geom_point(data = param_data, aes(x = Longitude, y = Latitude,color = sstat.trsig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(Yes = alpha("white", 0),No = alpha("#353b48", 0.07))) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  xlab("Longitude") +
  ylab("Latitude") +
  # ggtitle("Significant Trend & SOI in location of Global Ocean Extremes from 1982-2020") +
  guides(fill=guide_colorbar(title=expression(paste("°C·yr"^" -1")))) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme() 
savename <- paste0("s.tr_soi.stat.smr.png")
ggsave(savename,width=8.27, height=3.44, dpi=300)

##############################################################################
############################ Figure plot (Winter) ############################
### I. SOI in Location and Scale
rm(list=ls())
setwd("/Volumes/Doris/SSTmax")
load("/Volumes/Doris/SSTmax/trend_location.Rdata")
param_data <- trend_loc
param_data <- param_data[,c(-6)]
load("/Volumes/Doris/SSTmax/SOI_winter_analysis.Rdata")

# l
temp <- l.soi[,ncol(l.soi):1]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","lsoi")
param_data$lsoi <- temp$lsoi

temp <- sig_or_not_l.soi[,ncol(sig_or_not_l.soi):1]
#temp <- temp[nrow(temp):1,]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","lsig")
param_data$lsig <- temp$lsig

ind <- which(param_data$lsig == 0)
param_data$lsig[ind] <- "No"
ind <- which(param_data$lsig == 1)
param_data$lsig[ind] <- "Yes"
ind <- which(is.na(param_data$lsig))
param_data$lsig[ind] <- "No" #NA

# s
temp <- s.soi[,ncol(s.soi):1]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","ssoi")
param_data$ssoi <- temp$ssoi

temp <- sig_or_not_s.soi[,ncol(sig_or_not_s.soi):1]
#temp <- temp[nrow(temp):1,]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","ssig")
param_data$ssig <- temp$ssig

ind <- which(param_data$ssig == 0)
param_data$ssig[ind] <- "No"
ind <- which(param_data$ssig == 1)
param_data$ssig[ind] <- "Yes"
ind <- which(is.na(param_data$ssig))
param_data$ssig[ind] <- "No" #NA

trend_pallette <- brewer.pal(9, "Purples")

# l sig points
p <- ggplot() +
  geom_raster(data = param_data, aes(x = Longitude, y = Latitude,fill = lsoi)) +
  scale_fill_gradientn(colours=trend_pallette,na.value="#353b48",limits=c(0,1),oob=squish) +
  geom_point(data = param_data, aes(x = Longitude, y = Latitude,color = lsig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(Yes = alpha("white", 0),No = alpha("#353b48", 0.07))) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  xlab("Longitude") +
  ylab("Latitude") +
  # ggtitle("Significant SOI in location of Global Ocean Extremes from 1982-2020") +
  # guides(fill=guide_colorbar(title=expression(paste("°C·yr"^" -1")))) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme() 
savename <- paste0("l.soi.wnt.png")
ggsave(savename,width=8.27, height=3.44, dpi=300)

# s sig points
p <- ggplot() +
  geom_raster(data = param_data, aes(x = Longitude, y = Latitude,fill = ssoi)) +
  scale_fill_gradientn(colours=trend_pallette,na.value="#353b48",limits=c(0,1),oob=squish) +
  geom_point(data = param_data, aes(x = Longitude, y = Latitude,color = ssig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(Yes = alpha("white", 0),No = alpha("#353b48", 0.07))) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  xlab("Longitude") +
  ylab("Latitude") +
  # ggtitle("Significant SOI in location of Global Ocean Extremes from 1982-2020") +
  # guides(fill=guide_colorbar(title=expression(paste("°C·yr"^" -1")))) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme() 
savename <- paste0("s.soi.wnt.png")
ggsave(savename,width=8.27, height=3.44, dpi=300)

### II. TREND & SOI in Location and Scale
# l
temp <- l.tr_soi[,ncol(l.tr_soi):1]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","ltrsoi")
param_data$ltrsoi <- temp$ltrsoi

temp <- sig_or_not_l.tr_soi[,ncol(sig_or_not_l.tr_soi):1]
#temp <- temp[nrow(temp):1,]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","ltrsig")
param_data$ltrsig <- temp$ltrsig

ind <- which(param_data$ltrsig == 0)
param_data$ltrsig[ind] <- "No"
ind <- which(param_data$ltrsig == 1)
param_data$ltrsig[ind] <- "Yes"
ind <- which(is.na(param_data$ltrsig))
param_data$ltrsig[ind] <- "No" #NA

# s
temp <- s.tr_soi[,ncol(s.tr_soi):1]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","strsoi")
param_data$strsoi <- temp$strsoi

temp <- sig_or_not_s.tr_soi[,ncol(sig_or_not_s.tr_soi):1]
#temp <- temp[nrow(temp):1,]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","strsig")
param_data$strsig <- temp$strsig

ind <- which(param_data$strsig == 0)
param_data$strsig[ind] <- "No"
ind <- which(param_data$strsig == 1)
param_data$strsig[ind] <- "Yes"
ind <- which(is.na(param_data$strsig))
param_data$strsig[ind] <- "No" #NA

trend_pallette <- rev(brewer.pal(10, "RdBu"))
# l sig points
p <- ggplot() +
  geom_raster(data = param_data, aes(x = Longitude, y = Latitude,fill = ltrsoi)) +
  scale_fill_gradientn(colours=trend_pallette,na.value="#353b48",limits=c(-0.25,0.25),oob=squish) +
  geom_point(data = param_data, aes(x = Longitude, y = Latitude,color = ltrsig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(Yes = alpha("white", 0),No = alpha("#bdc3c7", 0.1))) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  xlab("Longitude") +
  ylab("Latitude") +
  # ggtitle("Significant Trend & SOI in location of Global Ocean Extremes from 1982-2020") +
  guides(fill=guide_colorbar(title=expression(paste("°C·yr"^" -1")))) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme() 
savename <- paste0("l.tr_soi.wnt_1220.png")
ggsave(savename,width=8.27, height=3.44, dpi=300)

# s sig points
p <- ggplot() +
  geom_raster(data = param_data, aes(x = Longitude, y = Latitude,fill = strsoi)) +
  scale_fill_gradientn(colours=trend_pallette,na.value="#353b48",limits=c(-0.25,0.25),oob=squish) +
  geom_point(data = param_data, aes(x = Longitude, y = Latitude,color = strsig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(Yes = alpha("white", 0),No = alpha("#353b48", 0.07))) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  xlab("Longitude") +
  ylab("Latitude") +
  # ggtitle("Significant Trend & SOI in location of Global Ocean Extremes from 1982-2020") +
  guides(fill=guide_colorbar(title=expression(paste("°C·yr"^" -1")))) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme() 
savename <- paste0("s.tr_soi.wnt.png")
ggsave(savename,width=8.27, height=3.44, dpi=300)

### III. TREND & SOI vs STAT
# l
temp <- sig_or_not_l.stat.tr_soi[,ncol(sig_or_not_l.stat.tr_soi):1]
#temp <- temp[nrow(temp):1,]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","lstat.trsig")
param_data$lstat.trsig <- temp$lstat.trsig

ind <- which(param_data$lstat.trsig == 0)
param_data$lstat.trsig[ind] <- "No"
ind <- which(param_data$lstat.trsig == 1)
param_data$lstat.trsig[ind] <- "Yes"
ind <- which(is.na(param_data$lstat.trsig))
param_data$lstat.trsig[ind] <- "No" #NA

# s
temp <- sig_or_not_s.stat.tr_soi[,ncol(sig_or_not_s.stat.tr_soi):1]
#temp <- temp[nrow(temp):1,]
temp <- matrix2raster(temp,x=lon,y=lat,layer=1)
temp <- as.data.frame(temp,xy=T)
colnames(temp) <- c("Lon","Lat","sstat.trsig")
param_data$sstat.trsig <- temp$sstat.trsig

ind <- which(param_data$sstat.trsig == 0)
param_data$sstat.trsig[ind] <- "No"
ind <- which(param_data$sstat.trsig == 1)
param_data$sstat.trsig[ind] <- "Yes"
ind <- which(is.na(param_data$sstat.trsig))
param_data$sstat.trsig[ind] <- "No" #NA

trend_pallette <- rev(brewer.pal(10, "RdBu"))

# l sig points
p <- ggplot() +
  geom_raster(data = param_data, aes(x = Longitude, y = Latitude,fill = ltrsoi)) +
  scale_fill_gradientn(colours=trend_pallette,na.value="#353b48",limits=c(-0.25,0.25),oob=squish) +
  geom_point(data = param_data, aes(x = Longitude, y = Latitude,color = lstat.trsig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(Yes = alpha("white", 0),No = alpha("#353b48", 0.07))) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  xlab("Longitude") +
  ylab("Latitude") +
  # ggtitle("Significant Trend & SOI in location of Global Ocean Extremes from 1982-2020") +
  guides(fill=guide_colorbar(title=expression(paste("°C·yr"^" -1")))) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme() 
savename <- paste0("l.tr_soi.stat.wnt.png")
ggsave(savename,width=8.27, height=3.44, dpi=300)

# s sig points
p <- ggplot() +
  geom_raster(data = param_data, aes(x = Longitude, y = Latitude,fill = strsoi)) +
  scale_fill_gradientn(colours=trend_pallette,na.value="#353b48",limits=c(-0.25,0.25),oob=squish) +
  geom_point(data = param_data, aes(x = Longitude, y = Latitude,color = sstat.trsig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(Yes = alpha("white", 0),No = alpha("#353b48", 0.07))) +
  coord_cartesian(xlim = c(-180,180), ylim = c(-90, 90),expand=F) +
  xlab("Longitude") +
  ylab("Latitude") +
  # ggtitle("Significant Trend & SOI in location of Global Ocean Extremes from 1982-2020") +
  guides(fill=guide_colorbar(title=expression(paste("°C·yr"^" -1")))) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme() 
savename <- paste0("s.tr_soi.stat.wnt.png")
ggsave(savename,width=8.27, height=3.44, dpi=300)





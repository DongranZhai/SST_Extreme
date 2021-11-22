# This is code to extract annual SST maximums from the OISST daily gridded dataset.
# The data was downloaded from 1982-2020 (full years available at the time of download on 04/09/2021) here: https://psl.noaa.gov/cgi-bin/db_search/DBListFiles.pl?did=132&tid=92022&vid=2423
# Code from Claudie Beaulieu

# Load libraries
library(ncdf4)

# Input land mask (0=land, 1=ocean)
lm_filename <- paste0("/Users/cb1y12/Documents/Research_projects/SSTextremes/Data/lsmask.oisst.v2.nc")
lm_file <- nc_open(lm_filename)
mask_mat <- ncvar_get(lm_file, "lsmask")
lon <- ncvar_get(lm_file, "lon")
lat <- ncvar_get(lm_file, "lat")
nc_close(lm_file)
lon <- lon - 180 #makes the latitude on a -180-180 scale

# Initialisation
nlon <- length(lon)
nlat <- length(lat)
year <- 1982:2020
n <- length(year)
sstmax_array <- array(data=NA,dim=c(nlon,nlat,n)) # Array to be filled with SST max value each year

# Input data (looping through years)
for (i in 1:n){
  
  # Open the ncdf file
  input_filename <- paste0("/Users/cb1y12/Documents/Research_projects/SSTextremes/Data/sst.day.mean.", year[i], ".nc")
  input_file <- nc_open(input_filename)
  
  # Get the SST array
  sst_array <- ncvar_get(input_file, "sst")
  
  # Extract maximum for each grid cell for that year
  sstmax_mat <- apply(sst_array, 1:2, max, na.rm = T) 
  
  # Put the land as NAs
  sstmax_mat[mask_mat == F] <- NA 
  
  # Put the matrix with SST max at all locations for that year in the array
  sstmax_array[,,i] <- sstmax_mat
  
  # Close file
  nc_close(input_file)
  
}
# Save the array 
save(sstmax_array,lon,lat,nlon,nlat,n,year,file="sst_annual_max.Rdata")

## Nov 22
##### I made some changes.

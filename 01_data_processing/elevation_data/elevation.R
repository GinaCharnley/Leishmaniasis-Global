# Process NASA Shuttle Radar Topography Mission Global 
# https://lpdaac.usgs.gov/products/srtmgl3v003/

# Load packages 
library(dplyr)
library(terra)

# Load file
rast <- rast("wc2.1_10m_elev.tif")

# Set the desired resolution 
# 0.1° x 0.1° resolution
new_res <- c(0.25, 0.25)  

# Convert the raster to the correct spatial scale
template <- rast(ext(rast), res = new_res, crs = crs(rast))
rast2 <- resample(rast, template, method = "bilinear")

# Convert to a data frame 
rast2 <- as.data.frame(rast2, xy = TRUE)

# Save 
readr::write_rds(rast2, "nasa_srtm_elev.rds")





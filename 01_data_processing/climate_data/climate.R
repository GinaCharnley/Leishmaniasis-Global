## Process the ERA5 ECMWF Data
# https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels-monthly-means?tab=download

# Load packages 
library(dplyr)
library(purrr)
library(readr)
library(raster)

# Save file names 
files <- list.files()

result <- files %>% map(~{
  rasters <- brick(.x)
  data <- as.data.frame(rasters, xy = T)
  data <- tidyr::gather(data, date, var, 3:max(col(data))) 
  data$dates <- gsub("X","",as.character(data$dates))
  data
})  

date_col <- c("x", "y", as.character(seq(from = zoo::as.Date("1979-01-01"), to = zoo::as.Date("2018-12-01"), by = "month")))
colnames(data) <- date_col

var <- substr(files, 1, 5)
output <- reduce(result, left_join, by = c("x", "y", "dates"))
colnames(output) <- c("x", "y","date", var)

# Transform units of measure 
# Temperature from Kelvin to degrees C 
output <- output %>% mutate(tdps = tdps - 273.15)

# Calculate relative humidity
b <- 17.625
c <- 243.04
output$hurs <- 100*exp(b * c * (output$tdps - v4$tas) / ((c + output$tdps) * (c + output$tas)))

# Remove dew-point temperature 
output$tdps <- NULL

# Soil water is m³/m³

# Go back to your project w/d 
# Save the data 
write_rds(output, "era5_to_worldclim.rds")







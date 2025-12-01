# Process NASA GPW Data
# https://cmr.earthdata.nasa.gov/search/concepts/C1597158029-SEDAC.html 

# Load packages 
library(purrr)
library(dplyr)
library(raster)

# List and load files 
files <- list.files()
rasts <- files %>% map(~ raster(.x))

# Convert to data frame 
data <- rasts %>% map(~ as.data.frame(.x, xy = T))

# Save the years and name the list elements 
year <- c(2000, 2005, 2010, 2015, 2020)

# Merge to one data frame 
x <- data[[1]]$x
y <- data[[1]]$y
data <- data %>% map(~ .x %>% dplyr::select(-x, -y))
data <- do.call(cbind, data)
colnames(data) <- year
data <- cbind(data, x)
data <- cbind(data, y)

# Save 
write_rds(data, "population.rds")




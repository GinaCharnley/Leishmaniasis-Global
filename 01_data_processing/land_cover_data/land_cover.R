## Process the Copernicus Global Land Cover data 

# Load packages 
library(terra)
library(dplyr)
library(purrr)

# Check w/d and List file names 
files <- list.files()

# Set the desired resolution 
# 0.1° x 0.1° resolution
new_res <- c(0.1, 0.1)  

# Extract the year of the raster 
year <- substr(files, 28, 31)

# Extract the land cover type 
land <- substr(files, 37, nchar(files))
dash_pos <- regexpr("-", land)
land <- substr(land, 1, dash_pos - 1)

# Process the rasters 
result <- map(files, ~{
  rast1 <- rast(.x)
  template <- rast(ext(rast1), res = new_res, crs = crs(rast1))
  r_resampled <- resample(rast1, template, method = "bilinear")
  r_resampled_df <- as.data.frame(r_resampled, xy = TRUE)
})

# Merge to one dataframe and rename the columns to the land types 
result <- reduce(result, left_join, by = c("x", "y"))
colnames(result) <- c("x", "y", land)

# Save 
readr::write_rds(result, "copernicus_global_land_use_2019.rds")

# To aggreagte to a courser resolution 
# Round my coordinates 
data$x <- round(data$x, digits = 1)
data$y <- round(data$y, digits = 1)

# Calculate new coordinates from 0.1x0.1 to 0.25x0.25
data <- data %>%
  mutate(x_new = floor(x / 0.25) * 0.25,
         y_new = floor(y / 0.25) * 0.25) 

land_cover_cols <- c("BuiltUp", "MossLichen", "SeasonalWater", "Snow", "Bare", 
                     "Crops", "Forest", "PermanentWater", "Shrub", "Tree", "Grass")

# Aggregate by new grid cells (adjust summarisation as needed)
aggregated_data <- data %>%
  group_by(x_new, y_new) %>%
  summarise(across(all_of(land_cover_cols), mean, na.rm = TRUE))

# You can test how the data looks to make sure nothing is missing 
ggplot(aggregated_data, aes(x = x_new, y = y_new, fill = Crops)) + geom_tile()

# Rename coordinate columns to merge to other datasets 
aggregated_data <- aggregated_data %>% rename("x" = x_new, "y" = y_new)

# Save 
readr::write_rds(aggregated_data, "copernicus_global_land_use_2015_0.25.rds")














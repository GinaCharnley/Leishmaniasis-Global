# Spatially harmonise the data 

# Load packages
library(sf)
library(dplyr)
library(readr)

# Extract the grid 
clim <- read_rds("era5_to_worldclim.rds")
coords <- clim %>% select(x, y) %>% distinct()
coords$id <- seq(1,250560,1)
rm(clim)

# Load the GADM shapefile to assign countries to coordinates 
gadm <- read_sf("gadm_410-levels.gpkg", layer = "ADM_0")

# Identify which coordinates fall into which country POLYGONS
sf_use_s2(FALSE)
coords_sf <- st_as_sf(coords, coords = c("x", "y"), crs = st_crs(gadm))
coords_sf$id <- seq(1,250560,1)
coords_within <- st_join(coords_sf, gadm, join = st_within)

# Save 
write_csv(coords_within, "latloncountry.csv")
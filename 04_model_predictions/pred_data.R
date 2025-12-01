## Create Prediction Dataset

# Load packages
library(dplyr)
library(readr)

# Create a grid for the correct resolution and time period 
latloncountry <- read_csv("latloncountry.csv")
iso <- unique(latloncountry$ISO3)
years <- 1951:2024 
grid <- expand.grid(ISO3 = iso, year = years)
grid$year <- as.numeric(grid$year)

# Load and merge my feature datasets
population <- read_rds("population.rds")
grid <- left_join(grid, population)
rm(population)

elev <- read_rds("nasa_srtm_elev.rds")
grid <- left_join(grid, elev)
rm(elev)

land <- read_rds("copernicus_global_land_use_2019_0.25.rds")
land <- land %>% ungroup() %>% dplyr::select(x, y, Grass, Bare)
grid <- left_join(grid, land)
rm(land)

humid <- read_rds("humidity_driest.rds")
grid <- left_join(grid, humid)
rm(humid)

bio04 <- read_rds("bio04.rds")
bio04$year <- as.numeric(bio04$year)
grid <- left_join(grid, bio04)
rm(bio04)

bio19 <- read_rds("bio19.rds")
bio19$year <- as.numeric(bio19$year)
grid <- left_join(grid, bio19)
rm(bio19)

grid[is.na(grid)] <- 0

hdi <- readxl::read_excel("spatial_info.xlsx")
hdi <- data.frame(ISO3 = hdi$adm0, HDI_val = hdi$HDI_val)
grid <- left_join(grid, hdi)

grid$hdi[is.na(grid$hdi)] <- 0

# Remove non-land grid cells 
grid <- na.omit(grid)

write_rds(grid, "prediction_dataset.rds")









## Harmonising all the datasets into one 

# Load packages
library(dplyr)
library(readr)

# Have the climate data as my starting point, as this is the max scale and time
dat_all <- read_rds("era5_to_worldclim.rds")
dat_all <- dat_all %>% mutate(across(everything(), ~ ifelse(is.nan(.x), NA, .x)))
# dat_all <- dat_all %>% mutate(across(everything(), ~ ifelse(is.na(.x), 0, .x)))

# Force coordinates to correct rounding for joining 
dat_all <- dat_all %>%
  mutate(x = formatC(x, format = "f", digits = 2),
         y = formatC(y, format = "f", digits = 2))

## TO ALTITUDE 
# Load data 
alt <- read_rds("nasa_srtm_elev.rds")

# Force coordinates to correct rounding for joining 
alt <- alt %>%
  mutate(x = formatC(x, format = "f", digits = 2),
         y = formatC(y, format = "f", digits = 2))

alt <- alt %>% group_by(x, y) %>% mutate(elevation = mean(alt, na.rm = T)) %>% 
  ungroup() %>% select(-alt) %>% distinct() 

# Merge to data_all 
dat_all <- left_join(dat_all, alt)
dat_all <- na.omit(dat_all)
dat_all <- dat_all %>% distinct()
rm(alt)

## TO LAND
# Load data 
land <- read_rds("copernicus_global_land_use_2019_0.25.rds")

# Force coordinates to correct rounding for joining 
land <- land %>%
  mutate(x = formatC(x, format = "f", digits = 2),
         y = formatC(y, format = "f", digits = 2))

# Merge to data_all 
dat_all <- left_join(dat_all, land)
rm(land)
dat_all <- na.omit(dat_all)

## TO POPULATION 
# Load data 
pop <- read_rds("population.rds")

# Format grid cell
pop <- pop %>%
  mutate(x_new = floor(x / 0.25) * 0.25,
         y_new = floor(y / 0.25) * 0.25) %>% distinct()

# Format data frame
pop <- data.frame(x = pop$x_new, y = pop$y_new, population = pop$'2020')
pop <- pop %>%
  mutate(x = formatC(x, format = "f", digits = 2),
         y = formatC(y, format = "f", digits = 2))

# Remove areas with no population
pop <- pop %>% distinct()
pop <- na.omit(pop)

# Merge to data_all 
dat_all <- left_join(dat_all, pop)

# Transform NAs to 0 for no population
dat_all <- dat_all %>% mutate(population = ifelse(is.na(population), 0, population))
dat_all <- dat_all %>% distinct()

# Load spatial data 
dat_all <- left_join(dat_all, latloncountry)

# Average all the features to national 
dat_all <- dat_all %>% group_by(ISO3, year) %>% 
  mutate(across(everything(), mean, na.rm = TRUE)) %>% ungroup()

# Join the disease data 
# Load endemicity data 
end <- read_rds("country_endemicity.rds")

# Identify endemicity of each country
dat_all <- left_join(data1, end)

# Load disease data 
dis <- read_delim("disease_combined.csv", delim = ";", 
                  escape_double = FALSE, trim_ws = TRUE)

# Merge to harmonised endemic data 
dat_all <- left_join(dat_all, dis)

# Transform no disease data to zero
data1 <- data1 %>% mutate(disease = if_else(is.na(disease), 0, disease))

# Save my harmonised data 
write_rds(dat_all, "harmonised_dataset.rds")



# Convert ERA5 to WorldClim bioclimatic varaibles 

# WorldClim provides a variety of historical and future climate data which is well documented and highly used. 
# They also provide the "19 bioclimatic varaibles" which you see a lot in climate and climate-epi research 
# Here is a link to their histroical data
# https://www.worldclim.org/data/worldclim21.html

# The bioclimatic variables are derived from the monthly temperature and rainfall values 
# The idea is that they are more biologically meaningful variables. 
# The bioclimatic variables represent annual trends (e.g., mean annual temperature, annual precipitation) 
  # seasonality (e.g., annual range in temperature and precipitation) 
  # and extreme or limiting environmental factors (e.g., temperature of the coldest and warmest month, and precipitation of the wet and dry quarters). 
# A quarter is a period of three months (1/4 of the year).

# They are coded as follows:
  # BIO1 = Annual Mean Temperature
  # BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
  # BIO3 = Isothermality (BIO2/BIO7) (×100)
  # BIO4 = Temperature Seasonality (standard deviation ×100)
  # BIO5 = Max Temperature of Warmest Month
  # BIO6 = Min Temperature of Coldest Month
  # BIO7 = Temperature Annual Range (BIO5-BIO6)
  # BIO8 = Mean Temperature of Wettest Quarter
  # BIO9 = Mean Temperature of Driest Quarter
  # BIO10 = Mean Temperature of Warmest Quarter
  # BIO11 = Mean Temperature of Coldest Quarter
  # BIO12 = Annual Precipitation
  # BIO13 = Precipitation of Wettest Month
  # BIO14 = Precipitation of Driest Month
  # BIO15 = Precipitation Seasonality (Coefficient of Variation)
  # BIO16 = Precipitation of Wettest Quarter
  # BIO17 = Precipitation of Driest Quarter
  # BIO18 = Precipitation of Warmest Quarter
  # BIO19 = Precipitation of Coldest Quarter
  
# Load packages 
library(purrr)
library(dplyr)

# Load ERA5
cruts <- readr::read_rds("era5.rds")

# Calculate the bioclimatic variables for each year 
cruts$month <- as.numeric(cruts$month)

cruts_and_bioclims <- cruts %>% 
  mutate(
    # Calculate temperature range which is needed for BIO2
    tmp_range = tmx - tmn,
    # Create a column for the quarter 
    quarter = case_when(month >= 1 & month <= 3 ~ 1,
                        month >= 4 & month <= 6 ~ 2,
                        month >= 7 & month <= 9 ~ 3,
                        month >= 10 & month <= 12 ~ 4)
         ) %>% 
  group_by(x, y, year) %>%
  mutate(
    
    # BIO1 = Annual Mean Temperature
    bio1 = mean(tmp, na.rm = T),
    
    # BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
    bio2 = mean(tmp_range, na.rm = T),
    
    # BIO4 = Temperature Seasonality (standard deviation ×100)
    bio4 = sd(tmp, na.rm = T),
    
    # BIO5 = Max Temperature of Warmest Month
    bio5 = max(tmx),
    
    # BIO6 = Min Temperature of Coldest Month
    bio6 = min(tmn),
    
    # BIO7 = Temperature Annual Range (BIO5-BIO6)
    bio7 = bio5 - bio6,
    
    # BIO3 = Isothermality (BIO2/BIO7) (×100)
    bio3 = ((bio2/bio7)*100),
    
    # BIO12 = Annual Precipitation
    bio12 = sum(pre, na.rm = T),
    
    # BIO13 = Precipitation of Wettest Month
    bio13 = max(pre, na.rm = T),

    # BIO14 = Precipitation of Driest Month
    bio14 = min(pre, na.rm = T),
    
    # BIO15 = Precipitation Seasonality (Coefficient of Variation)
    bio15 = ((sd(pre, na.rm = T)/(bio12/12))*100)
         ) %>% 
  ungroup() %>% 
  group_by(x, y, year, quarter) %>% 
  # Calculate the quarter averages for temperature min and max and sum of precipitation  
  mutate(across(c(tmx, tmn), ~ mean(.x, na.rm = T), .names = "{.col}_quarter")) %>%
  mutate(pre_quarter = sum(pre, na.rm = T)) %>%
  ungroup() %>%
  group_by(x, y, year) %>%
  # Identify the driest, wettest, coldest and warmest quarters 
  mutate(
    driest = if_else(pre_quarter == min(pre_quarter, na.rm = T), 1, 0),
    coldest = if_else(tmn_quarter == min(tmn_quarter, na.rm = T), 1, 0),
    warmest = if_else(tmx_quarter == max(tmx_quarter, na.rm = T), 1, 0),
    wettest = if_else(pre_quarter == max(pre_quarter, na.rm = T), 1, 0)
  ) %>%
  group_by(x, y, year, wettest) %>%
  mutate(
    
    # BIO8 = Mean Temperature of Wettest Quarter
    bio8 = if_else(wettest == 1, mean(tmp, na.rm = T), NA_real_),
    
    # BIO16 = Precipitation of Wettest Quarter
    bio16 = if_else(wettest == 1, mean(pre, na.rm = T), NA_real_)
  ) %>%
  ungroup() %>%
  group_by(x, y, year) %>%
  mutate(
    bio8 = if_else(wettest == 1, bio8, first(na.omit(bio8))),
    bio16 = if_else(wettest == 1, bio16, first(na.omit(bio16)))
  ) %>% 
  ungroup() %>%
  group_by(x, y, year, driest) %>%
  mutate(
    
    # BIO9 = Mean Temperature of Driest Quarter
    bio9 = if_else(driest == 1, mean(tmp, na.rm = T), NA_real_),

    # BIO17 = Precipitation of Driest Quarter
    bio17 = if_else(driest == 1, mean(pre, na.rm = T), NA_real_)
  ) %>%
  ungroup() %>%
  group_by(x, y, year) %>%
  mutate(
    bio9 = if_else(driest == 1, bio9, first(na.omit(bio9))),
    bio17 = if_else(driest == 1, bio17, first(na.omit(bio17)))
  ) %>% 
  ungroup() %>%
  group_by(x, y, year, warmest) %>%
  mutate(
    
    # BIO10 = Mean Temperature of Warmest Quarter
    bio10 = if_else(warmest == 1, mean(tmp, na.rm = T), NA_real_),
    
    # BIO18 = Precipitation of Warmest Quarter
    bio18 = if_else(warmest == 1, mean(pre, na.rm = T), NA_real_)
  ) %>%
  ungroup() %>%
  group_by(x, y, year) %>%
  mutate(
    bio10 = if_else(warmest == 1, bio10, first(na.omit(bio10))),
    bio18 = if_else(warmest == 1, bio18, first(na.omit(bio18)))
  ) %>% 
  ungroup() %>%
  group_by(x, y, year, coldest) %>%
  mutate(
    
    # BIO11 = Mean Temperature of Coldest Quarter
    bio11 = if_else(coldest == 1, mean(tmp, na.rm = T), NA_real_),

    # BIO19 = Precipitation of Coldest Quarter
    bio19 = if_else(coldest == 1, mean(pre, na.rm = T), NA_real_)
  ) %>%
  ungroup() %>%
  group_by(x, y, year) %>%
  mutate(
    bio11 = if_else(coldest == 1, bio11, first(na.omit(bio11))),
    bio19 = if_else(coldest == 1, bio19, first(na.omit(bio19)))
  ) %>% 
  ungroup() %>% 
  select(-tmp_range, -quarter, -pre_quarter, -tmx_quarter, -tmn_quarter, -driest, -coldest, -warmest, -wettest)

readr::write_rds(cruts_and_bioclims, "era5_to_worldclim.rds")






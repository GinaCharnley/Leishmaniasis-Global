# Process the WHO Endemicity data 

# Load packages
library(readr)
library(dplyr)

# Load raw files 
cl_endemicity <- read_delim("cl_endemicity.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
vl_endemicity <- read_delim("vl_endemicity.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Transform to a binary outcome 
cl_endemicity <- cl_endemicity %>% rename("adm0" = SpatialDimValueCode) %>% 
  mutate(cl = if_else(Value == "Endemic", 1, 0)) %>% 
  select(-Location, -Indicator, -Value)

vl_endemicity <- vl_endemicity %>% rename("adm0" = SpatialDimValueCode) %>% 
  mutate(vl = if_else(Value == "Endemic", 1, 0)) %>% 
  select(-Location, -Disease, -Value)

# Merge to one data frame 
endemicity <- left_join(cl_endemicity, vl_endemicity)

# Create a binary outcome for endemicity to at least one 
endemicity <- endemicity %>% mutate(endemic = case_when(
  cl == 1 & vl == 1 ~ 1,
  cl == 1 & vl == 0 ~ 1,
  cl == 0 & vl == 1 ~ 1,
  cl == 0 & vl == 0 ~ 0
))

# Clean and save 
endemicity <- endemicity %>% select(adm0, endemic)
write_rds(endemicity, "country_endemicity.rds")





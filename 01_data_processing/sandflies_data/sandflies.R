# Process the GBIF sandfly data 
# https://www.gbif.org/dataset/193638ea-0d3c-4c23-865c-df53941174b8

# Load packages
library(readr)
library(tidyr)

# Read EXCEL
gbif <- read_excel("gbif.xlsx")

# Format correctly 
colnames(gbif) <- gbif[1, ]
gbif <- gbif[-c(1),]

gbif <- tibble(x = gbif$decimalLongitude,
               y = gbif$decimalLatitude,
               year = gbif$year,
               CNTR_CODE = gbif$countryCode,
               genus = gbif$genus)

# Save 
write_rds(gbif, "sandly_occurance.rds")

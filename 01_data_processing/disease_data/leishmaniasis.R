# Process the Pigott data 

library(readxl)
library(dplyr)
library(purrr)
library(countrycode)

CL <- read_excel("CL_final_dataset.xlsx")
VL <- read_excel("VL_final_dataset.xlsx")

list1 <- list(CL, VL)
disease <- c('cl', 'vl')
names(list1) <- disease

list1 <- map(list1, ~ .x %>% 
               select(YEAR, COUNTRY, DISEASE) %>% 
               rename("year" = YEAR, "adm0" = COUNTRY, "disease" = DISEASE) %>%
               mutate(disease = 1))

data1 <- bind_rows(list1)
data1 <- data1 %>% distinct()

data1$ISO3 <- countrycode(data1$adm0, "country.name", "iso3c")

readr::write_csv(data1, "disease.csv")

# Process the WHO data 
vl <- read_csv("occurance_vl.csv")
cl <- read_csv("occurance_cl.csv")

cl <- cl %>% select(SpatialDimValueCode, Period, Value) %>% 
  rename("ISO3" = SpatialDimValueCode, "year" = Period, "cl" = Value) %>% 
  mutate(leish = if_else(cl > 0, 1, 0)) %>% select(-cl)

vl <- vl %>% select(SpatialDimValueCode, Period, Value) %>% 
  rename("ISO3" = SpatialDimValueCode, "year" = Period, "vl" = Value) %>% 
  mutate(leish = if_else(vl > 0, 1, 0)) %>% select(-vl)

leish <- rbind(cl, vl)
leish <- leish %>% distinct()

write_csv(leish, "who_leish.csv")




## Post Processing Results and Plotting 

# Load packages 
library(dplyr)
library(readr)
library(purrr)
library(sf)
library(ggplot2)

## CHECK AGAINST OUT-OF-SAMPLE DATA (WHO)

# Load prediction results 
results <- read_rds("prediction_results.csv")

# Load WHO data
leish <- read_csv("who_leish.csv")

# Merge to a testing dataset 
testing <- results %>% select(ISO3, year, pred_prob) %>% distinct()
testing <- left_join(leish, testing)

# Test how well the predictions are correlated to the WHO data 
library(pROC)
cor(testing$leish, testing$pred_prob, method = "pearson", use = "complete.obs")
roc_curve <- roc(testing$leish, testing$pred_prob)
auc(roc_curve)

## SPATIAL AVERAGES 

# Load the spatial data 
spatial <- readxl::read_excel("spatial_info.xlsx")
spatial <- spatial %>% rename("ISO3" = adm0)

# Merge my spatial information to my predictions 
results <- left_join(results, spatial)
rm(spatial)

# Define the baseline and comparitor periods 
results <- results %>% mutate(time_periods = 
                                case_when(year > 1989 & year < 2000 ~ 1,
                                          year > 2013 & year < 2024 ~ 2))

# Create annual who averages 
results <- results %>% group_by(WHO, year) %>% 
  mutate(leish_pred_who = mean(pred_prob, na.rm = T)) %>% ungroup()

# Create annual lancet averages 
results <- results %>% group_by(Lancet, year) %>% 
  mutate(leish_pred_lancet = mean(pred_prob, na.rm = T)) %>% ungroup()

# Create annual global averages 
results <- results %>% group_by(year) %>% 
  mutate(leish_pred_global = mean(pred_prob, na.rm = T)) %>% ungroup()

# Create period national averages 
results <- results %>% group_by(ISO3, time_periods) %>% 
  mutate(pred_adm0_compare = mean(pred_prob, na.rm = T)) %>% ungroup()

# Create period who averages 
results <- results %>% group_by(WHO, time_periods) %>% 
  mutate(pred_who_compare = mean(pred_prob, na.rm = T)) %>% ungroup()

# Create period lancet averages 
results <- results %>% group_by(Lancet, time_periods) %>% 
  mutate(pred_lancet_compare = mean(pred_prob, na.rm = T)) %>% ungroup()

# Create period global averages 
results <- results %>% group_by(time_periods) %>% 
  mutate(pred_global_compare = mean(pred_prob, na.rm = T)) %>% ungroup()

## PLOT TIME-SERIES 

# Plot the national annual averages 
results %>% dplyr::select(year, leish_pred_adm0, ISO3) %>% distinct() %>%
ggplot(aes(x = year, y = leish_pred_adm0, color = ISO3)) + geom_line() + 
  geom_text(data = results %>%
      group_by(ISO3) %>%
      filter(year == max(year)),
    aes(label = ISO3, x = year, y = leish_pred_adm0),
    hjust = -0.1, vjust = 0, size = 3, fontface = "bold"
  ) + 
  theme_minimal() + labs(x = "Year", y = "Predicted Leishmaniasis Risk (0-1)") +
  theme(legend.position = "none", text = element_text(face = "bold"))

# Plot the Lancet annual averages 
results %>% dplyr::select(year, leish_pred_lancet, Lancet) %>%
  filter(!is.na(leish_pred_lancet) & !is.na(Lancet)) %>%
  distinct() %>% 
  ggplot(aes(x = year, y = leish_pred_lancet, color = Lancet)) + geom_line() + 
  geom_text(data = results %>%
              group_by(ISO3) %>%
              filter(year == max(year)),
            aes(label = Lancet, x = year, y = leish_pred_lancet),
            hjust = -0.1, vjust = 0, size = 3, fontface = "bold", check_overlap = TRUE
  ) + 
  theme_minimal() + labs(x = "Year", y = "Predicted Leishmaniasis Risk (0-1)") +
  theme(legend.position = "none", text = element_text(face = "bold")) + xlim(1950, 2040)

# Plot the WHO annual averages 
results %>% dplyr::select(year, leish_pred_who, WHO) %>%
  filter(!is.na(leish_pred_who) & !is.na(WHO)) %>%
  distinct() %>% 
  ggplot(aes(x = year, y = leish_pred_who, color = WHO)) + geom_line() + 
  theme_minimal() + labs(x = "Year", y = "Predicted Leishmaniasis Risk (0-1)") +
  theme(legend.position = "top", text = element_text(face = "bold")) 

# Save the averages 
averages <- results %>% 
  dplyr::select(ISO3, year, Lancet, WHO, pred_prob, leish_pred_lancet, leish_pred_who, leish_pred_global) %>%
  distinct()
write_csv(averages, "averages.csv")

## PLOT SPATIAL CHANGES 

# Calculate the change in predicted occurrence for country and grid 
comparisons <- results %>% 
  dplyr::select(ISO3, Lancet, WHO, time_periods, pred_adm0_compare, pred_who_compare, pred_lancet_compare, pred_global_compare) %>% 
  distinct()
comparisons <- na.omit(comparisons)
comparisons <- comparisons %>% mutate(time_periods = if_else(time_periods == 1, "period_90_99", "period_14_23"))
comparisons <- list(iso3 = comparisons[c(1,4,5)],
                    lancet = comparisons[c(2,4,7)],
                    who = comparisons[c(3,4,6)],
                    global = comparisons[c(4,8)])
target_columns <- c("pred_adm0_compare", "pred_who_compare", "pred_lancet_compare", "pred_global_compare")
comparisons <- map(comparisons, function(df) {
  matching_col <- intersect(names(df), target_columns)
  if (length(matching_col) > 0) {
    df <- rename(df, pred_compare = all_of(matching_col[1])) 
  }
  df
})
comparisons <- map(comparisons, ~ .x %>%
                     distinct() %>% 
                     tidyr::spread(time_periods, pred_compare) %>% 
                     mutate(change = period_14_23 - period_90_99) %>% 
                     mutate(perc_change = (period_14_23 - period_90_99)/period_90_99*100)
)

# Load the world map 
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

world <- world %>% dplyr::select(adm0_a3, geometry)
colnames(world)[1] <- "ISO3"
world <- world %>% 
  mutate(ISO3 = ifelse(ISO3 == "SOL", "SOM", ISO3)) %>% 
  mutate(ISO3 = ifelse(ISO3 == "SDS", "SSD", ISO3)) %>% 
  mutate(ISO3 = ifelse(ISO3 == "KOS", "XKK", ISO3)) %>% filter(ISO3 != "ATA")

# Plot change by country 
world <- left_join(world, comparisons[[1]])

ggplot(world) + geom_sf(aes(fill = change)) + 
  scale_fill_gradient2(low = "#377EB8", mid = "white", high = "#E41A1C", midpoint = 0) +
  theme_minimal() + geom_sf(data = world, fill = NA, alpha = 0.4) + 
  labs(fill = "Change in Predicted Risk of Leishmaniasis from 1990-1999 to 2014-2023") + 
  theme(text = element_text(face = "bold"), legend.position = "top")

# Save comparisons 
writexl::write_xlsx(comparisons, path = "comparisons.xlsx")


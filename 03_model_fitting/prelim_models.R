## Fit Preliminary Models 

# Load packages 
library(readr)
library(dplyr)
library(corrplot)
library(Hmisc)
library(reshape2)
library(xgboost)
library(shapviz)
library(mlr3)
library(mlr3learners)
library(ggplot2)

# Load fit data 
fit_dat <- read_rds("harmonised_dataset.rds")

## CORRELATIONS 

# Explore the correlations between the climate features 
cor_dat <- fit_dat[c(3:30)]

# Calculate the Pearsons correlations 
cor_matrix <- cor(cor_dat, use = "complete.obs", method = "pearson")

# Plot and save 
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)

# Save as table 
cor_matrix <- melt(cor_matrix)

# Assign a cut off to prevent over-fitting 
cor_matrix <- cor_matrix %>% 
  mutate(r_cutoff = if_else(value > 0.7 | value < -0.7, 1, 0))

# Save 
write_csv(cor_matrix, "correlation_results.csv")

## SHAP

# Transform outcome variable to numeric
shap_dat <- fit_dat[-c(1:3, 43:46)]
shap_dat$disease <- as.numeric(shap_dat$disease)

# Convert to matrix for xgboost
X <- as.matrix(shap_dat[, -40])
y <- shap_dat$disease

# Train xgboost model
dtrain <- xgb.DMatrix(X, label = y)
params <- list(objective = "binary:logistic")
model <- xgb.train(params, dtrain, nrounds = 50)

# Compute and plot SHAP values and summaries using 
# Method 1: 
shap_values2 <- xgb.importance(feature_names = colnames(X), model = model)
# Gain: the average improvement in model accuracy when a feature is used in a split, higher Gain are more influential in making predictions.
# Cover: the average number of observations (samples) affected by a feature when used for splitting, features with higher Cover affect more samples in the dataset.
# Frequency: the number of times a feature is used in all splits across all trees, aeatures used more frequently tend to be more relevant, but this doesn’t always mean they are more important than those with higher Gain.
# Importance: is sometimes included as a normalized version of Gain, it represents the feature importance as a percentage.
xgb.plot.importance(shap_values2)
ggplot(shap_values2, aes(x = reorder(Feature, Importance), y = Importance)) + # this makes the same plot as sv_importance()
  geom_bar(stat = "identity", fill = "#FFA500") + coord_flip() + 
  labs(x = "", y = "Importance")

# Method 2: 
shap_values <- shapviz(model, X = X, X_pred = dtrain)
sv_importance(shap_values, show_numbers = TRUE) 
sv_dependence(shap_values, v = c("BIO03", "Bare"))
sv_interaction(shap_values, v = "BIO03")

# The two methods give slightly different top 15 features because: 
# xgb.importance(): Is biased towards features used in early splits, meaning less frequent but highly impactful features might be ranked lower.
# shapviz() (sv_importance(): better capture non-linear interactions and are less biased towards high-frequency features.

# sv_importance only plots to top 15, so I can visualise all the features 
# I am exporting them as a plot and as a table (same as the correlations)
mean_abs_shap <- apply(abs(shap_values$S), 2, mean)
importance_df <- data.frame(
  Feature = colnames(shap_values$S),
  Importance = mean_abs_shap
)
row.names(importance_df) <- NULL
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]
write_csv(importance_df, "shap_values.csv")

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) + 
  geom_bar(stat = "identity", fill = "#FFA500") + coord_flip() + 
  labs(x = "", y = "mean(|SHAP value|)") + theme_minimal() + 
  geom_text(aes(label = round(Importance, 2)), hjust = -0.1, color = "black") +
  theme(text = element_text(face = "bold"))

## FEATURE SUBSET 

# Subset to the top 10 features and identify those which are clustered according to correlation 

shap <- read_csv("shap_values.csv")
corr <- read_csv("correlation_results.csv")

# Filter to the top 10 features (based on importance)
feat_sub <- shap %>%
  arrange(desc(Importance)) %>%  
  slice_head(n = 10)

# Subset correlations to the top 10 features and those which meet the correlations threshold 
feat <- unique(feat_sub$Feature)
corr_sub <- corr %>% filter(Var1 %in% feat) %>% filter(r_cutoff == 1)

# Identify clusters for the features 
# BIO05, 09, 10 are all highly correlated 
# Humidity in warmest and coldest quarters are also highly correlated 
feat_sub$cluster <- c(1,2,3,2,4,5,6,6,7,6)
feat_sub$Importance <- NULL

# Save selected features 
write_csv(feat_sub, "feature_subset.csv")







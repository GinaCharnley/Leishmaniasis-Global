## Make Predictions for Full Time-period 

# Load packages 
library(readr)
library(data.table)
library(dplyr)
library(xgboost)
library(Metrics)
library(boot)
library(pdp)

## FIT BEST FIT MODEL 

# Load dataset (replace with actual data loading)
fit_data <- read_rds("harmonised_dataset.rds")

# Check NAs and column incompatibilities 
# sum(is.na(fit_data))
fit_data$HDI_val <- as.numeric(fit_data$HDI_val)

# Define features and target
features <- c("Bare", "elevation", "BIO10", "HDI_val")
target <- "disease"

X <- as.matrix(fit_data[, features])
y <- as.numeric(fit_data$disease)

# Convert to xgboost DMatrix
dtrain <- xgb.DMatrix(data = X, label = y)

# Define XGBoost parameters
params <- list(
  eta = 0.1,
  max_depth = 9,
  min_child_weight = 10,
  subsample = 0.6,
  colsample_bytree = 1,
  objective = "binary:logistic",
  eval_metric = "auc",
  seed = 42
)

# Train the model
num_round <- 100  # Number of boosting rounds
set.seed(42)
model <- xgb.train(params = params, data = dtrain, nrounds = num_round, verbose = 1)

# Make predictions to get AUX
y_pred <- predict(model, dtrain)
auc_score <- auc(y, y_pred)
print(paste("AUC Score:", round(auc_score, 4)))

## PLOT PARTIAL DEPENDENCY PLOTS  

X_df <- as.data.frame(X)
colnames(X_df) <- features  

#Create a PDP for each feature 
pdp_plots <- list()
for (i in 1:length(features)) {
  pdps <- partial(model, pred.var = features[i], grid.resolution = 50, train = X_df)
  pdp_plots[[i]] <- plot(pdps)
}

## MAKE PREDICTIONS 

# Load my predictions data (features data for 1951-2024)
pred_data <- read_rds("prediction_dataset.rds")

# Check NAs and column incompatibilities 
sum(is.na(pred_data))
pred_data$HDI_val <- as.numeric(pred_data$HDI_val)

# Convert to DMatrix format
dtest <- xgb.DMatrix(data = X_pred)

# Create a function for the bootstrap 
boot_predict <- function(model, data, indices) {
  dtest_resample <- data[indices, , drop = FALSE]  
  dtest_resample <- xgb.DMatrix(data = as.matrix(dtest_resample))
  pred <- predict(model, dtest_resample) 
  return(pred)
}

# Prepare the predictor matrix (ensure the same features as training)
X_pred <- as.matrix(pred_data[, features, drop = FALSE])  

# Run bootstrap prediction using a matrix 
pred_prob <- boot(data = X_pred, statistic = boot_predict, R = 1000, model = model)
# pred_prob <- predict(model, dtest) without CI

# Extract bootstrap predictions (each row corresponds to a bootstrap sample)
bootstrap_preds <- pred_prob$t  # Matrix of bootstrap samples

# Compute mean and 95% confidence intervals
pred_mean <- colMeans(bootstrap_preds)  # Mean prediction per observation
pred_lower <- apply(bootstrap_preds, 2, quantile, probs = 0.025)  # 2.5% quantile
pred_upper <- apply(bootstrap_preds, 2, quantile, probs = 0.975)  # 97.5% quantile

# Combine into a data frame
pred_df <- data.frame(
  mean_pred = pred_mean,
  lower_CI = pred_lower,
  upper_CI = pred_upper
)

# Add predictions to the location data 
idfy <- pred_data %>% dplyr::select(ISO3, year)
prediction_results <- cbind(idfy, pred_df)

# Check for NAs to make sure all the predictions have worked 
sum(is.na(prediction_results))

# Save results 
write_csv(prediction_results, "prediction_results.csv")












 


## Identify Best Fit Model 

# Load packages 
library(readr)
library(dplyr)
library(xgboost)
library(caret)
library(pROC) 

# Load fitting dataset 
fit_dat <- read_rds("harmonised_dataset.rds")

# Load my feature subset 
feats <- read_csv("feature_subset.csv")

# Filter fitting dataset to the selected features 
cols_to_keep <- c("disease", feats$Feature, "population", "HDI_val")
final_dat <- fit_dat %>% select(all_of(cols_to_keep))
final_dat$HDI_val <- as.numeric(final_dat$HDI_val)
final_dat$HDI_val[is.na(final_dat$HDI_val)] <- 0

# Check if there are any NAs in the final dataset
any_na <- any(is.na(final_dat))
if (any_na) {
  print("There are NAs in the dataset.")
} else {
  print("There are no NAs in the dataset.")
}

# Subset training and testing dataset (75:25%)
split <- rsample::initial_split(final_dat, prop = .75)
train <- rsample::training(split)
test  <- rsample::testing(split)

# Identify all my unique formulas 
formulas_list <- list()
for (i in 2:max(feats$cluster)) { 
  cluster_combinations <- combn(unique(feats$cluster), i, simplify = FALSE)
  for (combo in cluster_combinations) {
    selected_features <- lapply(combo, function(c) {
      sample(feats$Feature[feats$cluster == c], 1)
    })
    formula <- paste("disease ~", paste(unlist(selected_features), collapse = "+"))
    formulas_list[[length(formulas_list) + 1]] <- formula
  }
}
formulas_df <- data.frame(formula = unlist(formulas_list))
formulas_df$formula_id <- 1:nrow(formulas_df)

## FITTING SUBSET OF ENVIRONMENTAL FEATURES 

# Create a vector to store model performance
results <- data.frame()

# Set the number of k-folds 
k <- 5

# Fit models for each formula and extract the performance metrics based on K-fold on the training and against the testing set 
for (i in 1:nrow(formulas_df)) {
  
  formula_str <- formulas_df$formula[i]
  formula <- as.formula(formula_str)  
  
  # Create model matrix for train and test
  model_data <- model.matrix(formula, data = train)
  test_data <- model.matrix(formula, data = test)
  
  target <- train$disease
  target_test <- test$disease
  predictors <- model_data[, -1]  
  predictors_test <- test_data[, -1]
  
  # Cross-validation
  folds <- createFolds(target, k = k, list = TRUE)
  metrics <- data.frame(rmse = numeric(k), sensitivity = numeric(k), specificity = numeric(k), auc = numeric(k))
  
  for (j in 1:k) {
    val_idx <- folds[[j]]
    train_idx <- setdiff(seq_len(nrow(predictors)), val_idx)
    
    train_X <- predictors[train_idx, , drop = FALSE]
    train_y <- target[train_idx]
    val_X <- predictors[val_idx, , drop = FALSE]
    val_y <- target[val_idx]
    
    xgb_model <- xgboost(data = as.matrix(train_X), label = train_y, nrounds = 100, objective = "binary:logistic", verbose = 0)
    
    preds <- predict(xgb_model, as.matrix(val_X))
    preds_class <- ifelse(preds > 0.5, 1, 0)
    
    metrics$rmse[j] <- sqrt(mean((preds - val_y)^2))
    confusion <- confusionMatrix(factor(preds_class), factor(val_y))
    metrics$sensitivity[j] <- confusion$byClass["Sensitivity"]
    metrics$specificity[j] <- confusion$byClass["Specificity"]
    roc_curve <- roc(val_y, preds)
    metrics$auc[j] <- auc(roc_curve)
  }
  
  # Train final model on full train set
  final_xgb_model <- xgboost(data = as.matrix(predictors), label = target, nrounds = 100, objective = "binary:logistic", verbose = 0)
  
  # Predict on test set
  test_preds <- predict(final_xgb_model, as.matrix(predictors_test))
  test_preds_class <- ifelse(test_preds > 0.5, 1, 0)
  
  # Test set metrics
  test_rmse <- sqrt(mean((test_preds - target_test)^2))
  test_confusion <- confusionMatrix(factor(test_preds_class), factor(target_test))
  test_sensitivity <- test_confusion$byClass["Sensitivity"]
  test_specificity <- test_confusion$byClass["Specificity"]
  test_roc_curve <- roc(target_test, test_preds)
  test_auc <- auc(test_roc_curve)
  
  # Store results
  results <- rbind(results, data.frame(
    formula = formulas_df$formula_id[i], 
    cv_rmse = mean(metrics$rmse, na.rm = TRUE), 
    cv_sensitivity = mean(metrics$sensitivity, na.rm = TRUE), 
    cv_specificity = mean(metrics$specificity, na.rm = TRUE), 
    cv_auc = mean(metrics$auc, na.rm = TRUE),
    test_rmse = test_rmse,
    test_sensitivity = test_sensitivity,
    test_specificity = test_specificity,
    test_auc = test_auc
  ))
}

# Identify best models 
best_mod <- results %>%
  arrange(desc(cv_auc), desc(test_auc)) %>% 
  slice_head(n = 3)

## FITTING BEST ENVIRONMENTAL MODEL WITH SOCIO-ECONOMIC VARIABLES  

# Evaluate if the socio-economic covariates improve performance to the best model 
best_form <- best_mod$formula
best_form <- formulas_df %>% filter(formula_id %in% best_form)

best_form_socio <- data.frame(
  formula = c(
    best_form$formula,
    paste0(best_form$formula, " + population"),
    paste0(best_form$formula, " + HDI_val"),
    paste0(best_form$formula, " + population + HDI_val")
  ),
  formula_id = c(1, 2, 3, 4, 5, 6, 7, 8, 3, 10, 11, 12)
)

# Create a vector to store model performance
results2 <- data.frame()

for (i in 1:nrow(best_form_socio)) {
  
  formula_str <- best_form_socio$formula[i]
  formula <- as.formula(formula_str)  
  
  # Create model matrix for train and test
  model_data <- model.matrix(formula, data = train)
  test_data <- model.matrix(formula, data = test)
  
  target <- train$disease
  target_test <- test$disease
  predictors <- model_data[, -1]  
  predictors_test <- test_data[, -1]
  
  # Cross-validation
  folds <- createFolds(target, k = k, list = TRUE)
  metrics <- data.frame(rmse = numeric(k), sensitivity = numeric(k), specificity = numeric(k), auc = numeric(k))
  
  for (j in 1:k) {
    val_idx <- folds[[j]]
    train_idx <- setdiff(seq_len(nrow(predictors)), val_idx)
    
    train_X <- predictors[train_idx, , drop = FALSE]
    train_y <- target[train_idx]
    val_X <- predictors[val_idx, , drop = FALSE]
    val_y <- target[val_idx]
    
    xgb_model <- xgboost(data = as.matrix(train_X), label = train_y, nrounds = 100, objective = "binary:logistic", verbose = 0)
    
    preds <- predict(xgb_model, as.matrix(val_X))
    preds_class <- ifelse(preds > 0.5, 1, 0)
    
    metrics$rmse[j] <- sqrt(mean((preds - val_y)^2))
    confusion <- confusionMatrix(factor(preds_class), factor(val_y))
    metrics$sensitivity[j] <- confusion$byClass["Sensitivity"]
    metrics$specificity[j] <- confusion$byClass["Specificity"]
    roc_curve <- roc(val_y, preds)
    metrics$auc[j] <- auc(roc_curve)
  }
  
  # Train final model on full train set
  final_xgb_model <- xgboost(data = as.matrix(predictors), label = target, nrounds = 100, objective = "binary:logistic", verbose = 0)
  
  # Predict on test set
  test_preds <- predict(final_xgb_model, as.matrix(predictors_test))
  test_preds_class <- ifelse(test_preds > 0.5, 1, 0)
  
  # Test set metrics
  test_rmse <- sqrt(mean((test_preds - target_test)^2))
  test_confusion <- confusionMatrix(factor(test_preds_class), factor(target_test))
  test_sensitivity <- test_confusion$byClass["Sensitivity"]
  test_specificity <- test_confusion$byClass["Specificity"]
  test_roc_curve <- roc(target_test, test_preds)
  test_auc <- auc(test_roc_curve)
  
  # Store results
  results2 <- rbind(results2, data.frame(
    formula = best_form_socio$formula_id[i], 
    cv_rmse = mean(metrics$rmse, na.rm = TRUE), 
    cv_sensitivity = mean(metrics$sensitivity, na.rm = TRUE), 
    cv_specificity = mean(metrics$specificity, na.rm = TRUE), 
    cv_auc = mean(metrics$auc, na.rm = TRUE),
    test_rmse = test_rmse,
    test_sensitivity = test_sensitivity,
    test_specificity = test_specificity,
    test_auc = test_auc
  ))
}

# Identify if the addition of socio-economic variables improves the performance  
best_mod <- results2 %>%
  arrange(desc(cv_auc), desc(test_auc)) %>% 
  slice_head(n = 1)

# Evaluate if the socio-economic covariates improve performance to the best model 
best_form <- best_mod$formula
best_form <- best_form_socio %>% filter(formula_id %in% best_form)

# Extract and save the performance of the best models 
results <- results %>% rename("formula_id" = formula)
results <- left_join(results, formulas_df)
results2 <- results2 %>% rename("formula_id" = formula)
results2 <- left_join(results2, best_form_socio)
all_results <- rbind(results, results2)
write_csv(all_results, "model_performance_metrics.csv")

## HYPERPARAMETER TUNING OF BEST ENVIRO-SOCIO MODEL 

# Convert the formula string to a formula object
best_formula <- as.formula(paste(best_form$formula))
formula_vars <- all.vars(best_formula)

# Subset train and test data to only include relevant variables
train_subset <- train[, formula_vars, drop = FALSE]
test_subset <- test[, formula_vars, drop = FALSE]

# Create model matrices
model_data_train <- model.matrix(best_formula, data = train_subset)
model_data_test <- model.matrix(best_formula, data = test_subset)

# Separate target variable
target_train <- train_subset$disease
target_test <- test_subset$disease

# Prepare predictor matrices (remove intercept column)
predictors_train <- model_data_train[, -1, drop = FALSE]
predictors_test <- model_data_test[, -1, drop = FALSE]

# Define grid search for tuning parameters
param_grid <- expand.grid(
  eta = c(0.01, 0.1, 0.3),  # Learning rate
  max_depth = c(3, 6, 9),   # Tree depth
  min_child_weight = c(1, 5, 10),  # Minimum child weight
  subsample = c(0.6, 0.8, 1.0),  # Subsampling fraction
  colsample_bytree = c(0.6, 0.8, 1.0)  # Feature subsampling
)

# Store tuning results
tuning_results <- data.frame(
  eta = numeric(), max_depth = numeric(), min_child_weight = numeric(),
  subsample = numeric(), colsample_bytree = numeric(),
  rmse = numeric(), auc = numeric()
)

# Loop over each combination of hyperparameters
for (i in 1:nrow(param_grid)) {
  
  params <- param_grid[i, ]
  
  # Train XGBoost model with current hyperparameters
  xgb_model <- xgboost(
    data = as.matrix(predictors_train), label = target_train,
    nrounds = 100, eta = params$eta, max_depth = params$max_depth,
    min_child_weight = params$min_child_weight, subsample = params$subsample,
    colsample_bytree = params$colsample_bytree, objective = "binary:logistic",
    verbose = 0
  )
  
  # Make predictions on the test set
  preds_prob <- predict(xgb_model, as.matrix(predictors_test))
  preds_class <- ifelse(preds_prob > 0.5, 1, 0)
  
  # Compute RMSE
  rmse <- sqrt(mean((preds_prob - target_test)^2))
  
  # Compute AUC using pROC
  roc_curve <- roc(target_test, preds_prob)
  auc_value <- auc(roc_curve)
  
  # Store results
  tuning_results <- rbind(tuning_results, data.frame(
    eta = params$eta, max_depth = params$max_depth,
    min_child_weight = params$min_child_weight, subsample = params$subsample,
    colsample_bytree = params$colsample_bytree,
    rmse = rmse, auc = auc_value
  ))
}

# Select the best model based on the lowest RMSE (or highest AUC)
best_params <- tuning_results[which.max(tuning_results$auc), ]
print(best_params)

# Save my tuning results 
write_csv(tuning_results, "tuning_results.csv")

# The best fit model is: 
features <- c("Bare", "elevation", "BIO10", "HDI_val")
target <- "disease"
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




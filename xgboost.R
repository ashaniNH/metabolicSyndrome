attach(trainingSet_clean)
attach(testingSet_clean)
library(dplyr)
library(dplyr)

# Convert specific columns

trainingSet_clean <- trainingSet_clean %>%
  mutate(across(c(Marital, Race, Sex, Albuminuria, MetabolicSyndrome, ), as.factor))

testingSet_clean <- testingSet_clean %>%
  mutate(across(c(Marital, Race, Sex, Albuminuria, MetabolicSyndrome, ), as.factor))


str(trainingSet_clean)
str(testingSet_clean)
# Load required packages
library(xgboost)
library(caret)
library(pROC)
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)

# Set seed for reproducibility
set.seed(42)

### 1. Data Preparation ------------------------------------------------------

# Convert target to 0/1 (XGBoost requirement)
trainingSet_clean$MetabolicSyndrome <- as.numeric(trainingSet_clean$MetabolicSyndrome) - 1
testingSet_clean$MetabolicSyndrome <- as.numeric(testingSet_clean$MetabolicSyndrome) - 1

# Convert categorical variables
convert_categorical <- function(df) {
  df %>%
    mutate(across(where(is.character), ~as.numeric(factor(.)))) %>%
    mutate(across(where(is.factor), ~as.numeric(.) - 1))
}

training_processed <- convert_categorical(trainingSet_clean)
testing_processed <- convert_categorical(testingSet_clean)

# Create DMatrix objects
dtrain <- xgb.DMatrix(
  data = as.matrix(training_processed %>% select(-MetabolicSyndrome)),
  label = training_processed$MetabolicSyndrome
)

dtest <- xgb.DMatrix(
  data = as.matrix(testing_processed %>% select(-MetabolicSyndrome)),
  label = testing_processed$MetabolicSyndrome
)

### 2. XGBoost Parameters ----------------------------------------------------

params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 6,
  eta = 0.01,
  subsample = 0.8,
  colsample_bytree = 0.8,
  gamma = 1,
  min_child_weight = 3,
  scale_pos_weight = sum(training_processed$MetabolicSyndrome == 0) / 
    sum(training_processed$MetabolicSyndrome == 1)
)

### 3. Model Training with Early Stopping ------------------------------------

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 1000,
  early_stopping_rounds = 50,
  watchlist = list(train = dtrain, test = dtest),
  print_every_n = 50,
  verbose = 1
)

### 4. Prediction & Evaluation -----------------------------------------------

get_metrics <- function(model, data, true_labels) {
  pred_prob <- predict(model, data)
  pred_class <- ifelse(pred_prob > 0.5, 1, 0)
  
  conf_matrix <- confusionMatrix(factor(pred_class, levels = c(0, 1)),
                                 factor(true_labels, levels = c(0, 1)),
                                 positive = "1")
  
  roc_obj <- roc(true_labels, pred_prob)
  
  return(list(
    Accuracy = conf_matrix$overall["Accuracy"],
    Sensitivity = conf_matrix$byClass["Sensitivity"],
    Specificity = conf_matrix$byClass["Specificity"],
    F1 = conf_matrix$byClass["F1"],
    AUC = as.numeric(auc(roc_obj))  # fix: convert AUC to numeric
  ))
}

# Get metrics
train_metrics <- get_metrics(xgb_model, dtrain, training_processed$MetabolicSyndrome)
test_metrics  <- get_metrics(xgb_model, dtest, testing_processed$MetabolicSyndrome)

### 5. Performance Comparison Table ------------------------------------------

comparison <- bind_rows(
  train_metrics %>% as.data.frame() %>% mutate(Dataset = "Training"),
  test_metrics %>% as.data.frame() %>% mutate(Dataset = "Test")
) %>%
  select(Dataset, Accuracy, Sensitivity, Specificity, F1, AUC)

# Print formatted table
kable(comparison, digits = 4, caption = "XGBoost Performance Comparison")


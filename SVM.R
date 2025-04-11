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
# Load required libraries
# Load packages
library(caret)
library(e1071)
library(pROC)
library(dplyr)

# Set seed for reproducibility
set.seed(42)

# Check target variable has both classes
table(trainingSet_clean$MetabolicSyndrome)
table(testingSet_clean$MetabolicSyndrome)

# Ensure target is factor with levels "0" and "1"
trainingSet_clean$MetabolicSyndrome <- factor(trainingSet_clean$MetabolicSyndrome, levels = c(0, 1))
testingSet_clean$MetabolicSyndrome  <- factor(testingSet_clean$MetabolicSyndrome,  levels = c(0, 1))

# Convert character columns to numeric/factor
convert_categorical <- function(df) {
  df %>%
    mutate(across(where(is.character), ~as.numeric(factor(.)))) %>%
    mutate(across(where(is.factor), ~as.numeric(.) - 1))
}

# Save target separately
y_train <- trainingSet_clean$MetabolicSyndrome
y_test <- testingSet_clean$MetabolicSyndrome

# Preprocess predictors
training_processed <- convert_categorical(trainingSet_clean %>% select(-MetabolicSyndrome))
testing_processed <- convert_categorical(testingSet_clean %>% select(-MetabolicSyndrome))

# Train SVM model
svm_model <- svm(x = training_processed, 
                 y = y_train, 
                 kernel = "radial", 
                 probability = TRUE)

# Metrics function
get_metrics <- function(model, data, true_labels) {
  # Predict classes and probabilities
  pred <- predict(model, data, probability = TRUE)
  prob_attr <- attr(pred, "probabilities")
  
  # Check structure
  if (is.null(prob_attr) || !"1" %in% colnames(prob_attr)) {
    stop("Probability for class '1' not found in prediction.")
  }
  
  pred_prob <- prob_attr[, "1"]
  
  # Ensure factor levels
  pred_class <- factor(pred, levels = c("0", "1"))
  true_labels <- factor(true_labels, levels = c("0", "1"))
  
  # Match lengths
  if (length(pred_prob) != length(true_labels)) {
    stop("Predicted probabilities and true labels have mismatched lengths.")
  }
  
  # Confusion matrix
  conf_matrix <- confusionMatrix(pred_class, true_labels, positive = "1")
  
  # ROC and AUC
  roc_obj <- roc(as.numeric(as.character(true_labels)), pred_prob)
  
  return(list(
    Accuracy = conf_matrix$overall["Accuracy"],
    Sensitivity = conf_matrix$byClass["Sensitivity"],
    Specificity = conf_matrix$byClass["Specificity"],
    F1 = conf_matrix$byClass["F1"],
    AUC = as.numeric(auc(roc_obj))
  ))
}


# Get metrics
train_metrics <- get_metrics(svm_model, training_processed, y_train)
test_metrics <- get_metrics(svm_model, testing_processed, y_test)

# Combine results
comparison <- bind_rows(
  as.data.frame(train_metrics) %>% mutate(Dataset = "Training"),
  as.data.frame(test_metrics) %>% mutate(Dataset = "Test")
) %>%
  select(Dataset, Accuracy, Sensitivity, Specificity, F1, AUC)

# Print result
knitr::kable(comparison, digits = 4, caption = "SVM Performance Comparison")

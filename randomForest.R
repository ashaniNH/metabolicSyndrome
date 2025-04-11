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
library(randomForest)  # For Random Forest implementation
library(caret)        # For confusion matrix and metrics
library(pROC)         # For AUC calculation
library(ggplot2)      # For visualization

# Set seed for reproducibility
set.seed(42)

# 1. Data Preparation -----------------------------------------------------
# Convert target to factor (if not already done)
trainingSet_clean$MetabolicSyndrome <- as.factor(trainingSet_clean$MetabolicSyndrome)
testingSet_clean$MetabolicSyndrome <- as.factor(testingSet_clean$MetabolicSyndrome)

# 2. Build Random Forest Model --------------------------------------------
rf_model <- randomForest(MetabolicSyndrome ~ .,
                         data = trainingSet_clean,
                         ntree = 500,       
                         mtry = sqrt(ncol(trainingSet_clean)-1),  # Fixed parentheses
                         importance = TRUE,  
                         na.action = na.omit)
                         
                         # View model summary
                         print(rf_model)
                         
                         # 3. Make Predictions ----------------------------------------------------
                         # For training set
                         train_pred_prob <- predict(rf_model, trainingSet_clean, type = "prob")[,2]
                         train_pred_class <- predict(rf_model, trainingSet_clean, type = "class")
                         
                         # For test set
                         test_pred_prob <- predict(rf_model, testingSet_clean, type = "prob")[,2]
                         test_pred_class <- predict(rf_model, testingSet_clean, type = "class")
                         
                         # 4. Evaluate Performance ------------------------------------------------
                         # Training set metrics
                         train_conf <- confusionMatrix(train_pred_class, 
                                                       trainingSet_clean$MetabolicSyndrome,
                                                       positive = "1")
                         
                         # Test set metrics
                         test_conf <- confusionMatrix(test_pred_class, 
                                                      testingSet_clean$MetabolicSyndrome,
                                                      positive = "1")
                         
                         # 5. Create Comparison Table ---------------------------------------------
                         comparison <- data.frame(
                           Dataset = c("Training", "Test"),
                           Accuracy = c(train_conf$overall["Accuracy"], test_conf$overall["Accuracy"]),
                           Sensitivity = c(train_conf$byClass["Sensitivity"], test_conf$byClass["Sensitivity"]),
                           Specificity = c(train_conf$byClass["Specificity"], test_conf$byClass["Specificity"]),
                           F1 = c(train_conf$byClass["F1"], test_conf$byClass["F1"]),
                           AUC = c(auc(roc(trainingSet_clean$MetabolicSyndrome, train_pred_prob)),
                                   auc(roc(testingSet_clean$MetabolicSyndrome, test_pred_prob)))
                         )
                         
                         # Print formatted comparison
                         knitr::kable(comparison, digits = 3, 
                                      caption = "Random Forest Performance Comparison")
                         
                         # 6. Visualization ------------------------------------------------------
                         # Metric comparison plot
                         metrics_long <- comparison %>%
                           pivot_longer(cols = -Dataset, names_to = "Metric", values_to = "Value")
                         
                         ggplot(metrics_long, aes(x = Metric, y = Value, fill = Dataset)) +
                           geom_bar(stat = "identity", position = position_dodge()) +
                           geom_text(aes(label = round(Value, 3)), 
                                     position = position_dodge(width = 0.9), vjust = -0.5) +
                           scale_fill_manual(values = c("Training" = "steelblue", "Test" = "salmon")) +
                           labs(title = "Random Forest Performance Metrics",
                                y = "Score") +
                           ylim(0, 1) +
                           theme_minimal() +
                           theme(axis.text.x = element_text(angle = 45, hjust = 1))
                         
                         # Variable importance plot
                         varImpPlot(rf_model, main = "Variable Importance")
                         
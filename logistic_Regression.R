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

# Install the package (only need to do this once)
#install.packages("tidyverse")

# Then load it
library(tidyverse)
# Load required packages
library(tidyverse)
library(caret)
library(pROC)

# Set seed for reproducibility
set.seed(42)

# 1. Data Preparation -----------------------------------------------------
# (Assuming your data is already loaded as trainingSet_clean and testingSet_clean)

# Convert target to factor if needed
trainingSet_clean$MetabolicSyndrome <- as.factor(trainingSet_clean$MetabolicSyndrome)
testingSet_clean$MetabolicSyndrome <- as.factor(testingSet_clean$MetabolicSyndrome)

# 2. Build Logistic Regression Model --------------------------------------
logit_model <- glm(MetabolicSyndrome ~ .,
                   data = trainingSet_clean,
                   family = binomial(link = "logit"))

# View model summary
summary(logit_model)

# 3. Make Predictions ----------------------------------------------------
# For training set
train_pred_prob <- predict(logit_model, newdata = trainingSet_clean, type = "response")
train_pred_class <- ifelse(train_pred_prob > 0.5, "1", "0") %>% 
  factor(levels = levels(trainingSet_clean$MetabolicSyndrome))

# For test set
test_pred_prob <- predict(logit_model, newdata = testingSet_clean, type = "response")
test_pred_class <- ifelse(test_pred_prob > 0.5, "1", "0") %>% 
  factor(levels = levels(testingSet_clean$MetabolicSyndrome))

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
knitr::kable(comparison, digits = 3, caption = "Model Performance Comparison")

# 6. Visualization ------------------------------------------------------
# Accuracy comparison plot
ggplot(comparison, aes(x = Dataset, y = Accuracy, fill = Dataset)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = round(Accuracy, 3)), vjust = -0.5) +
  scale_fill_manual(values = c("Training" = "steelblue", "Test" = "salmon")) +
  labs(title = "Accuracy Comparison",
       subtitle = "Logistic Regression Performance",
       y = "Accuracy") +
  ylim(0, 1) +
  theme_minimal()


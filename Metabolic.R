attach(Metabolic_Syndrome)
DataSet <- Metabolic_Syndrome
#Dimension of original data set
dim(DataSet)
str(DataSet)

DataSet$Albuminuria <- as.factor(DataSet$Albuminuria)
DataSet$MetabolicSyndrome <- as.factor(DataSet$MetabolicSyndrome)
DataSet$Sex <- as.factor(DataSet$Sex)
DataSet$Marital <- as.factor(DataSet$Marital)
DataSet$Race <- as.factor(DataSet$Race)


# Load necessary library
library(dplyr)


###DUPLICATES###
# Identify duplicate rows
duplicates <- DataSet[duplicated(DataSet), ]
#find no of duplicates 
dim(duplicates)
###Data Splitting###
#install.packages("caTools")
library(caTools)
# Set a random seed for reproducibility
set.seed(123)
# Split data: 80% training, 20% testing
split <- sample.split(DataSet$MetabolicSyndrome, SplitRatio = 0.8)  # Assuming 'Price' is the target variable
trainingSet <- subset(DataSet, split == TRUE)
testingSet  <- subset(DataSet, split == FALSE)
# Check dimensions
dim(trainingSet)  # Should be ~80% of total rows
dim(testingSet)   # Should be ~20% of total rows
str(trainingSet)
###MISSING VALUES###
colSums(is.na(trainingSet))
library(ggplot2)

# Calculate the mode (most frequent value)
marital_mode <- names(which.max(table(trainingSet$Marital)))

# Replace NAs with the mode
trainingSet$Marital[is.na(trainingSet$Marital)] <- marital_mode

# Verify no NAs remain
sum(is.na(trainingSet$Marital))  # Should return 0

library(ggplot2)
library(patchwork)  # For combining plots

library(ggplot2)
ggplot(trainingSet, aes(x = BMI)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 50, alpha = 0.7) +
  labs(title = "BMI Distribution", x = "BMI", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

library(ggplot2)
library(scales)  # For probability scales

library(ggplot2)
library(patchwork)
colSums(is.na(trainingSet))
# 1. Apply normal quantile transformation on BMI
trainingSet$BMI_normalized <- qnorm(
  rank(trainingSet$BMI, na.last = "keep") / (sum(!is.na(trainingSet$BMI)) + 1)
)
testingSet$BMI_normalized <- qnorm(
  rank(testingSet$BMI, na.last = "keep") / (sum(!is.na(testingSet$BMI)) + 1)
)

# 2. Apply normal quantile transformation on WaistCirc
trainingSet$WaistCirc_normalized <- qnorm(
  rank(trainingSet$WaistCirc, na.last = "keep") / (sum(!is.na(trainingSet$WaistCirc)) + 1)
)
testingSet$WaistCirc_normalized <- qnorm(
  rank(testingSet$WaistCirc, na.last = "keep") / (sum(!is.na(testingSet$WaistCirc)) + 1)
)

# 3. Apply normal quantile transformation on Income
trainingSet$Income_normalized <- qnorm(
  rank(trainingSet$Income, na.last = "keep") / (sum(!is.na(trainingSet$Income)) + 1)
)
testingSet$Income_normalized <- qnorm(
  rank(testingSet$Income, na.last = "keep") / (sum(!is.na(testingSet$Income)) + 1)
)
colSums(is.na(trainingSet))
colSums(is.na(testingSet))
# Remove specific columns by name
trainingSet_clean <- trainingSet[, !(names(trainingSet) %in% c("BMI", "WaistCirc", "Income_normalized"))]
testingSet_clean <- testingSet[, !(names(testingSet) %in% c("BMI", "WaistCirc", "Income_normalized"))]

# Verify removal
names(trainingSet_clean)  # Should not include the removed columns
names(testingSet_clean)  # Should not include the removed columns

# 2. Plot original vs. transformed distributions
p_original <- ggplot(trainingSet, aes(x = BMI)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 50, alpha = 0.7) +
  labs(title = "BMI Distribution", x = "BMI", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

p_transformed <- ggplot(trainingSet, aes(x = BMI_normalized)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 50, alpha = 0.7) +
  labs(title = "BMI_normalized Distribution", x = "BMI_normalized", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Combine plots
p_original + p_transformed + plot_layout(ncol = 2)

# 3. Verify normality
shapiro.test(trainingSet$BMI_normalized)  # Should have p-value > 0.05 if normally distributed

# 2. Plot original vs. transformed distributions
p_original <- ggplot(trainingSet, aes(x = WaistCirc)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 50, alpha = 0.7) +
  labs(title = "WaistCirc Distribution", x = "WaistCirc", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

p_transformed <- ggplot(trainingSet, aes(x = WaistCirc_normalized)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 50, alpha = 0.7) +
  labs(title = "WaistCirc_normalized Distribution", x = "WaistCirc_normalized", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Combine plots
p_original + p_transformed + plot_layout(ncol = 2)

# 3. Verify normality
shapiro.test(trainingSet$WaistCirc_normalized)  # Should have p-value > 0.05 if normally distributed


colSums(is.na(trainingSet_clean))

# Calculate means (excluding NAs)
bmi_mean <- mean(trainingSet_clean$BMI_normalized, na.rm = TRUE)
waist_mean <- mean(trainingSet_clean$WaistCirc_normalized, na.rm = TRUE)

# Impute missing values
trainingSet_clean$BMI_normalized[is.na(trainingSet_clean$BMI_normalized)] <- bmi_mean
trainingSet_clean$WaistCirc_normalized[is.na(trainingSet_clean$WaistCirc_normalized)] <- waist_mean

# Verify no NAs remain
colSums(is.na(trainingSet_clean[, c("BMI_normalized", "WaistCirc_normalized")]))
# Remove rows where Income is NA
trainingSet_clean <- trainingSet_clean[!is.na(trainingSet_clean$Income), ]
testingSet_clean <- testingSet_clean[!is.na(testingSet_clean$Income), ]

# Verify removal
sum(is.na(trainingSet_clean$Income))  # Should return 0
sum(is.na(testingSet_clean$Income))  # Should return 0
dim(trainingSet_clean)
dim(testingSet_clean)
colSums(is.na(trainingSet_clean))


#add your file location
write.csv(trainingSet_clean, "D:/UOC/3rd year/Sem 2/ST 3082 - Machine learning/Final Project/Metabolic_Syndrome/trainingSet_clean.csv", row.names = FALSE)
write.csv(testingSet_clean, "D:/UOC/3rd year/Sem 2/ST 3082 - Machine learning/Final Project/Metabolic_Syndrome/testingSet_clean", row.names = FALSE)














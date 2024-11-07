############### 1 ############
library(MASS)
library(dplyr)
hdata <- read.csv("heart.csv")
sum(is.na(hdata))
impute(hdata,what = "mean")

train_data <- hdata[1:(0.9*nrow(hdata)), ]
test_data <- hdata[((0.9*nrow(hdata))+1):nrow(hdata) , ]

lda_model <- lda(train_data$coronary.hd ~ ., data = train_data)
qda_model <- qda(train_data$coronary.hd ~ ., data = train_data)
# Predict on test data and calculate misclassification rate for LDA
lda_test_pred <- predict(lda_model, test_data)$class
lda_test_misclass_rate <- mean(lda_test_pred != test_data$coronary.hd)

# Predict on test data and calculate misclassification rate for QDA
qda_test_pred <- predict(qda_model, test_data)$class
qda_test_misclass_rate <- mean(qda_test_pred != test_data$coronary.hd)
cat("LDA - Test Set:", lda_test_misclass_rate, "\n")
cat("QDA - Test Set:", qda_test_misclass_rate, "\n")

############## 2 ################

# Load the dataset
currency_crisis <- read.csv("currency_crisis.csv",header = TRUE)
labels <- currency_crisis[1,-1]
currency_crisis <- currency_crisis[-1,]
for(i in c(2:18)){
  currency_crisis[,i] <- as.numeric(currency_crisis[,i])
}

# Data Preparation: Check and handle missing values if necessary
# Assuming there are no missing values; if there are, handle them appropriately (e.g., by imputation).

# Define the classification variable as factor for LDA and QDA
currency_crisis$CRISIS.INDICATOR <- as.factor(currency_crisis$CRISIS.INDICATOR)

# Scenario (i): Split data with the last 10% of rows as the test set
set.seed(123)
n <- nrow(currency_crisis)
test_size <- floor(0.1 * n)
train_data_i <- currency_crisis[1:(n - test_size),-1]
test_data_i <- currency_crisis[(n - test_size + 1):n,-1]

# Scenario (ii): Split data with every 10th observation in the test set
test_indices_ii <- seq(10, n, by = 10)
train_data_ii <- currency_crisis[-test_indices_ii,-1]
test_data_ii <- currency_crisis[test_indices_ii,-1]

# Function to calculate misclassification rate
misclassification_rate <- function(pred, actual) {
  mean(pred != actual)
}

# (a) Fisher Linear Discriminant Analysis
# Train LDA model on both splits
lda_model_i <- lda(CRISIS.INDICATOR ~ ., data = train_data_i)
lda_model_ii <- lda(CRISIS.INDICATOR ~ ., data = train_data_ii)

# Predictions and misclassification rates for Scenario (i)
lda_pred_train_i <- predict(lda_model_i, train_data_i)$class
lda_pred_test_i <- predict(lda_model_i, test_data_i)$class
lda_train_misclassification_i <- misclassification_rate(lda_pred_train_i, train_data_i$CRISIS.INDICATOR)
lda_test_misclassification_i <- misclassification_rate(lda_pred_test_i, test_data_i$CRISIS.INDICATOR)

# Predictions and misclassification rates for Scenario (ii)
lda_pred_train_ii <- predict(lda_model_ii, train_data_ii)$class
lda_pred_test_ii <- predict(lda_model_ii, test_data_ii)$class
lda_train_misclassification_ii <- misclassification_rate(lda_pred_train_ii, train_data_ii$CRISIS.INDICATOR)
lda_test_misclassification_ii <- misclassification_rate(lda_pred_test_ii, test_data_ii$CRISIS.INDICATOR)

# (b) Quadratic Discriminant Analysis
# Train QDA model on both splits
qda_model_i <- qda(CRISIS.INDICATOR ~ ., data = train_data_i)
qda_model_ii <- qda(CRISIS.INDICATOR ~ ., data = train_data_ii)

# Predictions and misclassification rates for Scenario (i)
qda_pred_train_i <- predict(qda_model_i, train_data_i)$class
qda_pred_test_i <- predict(qda_model_i, test_data_i)$class
qda_train_misclassification_i <- misclassification_rate(qda_pred_train_i, train_data_i$CRISIS.INDICATOR)
qda_test_misclassification_i <- misclassification_rate(qda_pred_test_i, test_data_i$CRISIS.INDICATOR)

# Predictions and misclassification rates for Scenario (ii)
qda_pred_train_ii <- predict(qda_model_ii, train_data_ii)$class
qda_pred_test_ii <- predict(qda_model_ii, test_data_ii)$class
qda_train_misclassification_ii <- misclassification_rate(qda_pred_train_ii, train_data_ii$CRISIS.INDICATOR)
qda_test_misclassification_ii <- misclassification_rate(qda_pred_test_ii, test_data_ii$CRISIS.INDICATOR)

# Display the results
cat("Misclassification Rates:\n")
cat("LDA - Scenario (i): Train =", lda_train_misclassification_i, ", Test =", lda_test_misclassification_i, "\n")
cat("LDA - Scenario (ii): Train =", lda_train_misclassification_ii, ", Test =", lda_test_misclassification_ii, "\n")
cat("QDA - Scenario (i): Train =", qda_train_misclassification_i, ", Test =", qda_test_misclassification_i, "\n")
cat("QDA - Scenario (ii): Train =", qda_train_misclassification_ii, ", Test =", qda_test_misclassification_ii, "\n")


######## 3 ###########

wine_data <- read.csv("wine.csv")
str(wine_data)
new_data <- wine_data[1:130 ,1:14]
sum(is.na(new_data))
new_data$Type <- ifelse(new_data$Type == "A", 0, 1)
new_data$Type <- as.factor(new_data$Type)

train <- new_data[1:(0.9*nrow(new_data)), ]
test <- new_data[(0.9*nrow(new_data) +1):nrow(new_data),]

lda <- lda(Type ~ ., train)
qda <- qda(Type ~. , train)

#### misclassification probability
predicted_test <- predict(lda, test)$class
missclassification_prob <- mean(predicted_test != test$Type)

pred_qda_test <- predict(qda,test)$class
miss_prob <- mean(pred_qda_test != test$Type)

#winter break project - random forest regression

#installing tidyverse library
library(tidyverse)
library(caret)   

#importing the dataset
bh <- read.csv("~/Desktop/BostonHousing.csv")

#quick glances at the datasets
str(bh)
summary(bh)

#removing all the missing observations from the dataset -> cleaning it 
bh_clean <- na.omit(bh)
sum(is.na(bh_clean))   # should now be 0

#checking for missing values in the dataset
colSums(is.na(bh_clean))

#checking for duplicates
sum(duplicated(bh_clean)) #no duplicate values 

#evaluating performance on unseen data
set.seed(123)  # reproducible
n <- nrow(bh_clean)
train_idx <- sample(seq_len(n), size = 0.7 * n)  # 70% training

train <- bh_clean[train_idx, ]
test <- bh_clean[-train_idx, ]

install.packages(("randomForest"))
library(randomForest)

train_clean <- na.omit(train)
test_clean  <- na.omit(test)

rf_model <- randomForest(
  medv ~ .,
  data = train_clean,
  ntree = 500,
  mtry = floor(sqrt(ncol(train_clean)-1)),
  importance = TRUE
)

rf_model

#predicting on the test data
preds <- predict(rf_model, newdata = test_clean)

preds

#evaluating performance
# RMSE - tells us how far our predicted values are, on average, from the true/actual values 
rmse <- sqrt(mean((preds - test_clean$medv)^2))
rmse

# R-squared
sst <- sum((test_clean$medv - mean(train_clean$medv))^2)  # total variance
sse <- sum((preds - test_clean$medv)^2)                    # unexplained variance
r2 <- 1 - sse/sst
r2

#checking feature importance
# Numeric importance
importance(rf_model)

# Plot
varImpPlot(rf_model)

#visualizing predictions vs actuals
plot(test_clean$medv, preds,
     xlab = "Actual medv",
     ylab = "Predicted medv",
     main = "Random Forest Predictions vs Actuals")
abline(a = 0, b = 1, col = "red")  # perfect predictions line
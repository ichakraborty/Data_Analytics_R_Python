# Set working directory and place train and test dataset to working directory
#setwd("/Users/atrivedi/Documents/TACC/GitHub/Global_Big_Data_Conference_Materials/Santa_Clara_2016/Predictive_Analytics_R/R_Tutorial/scripts/7_predictive_analytics_case_study")

# Read train dataset
train <- read.csv("./data/train.csv")

# remove TransactionID from train dataset
train <- train[,-1]

# Create model: glm is used to
# fit generalized linear models, 
# specified by giving a symbolic description
# of the linear predictor and a description
# of the error distribution.

model <- glm(train$label~.,family=binomial(),data=train)

# Calculate accuracy of model
predicted <- round(predict(model,newdata=train,type="response"))
actual <- train$label
xtab <- table(predicted, actual)
# load Caret package for computing Confusion matrix
library(caret) 
confusionMatrix(xtab)

#Load test dataset 
test <- read.csv("./data/test.csv")

#Predict for test data
test_predict <- predict(model,newdata=test,type="response")

#creating label for test dataset
label <- rep(0,nrow(test))

# set label equal to 1 where probabilty of return > 0.6
label[test_predict>0.6] <- 1

# attach label to test dataset
test$label <- label

# view test dataset containing label predicted by model.
# It can be viewed by clicking on test from workspace.
# label is generated in last column of test dataset.
# The label gives us high or low web transactions.

# Identify transactionID where label is 1. 
high_prob_transactionIds <- test$TransactionID[test$label==1]
high_prob_transactionIds


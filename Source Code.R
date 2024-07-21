rm(list=ls()) #clears objects from workspace

#importing libraries
library(MASS)
library(mfx)
library(mlbench)
library(naivebayes)
library(caret)
library(e1071)


#import dataset
d = read.csv("/Users/stlp/Downloads/creditcardclientsdefault.csv")

#EXPLORATORY DATA ANALYSIS
#########################

str(d) #gives the structure of the dataset

head(d)

View(d)

summary(d) #summary statistics table

#checking for missing values
any_missing <- any(is.na(d)) 

#converting the variable into a factor variable
d$default.payment.next.month = as.factor(d$default.payment.next.month) 

hist(d$AGE, main = "Age Distribution", xlab = "Age")
hist(d$EDUCATION, main = "Education Distribution", xlab = "1 = graduate school; 2 = university; 3 = high school")

#logistic regression
logit_model = glm(default.payment.next.month ~ LIMIT_BAL + EDUCATION + AGE + PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6, d, family = "binomial")
summary(logit_model)
logitmfx(default.payment.next.month ~ LIMIT_BAL + EDUCATION + AGE + PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6, d)

#probit regression
probit_model = glm(default.payment.next.month ~ LIMIT_BAL + EDUCATION + AGE + PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6, data = d, family = binomial(link = "probit"))
summary(probit_model)
probitmfx(default.payment.next.month ~ LIMIT_BAL + EDUCATION + AGE + PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6, d)

# PREDICTION MODEL
#########################

set.seed(0)
train_Control = trainControl(method = "cv", number = 5)

# training the knn model
knn_caret = train(default.payment.next.month ~ LIMIT_BAL + EDUCATION + AGE + PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6, 
                  d,method = "knn", trControl = train_Control,
                  tuneLength = 10)
knn_caret

# accuracy versus number of neighbors
plot(knn_caret)

#training the naive bayes model
naivebayes_caret = train(default.payment.next.month ~ LIMIT_BAL + EDUCATION + AGE + PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6, 
                         d,method = "naive_bayes", trControl = train_Control,
                         tuneLength = 10)
naivebayes_caret










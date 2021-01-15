# TASK : APPLY LOGISTIC REGRESSION ALGORITHM TO MNIST DATASET
###Step 1: Importing required libraries###
library(MASS)
library(class)
library(gmodels)
library(caret)
library(dplyr)
library(tidyverse)
library(data.table)
library(mltools)
library(caret)
library(dataPreparation)
library(MASS)
library(bigmemory)
library(rpart)
library(rpart.plot)
library(xgboost)




###Step 2:Importing Training and Testing dataset###
train._lr_data <- read.csv(file.choose(),  stringsAsFactors=FALSE)
test._lr_data <- read.csv(file.choose(), stringsAsFactors=FALSE)

###Step 3:reducing the dataset into 30%###
train._lr_data <- head(train._lr_data, 18000)
test._lr_data <- head(test._lr_data, 3000)

set.seed(1234)

training.samples <- createDataPartition(train._lr_data,p = 0.7, list = FALSE)

MyTrainData.labels <- train._lr_data[1:17999, 1]

model_dt <- rpart(as.factor(train_label) ~ ., data = train._lr_data, method = "class")

train_label <- train._lr_data[1:17999, 1]

predict_unseen <- predict(model_dta, test._lr_data, type = "class")

testing <- na.omit(test._lr_data)

tsa3_labels <- tsa3[1:17999, 1]

qol_model <- C5.0(as.factor[,-c(1)], as.factor(tsa3$X9))

qol$cd<-factor(qol$cd, levels=c(F, T), labels = c("minor_disease", "severe_disease"))

vars <- c(tsa3$X0, tsa3$X0.1)


##### changing models into mtrix form to be used in xgboost for training and testing ######
labeltrain <- tra3$X9

tra4_train <- xgb.DMatrix(data.matrix(tra3), label = labeltrain)

labeltest <- tsa3$X9

tsa4_train <- xgb.DMatrix(data.matrix(tsa3), label = labeltest)

parameters <- list(eta = 0.1, max_depth = 4,   eval_metric = "error")

qol_xgboost <- xgboost(data=tra4_train, nround=20, params=parameters)

qol_xgboost_2 <- xgboost(data=tsa4_train, nround=20, params=parameters)

xg <- xgboost::xgboost(data = as.matrix(tra4_train), label = labeltrain, 
                       max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")

tsa_testing <- tsa3[, -1]

# TASK : APPLY K-NN ALGORITHM TO FASHION-MNIST DATASET
###Step 1: Importing required libraries###
library(MASS)
library(class)
library(gmodels)
library(caret)
library(dplyr)
library(rpart)
library(rpart.plot)
library(gbm)


###Step 2:Importing Training and Testing dataset###
train._lr_data <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE)
test._lr_data <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE)

train.dt.data <- train._lr_data[sample(nrow(train._lr_data), 18000), ]
test.dt.data <- test._lr_data[sample(nrow(train._lr_data), 3000), ]

tra1 <- train._lr_data[, 1:105]
tsa1 <- test._lr_data[, 1:105]

tra2 <- tra1[sample(nrow(tra1), 18000), ]
tsa2 <- tsa1[sample(nrow(tsa1), 3000), ]

tra3 <- filter(tra2, (tra2$X9 == 1) | (tra2$X9 == 0))
tsa3 <- filter(tsa2, (tsa2$X9 == 1) | (tsa2$X9 == 0))

xg <- xgboost::xgboost(data = as.matrix(tsa3), label = tsa3$X9, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")


trr2 <- ifelse(tr2 == 1, "1","0")

tr <- train.dt.data[, 1:4]
ts <- test.dt.data[, 1:4]

#### gradient boosting with taking all columns together ########

model_test <- gbm(formula = tra3$X9 ~ tra3$X0 + tra3$X0.1 
                  + tra3$X0.2 + tra3$X0.3 + tra3$X0.4 + 
                    tra3$X0.5 + tra3$X0.6 + tra3$X0.7 + tra3$X0.8 
                  + tra3$X0.9 + tra3$X0.10 + tra3$X0.11 + tra3$X0.12
                  + tra3$X0.13 + tra3$X0.14 + tra3$X0.15 + tra3$X0.16
                  + tra3$X0.17 + tra3$X0.18 + tra3$X0.19 + tra3$X0.20
                  + tra3$X0.21 + tra3$X0.22 + tra3$X0.23 + tra3$X0.24
                  + tra3$X0.25 + tra3$X0.26 + tra3$X0.27 + tra3$X0.28
                  + tra3$X0.29 + tra3$X0.30 + tra3$X0.31 + tra3$X0.32
                  + tra3$X0.33 + tra3$X0.34 + tra3$X0.35 + tra3$X0.36 
                  + tra3$X0.37 + tra3$X0.38 + tra3$X0.39 + tra3$X0.40 + tra3$X0.41 + tra3$X0.42 + tra3$X0.43 + tra3$X0.44 + tra3$X0.45 + tra3$X0.46 
                  + tra3$X0.47 + tra3$X0.48 + tra3$X0.49 + tra3$X0.50 + tra3$X0.51 + tra3$X0.52 + tra3$X0.53 + tra3$X0.54 + tra3$X0.55 + tra3$X0.56 + tra3$X0.57 + tra3$X0.58 + tra3$X0.59 + tra3$X0.60 + tra3$X0.61 + tra3$X0.62 + tra3$X0.63 + tra3$X0.64 + tra3$X0.65 + tra3$X0.66 + tra3$X0.67 + tra3$X0.68 + tra3$X0.69 + tra3$X0.70 + tra3$X0.71 + tra3$X0.72 + tra3$X0.73 + tra3$X0.74 + tra3$X0.75 + tra3$X0.76 + tra3$X0.77 + tra3$X0.78 + tra3$X0.79 +
                    tra3$X0.80 + tra3$X0.81 + tra3$X0.82 + tra3$X0.83 + tra3$X0.84 + tra3$X0.85 + tra3$X0.86 + tra3$X0.87 + tra3$X0.88 + tra3$X0.89 + tra3$X0.90 + tra3$X0.91 + tra3$X0.92 + tra3$X0.93 + tra3$X0.94 + tra3$X0.95 + tra3$X0.96 + tra3$X0.97 + tra3$X0.98 + tra3$X0.99 , data = tra3, n.trees = 100, distribution = "bernoulli")


pred <- predict(model_test, tsa3)

test.dt.data <- test.dt.data[,1:4]

train_testing_data = train._lr_data[, 1: 4]

testing_testing = test._lr_data[, 1:4]

caret::confusionMatrix(as.factor(testing_testing$X9, levels = 1:100), as.factor(pr, levels = 1:100))

caret::confusionMatrix(as.factor(tsa3$X9), as.factor(pr), cutoff = 0.5)

train_lr_data <- train.data
test_lr_data <- test.data

model <- glm(train_lr_data$X5 ~ . - train._lr_data$X5, data = train._lr_data)

### C.50 algoptrithm for the following ##


model<-C5.0(train.data[,-c(1)], train.data$X9)  

model  

# produce summary of decision tree fitted on Training set
summary(model)  

summary(train.data$X9)

###### Plotting the decision tree
plot(model, type="simple")   # quite a messy image. Find out how to get a clean graph.
plot(model, subtree=4)  # tree from node 4

plot(model, subtree=8) # tree from node 8 which is leave node


qol_predC50 <- predict.C5.0(qol_model, qol_test [, -c(40,41)], type="class")


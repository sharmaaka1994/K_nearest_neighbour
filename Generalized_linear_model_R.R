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



###Step 2:Importing Training and Testing dataset###
train._lr_data <- read.csv(file.choose(),  stringsAsFactors=FALSE)
test._lr_data <- read.csv(file.choose(), stringsAsFactors=FALSE)

###Step 3:reducing the dataset into 30%###
train._lr_data <- head(train._lr_data, 18000)
test._lr_data <- head(test._lr_data, 3000)


### making the dataset according the digit need ### 
train_1_lr_data <- train._lr_data
train_2_lr_data <- train._lr_data
train_3_lr_data <- train._lr_data
train_4_lr_data <- train._lr_data
train_5_lr_data <- train._lr_data
train_6_lr_data <- train._lr_data
train_7_lr_data <- train._lr_data
train_8_lr_data <- train._lr_data
train_9_lr_data <- train._lr_data


###Step 4:filtering the data for the predictor ###
train_1_lr_data$X5 <- data.frame(ifelse(train_1_lr_data$X5==1, 1, 0))
train_2_lr_data$X5 <- data.frame(ifelse(train_2_lr_data$X5==2, 1, 0))
train_3_lr_data$X5 <- data.frame(ifelse(train_3_lr_data$X5==3, 1, 0))
train_4_lr_data$X5 <- data.frame(ifelse(train_4_lr_data$X5==4, 1, 0))
train_5_lr_data$X5 <- data.frame(ifelse(train_5_lr_data$X5==5, 1, 0))
train_6_lr_data$X5 <- data.frame(ifelse(train_6_lr_data$X5==6, 1, 0))
train_7_lr_data$X5 <- data.frame(ifelse(train_7_lr_data$X5==7, 1, 0))
train_8_lr_data$X5 <- data.frame(ifelse(train_8_lr_data$X5==8, 1, 0))
train_9_lr_data$X5 <- data.frame(ifelse(train_5_lr_data$X5==9, 1, 0))

### Step 5: storing the list of columns having null values ###
str <- structure(list(names(Filter(function(x) length(unique(x[!is.na(x)])) > 1, train._lr_data))))


### logistic regression for the predicting model exclusion of constant character ##      
model_final <- glm(train_1_lr_data$X5 ~ .  - X0.1 - X0.2 - X0.3 - X0.4 - X0.5 - X0.6 - X0.7 - X0.8 - X0.9 - X0.10 - X0.11 - X0.16 
                   - X0.17 - X0.18 - X0.19 - X0.20 - X0.21 - X0.22 - - X0.23 - X0.24 - X0.25 
                   - X0.26 - X0.27 - X0.28 - X0.29 - X0.30 - X0.31 - X0.32 - X0.52 - X0.53 - X0.54 - X0.55 - X0.56 - X0.57 - X0.58 - X0.59 - X0.82 - X0.83 - X0.84 - X0.85 - X0.86 - X0.110 - X0.111 - X0.112 - X0.113 - X0.114 - X0.139 - X0.140 - X0.141 - X0.156 - X0.157 - X0.263 - X0.287 - X0.308 - X0.309 - X0.330 - X0.376 - X0.421 - X0.441 - X0.460 - X0.478 - X0.479 - X0.496 - X0.497 - X0.513 - X0.514 - X0.515 - X0.532 - X0.533 - X0.534 - X0.535 - X0.536 - X0.559 - X0.560 - X0.561 - X0.562 - X0.563 - X0.564 - X0.586 - X0.587 - X0.588 - X0.589 - X0.590 - X0.591 - X0.592 - 
                     X0.593 - X0.613 - X0.614 - X0.615 - X0.616 - X0.617,family=binomial(link=logit), data = train_1_lr_data)
      
      
test_lr_data <- data.frame(test._lr_data[test._lr_data$X0,test._lr_data$X0.1,test._lr_data$X0.2,test._lr_data$X0.3,test._lr_data$X0.4])
      
test_lr_data <- test._lr_data[,1:3]
      
test__data <- data.frame(test._lr_data[test._lr_data$X7, test._lr_data$X0.93])


test_1_data <- subset(test._lr_data, select = -c( X0.1 , X0.2 , X0.3 , X0.4 , X0.5 , X0.6 , X0.7 , X0.8 , X0.9 , X0.10 , X0.11 , X0.16 , X0.17 , X0.18 , X0.19 , X0.20 , X0.21 , X0.22 ,  X0.23 , X0.24 , X0.25 , X0.26 , X0.27 , X0.28 , X0.29 , X0.30 , X0.31 , X0.32 , X0.52 , X0.53 , X0.54 , X0.55 , X0.56 , X0.57 , X0.58 , X0.59 , X0.82 , X0.83 , X0.84 , X0.85 , X0.86 , X0.110 , X0.111 , X0.112 , X0.113 , X0.114 , X0.139 , X0.140 , X0.141 , X0.156 , X0.157 , X0.263 , X0.287 , X0.308 , X0.309 , X0.330 , X0.376 , X0.421 , X0.441 , X0.460 , X0.478 , X0.479 , X0.496 , X0.497 , X0.513 , X0.514 , X0.515 , X0.532 , X0.533 , X0.534 , X0.535 , X0.536 , X0.559 , X0.560 , X0.561 , X0.562 , X0.563 , X0.564 , X0.586 , X0.587 , X0.588 , X0.589 , X0.590 , X0.591 , X0.592 , X0.593 , X0.613 , X0.614 , X0.615 , X0.616 , X0.617))

test_2_data <- subset(test._lr_data, select = c(train._lr_data$X0.12 , train._lr_data$X0.13 , train._lr_data$X0.14 , train._lr_data$X0.15 , train._lr_data$X0.33 , train._lr_data$X0.34 , train._lr_data$X0.34 , train._lr_data$X0.35 , train._lr_data$X0.36 , train._lr_data$X0.37 , train._lr_data$X0.38 , train._lr_data$X0.39 , train._lr_data$X0.40 , train._lr_data$X0.41 , train._lr_data$X0.42 , train._lr_data$X0.43 , train._lr_data$X0.44 , train._lr_data$X0.45 , train._lr_data$X0.46 , train._lr_data$X0.47 , train._lr_data$X0.48 , train._lr_data$X0.49 , train._lr_data$X0.50 , train._lr_data$X0.51 , train._lr_data$X0.60 , train._lr_data$X0.61 , train._lr_data$X0.62 , train._lr_data$X0.63 , train._lr_data$X0.64 , train._lr_data$X0.65 , train._lr_data$X0.66 , train._lr_data$X0.67 , train._lr_data$X0.68 , train._lr_data$X0.69 , train._lr_data$X0.70 , train._lr_data$X0.71 , train._lr_data$X0.72 , train._lr_data$X0.73 , train._lr_data$X0.74 , train._lr_data$X0.75 , train._lr_data$X0.76 , train._lr_data$X0.77 , train._lr_data$X0.78 , train._lr_data$X0.79 , train._lr_data$X0.80 , train._lr_data$X0.81 , train._lr_data$X0.87 , train._lr_data$X0.88 , train._lr_data$X0.89 , train._lr_data$X0.90 , train._lr_data$X0.91 , train._lr_data$X0.92 , train._lr_data$X0.93 , train._lr_data$X0.94 , train._lr_data$X0.95 , train._lr_data$X0.96 , train._lr_data$X0.97 , train._lr_data$X0.98 , train._lr_data$X0.99 , train._lr_data$X0.100 , train._lr_data$X0.101 , train._lr_data$X0.102 , train._lr_data$X0.103 , train._lr_data$X0.104 , train._lr_data$X0.105))

test_1_lr_data <- data.frame(ifelse(test._lr_data$X7==1, 1, 0))

### summary and exponential for the following ##
summary(model_final)
      
exp(coef(model_final))

test_dataset_label  <- test._lr_data[1:3000, 1]
    
## predicting the values  with test dataset ###      
predicting <- predict(model_final, test_dataset_label)


### creation of confusion matrix ###      
confusionMatrix(table(as.factor(test._lr_data, levels = 1:100), as.factor(predicting, levels = 1:100)), na.action = na.pass)

rotate <- function(x) {
  return(t(apply(x, 2, rev)))
}

plot_matrix <- function(vec) {
  q <- matrix(vec, 28, 28, byrow = TRUE)
  nq <- apply(q, 2, as.numeric)
  image(rotate(nq), col = gray((0:255)/255))
}

plot_matrix(mydata[3, 2:785])


      
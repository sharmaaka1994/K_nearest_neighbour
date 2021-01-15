# TASK : APPLY nueral network ALGORITHM TO MNIST DATASET
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
library(keras)




###Step 2:Importing Training and Testing dataset###
train._lr_data <- read.csv(file.choose(),  header = TRUE,stringsAsFactors=FALSE)
test._lr_data <- read.csv(file.choose(), header = TRUE, stringsAsFactors=FALSE)

###Step 3:reducing the dataset into 30%###
train._lr_data <- head(train._lr_data, 18000)

test_train._lr_data <- train._lr_data
test._lr_data <- head(test._lr_data, 3000)

str(train._lr_data)
str(test._lr_data)

dim(train._lr_data)
dim(test._lr_data)


##matrix representation of my matrix of the following dataset###
table(train._lr_data$X9,train._lr_data$X9)
table(test._lr_data $X7, test._lr_data$X7)

###Adding 7 different columns naming A-G ###

test_train._lr_data$A[test_train._lr_data$X9 == 0 | test_train._lr_data$X9 == 2 | test_train._lr_data$X9 == 3 
                      | test_train._lr_data$X9 == 5 | test_train._lr_data$X9 == 6 | test_train._lr_data$X9 == 7 
                      | test_train._lr_data$X9 == 8 | test_train._lr_data$X9 == 9] <- "1"

test_train._lr_data$B[test_train._lr_data$X9 == 0 | test_train._lr_data$X9 == 1 | test_train._lr_data$X9 == 2 
                      | test_train._lr_data$X9 == 3 | test_train._lr_data$X9 == 4 | test_train._lr_data$X9 == 7 
                      | test_train._lr_data$X9 == 8 | test_train._lr_data$X9 == 9] <- "1"

test_train._lr_data$C[test_train._lr_data$X9 == 0 | test_train._lr_data$X9 == 1 | test_train._lr_data$X9 == 3 
                      | test_train._lr_data$X9 == 5 | test_train._lr_data$X9 == 6 | test_train._lr_data$X9 == 7 
                      | test_train._lr_data$X9 == 8 | test_train._lr_data$X9 == 9] <- "1"

test_train._lr_data$D[test_train._lr_data$X9 == 0 | test_train._lr_data$X9 == 2 | test_train._lr_data$X9 == 3 
                      | test_train._lr_data$X9 == 5 | test_train._lr_data$X9 == 6 | test_train._lr_data$X9 == 8 ] <- "1"

test_train._lr_data$E[test_train._lr_data$X9 == 0 | test_train._lr_data$X9 == 2  | test_train._lr_data$X9 == 6 | test_train._lr_data$X9 == 8 ] <- "1"

test_train._lr_data$F[test_train._lr_data$X9 == 0 | test_train._lr_data$X9 == 4  | test_train._lr_data$X9 == 5 | test_train._lr_data$X9 == 6 | test_train._lr_data$X9 == 8 | test_train._lr_data$X9 == 9 ] <- "1"

test_train._lr_data$G[test_train._lr_data$X9 == 2 | test_train._lr_data$X9 == 3 | test_train._lr_data$X9 == 4 | test_train._lr_data$X9 == 5 | test_train._lr_data$X9 == 6 | test_train._lr_data$X9 == 8 | test_train._lr_data$X9 == 9 ] <- "1"

test._lr_data$A[test._lr_data$X9 == 0 | test._lr_data$X9 == 2 | test._lr_data$X9 == 3 
                | test._lr_data$X9 == 5 | test._lr_data$X9 == 6 | test._lr_data$X9 == 7 
                | test._lr_data$X9 == 8 | test._lr_data$X9 == 9] <- "1"

test._lr_data$B[test._lr_data$X9 == 0 | test._lr_data$X9 == 1 | test._lr_data$X9 == 2 
                | test._lr_data$X9 == 3 | test._lr_data$X9 == 4 | test._lr_data$X9 == 7 
                | test._lr_data$X9 == 8 | test._lr_data$X9 == 9] <- "1"


test._lr_data$C[test._lr_data$X9 == 0 | test._lr_data$X9 == 1 | test._lr_data$X9 == 3 
                | test._lr_data$X9 == 5 | test._lr_data$X9 == 6 | test._lr_data$X9 == 7 
                | test._lr_data$X9 == 8 | test._lr_data$X9 == 9] <- "1"

test._lr_data$D[test._lr_data$X9 == 0 | test._lr_data$X9 == 2 | test._lr_data$X9 == 3 
                | test._lr_data$X9 == 5 | test._lr_data$X9 == 6 | test._lr_data$X9 == 8 ] <- "1"

test._lr_data$E[test._lr_data$X9 == 0 | test._lr_data$X9 == 2  | test._lr_data$X9 == 6 | test._lr_data$X9 == 8 ] <- "1"

test._lr_data$F[test._lr_data$X9 == 0 | test._lr_data$X9 == 4  | test._lr_data$X9 == 5 | test._lr_data$X9 == 6 | test._lr_data$X9 == 8 | test._lr_data$X9 == 9 ] <- "1"

test._lr_data$G[test._lr_data$X9 == 2 | test._lr_data$X9 == 3 | test._lr_data$X9 == 4 | test._lr_data$X9 == 5 | test._lr_data$X9 == 6 | test._lr_data$X9 == 8 | test._lr_data$X9 == 9 ] <- "1"


train_test_train._lr_data_features <- test_train._lr_data[, -1]
test_train._lr_data_labels <- test_train._lr_data[, 1]

test_test_features <- test._lr_data[, -1]
test_test_labelss <- test._lr_data[, 1]


##### one hot encoding ####
test_train._lr_data_labels <- to_categorical(test_train._lr_data_labels)
test_test_labelss <- to_categorical(test_test_labelss)

is.na(train_test_train._lr_data_features) <- 0

train_test_train._lr_data_features$A <- as.integer(train_test_train._lr_data_features$A)
train_test_train._lr_data_features$B <- as.integer(train_test_train._lr_data_features$B)
train_test_train._lr_data_features$C <- as.integer(train_test_train._lr_data_features$C)
train_test_train._lr_data_features$D <- as.integer(train_test_train._lr_data_features$D)
train_test_train._lr_data_features$E <- as.integer(train_test_train._lr_data_features$E)
train_test_train._lr_data_features$F <- as.integer(train_test_train._lr_data_features$F)
train_test_train._lr_data_features$G <- as.integer(train_test_train._lr_data_features$G)

test_test_features$A <- as.integer(test_test_features$A)
test_test_features$B <- as.integer(test_test_features$B)
test_test_features$C <- as.integer(test_test_features$C)
test_test_features$D <- as.integer(test_test_features$D)
test_test_features$E <- as.integer(test_test_features$E)
test_test_features$F <- as.integer(test_test_features$F)
test_test_features$G <- as.integer(test_test_features$G)

train_features <- train._lr_data[, -1]
train_label <- train._lr_data[, 1]

test_features <- test._lr_data[, -1]
test_label <- test._lr_data[, 1]


train_label_final <- to_categorical(train_label)
test_label_final <- to_categorical(test_label)

###Building model###
networkinger <- keras_model_sequential() %>%
  layer_dense(units = 791, activation = "relu", input_shape = c(791)) %>%
  layer_dense(units = 524, activation = "softmax")  %>%
  layer_dense(units = 10)  %>%
  layer_dense(units = 10)
# Show network data
network

### Set data for the compile function
networkinger %>% compile(
  optimizer = "rmsprop",
  loss='categorical_crossentropy',
  metrics = c("accuracy"),
  learning_rate=0.001
)
"sparse_"

###Train the network using fit function###
networkinger %>% fit(as.matrix(train_test_train._lr_data_features), test_train._lr_data_labels , epochs = 20, batch_size = 128)

### test fitting the data ###
metricsing <- networking %>% evaluate(as.matrix(test_test_features), test_test_labelss)

metricsing

test_test_labelss[1:10]

model1 <- neuralnet(test_train._lr_data$A+test_train._lr_data$B+test_train._lr_data$C+test_train._lr_data$D+test_train._lr_data$E+test_train._lr_data$F+test_train._lr_data$G ~ test_train._lr_data$X9, 
                    data=test_train._lr_data, hidden=c(10,10), rep = 5, err.fct = "ce", 
                    linear.output = FALSE, lifesign = "minimal", stepmax = 1000000,
                    threshold = 0.001)




# Part 1 Data processing
### packages
library(fastDummies)
library(class)
library(caret)
library(xgboost)
library(dplyr)
library(e1071)
library(ggplot2)
library(readr)

### load data
train<-read.csv("/Users/icho/Desktop/final/train.csv")
test<-read.csv("/Users/icho/Desktop/final/test.csv")

### count missing values
X<-sapply(train,function(x) sum(is.na(x)));X[X>0]
Y<-sapply(test,function(x) sum(is.na(x)));Y[Y>0]


### data description
head(train)
summary(test)
str(train)
str(test)


# Format data for training and testing

#Training
#transfer Gender and Vehicle_Damage to Boolean value
train$Gender<-factor(train$Gender,levels = c("Female","Male"),labels = c(0,1))
train$Vehicle_Damage<-factor(train$Vehicle_Damage,levels = c("No","Yes"),labels = c(0,1))

# create dummy for train
#Vehicle_Age
train<-dummy_cols(train,select_columns = "Vehicle_Age")
#Region_Code
train<-train %>%
  mutate(Region_Code_a=case_when(train$Region_Code<10~"1",train$Region_Code>=10~"0"),
         Region_Code_b=case_when(train$Region_Code>=10 & train$Region_Code<20~"1",train$Region_Code<10 | train$Region_Code>=20~"0"),
         Region_Code_c=case_when(train$Region_Code>=20 & train$Region_Code<30~"1",train$Region_Code<20 | train$Region_Code>=30~"0"),
         Region_Code_d=case_when(train$Region_Code>=30 & train$Region_Code<40~"1",train$Region_Code<30 | train$Region_Code>=40~"0"),
         Region_Code_e=case_when(train$Region_Code>=40 & train$Region_Code<50~"1",train$Region_Code<40 | train$Region_Code>=50~"0"),
         Region_Code_f=case_when(train$Region_Code>=50~"1",train$Region_Code<50~"0"))
#Policy_Sales_Channel
train<-train %>%
  mutate(Policy_Sales_Channel_a=case_when(train$Policy_Sales_Channel<=15~"1",train$Policy_Sales_Channel>15~"0"),
         Policy_Sales_Channel_b=case_when(train$Policy_Sales_Channel>=15 & train$Policy_Sales_Channel<30~"1",train$Policy_Sales_Channel<15 | train$Policy_Sales_Channel>=30~"0"),
         Policy_Sales_Channel_c=case_when(train$Policy_Sales_Channel>=30 & train$Policy_Sales_Channel<45~"1",train$Policy_Sales_Channel<30 | train$Policy_Sales_Channel>=45~"0"),
         Policy_Sales_Channel_d=case_when(train$Policy_Sales_Channel>=45 & train$Policy_Sales_Channel<60~"1",train$Policy_Sales_Channel<45 | train$Policy_Sales_Channel>=60~"0"),
         Policy_Sales_Channel_e=case_when(train$Policy_Sales_Channel>=60 & train$Policy_Sales_Channel<75~"1",train$Policy_Sales_Channel<60 | train$Policy_Sales_Channel>=75~"0"),
         Policy_Sales_Channel_f=case_when(train$Policy_Sales_Channel>=75 & train$Policy_Sales_Channel<90~"1",train$Policy_Sales_Channel<75 | train$Policy_Sales_Channel>=90~"0"),
         Policy_Sales_Channel_g=case_when(train$Policy_Sales_Channel>=90 & train$Policy_Sales_Channel<105~"1",train$Policy_Sales_Channel<90 | train$Policy_Sales_Channel>=105~"0"),
         Policy_Sales_Channel_h=case_when(train$Policy_Sales_Channel>=105 & train$Policy_Sales_Channel<120~"1",train$Policy_Sales_Channel<105 | train$Policy_Sales_Channel>=120~"0"),
         Policy_Sales_Channel_i=case_when(train$Policy_Sales_Channel>=120 & train$Policy_Sales_Channel<135~"1",train$Policy_Sales_Channel<120 | train$Policy_Sales_Channel>=135~"0"),
         Policy_Sales_Channel_j=case_when(train$Policy_Sales_Channel>=135 & train$Policy_Sales_Channel<150~"1",train$Policy_Sales_Channel<135 | train$Policy_Sales_Channel>=150~"0"),
         Policy_Sales_Channel_k=case_when(train$Policy_Sales_Channel>=150~"1",train$Policy_Sales_Channel<150~"0"))

train<-train[,-c(5,7,10)]  # after create dummy, the previous columns can be replaced
train[,1:29]<-lapply(train[,1:29],as.numeric) #vary to numeric for modeling
train$Gender<-train$Gender-1 # after as numeric, these two columns needs -1 to change back to 0,1 format
train$Vehicle_Damage<-train$Vehicle_Damage-1

#Rename dummy columns
# Rename the dummy columns of Region_Code
train <- rename(train,c("Region_Code_0_10"="Region_Code_a","Region_Code_10_20"="Region_Code_b","Region_Code_20_30"="Region_Code_c","Region_Code_30_40"="Region_Code_d","Region_Code_40_50"="Region_Code_e","Region_Code_equal&over_50"="Region_Code_f"))
# Rename the dummy columns of Policy_Sales_Channel
train<- rename(train,c( "Policy_Sales_Channel_0_15"="Policy_Sales_Channel_a","Policy_Sales_Channel_15_30"="Policy_Sales_Channel_b","Policy_Sales_Channel_30_45"="Policy_Sales_Channel_c","Policy_Sales_Channel_45_60"="Policy_Sales_Channel_d","Policy_Sales_Channel_60_75"="Policy_Sales_Channel_e","Policy_Sales_Channel_75_90"="Policy_Sales_Channel_f","Policy_Sales_Channel_90_105"="Policy_Sales_Channel_g","Policy_Sales_Channel_105_120"="Policy_Sales_Channel_h","Policy_Sales_Channel_120_135"="Policy_Sales_Channel_i","Policy_Sales_Channel_135_150"="Policy_Sales_Channel_j","Policy_Sales_Channel_equal_over_150"="Policy_Sales_Channel_k"))
colnames(train)


#Testing
##transfer Gender and Vehicle_Damage to Boolean value
test$Gender<-factor(test$Gender,levels = c("Female","Male"),labels = c(0,1))
test$Vehicle_Damage<-factor(test$Vehicle_Damage,levels = c("No","Yes"),labels = c(0,1))

# create dummy for test
test<-dummy_cols(test,select_columns = "Vehicle_Age")
#Region_Code
test<-test %>%
  mutate(Region_Code_a=case_when(test$Region_Code<10~"1",test$Region_Code>=10~"0"),
         Region_Code_b=case_when(test$Region_Code>=10 & test$Region_Code<20~"1",test$Region_Code<10 | test$Region_Code>=20~"0"),
         Region_Code_c=case_when(test$Region_Code>=20 & test$Region_Code<30~"1",test$Region_Code<20 | test$Region_Code>=30~"0"),
         Region_Code_d=case_when(test$Region_Code>=30 & test$Region_Code<40~"1",test$Region_Code<30 | test$Region_Code>=40~"0"),
         Region_Code_e=case_when(test$Region_Code>=40 & test$Region_Code<50~"1",test$Region_Code<40 | test$Region_Code>=50~"0"),
         Region_Code_f=case_when(test$Region_Code>=50~"1",test$Region_Code<50~"0"))
#Policy_Sales_Channel
test<-test %>%
  mutate(Policy_Sales_Channel_a=case_when(test$Policy_Sales_Channel<=15~"1",test$Policy_Sales_Channel>15~"0"),
         Policy_Sales_Channel_b=case_when(test$Policy_Sales_Channel>=15 & test$Policy_Sales_Channel<30~"1",test$Policy_Sales_Channel<15 | test$Policy_Sales_Channel>=30~"0"),
         Policy_Sales_Channel_c=case_when(test$Policy_Sales_Channel>=30 & test$Policy_Sales_Channel<45~"1",test$Policy_Sales_Channel<30 | test$Policy_Sales_Channel>=45~"0"),
         Policy_Sales_Channel_d=case_when(test$Policy_Sales_Channel>=45 & test$Policy_Sales_Channel<60~"1",test$Policy_Sales_Channel<45 | test$Policy_Sales_Channel>=60~"0"),
         Policy_Sales_Channel_e=case_when(test$Policy_Sales_Channel>=60 & test$Policy_Sales_Channel<75~"1",test$Policy_Sales_Channel<60 | test$Policy_Sales_Channel>=75~"0"),
         Policy_Sales_Channel_f=case_when(test$Policy_Sales_Channel>=75 & test$Policy_Sales_Channel<90~"1",test$Policy_Sales_Channel<75 | test$Policy_Sales_Channel>=90~"0"),
         Policy_Sales_Channel_g=case_when(test$Policy_Sales_Channel>=90 & test$Policy_Sales_Channel<105~"1",test$Policy_Sales_Channel<90 | test$Policy_Sales_Channel>=105~"0"),
         Policy_Sales_Channel_h=case_when(test$Policy_Sales_Channel>=105 & test$Policy_Sales_Channel<120~"1",test$Policy_Sales_Channel<105 | test$Policy_Sales_Channel>=120~"0"),
         Policy_Sales_Channel_i=case_when(test$Policy_Sales_Channel>=120 & test$Policy_Sales_Channel<135~"1",test$Policy_Sales_Channel<120 | test$Policy_Sales_Channel>=135~"0"),
         Policy_Sales_Channel_j=case_when(test$Policy_Sales_Channel>=135 & test$Policy_Sales_Channel<150~"1",test$Policy_Sales_Channel<135 | test$Policy_Sales_Channel>=150~"0"),
         Policy_Sales_Channel_k=case_when(test$Policy_Sales_Channel>=150~"1",test$Policy_Sales_Channel<150~"0"))

test<-test[,-c(5,7,10)]  # same as train
test[,1:28]<-lapply(test[,1:28],as.numeric)
test$Gender<-test$Gender-1
test$Vehicle_Damage<-test$Vehicle_Damage-1

## Rename the dummy columns of Region_Code
test <- rename(test,c("Region_Code_0_10"="Region_Code_a","Region_Code_10_20"="Region_Code_b","Region_Code_20_30"="Region_Code_c","Region_Code_30_40"="Region_Code_d","Region_Code_40_50"="Region_Code_e","Region_Code_equal&over_50"="Region_Code_f"))
# Rename the dummy columns of Policy_Sales_Channel
test<- rename(test,c( "Policy_Sales_Channel_0_15"="Policy_Sales_Channel_a","Policy_Sales_Channel_15_30"="Policy_Sales_Channel_b","Policy_Sales_Channel_30_45"="Policy_Sales_Channel_c","Policy_Sales_Channel_45_60"="Policy_Sales_Channel_d","Policy_Sales_Channel_60_75"="Policy_Sales_Channel_e","Policy_Sales_Channel_75_90"="Policy_Sales_Channel_f","Policy_Sales_Channel_90_105"="Policy_Sales_Channel_g","Policy_Sales_Channel_105_120"="Policy_Sales_Channel_h","Policy_Sales_Channel_120_135"="Policy_Sales_Channel_i","Policy_Sales_Channel_135_150"="Policy_Sales_Channel_j","Policy_Sales_Channel_equal_over_150"="Policy_Sales_Channel_k"))
colnames(test)

### export new .csv files after both train & test are ready
#Clean_train <- write.csv(train,file="/Users/icho/Desktop/Clean_train.csv",quote=F,row.names = F)
#Clean_test <- write.csv(test,file="/Users/icho/Desktop/Clean_test.csv",quote=F,row.names = F)



######## After cleaning, Start from here#########
### due to test set has no response, thus to test the accuracy of further models, the sub_train (80% of train) and sub_test (20% of train) sets are needed
set.seed(12345)
row_number<-sample(x=1:nrow(train),size = .8*nrow(train))
sub_train<-train[row_number,]
sub_test<-train[-row_number,]
#train.seed <- write.csv(sub_train,file="/Users/icho/Desktop/Train_seed.csv",quote=F,row.names = F)
#Test.seed <- write.csv(sub_test,file="/Users/icho/Desktop/test_seed.csv",quote=F,row.names = F)

#Part 2 Building Model
# since the ID of clients is unique and they should not impact the 'Response' thus they are excluded from modeling

### KNN
start_time_1_0 <- Sys.time()
KNN_3<-knn(train = sub_train[,-c(1)],test = sub_test[,-c(1)],cl = sub_train$Response,k=3,prob = T)
confusionMatrix(table(KNN_3,sub_test$Response)) # 0.8569
precision_knn_3 <- 1223/(8251+1223)
precision_knn_3
F1_knn_3  <-2*(precision_knn_3*0.9601)/(precision_knn_3+0.9601)
F1_knn_3
end_time_1_0 <- Sys.time()
print(end_time_1_0 - start_time_1_0)

start_time_1_1 <- Sys.time()
KNN_11<-knn(train = sub_train[,-c(1)],test = sub_test[,-c(1)],cl = sub_train$Response,k=11,prob = T)
confusionMatrix(table(KNN_11,sub_test$Response)) #0.8761
precision_knn_11 <- 190/(9284+190)
precision_knn_11
F1_knn_11  <-2*(precision_knn_11*0.99732)/(precision_knn_11+0.99732)
F1_knn_11
end_time_1_1 <- Sys.time()
print(end_time_1_1 - start_time_1_1)

start_time_1_2 <- Sys.time()
KNN_21<-knn(train = sub_train[,-c(1)],test = sub_test[,-c(1)],cl = sub_train$Response,k=21,prob = T)
confusionMatrix(table(KNN_21,sub_test$Response)) #0.876
precision_knn_21 <- 12/(9462+12)
precision_knn_21
F1_knn_21  <-2*(precision_knn_21*0.999925)/(precision_knn_21+0.999925)
F1_knn_21
end_time_1_2 <- Sys.time()
print(end_time_1_2 - start_time_1_2)

start_time_1_3 <- Sys.time()
KNN_9<-knn(train = sub_train[,-c(1)],test = sub_test[,-c(1)],cl = sub_train$Response,k=9,prob = T)
confusionMatrix(table(KNN_9,sub_test$Response)) #0.8753
precision_knn_9 <- 310/(9164+310)
precision_knn_9
F1_knn_9  <-2*(precision_knn_9*0.9947)/(precision_knn_9+0.9947)
F1_knn_9
end_time_1_3 <- Sys.time()
print(end_time_1_3 - start_time_1_3)

start_time_1_4 <- Sys.time()
KNN_13<-knn(train = sub_train[,-c(1)],test = sub_test[,-c(1)],cl = sub_train$Response,k=13,prob = T)
confusionMatrix(table(KNN_13,sub_test$Response)) #0.8764
precision_knn_13 <- 124/(9350+124)
precision_knn_13
F1_knn_13  <-2*(precision_knn_13*0.99859)/(precision_knn_13+0.99859)
F1_knn_13
end_time_1_4 <- Sys.time()
print(end_time_1_4 - start_time_1_4)

start_time_1_5 <- Sys.time()
KNN_15<-knn(train = sub_train[,-c(1)],test = sub_test[,-c(1)],cl = sub_train$Response,k=15,prob = T)
confusionMatrix(table(KNN_15,sub_test$Response)) #0.8763
precision_knn_15 <- 64/(9410+64)
precision_knn_15
F1_knn_15  <-2*(precision_knn_15*0.999401)/(precision_knn_15+0.999401)
F1_knn_15
end_time_1_5 <- Sys.time()
print(end_time_1_5 - start_time_1_5)
# through cross comparison, when K=13 the model reached highest accuracy (0.8764)

### decision tree with XGBoost
# prepare data
subtrain_label<-sub_train$Response
summary.factor(subtrain_label)
subtrain_matrix<-xgb.DMatrix(data.matrix(sub_train[,-c(1,9)]),label=subtrain_label)

subtest_label<-sub_test$Response
summary.factor(subtest_label)
subtest_matrix<-xgb.DMatrix(data.matrix(sub_test[,-c(1,9)]),label=subtest_label)

start_time_2_0 <- Sys.time()
para_1<-list(eta=0.3,max_depth=6,objective="binary:logistic",eval_metric="error")
xgb_1<-xgboost(data = subtrain_matrix,nrounds = 120,params = para_1)
xgbpred_1<-predict(xgb_1,subtest_matrix,type="response")
xgbpred_1<-as.integer(round(xgbpred_1))
confusionMatrix(table(xgbpred_1,factor(sub_test$Response))) #0.875
precision_xgb_1 <- 171/(9303+171)
precision_xgb_1
F1_xgb_1  <-2*(precision_xgb_1*0.99643)/(precision_xgb_1+0.99643)
F1_xgb_1
end_time_2_0 <- Sys.time()
print(end_time_2_0 - start_time_2_0)

start_time_2_1 <- Sys.time()
para_2<-list(eta=0.1,max_depth=6,objective="binary:logistic",eval_metric="error")
xgb_2<-xgboost(data = subtrain_matrix,nrounds = 120,params = para_2)
xgbpred_2<-predict(xgb_2,subtest_matrix,type="response")
xgbpred_2<-as.integer(round(xgbpred_2))
confusionMatrix(table(xgbpred_2,factor(sub_test$Response))) #0.876
precision_xgb_2 <- 16/(9458+16)
precision_xgb_2
F1_xgb_2  <-2*(precision_xgb_2*0.9997)/(precision_xgb_2+0.9997)
F1_xgb_2
end_time_2_1 <- Sys.time()
print(end_time_2_1 - start_time_2_1)

start_time_2_2 <- Sys.time()
para_3<-list(eta=0.05,max_depth=6,objective="binary:logistic",eval_metric="error")
xgb_3<-xgboost(data = subtrain_matrix,nrounds = 120,params = para_3)
xgbpred_3<-predict(xgb_3,subtest_matrix,type="response")
xgbpred_3<-as.integer(round(xgbpred_3))
confusionMatrix(table(xgbpred_3,factor(sub_test$Response))) #0.8759
precision_xgb_3 <- 4/(9470+4)
precision_xgb_3
F1_xgb_3  <-2*(precision_xgb_3*0.99985)/(precision_xgb_3+0.99985)
F1_xgb_3
end_time_2_2 <- Sys.time()
print(end_time_2_2 - start_time_2_2)

start_time_2_3 <- Sys.time()
para_4<-list(eta=0.1,max_depth=8,objective="binary:logistic",eval_metric="error")
xgb_4<-xgboost(data = subtrain_matrix,nrounds = 120,params = para_4)
xgbpred_4<-predict(xgb_4,subtest_matrix,type="response")
xgbpred_4<-as.integer(round(xgbpred_4))
confusionMatrix(table(xgbpred_4,factor(sub_test$Response))) #0.876
precision_xgb_4 <- 58/(9416+58)
precision_xgb_4
F1_xgb_4  <-2*(precision_xgb_4*0.998816)/(precision_xgb_4+0.998816)
F1_xgb_4
end_time_2_3 <- Sys.time()
print(end_time_2_3 - start_time_2_3)

start_time_2_4 <- Sys.time()
para_5<-list(eta=0.1,max_depth=4,objective="binary:logistic",eval_metric="error")
xgb_5<-xgboost(data = subtrain_matrix,nrounds = 120,params = para_5)
xgbpred_5<-predict(xgb_5,subtest_matrix,type="response")
xgbpred_5<-as.integer(round(xgbpred_5))
confusionMatrix(table(xgbpred_5,factor(sub_test$Response))) #0.8759
precision_xgb_5 <- 6/(9468+6)
precision_xgb_5
F1_xgb_5  <-2*(precision_xgb_5*0.9999251)/(precision_xgb_5+0.9999251)
F1_xgb_5
end_time_2_4 <- Sys.time()
print(end_time_2_4 - start_time_2_4)

start_time_2_5 <- Sys.time()
para_6<-list(eta=0.05,max_depth=6,objective="binary:logistic",eval_metric="error")
xgb_6<-xgboost(data = subtrain_matrix,nrounds = 120,params = para_6)
xgbpred_6<-predict(xgb_6,subtest_matrix,type="response")
summary(xgbpred_6)
xgbpred_6<-as.integer(round(xgbpred_6))
confusionMatrix(table(xgbpred_6,factor(sub_test$Response))) #0.8731
precision_xgb_6 <- 340/(9134+340)
precision_xgb_6
F1_xgb_6  <-2*(precision_xgb_6*0.99269)/(precision_xgb_6+0.99269)
F1_xgb_6
end_time_2_5 <- Sys.time()
print(end_time_2_5 - start_time_2_5)

# through cross comparison, when eta=0.1 max_depth=6 and nrounds=120 the model reached highest accuracy(0.876)

### Regression
#Logistic regression
options(warn=-1)
start_time_3_0 <- Sys.time()
lr_1<-glm(sub_train$Response~., family = binomial(link = logit),data=sub_train[,-c(1)])
lrpred_1<-predict(lr_1,newdata = sub_test,type = "response")
lrpred_1<-as.integer(round(lrpred_1))
confusionMatrix(table(lrpred_1,factor(sub_test$Response))) #0.8757
precision_lr_1 <- 1/(9473+1)
precision_lr_1 
F1_lr_1  <-2*(precision_lr_1*0.9998502)/(precision_lr_1+0.9998502)
F1_lr_1 
end_time_3_0 <- Sys.time()
print(end_time_3_0 - start_time_3_0)

#Stepwise(backward) regression 
start_time_3_1 <- Sys.time()
lr_2<-step(lr_1,direction = 'backward')
summary(lr_2)
lrpred_2<-predict(lr_2,newdata = sub_test,type = "response")
lrpred_2<-as.integer(round(lrpred_2))
confusionMatrix(table(lrpred_2,factor(sub_test$Response))) #0.8757
precision_lr_2 <- 1/(9437+1)
precision_lr_2 
F1_lr_2  <-2*(precision_lr_2*0.9998502)/(precision_lr_2+0.9998502)
F1_lr_2 
end_time_3_1 <- Sys.time()
print(end_time_3_1 - start_time_3_1)

# Naive Bayes
start_time_4 <- Sys.time()
nb1 <- naiveBayes(as.factor(Response)~.,data=sub_train[,-c(1)])
pred_nb1 <- predict(nb1,newdata = sub_test)
summary(pred_nb1)
confusionMatrix(pred_nb1,factor(sub_test$Response))
precision_NB <- 8059/(1415+8059)
precision_NB
F1_NB <-2*(precision_NB*0.7048)/(precision_NB+0.7048)
F1_NB
end_time_4_1 <- Sys.time()
print(end_time_4_1 - start_time_4_1)



# KNN (K=13) model has the highest accuracy
# However, consider all models, when K=3 the Kappa value (0.1131) is the hi

start_time_5 <- Sys.time()
final_pred <- predict(nb1,newdata = test)
end_time_5 <- Sys.time()
print(end_time_5 - start_time_5)
summary.factor(final_pred)
dim(test)
## using the carseats dataset for performing the sample function ##
install.packages("tidyverse")
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)
set.seed(1234)
Carseats_new <- data.frame(Carseats$Sales, Carseats$CompPrice, Carseats$Income, Carseats$Advertising, Carseats$Population, Carseats$Price, Carseats$ShelveLoc)     ## converting the dataset with one factor value ##
Carseats_new <- na.omit(Carseats_new)        ## eradicating the na vlues that are not required ##
Carseats_new <- transform(Carseats_new, Carseats_new$Carseats.ShelveLoc == as.factor(Carseats_new$Carseats.ShelveLoc, order = TRUE))         ## changing the datatype of char column into factor ##
Carseats_n <- factor(Carseats_new$Carseats.ShelveLoc, order = TRUE)        ## changing the datatype into factor ##
lapply(Carseats_new, class)           ##checking the datatype of the following ##
CFR <- sample(2, nrow(Carseats_new), replace = T,prob = c(0.6, 0.4))        ## distributing the values into 2 probabilities ##
CFR_test <- Carseats_new[ind == 1, ]              ##distributing the data on the basis of value of 1 ##
CFR_train <- Carseats_new[ind == 2, ]             ##distributing the data on the basis of value of 2 ##

## using the carseats dataset for performing the decision tree function ##
attach(Carseats_new)   ## attaching the dataset ##
cars_formula <- ShelveLoc ~ Sales + Income + Population + Price   ## creating the formmula for the decision tree ##
ctree.cars <- ctree(cars_formula, data = cl_CFR_test)  ## creating a decision tree ##
plot(ctree.cars)   ## plotting the decision tree ##
Carseats_test <- na.omit(CFR_test)     ## omitting the na values from the dataset ##
Carseats_train <- na.omit(CFR_train)   ## omitting the na values from the train dataset ##

## kmeans clustering for the us arrests dataset ##
library(tidyverse)  # data manipulation ##
library(cluster)    # clustering algorithms ##
library(factoextra)
df <- USArrests ## sending the data into one data frame ##
View(df) ## View the df values ##
df <- na.omit(df) ## removing the null values ##
df <- scale(df) ## normalizing the data ##
head(df) ## head valules of the following dataset ##
k2 <- kmeans(df, centers = 2, nstart = 25) ## running the k means function ##
str(k2) ## string of k2 ##
fviz_cluster(k2, data = df) ## cluster density plot ##
df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(USArrests)) %>%
  ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
  geom_text() ## plotting the k means ##
library(fpc) ## library for the fpc ##
install.packages("dbscan") ## dbscan installing ##
library(dbscan) ## library for dbscan ##
dbscan::kNNdistplot(df, k = 3)
abline(h = 30, lty = 2)
ds <- dbscan(df, eps = 35,MinPts = 4)
table(ds$cluster, USArrests$Murder)
plotcluster(df, ds$cluster)

## using the carseats dataset for performing predict function ##
predict_ctree_carseats <- predict(ctree.cars, newdata=Carseats_train ) ## predict function on the ctree for the following carseats dataset ##  
table(predict_ctree_carseats)c ##tabular for the following ##
set.seed(8953) ## set.seed for performing the following ##
Carseats_new <- na.omit(Carseats_new) ## deleting the na values ##
Carseats_new <-  scale(Carseats_new) ## normalizing the value ##
fviz_nbclust(as.matrix(Carseats_new), kmeans, method = "wss")  ## finding the 
carseats.kmeans <- kmeans(Carseats_new, 3)
table(carseats.kmeans$cluster)
Carseats <- na.omit(Carseats)
attach(Carseats_new)
plot(Carseats[c("Sales","Income")], col = carseats.kmeans$cluster)


##k means clustering and density based clustering##
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)
df <- USArrests
View(df)
df <- na.omit(df)
df <- scale(df)
head(df)
k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)
fviz_cluster(k2, data = df)
df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(USArrests)) %>%
  ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
  geom_text()
library(fpc)
install.packages("dbscan")
library(dbscan)
dbscan::kNNdistplot(df, k = 3)
abline(h = 30, lty = 2)
ds <- dbscan(df, eps = 35,MinPts = 4)
table(ds$cluster, USArrests$Murder)
plotcluster(df, ds$cluster)
library(factoextra)
library(NbClust)
# Scaling the data
data_1<-scale(data.frame(USArrests))
summary(data_1)
#getting the distance between observations 
distance <- get_dist(data_1)
print(distance)
fviz_nbclust(data_1, kmeans, method = "wss")
# K-means model implementation
final <- kmeans(data_1, 4, nstart = 25)
print(final)
summary(final)
# plotting the clustering 
fviz_cluster(final, data = data_1)

## density based clustering ##
library(factoextra)
library(NbClust)
# Scaling the data
data_1<-scale(data.frame(USArrests))
summary(data_1)
#getting the distance between observations 
distance <- get_dist(data_1)
print(distance)
fviz_nbclust(data_1, kmeans, method = "wss")
# K-means model implementation
final <- kmeans(data_1, 4, nstart = 25)
print(final)
summary(final)
# plotting the clustering 
fviz_cluster(final, data = data_1)




## answer k means sampling for the us arrests in different format ## 
Sample1 <- USArrests
Sample1 <- na.omit(Sample1) ## removing the null values ##
Sample1 <- scale(Sample1) ## normalizing the data ##
head(Sample1, 3) ## getting the head values top 3 ##
fviz_nbclust(Sample1, kmeans, method = "wss") ## to find the cluster for the following ##
usarrest.kmeans <- kmeans(Sample1, 3) ## performing the k means ##
table(usarrest.kmeans$cluster) ## tabulating the data ##
attach(USArrests) ## attaching the us arrest data ##
plot(Sample1[c(Murder, Assault)], col = usarrest.kmeans$cluster)
points(usarrest.kmeans$centers[, c(Murder,Assault)], col = 1:3, cex = 5, pch = "*")
table(USArrests$UrbanPop, usarrest$cluster)







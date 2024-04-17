install.packages('dplyr')
library('dplyr')

install.packages('gplots')
library('gplots')

library('ggplot2')

install.packages('tidyverse')
library(tidyverse)

install.packages('reshape2')
library(reshape2)

install.packages('rpart')
library(rpart)

install.packages('rpart.plot')
library(rpart.plot)

install.packages('caret')
library(caret)

install.packages('randomForest')
library(randomForest)

getwd()

############# Exporting Data ###################
car_data=read.csv('Car01.csv')
car_data


############# EDA #####################
head(car_data)
any(is.na(car_data)) # checking for missing value
any(is.null(car_data)) # Checking for missing value
summary(car_data)



############ Visualization ###############
# a. Class vs Lugboot
ggplot(car_data,aes(x=Class,fill=lugboot))+
  geom_histogram(stat="count")+
  labs(title="Class Vs Luggage boot",subtitle="Histogram",y="Frequency of Luggage boot",x="Class")

# b. Class vs Safety
ggplot(car_data, aes(Class , fill = safety )) +
  geom_bar(position = position_dodge()) + 
  ggtitle("Car class vs Safety") +
  xlab("Class") + 
  ylab("safety")

# c. Class vs buying
ggplot(car_data, aes(Class , fill = buying )) +
  geom_bar(position = position_dodge()) + 
  ggtitle("Car class vs Buying Price") +
  xlab("Class") + 
  ylab("Buying Price")

# d. Density Plot
ggplot(data = car_data,aes(fill=as.factor(doors),x=persons))+
  geom_density(alpha=0.3)

ggplot(data = car_data,aes(fill=as.factor(maint),x=Class))+
  geom_density(alpha=0.3)+facet_wrap(~Class)



################## Decision Tree ###############

set.seed(100)
classValues<-as.vector(car_data$Class)
train_test_split <- createDataPartition(y=classValues, p=0.7,list =FALSE)
train_data <-car_data[train_test_split,]
test_data <- car_data[-train_test_split,]
summary(train_data)

summary(test_data)

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
decision_tree <- train(Class ~., data = train_data, method = "rpart", parms = list(split = "information"), trControl = train_control, tuneLength = 10)
decision_tree

# plotting decision tree
prp(decision_tree$finalModel, type=3, main= "Probabilities per class")

# prediction of train data
train_pred <- predict(decision_tree, train_data)
head(train_pred)

table(train_pred, train_data$Class)

mean(train_pred  == train_data$Class)

# prediction of test data
test_pred <- predict(decision_tree, test_data)
head(test_pred)

table(test_pred, test_data$Class)

mean(test_pred  == test_data$Class)

# Confusion Matrix
confusionMatrix(test_pred, as.factor(test_data$Class))

### We see that the accuracy of the model is 87.6 %




############### Random Forest #######################

random_forest <- randomForest(as.factor(Class)~., data = train_data, importance = TRUE)
random_forest

#fine tuning the model
random_forest_1 <- randomForest(as.factor(Class)~., data = train_data, ntree = 500, mtry = 3, importance = TRUE)
random_forest_1

#prediction on train data set
train_pred1 <-predict(random_forest_1, train_data, type = "class")
table(train_pred1, train_data$Class)

mean(train_pred1 == train_data$Class)

#prediction on test data set
test_pred1 <-predict(random_forest_1, test_data, type = "class")
table(test_pred1, test_data$Class)

mean(test_pred1==test_data$Class)

confusionMatrix(test_pred1, as.factor(test_data$Class))

confusionMatrix(test_pred1, as.factor(test_data$Class), mode = "prec_recall", positive="1")


# We see that the accuracy of the model has improved and is 97.8 %



################# Conclusion #######################


# The model we built using decision tree had the accuracy of 87.6%.
# and to increase the accuracy we built model using random Forest method
# and the accuracy improved to 97.8 %.





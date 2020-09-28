---
title: "Prediction Assignment Writeup"
author: "Alejandra Barrera"
date: "9/28/2020"
output: html_document
---



## Prediction Assignment Writeup

The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.



```r
#Downloading the data
set.seed(1)
tn_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
ts_url <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(tn_url, "train.csv", method = "curl")
download.file(ts_url, "test.csv", method = "curl")
```
### Loading the data 
The first step, is load the data. The data are loaded in the train and test variables. 


```r
train <- read.csv("train.csv", na.strings = c("NA", "#DIV/0!"))
test <- read.csv("test.csv", na.strings = c("NA", "#DIV/0!"))

dim(train)
```

```
## [1] 19622   160
```

```r
table(train$classe)
```

```
## 
##    A    B    C    D    E 
## 5580 3797 3422 3216 3607
```
### Cleaning the data  

The first 7 columns are unnecessary for predicting. Therefore, should be removed.


```r
train <- train[,-c(1,2,3,4,5,6,7)] #remove the first 7 columns that are unnecessary
test <- test[,-c(1,2,3,4,5,6,7)]
```

Remove data with near zero variance

```r
train <- train[,colSums(is.na(train))== 0]
test <- test[,colSums(is.na(test))== 0]
```

### Data partitioning
We are going to use some libraries. The data will be partitioned using the 70% of the training set. A validation set is going to be builded using 30% of the data.  


```r
suppressPackageStartupMessages(library(caret))
train_set <- createDataPartition(train$classe, p=0.7, list = F )
train_full <- train[train_set,]
train_val <- train[-train_set,]
dim(train_full); #size of the training data
```

```
## [1] 13737    53
```

```r
dim(train_val) #size of the validation data
```

```
## [1] 5885   53
```

### Prediction Model
The chosen model is Random Forest. The model uses 10 trees. According to the definition The random forest is a classification algorithm consisting of many decisions trees. It uses bagging and feature randomness when building each individual tree to try to create an uncorrelated forest of trees whose prediction by committee is more accurate than that of any individual tree. Therefore, is useful for prediction in this case.


```r
suppressPackageStartupMessages(library(randomForest))

fit <- randomForest(as.factor(classe) ~ ., data=train_full, ntrees=10, importance = TRUE)
print(fit)
```

```
## 
## Call:
##  randomForest(formula = as.factor(classe) ~ ., data = train_full,      ntrees = 10, importance = TRUE) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 7
## 
##         OOB estimate of  error rate: 0.51%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 3902    2    0    0    2 0.001024066
## B   10 2642    6    0    0 0.006019564
## C    0   14 2381    1    0 0.006260434
## D    0    0   29 2222    1 0.013321492
## E    0    0    1    4 2520 0.001980198
```

### Model Validation
#### Training accuracy
Our model, as you can see is going to perform perfectly in the training data set since it was the one used to train it. We can see an accuracy of 100%. 


```r
train_full$classe <- as.factor(train_full$classe)
training_accuracy <- predict(fit, train_full)
print(confusionMatrix(training_accuracy, train_full$classe))
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 3906    0    0    0    0
##          B    0 2658    0    0    0
##          C    0    0 2396    0    0
##          D    0    0    0 2252    0
##          E    0    0    0    0 2525
## 
## Overall Statistics
##                                      
##                Accuracy : 1          
##                  95% CI : (0.9997, 1)
##     No Information Rate : 0.2843     
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 1          
##                                      
##  Mcnemar's Test P-Value : NA         
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
## Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
## Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Prevalence             0.2843   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2843   0.1935   0.1744   0.1639   0.1838
## Detection Prevalence   0.2843   0.1935   0.1744   0.1639   0.1838
## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000
```

#### Validation accuracy
The validation set serves as an test set since the model was not trained using this set. The model performs with an accuracy of 99.27% which is really good. 


```r
train_val$classe <- as.factor(train_val$classe)
validation_accuracy <- predict(fit, train_val)
print(confusionMatrix(validation_accuracy, train_val$classe))
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1672    1    0    0    0
##          B    2 1138    9    0    0
##          C    0    0 1012   12    0
##          D    0    0    5  951    2
##          E    0    0    0    1 1080
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9946          
##                  95% CI : (0.9923, 0.9963)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9931          
##                                           
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9988   0.9991   0.9864   0.9865   0.9982
## Specificity            0.9998   0.9977   0.9975   0.9986   0.9998
## Pos Pred Value         0.9994   0.9904   0.9883   0.9927   0.9991
## Neg Pred Value         0.9995   0.9998   0.9971   0.9974   0.9996
## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
## Detection Rate         0.2841   0.1934   0.1720   0.1616   0.1835
## Detection Prevalence   0.2843   0.1952   0.1740   0.1628   0.1837
## Balanced Accuracy      0.9993   0.9984   0.9919   0.9925   0.9990
```

### Test accuracy
Now we know that the model predicts with great accuracy, therefore, we are applying it to the test data. 


```r
test_accuracy <- predict(fit, test)
test_accuracy
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```

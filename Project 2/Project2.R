library(dplyr)
library(ggplot2)
library(corrplot)
library(mlbench)
library(caret)
library(pROC)
library(nnet)
library(DataExplorer)
library(NeuralNetTools)

# Read train and test data
train <- read.csv(file.choose(), header = T)
test <- read.csv(file.choose(), header = T)

# Summary and structure
summary(train)
str(train)

# Missing Values
plot_intro(train, title = 'Train Data')
plot_intro(test, title = 'Test Data')
# Distribution of target variable in train and test
table(train$Churn)   # 0.172, more cases of false than true
table(test$Churn)    # 0.166  more cases of false than true

# Histogram of numeric variables
train_num <- train[,-c(1,4,5,20)]
train_num[] <- sapply(train_num, as.numeric)
str(train_num)

# Correlation plot of numeric variables
corr <- cor(train_num)
corrplot(corr, method = 'color')

# Based on correlation plot, we can see that some variables are strongly correlated to each other
# This has the potential to skew our analysis, so we drop one of the correlated variables
# One from each of the 4 pairs

# Dropping those columns from train and test sets
train <- train[,-c(9,12,15,18)]
test <- test[,-c(9,12,15,18)]

# Correlation plot after dropping variables
train_num <- train[,-c(1,4,5,16)]
train_num[] <- sapply(train_num, as.numeric)
corr <- cor(train_num)
corrplot(corr, method = 'color')

# Histogram of numerical variables
plot_histogram(train_num)

# Converting Churn into factor with 0 and 1 level
train <- train %>% mutate(Churn=
                            case_when(Churn=='True'~0,
                                      Churn=='False'~1))
train$Churn <- as.factor(train$Churn)

test <- test %>% mutate(Churn=
                            case_when(Churn=='True'~0,
                                      Churn=='False'~1))
test$Churn <- as.factor(test$Churn)

# Converting International Plan to factor
train <- train %>% mutate(International.plan=
                            case_when(International.plan=='No'~0,
                                      International.plan=='Yes'~1))
train$International.plan <- as.factor(train$International.plan)

test <- test %>% mutate(International.plan=
                          case_when(International.plan=='No'~0,
                                    International.plan=='Yes'~1))
test$International.plan <- as.factor(test$International.plan)

# Converting Voice Mail plan to factor

train <- train %>% mutate(Voice.mail.plan=
                            case_when(Voice.mail.plan=='No'~0,
                                      Voice.mail.plan=='Yes'~1))
train$Voice.mail.plan <- as.factor(train$Voice.mail.plan)

test <- test %>% mutate(Voice.mail.plan=
                          case_when(Voice.mail.plan=='No'~0,
                                    Voice.mail.plan=='Yes'~1))
test$Voice.mail.plan <- as.factor(test$Voice.mail.plan)


# Dropping the state column from data
train_final[] <- train[,-c(1)]
test_final[] <- test[,-c(1)]
str(test_final)

#########################################################################################

# Logistic Regression
set.seed(123)
log_reg <- glm(Churn~., data=train_final, family = binomial(link = 'logit'))
summary(log_reg)

logit <- predict(log_reg, test_final, type = 'response')
prob <- as.factor(ifelse(logit>0.5,1,0))

# Confusion Matrix
confusionMatrix(prob, test_final$Churn, mode = 'everything')  # 0.853


library(pROC)
roc_qda=roc(response=test_final$Churn, predictor= factor(prob, 
                                                         ordered = TRUE), plot=TRUE)
plot(roc_qda, col="red", lwd=3, main="ROC curve Logistic Regression")
auc_qda<-auc(roc_qda)

o#########################################################################################

# Random Forest
set.seed(123) 
cvcontrol <- trainControl(method="repeatedcv", 
                          number = 5,
                          repeats = 5,
                          allowParallel=TRUE,
                          savePredictions = T)
set.seed(123)
forest <- train(Churn ~ . , 
                data=train_final,
                method="rf",
                trControl=cvcontrol,
                importance=TRUE)
plot(varImp(forest),
     main = "Variable Importance Plot for RF")

# Testing confusion matrix
p <- predict(forest, test_final, type = 'raw')
confusionMatrix(p, test_final$Churn, mode = 'everything')  # 0.9565

# ROC Curve 
roc_qda=roc(response=test_final$Churn, predictor= factor(p, 
                                                        ordered = TRUE), plot=TRUE)
plot(roc_qda, col="blue", lwd=3, main="ROC curve Random Forest")
auc_qda<-auc(roc_qda)   # AUC = 0.868

#########################################################################################
# Neural Network

set.seed(123)
g <- expand.grid(size = seq(from=3, to=9, by=1) ,
                 decay = seq(from=0.1, to=0.7, by=0.05))

Model.nn <- train(Churn ~., data=train_final,
                  method="nnet",
                  trControl=cvcontrol,
                  preProcess=c("center","scale"),
                  tuneGrid = g,
                  maxit = 100,
                  metric="Accuracy")

plotnet(Model.nn$finalModel)

testpred.nn <- predict(Model.nn, test_final)
confusionMatrix(test_final$Churn, testpred.nn, mode='everything')   #0.943

roc_qda=roc(response=test_final$Churn, predictor= factor(testpred.nn, 
                                                         ordered = TRUE), plot=TRUE)
plot(roc_qda, col="green", lwd=3, main="ROC curve Neural Network")
auc_qda<-auc(roc_qda)   # AUC = 0.844


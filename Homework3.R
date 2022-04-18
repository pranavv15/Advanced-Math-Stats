library(dplyr)
library(glmnet)
library(mlbench)
library(caret)
library(e1071)
library(lime)
library(DAAG)
library(party)
library(rpart)
library(rpart.plot)
library(mlbench)
library(pROC)
library(tree)
library(nnet)
library(dplyr)

d <- read.csv(file.choose(), header = T)
d <- na.omit(d)
d <- d[-25,]
summary(d)
str(d)
d$X1 <- as.factor(d$X1)
table(d$X1)


##To split the data into training and test set
set.seed(420) 
ind <- sample(2, nrow(d), replace = T, prob = c(0.8, 0.2))
train <- d[ind == 1,]
test <- d[ind == 2,]
# Logistic Regression

mod <- multinom(train$X1~.,data = train,family = 'multinomial')

##Apply the trained model to the test set
x_t <- model.matrix(~.,data = xts)

# p<-predict(mod,xts,type="class")
# posteriprob<-mypred4[,,1]
# yhat<-matrix(1,nrow(x),1)
# for(i in 1:nrow(xts))
# {
#   yhat[i]<-which.max(posteriprob[i,])
# }
# acc<-sum(yhat+2==yts)/nrow(xts)
# cat("Accuracy on the test set is", acc, "\n")

# Predict
p <- predict(mod, test, type = 'class')
confusionMatrix(p, test$X1)

# Neural Network
labels <- class.ind(train[,1])
l <- labels[ind==1]
myiris<-nnet(train[,-c(1)], labels,
             size=2, rang=0.1,
             decay=5e-4, maxit=200)


library(mlbench)
library(caret)
library(e1071)
library(lime)
library(DAAG)
library(party)
library(rpart)
library(rpart.plot)
library(pROC)
library(nnet)
library(dplyr)
library(corrplot)
library(DataExplorer)
library(glmnet)

d <- read.csv(file.choose(), header = T)
d <- na.omit(d)
d <- d[-25,]
summary(d)
str(d)
d$X1 <- as.factor(d$X1)
table(d$X1)

plot_intro(d)
##To split the data into training and test set
set.seed(420) 
ind <- sample(2, nrow(d), replace = T, prob = c(0.8, 0.2))
train <- d[ind == 1,]
test <- d[ind == 2,]


# Logistic Regression

mod <- glmnet(train[,-c(1)],train$X1,family = 'multinomial')

##Apply the trained model to the test set
newX <- model.matrix(~.-X1,data=test)
newX <- newX[,-c(1)]
mypred4<-predict(mod,newx=newX,type="response",s=0.01);
posteriprob<-mypred4[,,1];
yhat<-matrix(1,nrow(test),1);
for(i in 1:nrow(test))
{
  yhat[i]<-which.max(posteriprob[i,]);
}
acc<-sum(yhat==test[,c(1)])/nrow(test);
cat("Accuracy on the test set is", acc, "\n");

#########################################################################################
# Neural Network
train$X. <- as.numeric(train$X.)
train<- na.omit(train)
labels <- class.ind(train[,1])
l <- labels[ind==1]
myiris<-nnet(train[,-c(1)], labels,
             size=2, rang=0.1,
             decay=5e-4, maxit=200)
summary(myiris)

test.cl <- function(true, pred) {
  true <- max.col(true)
  cres <- max.col(pred)
  table(true, cres)
}
labels2 <- class.ind(test[,1])
conf<-test.cl(labels2, predict(myiris, test))
acc<-sum(diag(conf))/sum(conf)
cat("The accuracy on the test set is", acc,"\n")


#########################################################################################

# Banknotes dataset
d2 <- read.csv(file.choose(), header = F)

summary(d2)
d2$V5 <- as.factor(d2$V5)
str(d2)

# Corr Plot
corr <- cor(d2[,-c(5)])
corrplot(corr)

# Histogram
plot_histogram(d2[,-c(5)])

# Distribution of target variable
table(d2$V5)   # ALmost evenly distributed between the 2 classes

#To split the data into training and test set
set.seed(420) 
ind <- sample(2, nrow(d2), replace = T, prob = c(0.8, 0.2))
train <- d2[ind == 1,]
test <- d2[ind == 2,]

############################################################################################s
# Logistic Regression
mod <- glm(V5~.,data = train,family = 'binomial')

##Apply the trained model to the test set
p <- predict(mod, test, type = 'response')
p <- as.factor(ifelse(p >0.5,1,0))
confusionMatrix(p, test$V5)

########################################################################################

# Neural Network
set.seed(123)
cvcontrol <- trainControl(method="repeatedcv", 
                          number = 10,
                          repeats = 5,
                          allowParallel=TRUE,
                          savePredictions = T)

b <- 0


for (x in 1:100) {
  Model.nn <- train(V5 ~., data=train,
                    method="nnet",
                    trControl=cvcontrol,
                    preProcess=c("center","scale"),
                    tunelength = 5,
                    maxit = 100,
                    metric="Accuracy")
  
  # plotnet(Model.nn$finalModel)
  
  testpred.nn <- predict(Model.nn, test)
   k <- confusionMatrix(test$V5, testpred.nn, mode='everything')   #1
  
  a <- as.numeric(k[["overall"]][1])
  b <- b+a
  print(x)
  
}

accuracy <- (b)/100

g <- expand.grid(size = seq(from=3, to=9, by=1) ,
                 decay = seq(from=0.1, to=0.7, by=0.05))

Model.nn <- train(V5 ~., data=train,
                  method="nnet",
                  trControl=cvcontrol,
                  preProcess=c("center","scale"),
                  tuneGrid = g,
                  maxit = 100,
                  metric="Accuracy")

plot(Model.nn)

accuracy <- (b)/100


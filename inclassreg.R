# read data
 
auto <- read.csv(file.choose(), header = T)

auto$horsepower <- as.numeric(auto$horsepower)
auto <- na.omit(auto)
y <- auto[,1]
n <- nrow(auto)
p <- 8
x <- matrix(0, nrow(auto), (p-1))
for(i in 1:(p-1)) { x[,i]<-auto[,(i+1)];}
mylm <- lm(y~x)
summary(mylm)
plot(mylm)
par(mfrow = c(1,1))

library(car)



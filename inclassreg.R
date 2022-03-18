# read data
auto <- read.csv(file.choose(), header = T)

# Data cleaning
auto$horsepower <- as.numeric(auto$horsepower)
auto <- na.omit(auto)
y <- auto[,1]
n <- nrow(auto)
p <- 8
x <- matrix(0, nrow(auto), (p-1))
for(i in 1:(p-1)) { x[,i]<-auto[,(i+1)];}

# Running the model
mylm <- lm(y~x)
summary(mylm)

library(car)

# Calculating Residuals
residuals <- mylm$residuals

# Normality test
ks.test(residuals, rnorm(100,0,1))    # Since p value is very small, we can reject the null hypothesis
                                      # And say that residuals do not follow a normal distritbution
shapiro.test(residuals)               # Small value of Shapiro-Wilk test also leads up to the same result


# Constant Variance Assumption/Cook Weinberg Test
ncvTest(mylm)                         # Small value indicates that assumption is violated

plot(mylm$fitted.values, mylm$residuals)

# Spread Level Plot
myspread <- spreadLevelPlot(mylm)
myspread

# Spread Level transformation and New Model
z<-y^(myspread$PowerTransformation)
mylm2<-lm(z ~ x)
summary(mylm2)

# Cook Weinberg Test Again
ncvTest(mylm2)                      # Still a small value of p indicated non constant variance
plot(mylm2$fitted.values, mylm2$residuals)

# Spread Level transformation and New Model
z<-y^0.5
mylm3<-lm(z ~ x)
summary(mylm3)

# Cook Weinberg Test Again
ncvTest(mylm3)                      # Still a small value of p indicated non constant variance
plot(mylm2$fitted.values, mylm2$residuals)

# Spread Level transformation and New Model
z<-y^0.05
mylm3<-lm(z ~ x)
summary(mylm3)

# Cook Weinberg Test Again
ncvTest(mylm3)                      # A larger value of p indicates that transformation is successful and constant variance is achieved
plot(mylm2$fitted.values, mylm2$residuals)

# Normality Test 
ks.test(rnorm(nrow(x), 0, sd(mylm3$residuals)), mylm3$residuals)   # Large value of p indicates that residuals are normal


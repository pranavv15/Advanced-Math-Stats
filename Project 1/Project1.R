library(dplyr)
library(ggplot2)
library(Amelia)
library(car)

# Reading the data
data <- read.csv(file.choose(), header = T)

################################################################################################

# Data exploration

# Number of numeric variables
length(select_if(data, is.numeric))

# Number of categorical variables
length(select_if(data, is.character))

# Summary of data
summary(data)
str(data)

# Scatter plot of year vs # of attacks
data %>% group_by(iyear) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=iyear, y=count))+
  geom_point()

# Scan for missing values
missmap(data, col = c('yellow', 'black'), y.at = 1, y.labels = '', legend = TRUE)

# Geo  of location of attacks 
library(tidyverse)
library(ggmap)
world <- map_data("world")
world

# Dividing the data depending on # of casualties
data_maj <- data %>% filter(nkill>10)
data_min <- data %>% filter(nkill<3)
data_sm <- data %>% filter(nkill>=3&nkill<=10)

# Getting Counts
count <- data_maj %>% group_by(iyear) %>% 
  summarise(Count = n())

count2 <- data_min %>% group_by(iyear) %>% 
  summarise(Count = n())

count3 <- data_sm %>% group_by(iyear) %>% 
  summarise(Count = n())

# Scatter Plot

colors <- c("Major Attacks"="blue","Small Attacks"="Yellow","Minor Attacks"="red")
ggplot()+
  geom_point(data=count, aes(x=iyear,y=Count,color="Major Attacks"), show.legend = TRUE)+
  geom_point(data=count2, aes(x=iyear,y=Count,color="Minor Attacks"),show.legend = TRUE)+
  geom_point(data=count3, aes(x=iyear,y=Count,color="Small Attacks"),show.legend = TRUE)+
  ggtitle('Three types of terror attacks') +
  labs(color = "Type of Attack")+
  scale_color_manual(values = colors)+
  scale_x_continuous("Year of Attacks")+
  scale_y_continuous("Number of Attacks")

###################################################################################################

# For major attacks

# Geo Plot of attacks
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data = data_maj,
    aes(longitude, latitude,
        color = nkill),
    alpha = 0.5
  ) +
  labs(x = NULL, y = NULL, color = NULL)+
  theme_void() +
  theme(legend.position = "none")+
  labs(title="Major Terror Attack Locations")

# Scatter plot
plot(x=count$iyear,y=count$Count,main = "Major Terror Attacks",
     xlab = "Year",
     ylab = "Number of Attacks")
abline(lm(count$Count~count$iyear, data = count),col = "red")

# Model1
mod <- lm(count$Count~count$iyear, data = count)
summary(mod)

par(mfrow = c(2,2))
plot(mod)
par(mfrow = c(1,1))

# Calculating Residuals
residuals <- mod$residuals

# Normality test
ks.test(residuals, rnorm(100,0,1))    # Since p value is very small, we can reject the null hypothesis

# And say that residuals do not follow a normal distritbution
shapiro.test(residuals)               # Small value of Shapiro-Wilk test also leads up to the same result


# Constant Variance Assumption/Cook Weinberg Test
ncvTest(mod)                         # Small value indicates that assumption is violated

plot(mod$fitted.values, mod$residuals)

# Spread Level Plot
myspread <- spreadLevelPlot(mod)
myspread

y <- count$Count
x <- count$iyear

# Spread Level transformation and New Model
z<-y^(myspread$PowerTransformation)
mylm2<-lm(z ~ x)
summary(mylm2)

# Cook Weinberg Test Again
ncvTest(mylm2)                      # a high value of p indicates constant variance
plot(mylm2$fitted.values, mylm2$residuals)

# Normality Test 
residuals2 <- mylm2$residuals
n <- 47
sd1 <- sd(mylm2$residuals)

ks.test(rnorm(n,0,sd1),mylm2$residuals)   # Large value of p indicates that residuals are normal

###################################################################################################

# For minor attacks

# Geo Plot of Attacks
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data = data_min,
    aes(longitude, latitude,
        color = nkill),
    alpha = 0.5
  ) +
  labs(x = NULL, y = NULL, color = NULL)+
  theme_void() +
  theme(legend.position = "none")+
  labs(title=" Minor Terror Attack Locations")

# Scatter Plot 
plot(x=count2$iyear,y=count2$Count,main = "Minor Terror Attacks",
     xlab = "Year",
     ylab = "Number of Attacks")
abline(lm(count2$Count~count2$iyear, data = count2),col = "red")


# Model2
mod2 <- lm(count2$Count~count2$iyear, data = count2)
summary(mod2)

# Calculating Residuals
residuals <- mod2$residuals

# Normality test
ks.test(residuals, rnorm(100,0,1))    # Since p value is very small, we can reject the null hypothesis
# And say that residuals do not follow a normal distritbution
shapiro.test(residuals)               # Small value of Shapiro-Wilk test also leads up to the same result


# Constant Variance Assumption/Cook Weinberg Test
ncvTest(mod2)                         # Small value indicates that assumption is violated

plot(mod2$fitted.values, mod2$residuals)

# Spread Level Plot
myspread <- spreadLevelPlot(mod2)
myspread

y <- count2$Count
x <- count2$iyear

# Spread Level transformation and New Model
z<-y^(myspread$PowerTransformation)
mylm2<-lm(z ~ x)
summary(mylm2)

# Cook Weinberg Test Again
ncvTest(mylm2)                      # a high value of p indicates constant variance
plot(mylm2$fitted.values, mylm2$residuals)


myspread <- spreadLevelPlot(mylm2)
myspread

# Spread Level transformation and New Model
z<-z^(myspread$PowerTransformation)
mylm2<-lm(z ~ x)
summary(mylm2)

ncvTest(mylm2)                      # a high value of p indicates constant variance
plot(mylm2$fitted.values, mylm2$residuals)


# Normality Test 
residuals2 <- mylm2$residuals
n <- 47
sd1 <- sd(mylm2$residuals)

ks.test(rnorm(n,0,sd1),mylm2$residuals)   # Large value of p indicates that residuals are normal


###################################################################################################

# For small attacks

#Scatter Plot
plot(x=count3$iyear,y=count3$Count,main = "Small Terror Attacks",
     xlab = "Year",
     ylab = "Number of Attacks")
abline(lm(count3$Count~count3$iyear, data = count3),col = "red")

# Model3
mod3 <- lm(count3$Count~count3$iyear, data = count3)
summary(mod3)
par(mfrow = c(2,2))
plot(mod3)

# Calculating Residuals
residuals <- mod3$residuals

# Normality test
ks.test(residuals, rnorm(100,0,1))    # Since p value is very small, we can reject the null hypothesis
# And say that residuals do not follow a normal distritbution
shapiro.test(residuals)               # Small value of Shapiro-Wilk test also leads up to the same result


# Constant Variance Assumption/Cook Weinberg Test
ncvTest(mod3)                         # Small value indicates that assumption is violated

plot(mod3$fitted.values, mod3$residuals)

# Spread Level Plot
myspread <- spreadLevelPlot(mod3)
myspread

y <- count3$Count
x <- count3$iyear

# Spread Level transformation and New Model
z<-y^(myspread$PowerTransformation)
mylm2<-lm(z ~ x)
summary(mylm2)

# Cook Weinberg Test Again
ncvTest(mylm2)                      # a high value of p indicates constant variance
plot(mylm2$fitted.values, mylm2$residuals)

# Spread Level Plot
myspread <- spreadLevelPlot(mylm2)
myspread

z<-z^(myspread$PowerTransformation)
mylm2<-lm(z ~ x)
summary(mylm2)

ncvTest(mylm2)                      # a high value of p indicates constant variance
plot(mylm2$fitted.values, mylm2$residuals)

# Normality Test 
residuals2 <- mylm2$residuals
n <- 47
sd1 <- sd(mylm2$residuals)

ks.test(rnorm(n,0,sd1),mylm2$residuals)   # Large value of p indicates that residuals are normal


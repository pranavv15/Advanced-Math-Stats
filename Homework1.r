# a. Getting the data
library(UsingR)
data(father.son)
View(father.son)

# b. Scatter Plot of father's and son's height
plot(father.son$fheight,father.son$sheight, xlab = "Father's Height(in)", 
     ylab = "Son's Height(in)", pch=20)

# c. Regression line on scatter plot
abline(lm(father.son$sheight~father.son$fheight, data=father.son), col="red")

# d. Add SD line to plot
slope <- sd(father.son$sheight)/sd(father.son$fheight)
intercept <- (68.68407) - (67.6871)*1.025441
abline(a=intercept, b=slope, col="blue", lty=4, lwd=3)

# e. center of regression
points(mean(father.son$fheight),mean(father.son$sheight),col="yellow")

# f. Horizontal and vertical lines through center of regression
abline(h=mean(father.son$sheight), col="green")
abline(v=mean(father.son$fheight), col="green")

# g. Linear regression output
model <- lm(father.son$sheight~father.son$fheight, data = father.son)
summary(model)

####################################################################################################################### COMM 581 - Assignment 2 - Regression Analysis
# Gurpal Bisra
# Student #: 69295061
# Data: Fruits_veg_and_smoking
# Centers for Disease Prevention and Control (CDC) 
######################################################################################################################

mydata <- read.csv("Fruits_veg_and_smoking.csv", header=TRUE)
    # Explanatory variable: % of people who smoke every day
    # Response variable: % of people who eat at least 5 servings of fruits and vegetables every day

print(mydata)

str(mydata)

plot(fruits.veg ~ smoking, data=mydata, pch=16, main = "People who Eat Enough Fruits and Vegetables who Smoke", xlab="Every Day Smokers (%)", ylab="Eat Fruits and Vegetables (%)") 
# graph the response variable vs. the explanatory variable
lines(lowess(mydata$smoking, mydata$fruits.veg, delta=0.1), col="red")  # plots a line through data - cool
    # locally weighted averaged line
    # can change delta values

# Add a smoothed curve through the data (locally weighted regression)   -- but same as above
plot(fruits.veg ~ smoking, data=mydata, pch=16, main = "People who Eat Enough Fruits and Vegetables who Smoke", xlab="Every Day Smokers (%)", ylab="Eat Fruits and Vegetables (%)")  # y ~ x
x1 <- mydata$smoking[order(mydata$smoking)] #put the x-values in order
y1 <- mydata$fruits.veg[order(mydata$smoking)] # put the y-values in order based on the order of the x-values
lines(lowess(x1, y1, delta=12), col="red")

#Try some different values for delta, what do you see?
plot(fruits.veg ~ smoking, data=mydata, pch=16) # y ~ x
x1 <- mydata$smoking[order(mydata$smoking)] #put the x-values in order
y1 <- mydata$fruits.veg[order(mydata$smoking)] # put the y-values in order based on the order of the x-values
lines(lowess(x1, y1, delta=12), col="red")

plot(fruits.veg ~ smoking, data=mydata, pch=16) # y ~ x
x1 <- mydata$smoking[order(mydata$smoking)] #put the x-values in order
y1 <- mydata$fruits.veg[order(mydata$smoking)] # put the y-values in order based on the order of the x-values
lines(lowess(x1, y1, delta=100), col="red")
    # increasing delta smooths out the line you made

# -----------------------------------------------------------------
# Try fitting a linear model so that you can make the residual plot and look at it
z1 <- lm(fruits.veg ~ smoking, data=mydata)
    # z.1 <- lin(mydata$y~mydata$x)   # or do this
    # z.1 <- lin(y~x, data=mydata)    # or do this & same for plotting, plot(y~x, data=mydata)
    # z1 is the name you gave to model (i.e. x, y name of variables)
resid1 <- resid(z1)   # get residuals 
resid1

predict1 <- predict(z1)   # predictions from the results of various model fitting functions
predict1


# residual plot
plot(resid1 ~ predict1, pch=16) 

# residual plot with a line at residuals = 0
plot(resid1 ~ predict1, pch=16, main = "Residuals of Predicted Model", xlab="Predicted Model", ylab="Residuals")
abline(0,0, lty=2) #line type = 2

# note: par(mfrow=c(2,2))
# plot(lm(sales~dwelling.permits.data=mydata))

# Is this linear?
# NO - no balance

# Let's try another model
plot(fruits.veg ~ smoking, data=mydata, pch=16)
x2 <- mydata$smoking[order(mydata$smoking)]
y2 <- mydata$fruits.veg[order(mydata$smoking)]
lines(lowess(x2, y2, delta=0.1), col="red")


z2 <- lm(fruits.veg ~ smoking, data=mydata)
resid2 <- resid(z2)
resid2

predict2 <- predict(z2)
predict2

# Residual plot
plot(resid2 ~ predict2, pch=16)
abline(0,0, lty=2)

# looks linear, circle outliers for assumption 2
# hard to say if assumption 2 met or not because need more points

# Use the residual plot to assess Assumption #2
hist(resid2)
hist(resid2, breaks = seq(-10, 10, by=0.5))

# R provides you with some plots to look at model fit
plot(z2)  # then press enter in console   -- get Q-Q plot! :)

# To conduct the normality tests, you will need to install a package in R called "nortest"
    # slide 24, here we want H0 to be true, we want p > alpha value (i.e. 0.05) (opposite to normal)
    # in all 4 cases, fail to reject so that's great :)

library(nortest)
shapiro.test(resid2) #Shapiro-Wilk test; don't need library for this one
ad.test(resid2) #Anderson-Darling test
cvm.test(resid2) #Cramer-von Mises
lillie.test(resid2) #Kolmogorov-Smirnov

# recall, there are other ways to check this too

# Assumption 4, not known how data collected, so can't say yet

#Is the regression significant?    

anova(z2) # This gives the analysis of variance table with the F-test for the significance of the regression 
    # result with F-statistic of 0.4443. but find F-critical then compare
    # result with p-value = 0.5131 --> fail to reject H0 because p > alpha; therefore, meaning regression is not significant
        # thus, not work with data anymore

summary(z2) # This gives the co-efficients and t tests to see if they differ from zero. The test of the regression is at the bottom (with the F-statistic) and the r-squared value is given. However, if the regression is not significant, there is no value in looking at the r-squared value.

# Note the p-value from the anova table and for the t-test for the co-efficient of the x-variable. What would explain this observation?

------------
# NOW, NEW PART - test the intercept and slope
# plot with regression line
plot(fruits.veg ~ smoking, data=mydata, pch=16)
abline(z1, lty = 2)     # lty = line type, 2 makes dashed, nothing means solid
# Plot 5    - regression line fits well, yay!

# create vectors and dataframes with data for confidence bands (CI)
# CI either straight line or curved line (complicated)
# OR create new sequence of x-values, cover range of data, seen below
xnew <- seq(min(mydata$smoking), max(mydata$smoking), length.out = 100)   # 100 points
xnew
# Now plug in each value of x and calculate the CI value for it
ynew.ci <- data.frame(predict(z1, newdata = data.frame(smoking = xnew), interval = "confidence", level = 0.95))
# get fit (i.e. fitted values), lwr (i.e. lower band), and upr (i.e. upper band)
ynew.ci


# create the plot with the regression line and the confidence bands
# here predict a mean
plot(fruits.veg ~ smoking, data=mydata, pch=16, main = "Regression Line and Confidence Bands", xlab="Every Day Smokers (%)", ylab="Eat Fruits and Vegetables (%)") 
abline(z1, lty = 1, col = "red")
lines(ynew.ci$lwr ~ xnew, lty = 2, col = "blue")
lines(ynew.ci$upr ~ xnew, lty = 2, col = "blue")
# Plot 6 = regression line with confidence intervals


# create vectors and dataframes with data for prediction intervals -- ONLY DIFFERENCE select predict instead
# wider because predict a single point, not a mean
xnew <- seq(min(mydata$smoking), max(mydata$smoking), length.out = 100)
ynew.pred <- data.frame(predict(z1, newdata = data.frame(smoking = xnew), interval = "prediction", level = 0.95))

plot(fruits.veg ~ smoking, data=mydata, pch=16, main = "Regression Line and Prediction Intervals", xlab="Every Day Smokers (%)", ylab="Eat Fruits and Vegetables (%)")
abline(z1, lty = 1, col = "red")
lines(ynew.pred$lwr ~ xnew, lty = 2, col = "blue")
lines(ynew.pred$upr ~ xnew, lty = 2, col = "blue")
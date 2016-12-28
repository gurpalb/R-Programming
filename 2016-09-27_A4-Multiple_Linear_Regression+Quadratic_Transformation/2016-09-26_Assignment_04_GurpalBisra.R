#######################################################
# COMM 581 - Assignment 04 - Multiple Linear Regression
# Instructor: Martha Essak
# Gurpal Bisra
# Student #: 69295061
# Due date: Oct. 3, 2016 at 11 p.m.
#######################################################

# You are trying to determine the temperature at which to conduct a manufacturing process that generates the maximum yield. You have measured the yield at different temperatures. The quadratic model equation for the population will be: yi = ??0 + ??1 x1i + ??2 x2i + ??i where x1 is temperature and x2 is temperature squared.

### Question 1 -------------------------------------------------------------------
### Graph the relationship between yield and temperature. Does the relationship look linear? Discuss the form and strength of the relationship, and identify any outliers. 

mydata <- read.csv("Yield_and_temperature.csv", header=TRUE)
    # import data from csv, and declaring header row at top

str(mydata)     # Observe structure of data frame which has 40 rows, 3 columns
    # 'data.frame':	11 obs. of  2 variables:
    # $ yield      : int  127 139 147 147 155 154 153 148 146 136 ...
    # $ temperature: int  600 625 650 675 700 725 750 775 800 825 ...

plot(yield ~ temperature, data = mydata, col = "blue", pch = 16, main = "Manufacturing Yield is Nonlinear Function of Temperature", xlab="Temperature", ylab="Yield")
# see quadratic relationship, therefore, transform x
    # denote x1 = first variable
    # (1) Form of Relationship or association = linear or not?
    # association mean linear or not, while correlation means linear
    # (2) Direction = positive then negative
    # (3) Strength = no clustered points 
    # (4) Outliers = posibly only at 675

# Determine outliers by plotting a line through data of locally weighted averaged lines where I set the delta value (i.e. want delta as low as possible)
lines(lowess(mydata$temperature, mydata$yield, delta=0.1), col="red")  

### Question 2 -------------------------------------------------------------------
### Fit a quadratic model (including two explanatory variables: temperature and the square of temperature) and create the residual plot for the model. Using the residual plot, assess the assumptions of linearity and equal variance. Divide the residual plot into 3-4 segments to assess these assumptions. State any concerns you have and their consequences. (2 marks) 

# create a new column in your dataframe with transformed values of the x-variable
mydata$temperature.squared <- (mydata$temperature)^2    # must do as separate step, because R not know x or x^2 related
    # denote: temperature^2 = x2

# create model, of y then list x variables
z2 <- lm(yield ~ temperature + temperature.squared, data=mydata) 
    # fit a multiple linear regression model with the original x-variable and the transformed x-variable
    # New dataset --> temperature, temperature.squared

# Check if relationship is linear
    # Compute residuals of object z1 (linear fit of sales vs. number of employees)
resid1 <- resid(z2)
    # Calculate predicted y_hat values
predict1 <- predict(z2)

# Residual plot for model based on original units
plot(resid1 ~ predict1, pch=16, main = "Residuals of Predicted Model", xlab="Predicted Yield Values", ylab="Errors of Residuals of Yield")
    # Add line Residuals = 0
abline(0, 0, lty=2)

# Assess assumptions of linearity and equal variance

# Now test significance of each variable
# yi = Bo + B1x1i + B2(x1i^2) + ei
# Now, do t-test to see if we need x1i at all?

# y = yield, x1 = temperature, no need for back transformation

### Question 3 -------------------------------------------------------------------
### Check the normality assumption using a histogram, normality plot, and normality tests. Adjust the bin width for the histogram to be informative. State any concerns you have and their consequences.

# Look at the histogram to see whether the residuals are normally distributed
hist(resid1, breaks = seq(-4, 5, by=1.5), main = "Histogram of Predicted Model", xlab="Errors of Residuals of Yield", ylab="Frequency")

# plot(z2)        # get graphs that dont' make sense
qqnorm(resid1)
qqline(resid1, main = "Normal Q-Q plot of residuals", col = "red")

# Normality testing with alpha = 0.05
library(nortest)
shapiro.test(resid1)
ad.test(resid1)
cvm.test(resid1)
lillie.test(resid1)

# Plot yield vs. y_hat
mydata$predict1 <- predict(z2)
mydata$resid1 <- resid(z2)
plot(yield ~ predict1, data = mydata, pch = 16, col = "blue", main = "Yield Values Similar to Predicted Values", xlab="Predicted Yield", ylab="Yield")
# not Q-Q plot, but looking at how well the model matches the data
abline(0,1,lty=2)

### Question 5 -------------------------------------------------------------------
### Write the ANOVA table for this model showing each component of variation (variable 1, variable 2, error, total), its associated df, sum of squares and mean squares. (3 marks)
anova(z2)
# double check addition with summary(z2)
summary(z2)

# Calculate MSreg, MSE from anozva table

### Question 7 -------------------------------------------------------------------
### Can you simplify the model? Test if the co-efficient associated with temperature is equal to 0 (and could therefore be removed from the model). Use a t-test for this co-efficient. What does it mean if this co-efficient is not equal to 0?

summary(z2)

### Question 8 -------------------------------------------------------------------
### State the estimates of the co-efficients (b0, b1, b2) and calculate their confidence intervals using the standard errors from the outputs. Do the confidence intervals for b1 and b2 overlap zero? What does it mean if they do overlap zero?

confint(z2, level=0.95) # get the 95% confidence intervals for the co-efficients

### Question 9 -------------------------------------------------------------------
### Calculate the R2 value (co-efficient of multiple determination) and standard error of the estimate (root MSE) from the sums of squares. Show your calculations. Check if these are in agreement with the outputs from R.

SSY <- sum((mydata$yield - mean(mydata$yield))^2)
SSY
SSX <- sum((mydata$temperature - mean(mydata$temperature))^2)
SSX
mean(mydata$temperature)

### Question 10 + 11 -------------------------------------------------------------------
### What temperature gives the maximum yield based on the model? What is the predicted yield at the optimal temperature? Calculate the prediction interval associated with this yield.

# Find the prediction intervals
xnew <- seq(min(mydata$temperature), max(mydata$temperature), length.out = 100) 
    # create a new vector with 100 different points ranging from your minimum x-value to your maximum x-value
xnew <- as.data.frame(xnew) # convert this to a dataframe so that you can add additional columns
    # because now we have multiple parameters, temperature and temperature.squared
names(xnew)[1] <- c("temperature") 
    # rename the first column to match your original x-variable, I called it temperature before in the code, so I will     give it the same name now. This makes it easy for R to match up the new data with the variables that are in the        model
    # names must match!
xnew$temperature.squared <- (xnew$temperature)^2 
    # create a second column in your dataframe with temperature squared. Again, give this the same variable name it had     before in the code
xnew # This is a good point to view your new data to check that everything is working properly

ynew.ci <- data.frame(predict(z2, newdata = data.frame(xnew), interval = "prediction", level = 0.95)) 
    # R will calculate the predicted values and  prediction intervals for all the different new x values that you have     provided
ynew.ci
new.values <- cbind(xnew,ynew.ci) 
    # combine the dataframe  with the new x-values and the dataframe with the predicted y-values and prediction            intervals
new.values 
    # Check that all your columns are what you would expect them to be

### to find max in data frame
xnew[which.max(xnew$temperature), ]
ynew.ci[which.max(ynew.ci$fit), ]
# then it's corresponds to row 50 in xnew is 723.... 

predict(z2, data.frame(temperature = 723.7374, temperature.squared=523795.8, fit=154.0416),interval = "prediction", level = 0.95)

sqrt(((((723.7374 -725)^2)/68750)+(1/11)+1)*3.63)

### Question 12 -------------------------------------------------------------------
### Create a plot of the original data (yield vs. temperature) including the line of best fit and lines for the prediction intervals.

plot(mydata$yield~mydata$temperature, pch = 16, ylim=c(125,160), main = "Manufacturing Yield as Function of Temperature \n with Line of Best Fit", xlab="Temperature", ylab="Yield")
lines(new.values$fit ~ new.values$temperature, lty=1, col = "red")
    # this looks different because we we are using the columns from the data frame instead of vectors
lines(new.values$lwr ~ new.values$temperature, lty = 2, col = "blue")
lines(new.values$upr ~ new.values$temperature, lty = 2, col = "blue")
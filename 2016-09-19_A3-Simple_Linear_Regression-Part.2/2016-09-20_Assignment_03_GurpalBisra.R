######################################
# COMM 581 - Assignment 03
# Instructor: Martha Essak
# Gurpal Bisra
# Student #: 69295061
# Due date: Sept 27, 2015 at 1 p.m.
######################################

###############
# IMPORT DATA
###############
mydata <- read.csv("Company_employment_and_sales.csv", header=TRUE)  # import data from csv, and declaring header row at top

str(mydata)     # Observe structure of data frame which has 40 rows, 3 columns
    # 'data.frame':	40 obs. of  3 variables:
        # $ company  : Factor w/ 40 levels "Alaska Air Group",..: 1 2 3 ...
        # $ employees: int  3663 19166 1677 534 912 1024 1318 ...
        # $ sales    : int  14485 13900 1995 2100 1900 11577 ...

########################
# NON-TRANSFORMED MODEL
########################

### Question 1 -------------------------------------------------------------------
### Graph the relationship between sales and number of employees. Does the relationship look linear? Are there any outliers? Which companies do these outlying points represent? (1.5 marks)

# Plot of sales (in millions of $) agains number of employees 
plot(sales ~ employees, data = mydata, pch = 15, main = "Company Sales in Millions of Dollars for Given Number of Employees", xlab="Number of Employees", ylab="Sales in Millions ($)")

# (1) Form of Relationship or association = linear or not?
        # association mean linear or not, while correlation means linear
# (2) Direction = positive
# (3) Strength = clustered points or not for what we want to draw? 
# (4) Outliers = yes or no?
    # correlation, (+), strong below 20,000 employees, few outliers

# Determine outliers by plotting a line through data of locally weighted averaged lines where I set the delta value (i.e. want delta as low as possible)
lines(lowess(mydata$employees, mydata$sales, delta=0.1), col="red")  

### Question 2 -------------------------------------------------------------------
### Use a residual plot to help you assess the assumptions of linearity and equal variance
# Fitting Linear Model = lm function in what we just plotted, call object z1
z1 <- lm(sales ~ employees, data = mydata)

# Check if relationship is linear
    # Compute residuals of object z1 (linear fit of sales vs. number of employees)
resid1 <- resid(z1)
    # Calculate predicted y_hat values
predict1 <- predict(z1)

# Residual plot for model based on original units
plot(resid1 ~ predict1, pch=16, main = "Residuals of Predicted Model", xlab="Predicted Number of Employees", ylab="Errors of Residuals of Sales in Millions ($)")
    # Add line Residuals = 0
abline(0, 0, lty=2)

# Assess assumptions of linearity and equal variance

#################################
# ADD TRANSFORMATIONS TO DATASET
#################################
### Question 3 -------------------------------------------------------------------
### Try transforming the x and y variable using a natural logarithm. Graph the relationship between these new variables. Does the relationship look linear?

# Compute the natural log of each value in mydata$employees
mydata$employees.log <- log(mydata$employees)
    # Compute the natural log of each value in mydata$sales
mydata$sales.log <- log(mydata$sales)

str(mydata)     # Observe structure of data frame which has 40 rows, 3 columns
        # 'data.frame':	40 obs. of  3 variables:
        # $ company  : Factor w/ 40 levels "Alaska Air Group",..: 1 2 3 ...
        # $ employees: int  3663 19166 1677 534 912 1024 1318 ...
        # $ sales    : int  14485 13900 1995 2100 1900 11577 ...
        # $ employees.log: num  8.21 9.86 7.42 6.28 6.82 ...
        # $ sales.log    : num  9.58 9.54 7.6 7.65 7.55 ...

########################
# LOG TRANSFORMED MODEL
########################

# Plot of sales (in millions of $) agains number of employees 
plot(sales.log ~ employees.log, data = mydata, pch = 16, type = "p", main = "Log-Log Plot of Company Sales in Millions of Dollars \n for Given Number of Employees", xlab="Log of Number of Employees", ylab="Log of Sales in Millions ($)")

# (1) Form of Relationship or association = linear or not?
# association mean linear or not, while correlation means linear
# (2) Direction = positive
# (3) Strength = clustered points or not for what we want to draw? 
# (4) Outliers = yes or no?
# correlation, (+), strong for all points, no clear outliers

# Determine outliers by plotting a line through data of locally weighted averaged lines where I set the delta value (i.e. want delta as low as possible)
lines(lowess(mydata$employees.log, mydata$sales.log, delta=0.1), col="red")  

### Question 4 -------------------------------------------------------------------
### Use a residual plot to help you assess the assumptions of linearity and equal variance for this new model. State any concerns you have and their consequences. 

# Fitting Linear Model = lm function in what we just plotted, call object z2     
z2 <- lm(sales.log ~ employees.log, data = mydata)

# Check if relationship is linear
    # Compute residuals of object z2 (linear fit of sales.log vs. employees.log)
resid2 <- resid(z2)
    # Calculate predicted y_hat_new values
predict2 <- predict(z2)

# residual plot for model based on log of original units
plot(resid2 ~ predict2, pch=16, main = "Residuals of Log-Log Predicted Model", xlab="Log of Predicted Number of Employees", ylab="Errors of Residuals of Log-Sales in Millions ($)")
# Add line Residuals = 0
abline(0, 0, lty=2)
# plot(z2) # you can leave this commented out unless you are using it, then delete the and run the code


### Question 5 -------------------------------------------------------------------
### Check the other assumptions: normality (histogram, normality plot, normality tests), independence of observations, and assumptions related to sampling. State any concerns you have and their consequences. (2 marks)

# Look at the histogram to see whether the residuals are normally distributed
hist(resid2)
hist(resid2, breaks = seq(-2.5, 2.5, by=0.2), main = "Histogram of Log-Log Predicted Model", xlab="Errors of Residuals of Log-Sales in Millions ($)", ylab="Frequency")

# R provides you with some plots to look at model fit
plot(z2)  # then press enter in console   -- get Q-Q plot! :)
qqnorm(resid2)
qqline(resid2, main = "Normal Q-Q plot of residual", col = "red")

# To conduct the normality tests, you will need to install a package in R called "nortest"
# slide 24, here we want H0 to be true, we want p > alpha value (i.e. 0.05) (opposite to normal)
# in all 4 cases, fail to reject so that's great :)

library(nortest)
shapiro.test(resid2) #Shapiro-Wilk test; don't need library for this one
ad.test(resid2) #Anderson-Darling test
cvm.test(resid2) #Cramer-von Mises
lillie.test(resid2) #Kolmogorov-Smirnov

### Question 6 -------------------------------------------------------------------
### Test the significance of the regression

anova(z2) # This gives the analysis of variance table with the F-test for the significance of the regression 
# result with F-statistic of 0.4443. but find F-critical then compare
# result with p-value = 0.5131 --> fail to reject H0 because p > alpha; therefore, meaning regression is not significant
# thus, not work with data anymore

### Question 7 -------------------------------------------------------------------
### Calculate the predicted values (in log-transformed units), then back-transform these. Use these back-transformed values to calculate the errors in the original units. Calculate the SSE, SSY and SSR in original units (show your calculations, you are welcome to do this in R or Excel) (1.5 marks)

summary(z2) # This gives the co-efficients and t tests to see if they differ from zero. The test of the regression is at the bottom (with the F-statistic) and the r-squared value is given. However, if the regression is not significant, there is no value in looking at the r-squared value.


###################################################################
# CREATE DATASET WITH PREDICTED VALUES IN ORIGINAL UNITS TO EXPORT
###################################################################

yhat.original <- exp(predict2) # this is where we backtransform the predicted values into the original units
yhat.original

yhat.original <- as.data.frame(yhat.original) #make this into a data frame so that you will be able to add it to your original data frame

mydata.2 <- cbind(mydata, yhat.original) # since these data frames have the same number of rows, they should line up nicely
mydata.2

write.csv(mydata.2, file="2016-09-23_Assignment_03_data1.csv")

#################################
# CALCULATIONS IN ORIGINAL UNITS
#################################

# Get the predicted values in the original units, using e to the power of the predicted values; this is the exp() function

SSE <- sum((mydata.2$sales - mydata.2$yhat.original)^2)
SSE
SSY <- sum((mydata.2$sales - mean(mydata.2$sales))^2)
SSY
SSR <- sum((mean(mydata.2$sales) - mydata.2$yhat.original)^2)
SSR


# Calculate measures of goodness of fit in the original units
# Write your own code for this part


### Question 9 -------------------------------------------------------------------
### State the estimates of the co-efficients (b0, b1) and calculate their 95% confidence intervals. Back-transform these values into their original units.

#############################################
# CONFIDENCE INTERVALS FOR THE CO-EFFICIENTS
#############################################

confint(z2, level=0.95) # this is based on the log model, so everything will be in log units

### Question 10 -------------------------------------------------------------------
### 10.	Create a plot of the data including the regression line and the confidence bands in the log units.

####################################
# CREATE CONFIDENCE BANDS FOR PLOTS
####################################
xnew <- seq(min(mydata$employees.log), max(mydata$employees.log), length.out = 100)
xnew

ynew.ci <- data.frame(predict(z2, newdata = data.frame(employees.log = xnew), interval = "confidence", level = 0.95))
ynew.ci

new.values <- cbind(xnew,ynew.ci) # combine the vector with the new x-values and the dataframe with the predicted y-values and values for the confidene bands
print(new.values)

# create the plot with the regression line and the confidence bands in log units
plot(sales.log ~ employees.log, data = mydata, pch = 16, main = "Regression Line and Confidence Bands in Log-transformed Units", xlab="Log of Number of Employees", ylab="Log of Sales in Millions ($)") 
abline(z2, lty = 1, col = "red")
lines(ynew.ci$lwr ~ xnew, lty = 2, col = "blue")
lines(ynew.ci$upr ~ xnew, lty = 2, col = "blue")

### Question 11 -------------------------------------------------------------------
### Create a plot of the data including the regression line and the confidence bands in the original units

# backtransform everything
new.values
new.values$xnew.back <- exp(new.values$xnew) # this automatically adds new columns to the data frame. Remember that we have to add the backtransformed x-values because we transformed both x and y values.
new.values$fit.back <- exp(new.values$fit)
new.values$lwr.back <- exp(new.values$lwr)
new.values$upr.back <- exp(new.values$upr)

new.values

# plot the regression line and the confidence bands in the original units
plot(sales ~ employees, data = mydata, pch = 16, main = "Regression Line and Confidence Bands in Original Units", xlab="Number of Employees", ylab="Sales in Millions ($)") 
lines(new.values$fit.back ~ new.values$xnew.back, lty=1, col = "red") # this looks a little different because we are using columns from a dataframe instead of vectors
lines(new.values$lwr.back ~ new.values$xnew.back, lty = 2, col = "blue")
lines(new.values$upr.back ~ new.values$xnew.back, lty = 2, col = "blue")

# create the plot with the prediction interval. Try making it in log units and then in the original units
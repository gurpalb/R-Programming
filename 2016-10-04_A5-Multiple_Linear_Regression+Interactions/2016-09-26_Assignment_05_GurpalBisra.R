######################################################################
# COMM 581 - Assignment 05 - Multiple Linear Regression + Interactions
# Instructor: Martha Essak
# Gurpal Bisra
# Student #: 69295061
# Due date: Oct. 11, 2016 at 11 p.m.
######################################################################

# (1) Interaction Model -- Volha ~ baha + topht + baha*topht
# (2) Log Model (natural log) -- log(volha)  log(baha)  log(topht)
#                   log.volhan ~ log.baha + log.topht
#                   (backtransform predicted yi_hat) --> SSE, SSy which used for pseudo-R^2

# Keep volume of cone in mind, V = (pi*h*r^2)/3

######################################################################

### Question 1 -------------------------------------------------------------------
### 1.	Graph the relationship between each of the potential explanatory variables and the response variable. Describe what you see on each scatterplot (form, direction, strength, outliers). For each variable, state if you would transform the explanatory variable, and if so, which transformation(s) you would try. (1.5 marks)

mydata <- read.csv("Tree_Data.csv", header=TRUE)
    # import data from csv, and declaring header row at top

str(mydata)     
    # 'data.frame':	26 obs. of  6 variables:
    # $ volha  : num  442 376 451 467 306 ...   
    # $ age    : int  34 35 33 33 32 60 60 62 63 82 ... 
    # $ baha   : num  36.2 33.4 35.4 42 27.4 27.3 34 42.5 40.4 32.8 ... 
    # $ stemsha: int  3552 4368 2808 6096 3816 528 2160 1843 1431 1071 ...  
    # $ topht  : num  17.4 15.6 16.8 16.4 16.7 22.7 19.4 20.5 21 22.4 ...    
    # $ dbh    : num  13.8 12.2 14.7 12.2 12.5 24.4 9.9 13.2 16.1 22.2 ...

# Volume of timber per hectare (response variable) in m3 / ha - volha

# Average height of trees (m) - topht
# Average diameter at breast height (cm) - dbh
# Stems per hectare (number of trees per hectare) - stemsha
# Basal area per hectare (obtained from dbh and stems per hectare) m2 / ha - baha
# Average age of trees - age

plot(volha ~ age, data = mydata, col = "blue", pch = 16, main = "Volume of Timber Per Hectare Against Tree Age", xlab="Average Tree Age", ylab="Volume Per Hectare [m^3 / ha]")
    # Determine outliers by plotting a line through data of locally weighted averaged lines where I set the delta value (i.e. want delta as low as possible)
lines(lowess(mydata$age, mydata$volha, delta=0.1), col="red")  

plot(volha ~ baha, data = mydata, col = "blue", pch = 16, main = "Volume of Timber Per Hectare Against Basal Area Per Hectare", xlab="Basal Area Per Hectare (m^2/ha", ylab="Volume Per Hectare [m^3 / ha]")
lines(lowess(mydata$baha, mydata$volha, delta=0.1), col="red")  

plot(volha ~ stemsha, data = mydata, col = "blue", pch = 16, main = "Volume of Timber Per Hectare Against Stems Per Hectare", xlab="Stems Per Hectare (Trees/ha", ylab="Volume Per Hectare [m^3 / ha]")
lines(lowess(mydata$stemsha, mydata$volha, delta=0.1), col="red")  

plot(volha ~ topht, data = mydata, col = "blue", pch = 16, main = "Volume of Timber Per Hectare Against Average Tree Height", xlab="Average Tree Height (m)", ylab="Volume Per Hectare [m^3 / ha]")
lines(lowess(mydata$topht, mydata$volha, delta=0.1), col="red")  

plot(volha ~ dbh, data = mydata, col = "blue", pch = 16, main = "Volume of Timber Per Hectare Against Average Diameter at Breast Height", xlab="Average Diameter at Breast Height (cm)", ylab="Volume Per Hectare [m^3 / ha]")
lines(lowess(mydata$dbh, mydata$volha, delta=0.1), col="red")  

# ================================================================================
# INTERACTION MODEL
# ================================================================================

# 2.	Create a linear model that includes baha, topht and the interaction between the two (Interaction Model). What does the interaction term do in the model? Is this an additive or multiplicative model? (1 mark) 

z1 <- lm(volha ~ baha + topht + baha*topht, data=mydata)
    # fit a multiple linear regression model with the original x-variable and the transformed x-variable
    # New dataset --> temperature, temperature.squared

# --------------------------------------------------------------------------------
# 3.	Using the residual plot, assess the assumptions of linearity and equal variance. Divide the residual plot into 3-4 segments to assess these assumptions. State any concerns you have and their consequences. (0.5 marks)

# Check if relationship is linear
    # Compute residuals of object z1 (linear fit of baha + topht + baha*topht)
resid1 <- resid(z1)
    # Calculate predicted y_hat values
predict1 <- predict(z1)

# Residual plot for model based on original units
plot(resid1 ~ predict1, pch=16, main = "Residuals of Predicted Model", xlab="Predicted Volume of Timber Per Hectare Values", ylab="Errors of Residuals of Volume of Timber Per Hectare")
# Add line Residuals = 0
abline(0, 0, lty=2)

# Assess assumptions of linearity and equal variance

# --------------------------------------------------------------------------------
# 4.	Check the normality assumption using a histogram, normality plot, and normality tests. Adjust the bin width for the histogram to be informative. State any concerns you have and their consequences. (0.5 marks)

# Look at the histogram to see whether the residuals are normally distributed
hist(resid1, breaks = seq(-40, 40, by=10), main = "Histogram of Predicted Model", xlab="Errors of Residuals of Volume of Timber Per Hectare", ylab="Frequency")

# plot(z1)        # get graphs that dont' make sense
qqnorm(resid1)
qqline(resid1, main = "Normal Q-Q plot of residuals", col = "red")

# Normality testing with alpha = 0.05
library(nortest)
shapiro.test(resid1)
ad.test(resid1)
cvm.test(resid1)
lillie.test(resid1)

# Plot yield vs. y_hat
mydata$predict1 <- predict(z1)
mydata$resid1 <- resid(z1)
plot(volha ~ predict1, data = mydata, pch = 16, col = "blue", main = "Volume of Timber Per Hectare Values Similar to Predicted Values", xlab="Predicted Volume of Timber Per Hectare", ylab="Volume of Timber Per Hectare")
# not Q-Q plot, but looking at how well the model matches the data
abline(0,1,lty=2)

# --------------------------------------------------------------------------------
# 6.	Test the significance of the regression. (0.5 marks)

# check if anything worth keeping in: y ~ x1 + x2 + x1*x2
    # If the regression is significant, use t tests to test the co-efficient for each variable
summary(z1)

# F distribution
    # Find the F critical value for numerator degrees of freedom = 1, denominator df = 8, alpha = 0.05
qf(0.95, 3, 22) 
    # Fcritical = 3.049125

# --------------------------------------------------------------------------------
# 7.	Test the significance of the interaction term using a partial F-test - show the calculation of the partial F statistic based on the sums of squares. What does this result mean for the model? (1 mark)

# The drop1() command does these partial F tests for you, dropping one variable at a time.
# I used the drop1() command to determine the partial F-tests, by dropping one variable at a time:
    # Full Model:       	volha ~ baha + topht + baha*topht
    # Reduced Model:    	volha ~ baha + topht (i.e. dropped interaction) 

drop1(z1, test="F") 

# F distribution
    # Find the F critical value for numerator degrees of freedom = 1, denominator df = 8, alpha = 0.05
qf(0.95, 1, 22) 
    # Fcritical = 4.30095

anova(z1)

# --------------------------------------------------------------------------------
# 8.	Calculate the co-efficient of multiple determination (R2) and the standard error of the estimate (root MSE). Check your results with the R output. (0.5 marks)

SSY <- sum((mydata$volha - mean(mydata$volha))^2)
SSY

summary(z1)

# --------------------------------------------------------------------------------
# 9.	Write the equation for the model including the co-efficients from the R output.    (0.5 marks)

# Obtain the coefficients with:
summary(z1)
    # Coefficients:
        # Estimate Std. Error t value Pr(>|t|)    
    # (Intercept) -36.1176   151.7813  -0.238 0.814116    
    # baha         -2.1824     4.3492  -0.502 0.620790    
    # topht         2.5438     7.0644   0.360 0.722212    
    # baha:topht    0.8278     0.1987   4.165 0.000403 ***
        # ---
        # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# --------------------------------------------------------------------------------
# 10.	Using this equation, calculate the predicted volume per hectare for Area A with baha = 30 and topht = 24. Calculate the predicted volume per hectare for Area B with baha = 37 and topht = 20. Use R to obtain the prediction intervals for these point estimates. Which area do you predict will produce higher volume?              (1.5 marks)

# Find the prediction intervals
xnew <- seq(min(mydata$baha), max(mydata$baha), length.out = 100) 
    # create a new vector with 100 different points ranging from your minimum x-value to     your maximum x-value
xnew <- as.data.frame(xnew) 
    # convert this to a dataframe so that you can add additional columns
    # because now we have multiple parameters, temperature and temperature.squared
names(xnew)[1] <- c("baha") 
    # rename the first column to match your original x-variable, I called it temperature     before in the code, so I will     give it the same name now. This makes it easy for R     to match up the new data with the variables that are in the        model
    # names must match!
xnew1 <- seq(min(mydata$topht), max(mydata$topht), length.out = 100) 
xnew1 <- as.data.frame(xnew1) 
names(xnew1)[1] <- c("topht") 

xnew = cbind(xnew, xnew1) 
xnew 
    # This is a good point to view your new data to check that everything is working         properly

ynew.ci <- data.frame(predict(z1, newdata = data.frame(xnew), interval = "prediction", level = 0.95)) 
    # R will calculate the predicted values and  prediction intervals for all the            different new x values that you have provided
ynew.ci
new.values <- cbind(xnew,ynew.ci) 
    # combine the dataframe  with the new x-values and the dataframe with the predicted y    -values and prediction            intervals
new.values 
    # Check that all your columns are what you would expect them to be

### to find max in data frame
# xnew[which.max(xnew$temperature), ]
# ynew.ci[which.max(ynew.ci$fit), ]
    # then it's corresponds to row 50 in xnew is 723.... 

predict(z1, data.frame(baha = 30, topht=24, fit=555.4776),interval = "prediction", level = 0.95)

predict(z1, data.frame(baha = 37, topht=20, fit=546.5816),interval = "prediction", level = 0.95)

# ================================================================================
# LOG MODEL
# ================================================================================

# 11.	Create a linear model that includes log baha and log topht, and uses log volha as the response variable (Log model). Why does this model make sense based on the type of data? (1 mark)

mydata$baha.log <- log(mydata$baha)
mydata$topht.log <- log(mydata$topht)
mydata$volha.log <- log(mydata$volha)

str(mydata)

plot(volha.log ~ baha.log, data = mydata, col = "blue", pch = 16, main = "Log of Volume of Timber Per Hectare Against Log of Basal Area Per Hectare", xlab="Log of Basal Area Per Hectare (transformed m^2/ha)", ylab="Log of Volume Per Hectare [transformed m^3 / ha]")
lines(lowess(mydata$baha.log, mydata$volha.log, delta=0.1), col="red")  

plot(volha.log ~ topht.log, data = mydata, col = "blue", pch = 16, main = "Log of Volume of Timber Per Hectare Against Log of Average Tree Height", xlab="Log of Average Tree Height (transformed m)", ylab="Log of Volume Per Hectare [transformed m^3 / ha]")
lines(lowess(mydata$topht.log, mydata$volha.log, delta=0.1), col="red")  

z2 <- lm(volha.log ~ baha.log + topht.log, data = mydata) 
str(mydata)

# --------------------------------------------------------------------------------
# 12.	Assess the assumptions of linearity, equal variance and normality of errors. (Your conclusions regarding the assumption of independence should be the same for this model because both models are based on the same data). (1 mark)

resid2 <- resid(z2)
predict2 <- predict(z2)

# residual plot for model based on original units
plot(resid2 ~ predict2, pch=16, main = "Residuals of Log-transformed Predicted Model", xlab="Predicted Log-transformed Volume of Timber Per Hectare Values", ylab="Errors of Residuals of Log-transformed Volume of Timber Per Hectare")
# Add line Residuals = 0
abline(0, 0, lty=2)

# Look at the histogram to see whether the residuals are normally distributed
hist(resid2)
hist(resid2, breaks = seq(-0.2, 0.2, by=0.02), main = "Histogram of Log-transformed Predicted Model", xlab="Errors of Residuals of Log-transformed Volume of Timber Per Hectare", ylab="Frequency")

# plot(z1)        # get graphs that dont' make sense
qqnorm(resid2)
qqline(resid2, main = "Normal Q-Q plot of residuals", col = "red")

# Normality testing with alpha = 0.05
library(nortest)
shapiro.test(resid2)
ad.test(resid2)
cvm.test(resid2)
lillie.test(resid2)

# Plot yield vs. y_hat
mydata$predict2 <- predict(z2)
mydata$resid2 <- resid(z2)
plot(volha.log ~ predict2, data = mydata, pch = 16, col = "blue", main = "Log-transformed Volume of Timber Per Hectare Values Similar to Predicted Values", xlab="Predicted Volume of Timber Per Hectare", ylab="Log-transformed Volume of Timber Per Hectare")
# not Q-Q plot, but looking at how well the model matches the data
abline(0,1,lty=2)

# --------------------------------------------------------------------------------
# 13.	Test the significance of the regression. (0.5 marks)

summary(z2)

qf(0.95, 2, 23) 
    # Fcritical = 3.422132

# --------------------------------------------------------------------------------
# 14.	Test each of the explanatory variables using a partial F-test. Show the calculations for the partial F statistic based on the sums of squares. (2 marks)

# z2 <- lm(volha.log ~ baha.log + topht.log, data = mydata) 

# An alternative to t tests is the partial F test
z.baha.log <- lm(volha.log ~ baha.log, data=mydata) 
    # topht.log is removed so that's the variable I am checking
anova(z2, z.baha.log) 
    # Analysis of Variance Table

    # Model 1: volha.log ~ baha.log + topht.log
    # Model 2: volha.log ~ baha.log
    # Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
    # 1     23 0.03435                                  
    # 2     24 0.92241 -1  -0.88806 594.71 < 2.2e-16 ***
        # ---
        # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Find the F critical value, which uses n-m-1 from the full model. F_r_n-m-1_1-a = F(1)(26-2-1)(0.95).
qf(0.95, 1, 23) 
# Fcritical = 4.30095

anova(z2)

# --------------------------------------------------------------------------------
# An alternative to t tests is the partial F test
z.topht.log <- lm(volha.log ~ topht.log, data=mydata) 
    # baha.log is removed so that's the variable I am checking
anova(z2, z.topht.log) 
# Analysis of Variance Table

# Model 1: volha.log ~ baha.log + topht.log
# Model 2: volha.log ~ baha.log
# Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
# 1     23 0.03435                                  
# 2     24 0.92241 -1  -0.88806 594.71 < 2.2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Find the F critical value, which uses n-m-1 from the full model. F_r_n-m-1_1-a = F(1)(26-2-1)(0.95).
qf(0.95, 1, 23) 
# Fcritical = 4.30095

# --------------------------------------------------------------------------------
# 15.	Using R, convert the predicted values into their original units. Calculate the SSY, SSreg and SSE in original units. Calculate the pseudo-R2 and the standard error of the estimate (root MSE) based on the SSE in original units. (1 mark)

# backtransform the predicted values into the original units
yhat.original <- exp(predict2) 
yhat.original

yhat.original <- as.data.frame(yhat.original) 
    #make this into a data frame so that you will be able to add it to your        original data frame

mydata.2 <- cbind(mydata, yhat.original) # since these data frames have the same number of rows, they should line up nicely

SSE <- sum((mydata.2$volha - mydata.2$yhat.original)^2)
SSE
SSR <- sum((mean(mydata.2$volha) - mydata.2$yhat.original)^2)
SSR
SSY <- sum((mydata.2$volha - mean(mydata.2$volha))^2)
SSY

# --------------------------------------------------------------------------------
# 16.	In log units, calculate the confidence intervals for the slopes. What would these co-efficients be (in their correct units) if tree volume could be modeled perfectly with the equation for volume of a cone? Do the confidence intervals include these values? (1.5 marks)

summary(z2)

confint(z2, level=0.95) # this is based on the log model, so everything will be in log units

qt(0.975, 24, lower.tail=TRUE )

# --------------------------------------------------------------------------------
# 17.	Write the equation for the model including the co-efficients from the R output.    (0.5 marks)

summary(z2)
# log ((y_i ) ^) = -0.81561 + 0.93369*log(x_1i) + 1.24995*log(x_2i)

# --------------------------------------------------------------------------------
# 18.	Using this equation, calculate the predicted volume per hectare for Area A with baha = 30 and topht = 24. Calculate the predicted volume per hectare for Area B with baha = 37 and topht = 20. Use R to obtain the prediction intervals for these point estimates. Remember to backtransform these values into the original units. Which area do you predict will produce higher volume? (1.5 marks)

# Find the prediction intervals
xnew <- seq(min(mydata.2$baha.log), max(mydata.2$baha.log), length.out = 100) 
# create a new vector with 100 different points ranging from your minimum x-value to     your maximum x-value
xnew <- as.data.frame(xnew) 
# convert this to a dataframe so that you can add additional columns
# because now we have multiple parameters, temperature and temperature.squared
names(xnew)[1] <- c("baha.log") 
# names must match!
xnew1 <- seq(min(mydata.2$topht.log), max(mydata.2$topht.log), length.out = 100) 
xnew1 <- as.data.frame(xnew1) 
names(xnew1)[1] <- c("topht.log") 

xnew = cbind(xnew, xnew1) 
xnew 
# This is a good point to view your new data to check that everything is working         properly

ynew.ci <- data.frame(predict(z2, newdata = data.frame(xnew), interval = "prediction", level = 0.95)) 
# R will calculate the predicted values and  prediction intervals for all the            different new x values that you have provided
ynew.ci
new.values <- cbind(xnew,ynew.ci) 
# combine the dataframe  with the new x-values and the dataframe with the predicted y    -values and prediction            intervals
new.values 
# Check that all your columns are what you would expect them to be

### to find max in data frame
# xnew[which.max(xnew$temperature), ]
# ynew.ci[which.max(ynew.ci$fit), ]
# then it's corresponds to row 50 in xnew is 723.... 

predict(z2, data.frame(baha.log = log(30), topht.log= log(24), fit=6.3324623),interval = "prediction", level = 0.95)

predict(z2, data.frame(baha.log = log(37), topht.log= log(20), fit=6.30038350119),interval = "prediction", level = 0.95)






# ================================================================================
# COMPARING THE MODEL
# ================================================================================

# 19.	Compare the Interaction Model and the Log Model in terms of how well they met assumptions. Does one meet the assumptions better than the other? (0.5 marks)

# --------------------------------------------------------------------------------
# 20.	Compare the co-efficient of determination and standard error of the estimate for the Interaction Model and the Log Model. Which model has a better fit? (0.5 marks)

# --------------------------------------------------------------------------------
# 21.	Discuss how your predictions for Area A and B differed or were similar between the Interaction Model and the Log Model. (0.5 marks)

# --------------------------------------------------------------------------------
# 22.	Which model would you recommend the timber harvest company use for making predictions? Why? (1.5 marks)

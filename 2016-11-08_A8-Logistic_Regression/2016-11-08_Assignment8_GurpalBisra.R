##################################
# ASSIGNMENT 8 - LOGISTIC REGRESSION
# Instructor: Martha Essak
# Gurpal Bisra
# Student Number: 69295061 
# Nov. 8, 2016
##################################

# Who more likey to purchase subscription for company
# 1 = Yes, 0 = No to questions
# Income = $/year
# females are more caretakers
# married = more likely to have children, but not say more likely to buy kid's magazine (bad variable to use)
# College = yes because would affect culturally
# professional = may not be meaningful
# children = maybe
# unemployed repeating income, so decide which variable better for us to use*
# dual income = similar to married, but better than married
# previously purchased children's magazine = yes affects
# previously purchase parenting magazine = yes affects

#####################
# CREATE THE DATASET
#####################

# read in the csv file
mydata <- read.csv("KidCreative dataset.csv", header=TRUE)

str(mydata)

# 'data.frame':	673 obs. of  12 variables:
# $ buy            : int  0 1 0 1 0 0 0 0 0 0 ...
# $ income         : int  24000 75000 46000 70000 43000 24000 26000 38000 39000 49000 ...
# $ Female      : int  1 1 1 0 1 1 1 1 1 0 ...
# $ Married     : int  0 1 1 1 0 1 1 1 0 1 ...
# $ College    : int  1 1 0 0 0 0 1 0 1 0 ...
# $ Professional: int  1 1 0 1 0 0 0 0 1 0 ...
# $ Retired     : int  0 0 0 0 0 0 1 1 0 1 ...
# $ Unemployed     : int  0 0 0 0 0 0 0 0 0 0 ...
#$ Dual.Income    : int  0 1 1 0 0 0 0 0 0 0 ...
# $ Children       : int  0 0 1 0 0 0 1 0 0 0 ...
# $ Prev.Child.Mag : int  0 1 0 1 0 0 0 0 0 0 ...
# $ Prev.Parent.Mag: int  0 0 0 0 1 0 0 0 0 0 ...

# IF CHANGE 0 AND 1 TO NAMES
# mydata$Is.Female <- factor(mydata$Is.Female, labels = c("male", "female")) 
    # write the value for 0 first, then the value for 1

### 1.	What relationship do you expect between each potential explanatory variable and the response variable? Why? (You can say "no relationship", however, you must explain why.) (2 marks)

### 2.	Which 3-5 potential explanatory variables do you think are most likely to be good predictors of whether or not someone will buy a children's magazine? (Which potential explanatory variables do you think will have the strongest relationship with the response variable?)  (0.5 mark)

#################
# PLOT THE DATA
#################

plot(buy ~ income, data=mydata, pch=16, col = "blue", main = "Buy Vs. Income", xlab = "Income ($/year)", ylab = "Buy")

# What pattern do you see?
# looks like class notes, sigmoidal curve

# Lowess curve
lines(lowess(mydata$income, mydata$buy, delta=0.1), col="red")  
plot(jitter(buy, f = 0.5)~ income, data=mydata, pch=16, col = c("red","blue")[as.factor(mydata$Married)])

# What shape do you expect the model to take?
# sigmoidal, but get flat curve


### 3.	Fit a null model with the intercept only. What is the estimate of the intercept for this model? Convert this estimate from logit (log odds) units to probability units?             
#####################################
# NULL MODEL / INTERCEPT ONLY MODEL
#####################################

z.null <- glm(buy ~ 1, data=mydata, family="binomial"(link="logit"))
summary(z.null)
  # can also do partial F-test which is equivalent here to a global F-test

# The intercept is -1.47796 in log odds (the estimate of the mean on the logit scale).
# What is the estimate of the population proportion?
# You need to use the inverse equation to obtain this

exp(-1.47796)/(1 + exp(-1.47796))
  # = 0.5, so means half the sample did purchase and other half didn't purchase
  # only pick 1 number as cutoff point

# This might be a natural cut-off point, where any predicted values below the population mean would get a predicted value of 0, and any above would get a predicted value of 1

### 4.	Using the original data, calculate the probability of a purchase. What do you notice about this value and the intercept from the null model? (0.25 marks)

mean(mydata$buy)
# Note that your model has the same mean for predicted values

### 5.	Use R to obtain the log likelihood for the null model.
logLik(z.null)

### 6.	Fit a model with income only. Use R to obtain the log likelihood for this model.

###############################
# INTERPRETING CO-EFFICIENTS
###############################
# Fit the Continuous-only model
# glm = generalized linear model
# link says values can only take on 0 or 1
z1 <- glm(buy ~ income, data=mydata, family="binomial"(link="logit"))

summary(z1)

# Coefficients:
#     Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -9.344e+00  8.315e-01  -11.24   <2e-16 ***
#    income       1.494e-04  1.336e-05   11.18   <2e-16 ***

logLik(z1) 

### 7.	Write the full calculation for the likelihood ratio test statistic (based on log likelihood values from R) for the model including income. Test the significance of the model (include all four steps of your hypothesis test). (1 mark)

#################################
# LIKELIHOOD RATIO TEST
#################################

# You can compare the null model and your model using a likelihood ratio test
anova(z.null, z1, test="Chi")

# Analysis of Deviance Table
#
# Model 1: buy ~ 1
# Model 2: buy ~ income
# Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
# 1       672     646.05                          
# 2       671     249.32  1   396.73 < 2.2e-16 ***
#    ---
#    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Calculate G value
-2*(logLik(z.null) - logLik(z1))
  # log Lik.' 396.7289 (df=1)

# Look on chi-squared distribution table for critical value
# critica value = 3.84
  # rejecting null hypothesis, so go with more complex model (have age in model)

# 8.	Show the full calculation (based on log likelihood values from R) of the AIC value for the model including income. (0.25 marks)
# AIC = -2ln(L) + 2*(k+s)
    # k = levels of y -1
    # k = 2 - 1 = 1 for binary y(0,1)
    # s = number of predictor variables, including any indicator variables = 1

# Calculate AIC
-2*(logLik(z1))+2*(1+1)

# Double check with codes
library(AICcmodavg)
# AIC, smaller (more negative) is better
AIC(z1)     # takes into account number of variables (penalized if more)
AICc(z1)    # takes into account number of variables and sample size

### 9.	Fit a model with married only. Use R to obtain the log likelihood for this model.

###############################
# Model with Married Only
###############################
# Fit the categorical-only model
# glm = generalized linear model
# link says values can only take on 0 or 1
mydata$Married <- factor(mydata$Married)

z2 <- glm(buy ~ Married, data=mydata, family="binomial"(link="logit"))

summary(z2)
# Coefficients:
#    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -2.3830     0.1718 -13.870   <2e-16 ***
#    Married1      1.8699     0.2184   8.563   <2e-16 ***

logLik(z2) 

### 10.	Write the full calculation for the likelihood ratio test statistic (based on log likelihood values from R) for the model including married. Test the significance of the model (include all four steps of your hypothesis test). (1 mark)

# You can compare the null model and your model using a likelihood ratio test
anova(z.null, z2, test="Chi")

# Analysis of Deviance Table
#
# Model 1: buy ~ 1
# Model 2: buy ~ income
# Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
# 1       672     646.05                          
# 2       671     249.32  1   396.73 < 2.2e-16 ***
#    ---
#    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Calculate G value
-2*(logLik(z.null) - logLik(z2))
# log Lik.' 81.58756 (df=1)

# Look on chi-squared distribution table for critical value
# critica value = 3.84
# rejecting null hypothesis, so go with more complex model (have age in model)

# 8.	Show the full calculation (based on log likelihood values from R) of the AIC value for the model including income. (0.25 marks)
# AIC = -2ln(L) + 2*(k+s)
# k = levels of y -1
# k = 2 - 1 = 1 for binary y(0,1)
# s = number of predictor variables, including any indicator variables = 1

### 11.	Show the full calculation (based on log likelihood values from R) of the AIC value for the model including married. (0.25 marks)

# Calculate AIC
-2*(logLik(z2))+2*(1+1)

# Double check with codes
library(AICcmodavg)
# AIC, smaller (more negative) is better
AIC(z2)     # takes into account number of variables (penalized if more)
AICc(z2)    # takes into account number of variables and sample size

################################################################################

### 12.	Fit 11 models, each with one of the following explanatory variables: income, gender, married, education, professional job, retired, unemployed, dual income, children, bought children's magazine previously, and bought parenting magazine previously. Record information about each model in the following table. Organize your models from lowest AIC value to highest AIC value. Example table below. (4 marks)

# already did income
# already did Married
mydata$Female <- factor(mydata$Female)              # Female
mydata$College <- factor(mydata$College)            # College
mydata$Professional <- factor(mydata$Professional)  # Professional
mydata$Retired <- factor(mydata$Retired)            # Retired
mydata$Unemployed <- factor(mydata$Unemployed)      # Unemployed
mydata$Dual.Income <- factor(mydata$Dual.Income)    # Dual.Income
mydata$Children <- factor(mydata$Children)          # Children
mydata$Prev.Child.Mag <- factor(mydata$Prev.Child.Mag)      # Prev.Child.Mag
mydata$Prev.Parent.Mag <- factor(mydata$Prev.Parent.Mag)    # Prev.Parent.Mag

str(mydata)

# 'data.frame':	673 obs. of  12 variables:
#    $ buy            : int  0 1 0 1 0 0 0 0 0 0 ...
# $ income         : int  24000 75000 46000 70000 43000 24000 26000 38000 39000 49000 ...
# $ Female         : Factor w/ 2 levels "0","1": 2 2 2 1 2 2 2 2 2 1 ...
# $ Married        : Factor w/ 2 levels "0","1": 1 2 2 2 1 2 2 2 1 2 ...
# $ College        : Factor w/ 2 levels "0","1": 2 2 1 1 1 1 2 1 2 1 ...
# $ Professional   : Factor w/ 2 levels "0","1": 2 2 1 2 1 1 1 1 2 1 ...
# $ Retired        : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 2 2 1 2 ...
# $ Unemployed     : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
# $ Dual.Income    : Factor w/ 2 levels "0","1": 1 2 2 1 1 1 1 1 1 1 ...
# $ Children       : Factor w/ 2 levels "0","1": 1 1 2 1 1 1 2 1 1 1 ...
# $ Prev.Child.Mag : Factor w/ 2 levels "0","1": 1 2 1 2 1 1 1 1 1 1 ...
# $ Prev.Parent.Mag: Factor w/ 2 levels "0","1": 1 1 1 1 2 1 1 1 1 1 ...

# $ Female         : Factor w/ 2 levels "0","1": 2 2 2 1 2 2 2 2 2 1 ...
z3 <- glm(buy ~ Female, data=mydata, family="binomial"(link="logit"))
AIC(z3)                             # AIC value
-2*(logLik(z.null) - logLik(z3))    # G value
anova(z.null, z3, test="Chi")       # p value

# $ College        : Factor w/ 2 levels "0","1": 2 2 1 1 1 1 2 1 2 1 ...
z4 <- glm(buy ~ College, data=mydata, family="binomial"(link="logit"))
AIC(z4)                             # AIC value
-2*(logLik(z.null) - logLik(z4))    # G value
anova(z.null, z4, test="Chi")       # p value

# $ Professional   : Factor w/ 2 levels "0","1": 2 2 1 2 1 1 1 1 2 1 ...
z5 <- glm(buy ~ Professional, data=mydata, family="binomial"(link="logit"))
AIC(z5)                             # AIC value
-2*(logLik(z.null) - logLik(z5))    # G value
anova(z.null, z5, test="Chi")       # p value

# $ Retired        : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 2 2 1 2 ...
z6 <- glm(buy ~ Retired, data=mydata, family="binomial"(link="logit"))
AIC(z6)                             # AIC value
-2*(logLik(z.null) - logLik(z6))    # G value
anova(z.null, z6, test="Chi")       # p value

# $ Unemployed     : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
z7 <- glm(buy ~ Unemployed, data=mydata, family="binomial"(link="logit"))
AIC(z7)                             # AIC value
-2*(logLik(z.null) - logLik(z7))    # G value
anova(z.null, z7, test="Chi")       # p value

# $ Dual.Income    : Factor w/ 2 levels "0","1": 1 2 2 1 1 1 1 1 1 1 ...
z8 <- glm(buy ~ Dual.Income, data=mydata, family="binomial"(link="logit"))
AIC(z8)                             # AIC value
-2*(logLik(z.null) - logLik(z8))    # G value
anova(z.null, z8, test="Chi")       # p value

# $ Children       : Factor w/ 2 levels "0","1": 1 1 2 1 1 1 2 1 1 1 ...
z9 <- glm(buy ~ Children, data=mydata, family="binomial"(link="logit"))
AIC(z9)                             # AIC value
-2*(logLik(z.null) - logLik(z9))    # G value
anova(z.null, z9, test="Chi")       # p value

# $ Prev.Child.Mag : Factor w/ 2 levels "0","1": 1 2 1 2 1 1 1 1 1 1 ...
z10 <- glm(buy ~ Prev.Child.Mag, data=mydata, family="binomial"(link="logit"))
AIC(z10)                             # AIC value
-2*(logLik(z.null) - logLik(z10))    # G value
anova(z.null, z10, test="Chi")       # p value

# $ Prev.Parent.Mag: Factor w/ 2 levels "0","1": 1 1 1 1 2 1 1 1 1 1 ...
z11 <- glm(buy ~ Prev.Parent.Mag, data=mydata, family="binomial"(link="logit"))
AIC(z11)                             # AIC value
-2*(logLik(z.null) - logLik(z11))    # G value
anova(z.null, z11, test="Chi")       # p value

################################################################################

### 15.	Using R, create a graph of purchase vs. income with a method to visualize married. What are some overall patterns that you see? How can these help you answer your research question? (1 mark)

plot(jitter(buy, f = 0.5)~ income, data=mydata, pch=16, col = c("red","blue")[as.factor(mydata$Married)], main = "Buy Vs. Income", xlab = "Income ($/year)", ylab = "Buy") 

legend(1, 1, legend=c("Not Married", "Married"),
       col=c("red", "blue"), pch=16:16, cex=0.8)

################################################################################

### 16.	Create a model with the following explanatory variables: income, married, the interaction between income and married. (buy ~ income*married)
z12 <- glm(buy ~ income + Married + income*Married, data=mydata, family="binomial"(link="logit"))
AIC(z12)                             # AIC value
-2*(logLik(z.null) - logLik(z12))    # G value
anova(z.null, z12, test="Chi")       # p value

### 17.	Does this model meet the assumptions of generalized linear models? (1 mark)

# .	Statistical independence of observations
# .	Correct specification of link function
# .	Variance correspond to what is expected from the link function

### 18.	What does the interaction between income and married allow in this model allow?     (0.25 marks)


### 19.	Test the likelihood of the whole model compared to the likelihood of the null model using a likelihood ratio test. Show your calculation of the test statistic using the log-likelihoods from R. Confirm your results using R. (1 mark)

-2*(logLik(z.null) - logLik(z12))    # G value
anova(z.null, z12, test="Chi")       # p value

### 20.	Test each variable using a likelihood ratio test. This will require you to fit models that eliminate one variable. Show your calculation of the test statistics using the log-likelihoods from R. Confirm your results using R. (1 mark)

#There are a total of 4 models which could be used in such analyses which I will denote as:
    # (1) z1: Buy ~ income		
    # (2) z2: Buy ~ Married
    # (3) z13  Buy ~ income + Married
    # (4) z12: Buy ~ income + Married + income*Married

# Testing the Significance of Married [i.e. Compare (1) and (4)]
anova(z12, z1, test="Chi")       # p value
-2*(logLik(z1) - logLik(z12))    # G value

# Testing the Significance of income [i.e. Compare (2) and (4)]
anova(z12, z2, test="Chi")       # p value
-2*(logLik(z2) - logLik(z12))    # G value

### 21.	If both variables should remain in the model, test the interaction term only using a likelihood ratio test (you will have to fit a reduced Model A that excludes the interaction). Show your calculation of the test statistics using the log-likelihoods from R. Confirm your results using R. (1 mark)

# Model 3
z13 <- glm(buy ~ income + Married, data=mydata, family="binomial"(link="logit"))

# Testing the Significance of income [i.e. Compare (3) and (4)]
anova(z12, z3, test="Chi")       # p value
-2*(logLik(z3) - logLik(z12))    # G value

#################################
# MODEL B
#################################

### Model B: Model with income, married and professional job:
### 22.	Create a model with the following explanatory variables: income, married, the interaction between income and married, job, and the interaction between job and income. (buy ~ income*married + income*job)

z14 <- glm(buy ~ income + Married + Professional + income*Married + income*Professional, data=mydata, family="binomial"(link="logit"))

# Testing the Significance of Unemployed [i.e. Compare (3) and (4)]
anova(z14, z12, test="Chi")       # p value
-2*(logLik(z12) - logLik(z14))    # G value

#################################
# MODELS C THROUGH G
#################################

#--------------------------------------------------------------------------------------
# Testing the Significance of the variable Dual Income [i.e. Compare (Model A) and (Model C)]

# Model C
z15 <- glm(buy ~ income + Married + Dual.Income + income*Married + income*Dual.Income, data=mydata, family="binomial"(link="logit"))
      
anova(z15, z12, test="Chi")       # p value
-2*(logLik(z12) - logLik(z15))    # G value

#--------------------------------------------------------------------------------------
# Testing the Significance of the variable Unemployed [i.e. Compare (Model A) and (Model D)]

# Model D 
z16 <- glm(buy ~ income + Married + Unemployed + income*Married + income*Unemployed, data=mydata, family="binomial"(link="logit"))

anova(z16, z12, test="Chi")       # p value
-2*(logLik(z12) - logLik(z16))    # G value

#-------------------------------------------------------------------------------------- # Testing the Significance of the variable College [i.e. Compare (Model A) and (Model E)]
                                        
# Model E
z17 <- glm(buy ~ income + Married + College + income*Married + income*College, data=mydata, family="binomial"(link="logit"))

anova(z17, z12, test="Chi")       # p value
-2*(logLik(z12) - logLik(z17))    # G value

#-------------------------------------------------------------------------------------- # Testing the Significance of the variable Prev.Child.Mag [i.e. Compare (Model A) and (Model F)]
                                                            
# Model F 
z18 <- glm(buy ~ income + Married + Prev.Child.Mag + income*Married + income*Prev.Child.Mag, data=mydata, family="binomial"(link="logit"))

anova(z18, z12, test="Chi")       # p value
-2*(logLik(z12) - logLik(z18))    # G value

#--------------------------------------------------------------------------------------
# Testing the Significance of the variable Female [i.e. Compare (Model A) and (Model G)]
                                                                                
# Model G 
z19 <- glm(buy ~ income + Married + Female + income*Married + income*Female, data=mydata, family="binomial"(link="logit"))

anova(z19, z12, test="Chi")       # p value
-2*(logLik(z12) - logLik(z19))    # G value                                                     

# Model H
z20 <- glm(buy ~ income + Married + Female + income*Married, data=mydata, family="binomial"(link="logit"))

anova(z19, z20, test="Chi")       # p value
-2*(logLik(z20) - logLik(z19))    # G value

#################################
# FINAL MODEL
#################################

### 25.	For your final model, calculate the Pseudo-R2 and the scaled Pseudo-R2. (1 mark)

# Pseudo R2
1 - (logLik(z.null)/logLik(z12))^(2/nrow(mydata))

logLik(z.null)
# 'log Lik.' -323.0265 (df=1)

logLik(z12)
# 'log Lik.' -112.8367 (df=4)

nrow(mydata)
# 673

#Scaled R2
(1 - (logLik(z.null)/logLik(z12))^(2/nrow)/(1-(-323.0265)^(2/nrow(mydata)))

(1 - ((-323.0265))/(-112.8367)^(2/(673)))/(1-(-323.0265)^(0.002971768))
# > 2/673
# [1] 0.002971768
# 0.1807


(1 - (logLik(z.null)/logLik(z12))^(2/nrow(mydata)))/(1 - (-323.0265^(2/nrow(mydata))))

### 26.	Calculate the AIC value for your final model. Compare the AIC value of your final model to the AIC values for the models that had only one of the variables that are included in your final model (Single variable models). Put all of these models and AIC values in a table to make them easy to compare. How much does the AIC value improve from the single variable models of income, married and professional job to your final model? (2 marks)

library(AICcmodavg)
# AIC, smaller (more negative) is better
AIC(z12)     # takes into account number of variables (penalized if more)
# [1] 233.6734
AICc(z12)    # takes into account number of variables and sample size
# [1] 233.7333

#################################
# CLASSIFICATION TABLE
#################################

### 27.	Create a classification table for this data based on the final model. Include the full classification table in your output. Remember that you can output dataframes to a .csv file using write.csv().

# This will try out many different cut-off points to give you an idea of how to maximize or minimize different values.
# For example, you might want to maximize percentage of correct predictions.
# For example, you might want to minimize false negatives.

# Create an empty dataframe that you will fill with 
df <- data.frame(matrix(ncol = 9, nrow = 51))
colnames(df) <- c("correct.event", "correct.non.event", "incorrect.event", "incorrect.non.event", "correct.percent", "sensitivity", "specificity", "false.pos", "false.neg")
df

prob.level <- seq(0, 1, length.out=51) # create a vector with different possible probabilities
prob.level
class.table.data <- cbind(prob.level, df) # combine your vector of probabilities and your empty dataframe
class.table.data # Your dataframe has one row for each probability cut-off

# fill empty cells in your dataframe with 0
class.table.data$correct.non.event <- rep(c(0), c(51))
class.table.data$correct.event <- rep(c(0), c(51))
class.table.data$incorrect.non.event <- rep(c(0), c(51))
class.table.data$incorrect.event <- rep(c(0), c(51))
class.table.data

# This loop will try out the different probability cut-off values and fill in how many correct and incorrect events and non-events you have based on your data.
for (i in 1:51) {
    class.table <- table(mydata$buy, fitted(z12) > class.table.data$prob.level[i])
    
    col.true.num <- grep("TRUE", colnames(class.table))
    col.false.num <- grep("FALSE", colnames(class.table))
    
    if (length(col.true.num) > 0) {
        class.table.data$incorrect.non.event [i] <- class.table[1, col.true.num]
        class.table.data$correct.event [i] <- class.table[2, col.true.num] }
    
    if (length(col.false.num) > 0) {
        class.table.data$correct.non.event [i] <- class.table[1, col.false.num]
        class.table.data$incorrect.event [i] <- class.table[2, col.false.num] }  }

class.table.data

# You will use this information to fill in the rest of your classification table.
class.table.data$correct.percent <- (class.table.data$correct.event + class.table.data$correct.non.event)/nrow(mydata)
class.table.data$sensitivity <- (class.table.data$correct.event)/nrow(mydata)
class.table.data$specificity <- (class.table.data$correct.non.event)/nrow(mydata)
class.table.data$false.neg <- (class.table.data$incorrect.non.event)/nrow(mydata)
class.table.data$false.pos <- (class.table.data$incorrect.event)/nrow(mydata)
class.table.data

write.csv(class.table.data, file = "ClassTable.csv")

### 28.	For sensitivity, specificity, false negatives, false positives, which do you want to maximize or minimize, and which do you not care about? Why?     (2 marks)

### 29.	You decide to maximize the percentage of correct of predictions. Why? What probability cut-off should you use to maximize the percent of correct predictions?       (0.5 marks)
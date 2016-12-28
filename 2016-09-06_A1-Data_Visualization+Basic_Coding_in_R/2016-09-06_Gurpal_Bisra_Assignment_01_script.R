######################################
# COMM 581 - Assignment 01
# Instructor: Martha Essak
# Due date: Sept 12, 2016 at 11 p.m.
######################################


# We will start by importing our dataset. First, open the dataset in Excel and check that it is formatted properly for bringing into R.
# 1. Blanks should be replaced with NA
# 2. Variables names should be descriptive, with no spaces, and start with a letter (not a number or a symbol). Remember that these will be case-sensitive!
# 3. Data should be saved as a .csv file
# 4. Numbers should not have any commas in them (why do you think this could cause problems?)

# mydata <- read.csv(file.choose(), header=TRUE)
mydata <- read.csv("Real_Estate_Sales_Data.csv", header=TRUE)
# The command that we are using here is "read.csv".
# We are stating that there is a header, which includes all the variable names.
# We are saving this data to a data frame called "mydata". If we want to access anything from this data, we will call on the data frame "mydata". Generally, matrices contain data of all one type, while data frames can contain data of different types.


# View the whole dataset using the print command
print(mydata)

nrow(mydata) # number of rows for the data frame "mydata", this does not include the header row.
ncol(mydata) # number of columns.

names(mydata) # This gives you all the variable names. This is a useful reminder because we are writing code, so you must have the correctly spelled variable name (also case-sensitive).


str(mydata) # This command shows the structure of the variables in the data frame. If there are any characters/letters in the data, R will assume this is a factor (categorical variable). If there are only numbers, R will classify it as integer (if there are no decimals) and numeric if there are some decimals. Both integer and numeric are treated as continuous/quantitative variables.



summary(mydata) # This command works differently depending on what type of object you use it with. We will be using it with different object types later, for now we are using it with a data frame.
# What type of information does this show you?



# To call on a specific variable, for example, sale.price, you will use mydata$sale.price. You can use everything you learned about vectors with this column from the dataframe.

# If there are some variables that you want to change to categorical variables (factors), use the following:
mydata$bedrooms <- as.factor(mydata$bedrooms)

table(mydata$bedrooms)

# If you changed a variable to a factor by mistake, this is how you change it back. If you don't want the following line of code to run, you can put a # in front of it to turn the line into a comment
mydata$bedrooms <- as.numeric(as.character(mydata$bedrooms)) #first, you must change it to a character, then you can change it to a number.

# For now, change bedrooms to a factor

# Write your own code for variables you would like to change:
mydata$bathrooms <- as.factor(mydata$bathrooms)
mydata$bathrooms <- as.character(mydata$bathrooms)
mydata$bathrooms <- as.numeric(as.character(mydata))


# Check the structure of the data again, after you have changed some variables to factors.
str(mydata)

table(mydata$bedrooms) # This shows you the frequency distribution table for number of bedrooms.



# Let's start by looking at some of the continuous variables: sale.price and square footage (square.feet)
# Find the mean, median, standard deviation, variance, range for each variable. These have been written for sale price.

mean(mydata$sale.price)
median(mydata$sale.price)

# Which is higher, the mean or the median? What does this mean about the distribution?


range(mydata$sale.price)

# You may not be interested in the range, but rather in just the minimum, or just the maximum, particularly if you want to use this value as part of a formula
min(mydata$sale.price)
max(mydata$sale.price)
mean(mydata$sale.price)
median(mydata$sale.price)
range(mydata$sale.price)
sd(mydata$sale.price)
var(mydata$sale.price)

# use summary(mydata$sale.price)
summary(mydata$sale.price)

# Write your own code to find these values for square footage:
min(mydata$square.feet)
max(mydata$square.feet)
mean(mydata$square.feet)
median(mydata$square.feet)
range(mydata$square.feet)
sd(mydata$square.feet)
var(mydata$square.feet)



###############
# HISTORGRAMS
###############


# To make a histogram for price, use the following command:
hist(mydata$sale.price)

# What are the minimum and maximum values for the x axis of this histogram?

hist(mydata$sale.price, xlim = c(0,1000000), ylim = c(0, 200), main = "House price histogram", xlab = "Sale Price") # to change the minimum value on the x-axis to 0, make the y-axis go up to 200. Add a title and a label to the x-axis


hist(mydata$sale.price, xlim = c(0,1000000), ylim = c(0, 200), main = "House price histogram", xlab = "Sale Price", col="red", breaks = seq(0,1000000, by=50000)) # what are the other changes doing?


# Make a histogram for square footage and make some changes to make the graph look better:
hist(mydata$square.feet)







###############
# SCATTERPLOTS
###############

# To plot the relationship between price and square footage, you have a few options

plot(mydata$square.feet, mydata$sale.price) #x-variable first, then y-variable

plot(mydata$sale.price ~ mydata$square.feet) #y-variables first

plot(sale.price ~ square.feet, data=mydata) #this uses a separate argument to tell R where to get the data from. It also create axis labels that are a bit cleaner.

plot(sale.price ~ square.feet, data=mydata, pch = 16) # pch = plotting character; you can use different plotting characters

plot(sale.price ~ square.feet, data=mydata, pch = 16, col="purple") # you can also plot in different colors





###############
# BOXPLOTS
###############

# R automatically recognizes that bedrooms is a factor (because we changed bedrooms to a factor), so when you plot price vs. bedrooms, R will automatically create a boxplot
plot(sale.price ~ bedrooms, data=mydata)

# You also have the option of using the boxplot command to get the same result:
boxplot(sale.price ~ bedrooms, data=mydata) # this requires you to add your own labels for the x and y -axis

boxplot(sale.price ~ bedrooms, data=mydata, xlab = "Number of Bedrooms", ylab = "Sale Price")

# Boxplots in R show medians, but not means. Since we are usually testing differences between means, this is a useful piece of information to have on your graph. So we will add filled circles to represent the mean for each.

sale.price.bedrooms.means <- tapply(mydata$sale.price, mydata$bedrooms, FUN="mean")
# This is one of many ways to calculate means for each group. The first variable is what you are taking the mean of, and the second is the grouping variable. The last argument states the function that is being used.
boxplot(sale.price ~ bedrooms, data=mydata, xlab = "Number of bedrooms", ylab = "Sale Price")
points(sale.price.bedrooms.means, pch=16)

# Look at your graph and check if these means make sense with this data. If you made a mistake in the code, this kind of check is a quick way to catch that error.




###############
# TABLES
###############

# To find out how many of the houses have the different bedroom choices, we will use the table function

table(mydata$bedrooms)



  # You can use the table function with two variables to create a contingency table.
  table(mydata$bedrooms, mydata$bathrooms)
  



###################
# SUBSETTING DATA
###################

# create a new dataset that only has the houses with three bedrooms
# whenever you subset your data, give the new dataset a distinct name. Do not give it the name of your original dataset or else it will overwrite that, and you will have to reimport the data.
mydata.3.bed <- subset(mydata, mydata$bedrooms == "3")

nrow(mydata.3.bed)

print(mydata.3.bed)



# How many choices have at least two bathrooms?

table(mydata.3.bed$bathrooms)

barplot(table(mydata.3.bed$bathrooms), ylab = "Count", ylim = c(0, 100), xlab = "Number of bathrooms")


# Graph price in relationship to number of bathrooms

sale.price.bathrooms.means <- tapply(mydata.3.bed$sale.price, mydata.3.bed$bathrooms, FUN="mean")
boxplot(sale.price ~ bathrooms, data=mydata.3.bed, xlab = "Number of bathrooms", ylab = "Price")
points(sale.price.bathrooms.means, pch=16)





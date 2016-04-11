# Data Wrangling Exercise 2: Dealing with Missing Values
# Gurpal Bisra - April 10, 2016
# cat("\014") = command to clear console*

# Load packages
install.packages("dplyr")
install.packages("tidyr") 
install.packages("xlsx") # to work with Excel Files*

## 0: Load the data into RStudio
# read in the first worksheet from the workbook refine.xlsx
# first row contains variable names
library(xlsx)
mydata <- read.xlsx("C:/Users/Administrator/Desktop/Programming/R/Chapter_3-Data_Wrangling/Data_Wrangling_Excercises/2-Dealing_with_Missing_Values/titanic3.xls", 1)
# View(mydata) - allows seeing a table of the data
write.csv(mydata, file="titanic_original.csv", quote=TRUE, row.names=FALSE)
# quote = TRUE - means that "," within a column will NOT be separated
df <- read.csv("C:/Users/Administrator/Desktop/Programming/R/Chapter_3-Data_Wrangling/Data_Wrangling_Excercises/2-Dealing_with_Missing_Values/titanic_original.csv")
# read.csv(file, header = FALSE) - means the dataframe will load when there are fewer headers than columns

## 1: Port of Embarkation
# Find missing value in embarked column and replace missing value with S
df$embarked[is.na(df$embarked)] <- "S"

## 2: Age
# Calculate the mean of age column, exclusing NA values, and replace it to populate NA values
mean_age <- mean(df$age[!is.na(df$age)])
df$age[is.na(df$age)] <- mean_age
# Another function other than mean could be used to get an int value. 
# Otherwise, separate means could be performed to find the mean value of men and women separately.

## 3: Lifeboat
# Fill empty boat column values with NA
df$boat <- as.numeric(as.character(df$boat))
# Function worked on values "15 16" too which are not counted as numeric

## 4: Cabin
# Create new column and use binary to keep track of which cabins had passengers
# A missing value indicates a passgener was of a low class, and likely not able to secure a boat for escape
df$has_cabin_number <- df$cabin

library(dplyr)
library(tidyr)

df <- df  %>% mutate(has_cabin_number = ifelse(is.na(has_cabin_number), 0, 1))

## 5: Submit the project on GitHub
write.csv(df, file="titanic_clean.csv", quote=TRUE, row.names=FALSE)
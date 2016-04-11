# Data Wrangling Exercise 1: Basic Data Manipulation
# Gurpal Bisra - April 9, 2016
# cat("\014") = command to clear console*

# Load packages
install.packages("dplyr")
install.packages("tidyr") 
install.packages("xlsx") # to work with Excel Files*

## 0: Load the data into RStudio
# read in the first worksheet from the workbook refine.xlsx
# first row contains variable names
library(xlsx)
mydata <- read.xlsx("C:/Users/Administrator/Desktop/Programming/R/Chapter_3-Data_Wrangling/Data_Wrangling_Excercises/1-Basic_Data_Manipulation/refine.xlsx", 1)
# View(mydata) - allows seeing a table of the data
write.csv(mydata, file="refine_original.csv", quote=FALSE, row.names=FALSE)
df <- read.csv("C:/Users/Administrator/Desktop/Programming/R/Chapter_3-Data_Wrangling/Data_Wrangling_Excercises/1-Basic_Data_Manipulation/refine_original.csv", 1)

## 1: Clean up brand names
df[1:6,1] = "philips"
df[14:16,1] = "philips"
df[7:13,1] = "akzo"
df[17:21,1] = "van houten"
df[22:25,1] = "unilever"

## 2: Separate product code and number
# names(df) - see the column name is "Product.code...number."
library(dplyr)
library(tidyr)
df <- separate(df, "Product.code...number.", c("product_code", "product_number"), sep = "-", remove = TRUE)

## 3: Add product categories
# create product category column 
df$product_category <- 0

for (i in  1:nrow(df)){
    if(df$product_code[i] == "p") {
        df$product_category[i] <- "Smartphone"
    }
    else if(df$product_code[i] == "v") {
        df$product_category[i] <- "TV"
    }
    else if (df$product_code[i] == "x") {
        df$product_category[i] <- "Laptop"
    }
    else 
        df$product_category[i] <- "Tablet"
}

## 4: Add full addresses for geocoding
df <- unite(df, full_address, c(address,city,country), sep = ",", remove = FALSE)

## 5: Create dummy variables for company and product category
df$company_philips <- 0
df$company_akzo <- 0
df$company_van_houten <- 0
df$company_unilever <- 0

for (i in  1:nrow(df)){
    if(df$company[i] == "philips") {
        df$company_philips[i] <- 1
    }
    else if(df$company[i] == "akzo") {
        df$company_akzo[i] <- 1
    }
    else if (df$company[i] == "van houten") {
        df$company_van_houten[i] <- 1
    }
    else if (df$company[i] == "unilever") {
        df$company_unilever[i] <- 1
    }  
}

# must split up 2 for loops as the seem to break otherwise
df$product_smartphone <- 0
df$product_tv <- 0
df$product_laptop <- 0
df$product_tablet <- 0

for (i in  1:nrow(df)){
    if (df$product_category[i] == "Smartphone") {
        df$product_smartphone[i] <- 1
    }  
    else if (df$product_category[i] == "TV") {
        df$product_tv[i] <- 1
    }  
    else if (df$product_category[i] == "Laptop") {
        df$product_laptop[i] <- 1
    }  
    else if (df$product_category[i] == "Tablet") {
        df$product_tablet[i] <- 1
    }
}

## 6: Submit the project on GitHub
write.csv(df, file="refine_clean.csv", quote=FALSE, row.names=FALSE)
## Set the working directory
setwd("////Mac/Home/Desktop/BAMS580D/A1/")
getwd()

## I downloaded the files "admissions.csv" and "ptdata.csv" from the UBC Connect website and saved them in my working directory.

## Part 1: Read in "admissions.csv", clean and format the data, and add the column weekdayname, as we did in class.

# Read in "admissions.csv"
admissions <- read.csv("admissions.csv", header=TRUE, na.strings="")

# Change "emerg" column's data type from int to factor
admissions$emerg <- factor(admissions$emerg)

# Change "amdate" column's data type from factor to POSITlt type
admissions$admdate2 <- strptime(admissions$admdate, format = "%Y-%m-%d")
    # Change "amdate" column's data type from factor to Date type 
admissions$admdate3 <- as.Date(admissions$admdate)

# Add column weekdayname with factor levels of days of the week
admissions$weekdayname <- format(admissions$admdate2, "%A")
admissions$weekdayname <- factor(admissions$weekdayname, 
                                 levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#------------------------------------------------------------------
## Part 2: Read in "ptdata.csv", clean and format the data, and add the column agegroup, as we did in class.

# Read in "ptdata.csv"
ptdata <- read.csv("ptdata.csv", header=TRUE)

# Create a new column named "age" with numeric data types
    # remove the "yr" from data values
ptdata$age <- gsub("yr", "", ptdata$age2) 
    # Change "age" column's data type from char to numeric
ptdata$age <- as.numeric(ptdata$age)
    # Remove values containing -1
ptdata$age[which(ptdata$age<0)] <- NA

# Add the column "agegroup" containing sets of 5 years of ages
ptdata$agegroup <- 5*floor(ptdata$age/5)
    # Make the top factor level be for ages 90+
ptdata$agegroup[which(ptdata$agegroup == 95)] <- 90
    # Change the "agegroup" data type from numeric to factor
ptdata$agegroup <- factor(ptdata$agegroup)

#------------------------------------------------------------------
## Part 3: Merge the two data frames into a new one (name it whatever you like). The final data frame should contain the following columns (you may rename them if you like, and you may also include other columns):
    # 1. id as integer
    # 2. sex as factor
    # 3. age as numeric with range 1 to 99
    # 4. agegroup as factor (5 year age bins, from 0 to 90)
    # 5. emerg as factor
    # 6. admdate as date
    # 7. weekdayname as factor
    # 8. los as numeric

merged.file <- merge(ptdata[c(1,2,4,5)], admissions[c(1,2,7,8,5)], all.y=TRUE)

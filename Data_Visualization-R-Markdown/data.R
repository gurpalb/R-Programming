########################################################
# Name: Data Tidying/Wrangling + Visualizations ggplot2
# Author: Gurpal Bisra 
# Date: 2018-02-04 (yyyy-mm-dd)
# cat("\014") = command to clear console*
########################################################

# Set the working directory
getwd()
setwd("\\\\Mac/Home/Desktop/R-Scripts")

# Install and load libraries
install.packages("ggplot2")
install.packages("reshape2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("knitr")
# install.packages("scales") -- does not exist anymore?
library(ggplot2)
library(reshape2)
library(tidyr)
library(dplyr)
library(knitr)
# library(scales)

# Load data
myData <-read.csv("mydata.csv", stringsAsFactors = "FALSE")

# Select columns to use
keeps <- c("Time_In_Sec", "Service", "Type")
mySubData <- myData[keeps]

# Clean up characters
if(mySubData$Type == F){mySubData$Type <- "Fast"} 
mySubData$Type[which(mySubData$Type == "S")] <- "Slow"

# Visualize service times by Service and Type
histogram_data <- ggplot(mySubData, aes(x=Time_In_Sec, fill=Service)) + geom_histogram(binwidth = 60, color="black")
histogram_data + facet_grid(Type ~ Service) + ggtitle("Service Times at Fast or Slow Types") + labs(y="Count", x="Service Time at Type in Seconds") + theme(plot.title=element_text(hjust=0.5, size=14))

# Select only A and B services with no missing values, then plot histogram again
mySubData1 <- subset(mySubData, Type!="" & Time_In_Sec!="" & Service!="C")
histogram_data <- ggplot(mySubData1, aes(x=Time_In_Sec, fill=Service)) + geom_histogram(binwidth = 60, color="black")
histogram_data + facet_grid(Type ~ Service) + ggtitle("Service Times at Fast or Slow Types") + labs(y="Count", x="Service Time at Type in Seconds") + theme(plot.title=element_text(hjust=0.5, size=14))

# Calculate fast and slow sub-services for each Service
AData <- subset(mySubData, Service =="A")
BData <- subset(mySubData, Service =="B")
CData <- subset(mySubData, Service =="C")

sum(AData$Type == "F")/nrow(AData)  # % of A Fast lanes 
sum(AData$Type == "S")/nrow(AData)  # % of A Slow lanes
sum(AData$Type == "")               # num of obs where type not recorded

sum(BData$Type == "F")/nrow(AData)  # % of B Fast lanes 
sum(BData$Type == "S")/nrow(AData)  # % of B Slow lanes
sum(BData$Type == "")               # num of obs where type not recorded

sum(CData$Type == "F")/nrow(AData)  # % of C Fast lanes 
sum(CData$Type == "S")/nrow(AData)  # % of C Slow lanes
sum(CData$Type == "")               # num of obs where type not recorded

# Plot density plot of total flow times
density_plot <- ggplot(data = myData, aes(x= Time_In_Sec)) + geom_density(fill = "orange", colour = "orange2", alpha = 0.2) + theme_classic() + 
  scale_fill_discrete(guide=FALSE) + xlim(0,1000)
density_plot + facet_grid(Type ~ Service) + ggtitle("Density Plot of Fast or Slow Types") + labs(y="Count", x="Service Time at Type in Seconds") + theme(plot.title=element_text(hjust=0.5, size=14))

# Plot Boxplot of total flow times
summary(myData$Time_In_Sec)
boxplot <- ggplot(myData, aes(x= Service, y= Time_In_Sec)) + geom_boxplot(fill=c("lightblue"), alpha=0.5) + theme_classic() +
  ggtitle("BoxPlot of Service Times") + ylab("Lane Service Time (Sec)") 
boxplot + facet_grid(Type ~ Service)

# Plot Scatterplot
Scatterplot <- ggplot(data = myData, aes(x = Time_In_Sec, y = NumTasks)) + geom_point() + theme_classic()
Scatterplot + ggtitle("Scatterplot of Service Times Vs. Tasks to Complete") + xlab("Service Time (Sec)") + ylab("Tasks To Complete")

# t-Tests between Services A, B, C
t.test1 = t.test(AData$Time_In_Sec, BData$Time_In_Sec)
t.test2 = t.test(AData$Time_In_Sec, CData$Time_In_Sec)
t.test3 = t.test(BData$Time_In_Sec, CData$Time_In_Sec)
p1 = t.test1$p.value
p2 = t.test2$p.value
p3 = t.test2$p.value
p1
p2
p3

# Plot barplot of means
meanA <- mean(AData$Time_In_Sec)
meanB <- mean(BData$Time_In_Sec)
meanC <- mean(CData$Time_In_Sec)

Service <- c("A", "B", "C")
Average <- c(meanA, meanB, meanC)
data_frame <- data.frame(Average, Service)

Barplot <- ggplot(data_frame, aes(x = Service, y= Average, fill = Average)) + geom_bar(position = "dodge", colour="black", stat = "identity") 
Barplot
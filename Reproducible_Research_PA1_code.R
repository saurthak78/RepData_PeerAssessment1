#
# Set working director an download the data file from the link provided.
#
setwd("E:/R/coursera/Assignments/Reproducible Research")
#
if(!file.exists("./data")) {
        dir.create("./data")
        data.url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        download.file(data.url, 
                      destfile = "./data/Dataset.zip")
        unzip(zipfile = "./data/Dataset.zip", 
              exdir = "./data")
}
#
#
#
library(ggplot2)
library(dplyr)
#
activity <- read.csv("./data/activity.csv", header = TRUE)
# aggregating the number of steps taken per day
daily_activity <- aggregate(steps~date, activity, sum)
# plotting the histogram of the number of steps taken per day 
ggplot(daily_activity, aes(steps)) + 
        geom_histogram(fill = "pink", color = "black", binwidth = 2000) +
                       labs(x = "Daily steps",
                            title = "Total number of steps taken each day")
#calculate the mean and median of the daily steps taken.
mean_daily_act <- mean(daily_activity$steps, na.rm = TRUE)
median_daily_act <- median(daily_activity$steps, na.rm = TRUE)
#time series plot of the steps taken at 5 min intervals.
min_inter_steps <- aggregate(steps~interval, activity, mean)
# plot
ggplot(min_inter_steps,aes(interval)) +
        geom_line(aes(y = steps),color = "purple") +
        labs(x = "5 min intervals",
             y = "average steps",
             title = "5 min interval plot")
#
max_steps <- min_inter_steps[which.max(min_inter_steps$steps),]$interval
#




# replacing the "NA" with "0"]
activity$steps[ is.na(activity$steps)] <- 0


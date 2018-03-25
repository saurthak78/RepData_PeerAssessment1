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
#creating a back up
original_activity <- activity
# replacing the "NA" with "0"]
activity$steps[ is.na(activity$steps)] <- 0
# changing the date in the df from factor to date.
activity$date <- as.Date(activity$date)
# aggregating the number of steps taken per day
daily_activity <- aggregate(steps~date, activity, sum)
# plotting the histogram of the number of steps taken per day 
ggplot(daily_activity, aes(steps)) + 
        geom_histogram(fill = "skyblue", color = "black",
                       binwidth = 2000) +
                       labs(x = "Daily steps",
                            title = "Total number of steps taken each day")
#
mean_daily_act <- aggregate(steps~date, daily_activity, mean)
median_daily_act <- aggregate(steps~date, activity, median)
median_act <- median(activity$steps, na.rm = FALSE)



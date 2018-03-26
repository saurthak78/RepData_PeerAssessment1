---
title: "PA1_Reproducible_Research"
author: "Saurabh Thakur"
date: "25 March 2018"
output: html_document
---



# Reproducible Research - Peer Assignment 1#

## Read the data##


```r
#
#setwd("E:/R/coursera/Assignments/Reproducible Research")
#
activity <- read.csv("./data/activity.csv", header = TRUE)
```
### PreProcess the data###


```r
library(ggplot2)
library(dplyr)
```

### What is mean total number of steps taken per day?###


```r
daily_activity <- aggregate(steps~date, activity, sum)
```

###Make a histogram of the total number of steps taken each day###

```r
# plotting the histogram of the number of steps taken per day 
ggplot(daily_activity, aes(steps)) + 
        geom_histogram(fill = "pink", color = "black", binwidth = 2000) +
                       labs(x = "Daily steps",
                            title = "Total number of steps taken each day")
```

<img src="PA1_Reproducible_Research_files/figure-html/plot_steps-1.png" width="672" />
### Calculate and report the mean and median of the total number of steps taken per day###


```r
#calculate the mean and median of the daily steps taken.
mean_daily_act <- mean(daily_activity$steps, na.rm = TRUE)
median_daily_act <- median(daily_activity$steps, na.rm = TRUE)
```
##What is the average daily activity pattern?##
 
###Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)###
 

```r
min_inter_steps <- aggregate(steps~interval, activity, mean)
# plot
ggplot(min_inter_steps,aes(interval)) +
        geom_line(aes(y = steps),color = "purple") +
        labs(x = "5 min intervals",
             y = "average steps",
             title = "5 min interval plot")
```

<img src="PA1_Reproducible_Research_files/figure-html/plot_time-1.png" width="672" />
 
###Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?###

```r
max_steps <- min_inter_steps[which.max(min_inter_steps$steps),]$interval
```
 
##Imputing missing values##
 
###There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.###
 
###Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)###


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

###Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.###

```r
mod_activity <- activity

for (i in 1:nrow(mod_activity))
        {
        if (is.na(mod_activity$steps[i]))
                {
                mod_activity$steps[i] <- min_inter_steps[which(mod_activity$interval[i] == min_inter_steps$interval),]$steps
                }
        }
```
###Create a new dataset that is equal to the original dataset but with the missing data filled in.###


```r
summary(mod_activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 27.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##                   (Other)   :15840
```

```r
head(mod_activity)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
tail(mod_activity)
```

```
##           steps       date interval
## 17563 2.6037736 2012-11-30     2330
## 17564 4.6981132 2012-11-30     2335
## 17565 3.3018868 2012-11-30     2340
## 17566 0.6415094 2012-11-30     2345
## 17567 0.2264151 2012-11-30     2350
## 17568 1.0754717 2012-11-30     2355
```

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```
###Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?###


```r
#
mod_daily_activity <- aggregate(steps~date, mod_activity, sum)
# plotting the histogram of the number of steps taken per day 
ggplot(mod_daily_activity, aes(steps)) + 
        geom_histogram(fill = "pink", color = "black", binwidth = 2000) +
        labs(x = "Daily steps",
             title = "Total number of steps taken each day")
```

<img src="PA1_Reproducible_Research_files/figure-html/new_plot-1.png" width="672" />

###Mean and Median of the modified data###


```r
#calculate the mean and median of the daily steps taken.
mod_mean_daily_act <- mean(mod_daily_activity$steps, na.rm = TRUE)
mod_median_daily_act <- median(mod_daily_activity$steps, na.rm = TRUE)
mean_daily_act
```

```
## [1] 10766.19
```

```r
mod_mean_daily_act
```

```
## [1] 10766.19
```

```r
median_daily_act
```

```
## [1] 10765
```

```r
mod_median_daily_act
```

```
## [1] 10766.19
```

##Are there differences in activity patterns between weekdays and weekends?##

###For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.###

###Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.###

```r
mod_activity$week <- as.factor(ifelse(weekdays(as.Date(mod_activity$date),
                                               abbreviate = FALSE) %in%
                                              c("Saturday","Sunday"),
                                      "Weekend", "Weekday"))
str(mod_activity)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ week    : Factor w/ 2 levels "Weekday","Weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

###Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).###


```r
aggr_activity <- aggregate(steps~interval + week, mod_activity, mean)

ggplot(aggr_activity, aes(interval, steps)) +
        geom_line() + 
        facet_grid(week ~ .) +
        xlab("5-minute interval") + 
        ylab("avarage number of steps")
```

<img src="PA1_Reproducible_Research_files/figure-html/mod_plot1-1.png" width="672" />

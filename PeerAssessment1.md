---
title: "Reproducable Reserch: Peer Assessment 1"
author: "Arunava Roy"
date: "September 14, 2014"
output: html_document
---

## Loading Data from activity.csv


```r
activity <- read.csv("activity.csv")
```

## Generate  a histogram of the total number of steps taken each day


```r
library(plyr)

aggregate_steps <- ddply(activity, ~date, summarize, steps_count = sum(steps))

hist(aggregate_steps[, c(2)], main="Histogram of Steps per day", xlab="Steps Count", ylab="Frequency")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

##  Mean and Median total number of steps taken per day


```r
mean(aggregate_steps$steps_count, na.rm=TRUE)
```

```
## [1] 10766
```

```r
median(aggregate_steps$steps_count, na.rm=TRUE)
```

```
## [1] 10765
```

## Time series plot of interval vs average steps


```r
# The avreage number of steps in by interval
steps_interval <- aggregate(steps ~ interval, activity, mean)

# Create a time series plot of the 5-minute interval (x-axis) and the average number of steps taken,
# averaged across all days (y-axis)
plot(steps_interval$interval, steps_interval$steps, type='l',
     main="Average number of steps averaged over all days", xlab="Interval", 
     ylab="Average number of steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

## The interval with the maximum number of steps


```r
steps_interval[which.max(steps_interval$steps), ]
```

```
##     interval steps
## 104      835 206.2
```

## The total number of rows with NA

```r
nrow(activity[!complete.cases(activity),])
```

```
## [1] 2304
```

## Replace the NA

```r
activity2 <- merge(activity, steps_interval, by = "interval", suffixes = c("", 
    ".y"))
nas <- is.na(activity2$steps)
activity2$steps[nas] <- activity2$steps.y[nas]
activity2 <- activity2[, c(1:3)]
```

## Generate the Histogram

```r
aggregate_steps <- ddply(activity2, ~date, summarize, steps_count = sum(steps))

hist(aggregate_steps[, c(2)], main="Histogram of Steps per day - 2", xlab="Steps Count", ylab="Frequency")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

##  Mean and Median total number of steps taken per day


```r
mean(aggregate_steps$steps_count, na.rm=TRUE)
```

```
## [1] 10766
```

```r
median(aggregate_steps$steps_count, na.rm=TRUE)
```

```
## [1] 10766
```
The Mean and Median value is equal after replacing the NA.

## Determine Weekdays and Weekends

```r
activity2.day_type = factor(0)
activity2$day_type <- ifelse(weekdays(as.Date(activity2$date,"%Y-%m-%d")) %in%  c("Saturday", "Sunday"),'weekend','weekday')
```

## Make a panel plot


```r
par(mfrow = c(2, 1), pin=c(3, 1.6))


weekday_df <- aggregate(steps ~ interval, data = activity, subset = c(activity2$day_type == "weekday"), FUN = mean)
plot(weekday_df, type = "l", main = "Weekday Activity")

weekend_df <- aggregate(steps ~ interval, data = activity, subset = c(activity2$day_type == "weekend"), FUN = mean)

plot(weekend_df, type = "l", main = "Weekend Activity")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

The conclusion: In the weekday people are taking more steps.

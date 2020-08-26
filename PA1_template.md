---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

# Introduction

This assignment uses the data from a personal activity monitoring device. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. Thedevice collects data at 5 minute intervals through out the day. 

## The data

The variables included in dataset are:

* steps: Number of steps taken in a 5-minute interval
* date: The date when the measurement was taken
* interval: Identifier for the 5-minute interval in which measurement was taken

# SUBMISSION




## Loading and preprocessing the data

```r
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?


```r
steps_per_day <- aggregate(steps ~ date, activity, sum)
rmean <- mean(steps_per_day$steps)
rmedian <- median(steps_per_day$steps)
sprintf("MEAN of steps per day = %.3f", rmean)
```

```
## [1] "MEAN of steps per day = 10766.189"
```

```r
sprintf("MEDIAN of steps per day = %.3f", rmedian)
```

```
## [1] "MEDIAN of steps per day = 10765.000"
```

## Histogram of the total number of steps taken each day

```r
steps_per_day <- aggregate(steps ~ date, activity, sum)
hist(steps_per_day$steps, main = paste("Total Steps per Day"), col="orange", xlab="Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Time series plot of the average number of steps

```r
steps_by_interval <- aggregate(steps ~ interval, activity, mean)
plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## The 5-minute interval that, on average, contains the maximum number of steps

```r
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
sprintf("MAXIMUM number of steps in 5-minute interval = %.0f", max_interval)
```

```
## [1] "MAXIMUM number of steps in 5-minute interval = 835"
```

## Imputing missing values

```r
incomplete_data <- sum(!complete.cases(activity))
sprintf("MISSING data = %.0f", incomplete_data)
```

```
## [1] "MISSING data = 2304"
```



```r
dataimputed <- transform(activity, steps = ifelse(is.na(activity$steps), steps_by_interval$steps[match(activity$interval, steps_by_interval$interval)], activity$steps))
```

## Histogram of the total number of steps taken each day after missing values are imputed

```r
steps_imputed_per_day <- aggregate(steps ~ date, dataimputed, sum)
hist(steps_imputed_per_day$steps, main = paste("Total Steps Each Day"), col="green", xlab="Number of Steps")

hist(steps_per_day$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps", add=T)
legend("topright", c("NA Included", "NA Not Included"), col=c("green", "red"), lwd=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

### Difference of MEAN

```r
rmean <- mean(steps_per_day$steps)
imputed_rmean <- mean(steps_imputed_per_day$steps)
sprintf("MEAN of steps taken each day = %.3f", rmean)
```

```
## [1] "MEAN of steps taken each day = 10766.189"
```

```r
sprintf("MEAN of steps taken each day with IMPUTED data = %.3f", imputed_rmean)
```

```
## [1] "MEAN of steps taken each day with IMPUTED data = 10766.189"
```

```r
sprintf("The difference is %.3f ", imputed_rmean-rmean)
```

```
## [1] "The difference is 0.000 "
```

### Difference of MEDIAN

```r
rmedian <- median(steps_per_day$steps)
imputed_rmedian <- median(steps_imputed_per_day$steps)
sprintf("MEDIAN of steps taken each day = %.3f", rmedian)
```

```
## [1] "MEDIAN of steps taken each day = 10765.000"
```

```r
sprintf("MEDIAN of steps taken each day with IMPUTED data = %.3f", imputed_rmedian)
```

```
## [1] "MEDIAN of steps taken each day with IMPUTED data = 10766.189"
```

```r
sprintf("The difference is %.3f ", imputed_rmedian-rmedian)
```

```
## [1] "The difference is 1.189 "
```

### TOTAL steps difference

```r
total <- sum(steps_per_day$steps)
total_imputed <- sum(steps_imputed_per_day$steps)
sprintf("TOTAL of steps = %.3f", total)
```

```
## [1] "TOTAL of steps = 570608.000"
```

```r
sprintf("TOTAL of steps with IMPUTED data = %.3f", total_imputed)
```

```
## [1] "TOTAL of steps with IMPUTED data = 656737.509"
```

```r
sprintf("The difference is %.3f ", total_imputed-total)
```

```
## [1] "The difference is 86129.509 "
```

## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
weekend_days_set <- c("Saturday", "Sunday")
dataimputed$dow = as.factor(ifelse(is.element(weekdays(as.Date(dataimputed$date)),weekend_days_set), "Weekend", "Weekday"))
imputed_steps_by_interval <- aggregate(steps ~ interval + dow, dataimputed, mean)


xyplot(imputed_steps_by_interval$steps ~ imputed_steps_by_interval$interval|imputed_steps_by_interval$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

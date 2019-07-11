---
title: "PA1_template, Reproducible Research: Peer Assessment 1"
author: "Maria Harmening"
date: "July 11, 2019"
output: 
  html_document:
    keep_md: true
    self_contained: true
---


## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as \color{red}{\verb|NA|}NA)
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data
download the zip file.

unzip the file into a data subfolder

read the csv file into a dataframe

summarize the number of the steps by the date



```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
              destfile = "C:/Users/Jeff/Desktop/Maria/DataScienceCourse/Rsnippets/repdata%2Fdata%2Factivity.zip", 
              method = "curl")

unzip("repdata%2Fdata%2Factivity.zip", exdir="data")

dfActivity <- read.csv('data/activity.csv', header = T)

dfDailySteps <- aggregate(steps~date, data = dfActivity, sum, na.rm=TRUE)
```


## What is mean total number of steps taken per day?

Here is a distribution of the steps taken per day


```r
hist(dfDailySteps$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->



## What is the average daily activity pattern?

Calculate the mean and median number of steps taken per day

```r
mean(dfDailySteps$steps)
```

```
## [1] 10766.19
```

```r
median(dfDailySteps$steps)
```

```
## [1] 10765
```
* The **mean** value of the total steps taken per day is 
    1.0766189\times 10^{4} steps.
* The **median** value of the total steps taken per day is 
    10765 steps.
    
    
Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)    

```r
dfIntervalSteps <- aggregate(steps~interval, data = dfActivity, mean, na.rm=TRUE)
plot(steps~interval, data = dfIntervalSteps, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
dfIntervalSteps[which.max(dfIntervalSteps$steps),]$interval
```

```
## [1] 835
```

The max is interval **835**.


## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(dfActivity$steps))
```

```
## [1] 2304
```
Total NA rows is 2304.


I will devise a strategy for filling in all of the missing values in the dataset. 

i will aggregate the data by the intervals and calculate the mean.

I will use the calculated interval means to fill in the missing values in the original data set.


```r
dfAverageStepsByInterval <- aggregate(steps~interval, data = dfActivity, mean, na.rm=TRUE)

dfActivityImputed <- dfActivity 

for(i in 1:nrow(dfActivityImputed))
{
    if(is.na(dfActivityImputed[i,]$steps))
    {
        intervalRow <- subset(dfAverageStepsByInterval, 
                                              dfAverageStepsByInterval$interval == dfActivityImputed[i,]$interval)
        dfActivityImputed[i,]$steps <- intervalRow$steps

    }
}

dfDailyStepsImputed <- aggregate(steps~date, data = dfActivityImputed, sum, na.rm=TRUE)

hist(dfDailyStepsImputed$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
mean(dfDailyStepsImputed$steps)
```

```
## [1] 10766.19
```

```r
median(dfDailyStepsImputed$steps)
```

```
## [1] 10766.19
```
* The **mean** value of the total steps taken per day of the non-imputed data set is
    1.0766189\times 10^{4} steps and the **mean** value of the total steps taken per day of the 
    imputed data set is
    1.0766189\times 10^{4} steps.  This is a delta of 0 
    which is no difference.
* The **median** value of the total steps taken per day of the non-imputed data set is
    10765 steps and the **median** value of the total steps taken per day of the 
    imputed data set is
    1.0766189\times 10^{4} steps.  This is a delta of -1.1886792
    which is minimal difference.


## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
dfActivityImputed$day <- ifelse(as.POSIXlt(as.Date(dfActivityImputed$date))$wday%%6 == 0, "Weekend", "Weekday")

dfActivityImputed$day <- factor(dfActivityImputed$day,levels=c("Weekday","Weekend"))
```


Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
dfActivityImputedWeekday <- aggregate(steps~interval+day, dfActivityImputed, mean)

library(lattice)
xyplot(steps~interval|factor(day), data = dfActivityImputedWeekday, aspect=1/2, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

    

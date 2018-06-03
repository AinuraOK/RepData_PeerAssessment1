---
title: "PA1_template"
author: "AK"
date: "27 05 2018"
output: 
  html_document: 
    keep_md: yes
---



```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

#Load the data and process/transform the data (if necessary) into a format suitable for your analysis 

```r
OriginalData <- read.csv(file="~/Desktop/Data science lessons/R-programming/get and clean data /Project 1/activity.csv", header=TRUE, sep=",")
MyData <- na.omit(OriginalData)
MyData$date <- as.Date(MyData$date)
```
#Calculate the total number of steps taken per day

```r
stepstaken <- MyData %>%
group_by(date) %>%
summarize(TotalNumber=sum(steps))
```

#Make a histogram of the total number of steps taken each day

```r
hist(stepstaken$TotalNumber,
xlab="Total number of steps taken per day",
ylab="Count",
main="Histogram of total number of steps taken each day",
col=3)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

#Calculate and report the mean and median of the total number of steps taken per day


```r
Steps_mean <- mean(stepstaken$TotalNumber)
Steps_mean
```

```
## [1] 10766.19
```

```r
Steps_median <- median(stepstaken$TotalNumber)
Steps_median
```

```
## [1] 10765
```

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
AverageFivemin <- MyData %>%
    group_by(interval) %>%
    summarize(StepsAverage=mean(steps))
plot(AverageFivemin$interval, AverageFivemin$StepsAverage, 
     type="l",
     xlab="Interval",
     ylab="Average steps taken",
     main="Average steps taken during 5 minute interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
#a 5-minute interval, on average across all the days in the dataset, containing the maximum number of steps.

```r
maximum_number<-AverageFivemin$interval[which.max(AverageFivemin$StepsAverage)]
maximum_number
```

```
## [1] 835
```
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)



```r
missingValues <- sum(is.na(OriginalData$steps))
missingValues
```

```
## [1] 2304
```

#Devise a strategy for filling in all of the missing values in the dataset.


```r
FillIn <- OriginalData
for (i in 1:nrow(FillIn)) {
  if (is.na(FillIn$steps[i])) {
    index <- which(FillIn$interval[i] == AverageFivemin$interval)
    FillIn$steps[i] <- AverageFivemin[index,]$StepsAverage
  }
}

FillIn$date <- as.Date(FillIn$date)
```
#Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
Daily_Steps <- FillIn %>%
  group_by(date) %>%
  summarize(TotalNumber=sum(steps))
```

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```r
hist(Daily_Steps$TotalNumber,
     xlab="Total number of steps taken each day",
     ylab="Count",
     main="Histogram of total number of steps taken each day",
     col=3)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->



```r
#Do these values differ from the estimates from the first part of the assignment?What is the impact of imputing missing data on the estimates of the total daily number of steps?
FillIn_Mean <- mean(Daily_Steps$TotalNumber)
FillIn_Mean
```

```
## [1] 10766.19
```

```r
# Mean is the same.
FillIn_Median <- median(Daily_Steps$TotalNumber)
FillIn_Median
```

```
## [1] 10766.19
```

```r
 #Median changed.
```

#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
FillIn$day <- weekdays(FillIn$date)
FillIn$daytype <- "weekday"
FillIn$daytype[FillIn$day %in% c("суббота", "воскресенье")] <- "weekend"
```

#Make a panel plot containing a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
Average_Day <- FillIn %>%
group_by(daytype, interval) %>%
summarize(StepsAverage=mean(steps))
library(ggplot2)
qplot(interval, StepsAverage, data=Average_Day,
      type="l",
      geom="line",
      xlab="Interval",
      ylab="Average Number of Steps",
      main="Average steps taken Weekends vs. Weekdays",
      facets =daytype ~ .)
```

```
## Warning: Ignoring unknown parameters: type
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
According to plotted activities by daytype, average number of steps on weekdays are relatively higher than average number of steps on weekends. It means that people are more active on the weekdays compared to weekends.  


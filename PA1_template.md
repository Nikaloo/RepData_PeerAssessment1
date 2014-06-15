# Reproducible Research: Peer Assessment 1

This document contains peer assessment assignment #1 for the MOOC on Coursera - Reproducible Research.  
The document is based on the template provided in the assignment materials.
Before running the code following conditions must be fulfilled: 
 1. Working Directory should be set manually, and
 2. lattce package is already installed.

## Loading and preprocessing the data

1 First of all, the code checks for the data file "activity.csv". If it's not available - downloads it and unzips it.

```r
if (!file.exists("activity.csv")) {
  if(!file.exists("activity.zip")) {
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","activity.zip")
  }
  unzip("activity.zip")
}
```

2 The data is read into the data frame

```r
data<-read.csv("activity.csv",header=T)
```
3 The date variable needs to be transformed into proper format. 

```r
data$date<-as.Date(data$date)
```
4 Number of digits in variable "intervals"" vary from 1 to 4. This makes confusion while reading it and converting it to time. To fix this problem, "000" is added from the left, and afterwards only last four digits are saved in the column.

```r
data$interval<-paste("000",data$interval,sep="")
data$interval<-substr(data$interval,start=nchar(data$interval)-3,stop=nchar(data$interval))
```
5 The last point of preparation is to load the "lattice" package.

```r
library(lattice)
```

## What is mean total number of steps taken per day?

For this part of the assignment, the missing values in the data set are ignored.
In the first place an array representing total number of steps each day is created.

```r
totalSteps<-sapply(split(data$steps,data$date),sum,na.rm=T)
```

### Make a histogram of the total number of steps taken each day.

```r
histogram(totalSteps, nint=12, main="Total steps taken each day", xlab="Total steps taken per day", type="count")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 
  
### Calculate and report the mean and median total number of steps taken per day.

```r
cat("MEAN total number of steps per day is: ",mean(totalSteps,na.rm=T))
```

```
## MEAN total number of steps per day is:  9354
```

```r
cat("MEDIAN total number of steps per day is: ",median(totalSteps,na.rm=T))
```

```
## MEDIAN total number of steps per day is:  10395
```

## What is the average daily activity pattern?
For this task firs the average number of steps taken per interval needs to be calculated. 

```r
intervalMean<-sapply(split(data$steps,data$interval),mean,na.rm=T)
```
Then the sequence of intervals is transformed into proper time format:

```r
intervals<-as.POSIXct(strptime(names(intervalMean),format="%H%M"))
```

### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
xyplot(intervalMean ~ intervals,main="Average number of steps per inderval during the day",
       panel=panel.lines,
       scales=list(x=list(at=seq(intervals[1], by="2 hour", length=13), 
     labels=format(seq(intervals[1], by="2 hour", length=13),
                   "%H:%M"))))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

### Which 5 minute interval contains most steps in average?

```r
format(intervals[intervalMean==max(intervalMean)],"%H:%M")
```

```
## [1] "08:35"
```
### What is the maximum number of steps taken in average in a single period?

```r
max(intervalMean)
```

```
## [1] 206.2
```



## Imputing missing values

### Calculate and report the total number of missing values in the dataset.


```r
paste("The number of missing values in the dataset:",sum(is.na(data$steps)))
```

```
## [1] "The number of missing values in the dataset: 2304"
```

### Devise a strategy for filling in all of the missing values in the dataset. 

The NA handling methodology: average of the interval for whole data set. Average number of steps per interval is calculated for the period, discarding the missing values. Next the appropriate values are inserted instead of missing values.

### Create a new dataset that is equal to the original dataset but with the missing data filled in.

Calculating average number of steps per interval.

```r
intervalMean<-sapply(split(data$steps,data$interval),mean,na.rm=T)
```
Clone the database

```r
newData<-data
```

Find the missing values and insert appropriate average (rounded to 0 digits - integer).

```r
for (i in which(is.na(data$steps))) {
  newData[i,"steps"]<-round(intervalMean[data[i,"interval"]],0)
}
```

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

To Calculate total number of steps and plot the histogram.

```r
newTotSteps<-sapply(split(newData$steps,newData$date),sum)
histogram(newTotSteps, nint=10, main="Total steps taken each day (filled NAs)", xlab="Total steps taken per day", type="count")
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18.png) 

Calculate mean and median of new data.

```r
paste("After filling NAs, mean number of steps per day is -",round(mean(newTotSteps),1),"steps,","compared to",round(mean(totalSteps),1),"steps without NAs" )
```

```
## [1] "After filling NAs, mean number of steps per day is - 10765.6 steps, compared to 9354.2 steps without NAs"
```

```r
paste("Median is:",median(newTotSteps),"steps,","compared to",median(totalSteps),"steps without NAs")
```

```
## [1] "Median is: 10762 steps, compared to 10395 steps without NAs"
```

## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day. For this task, "newData" with filled NAs is used.

```r
for (i in 1:nrow(newData)) {
  day<-weekdays(newData[i,"date"])
  ifelse (day=="Sunday" | day=="Saturday", newData[i,"day"]<-"weekend",newData[i,"day"]<-"weekday")
}
newData$day<-as.factor(newData$day)
```
### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

For this task firs the average number of steps taken per interval needs to be calculated. 


```r
weekDay<-split(newData,newData$day)$weekday
weekEnd<-split(newData,newData$day)$weekend
MeanWD<-sapply(split(weekDay$steps,weekDay$interval),mean,na.rm=T)
MeanWE<-sapply(split(weekEnd$steps,weekEnd$interval),mean,na.rm=T)
a<-data.frame(interval=as.POSIXct(strptime(names(MeanWD),format="%H%M")),meanSteps=round(MeanWD,1),day="weekday")
b<-data.frame(interval=as.POSIXct(strptime(names(MeanWE),format="%H%M")),meanSteps=round(MeanWE,1),day="weekend")
mData<-rbind(a,b)
```
Finally the time series plot is made taking into account the factor variable "day".

```r
xyplot(meanSteps ~ interval | day,data=mData, 
       main="Average number of steps per inderval during the day",
       panel=panel.lines, layout = c(1,2),
       scales=list(x=list(at=seq(intervals[1], by="2 hour", length=13), 
     labels=format(seq(intervals[1], by="2 hour", length=13),
                   "%H:%M"))))
```

![plot of chunk unnamed-chunk-22](figure/unnamed-chunk-22.png) 

As expected, the subject's walking pattern differs in weekdays and during the weekend.

Thank you for your time!
-------------------------------------------

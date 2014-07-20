# ReproducibleCourseraA1
**Introduction**

The objective of this assignment is to train the skills of using R Markdown in writing a report. By Knit, the R Markdown can be transformed into Html, pdf and word formats.

**Data**

The data for this assignment is about the personal movement using activity monitoring devices. The device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

**Loading and preprocessing the data**

*Load raw data from a compressed file*


```r
unzip("repdata-data-activity.zip")
activity <- read.table("activity.csv", header=T, quote="\"", sep=",")
# Change class for the date variable
activity$date <- as.Date(activity$date)
```
**What is the mean total number of steps taken per day?**
  
  *1. Summarize the data by day*

```r
daily.steps <-
  aggregate(formula = steps~date, data = activity, FUN = sum, na.rm=TRUE)
```
  
  *2. Make the histogram plot*

```r
hist(daily.steps$steps, 
     main=" ",
     breaks=10,
     xlab="Total Number of Steps Taken Daily")
```

![plot of chunk unnamed-chunk-3](./ReproducibleA1_files/figure-html/unnamed-chunk-3.png) 
  
  *3. Calculate the mean and median values*
  

```r
mean(daily.steps$steps)
```

```
## [1] 10766
```

```r
median(daily.steps$steps)
```

```
## [1] 10765
```
**What is the average daily activity pattern?**
  
  *1. Calculate the mean across the days*

```r
steps.interval <-aggregate(formula=steps~interval, data=activity,FUN=mean, na.rm=TRUE)
```
 *2. Report the data for the interval with the most average activity across the days*

```r
steps.interval[which(steps.interval$steps==max(steps.interval$steps)),]
```

```
##     interval steps
## 104      835 206.2
```

  *3. Plot the time series of the 5-minute interval and the average number of steps taken, averaged across all days*
  

```r
plot(steps.interval, type = "l")
```

![plot of chunk unnamed-chunk-7](./ReproducibleA1_files/figure-html/unnamed-chunk-7.png) 
**Imputing missing values**

  *1. Calculate and report the total number of missing values in the dataset*
  

```r
sum(is.na(activity))
```

```
## [1] 2304
```
The data set contains a total of 2304 missing number of steps. The absence of these values might have an impact on the activity estimations

**Imputation Strtegy**

The means for the 5-minute intervals is used as fillers for missing values.

*1. Build a new dataset that with the missing data filled in*

```r
activity <- merge(activity, steps.interval, by = "interval", suffixes = c("", 
    ".y"))
nas <- is.na(activity$steps)
activity$steps[nas] <- activity$steps.y[nas]
activity <- activity[, c(1:3)]
```
*2. Plot histogram of the total number of steps taken each day*


```r
steps.date <- aggregate(steps ~ date, data = activity, FUN = sum)
barplot(steps.date$steps, names.arg = steps.date$date, xlab = "date", ylab = "steps")
```

![plot of chunk unnamed-chunk-10](./ReproducibleA1_files/figure-html/unnamed-chunk-10.png) 

*3. Calculate the mean and median total number of steps taken per day*

```r
mean(steps.date$steps)
```

```
## [1] 10766
```

```r
median(steps.date$steps)
```

```
## [1] 10766
```
Analysis: The values have small difference with the estimates from the first part of the assignment. The shape of the histogram remains the same as the histogram from removed missing values. However, the frequency counts increased as expected. In this case, The impact of the missing data seems rather low.

**Are there differences in activity patterns between weekdays and weekends?**

*1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day*


```r
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
activity$daytype <- as.factor(sapply(activity$date, daytype))
```

*2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days*


```r
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval, data = activity, subset = activity$daytype == 
        type, FUN = mean)
    plot(steps.type, type = "l", main = type)
}
```

![plot of chunk unnamed-chunk-13](./ReproducibleA1_files/figure-html/unnamed-chunk-13.png) 

**END OF REPORT**


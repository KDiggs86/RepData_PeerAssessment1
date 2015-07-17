---
title: "Peer Assessment 1"
author: "KDiggs86"
date: "July 16, 2015"
output: 
  html_document:
    keep_md: true
---

We first load the data in R.


```r
activity <- read.csv("activity.csv")
```

###Part 1: What is the mean total number of steps taken per day?

We are allowed to ignore missing values. So, we first remove them from the data frame using the filter() function in the dplyr package.


```r
library(dplyr)
activity1 <- filter(activity, !is.na(activity$step))
```

Next split the steps column by date and use sapply to sum the number of steps taken each day.



```r
steps <- aggregate(activity1$steps, list(activity1$date), sum)
names(steps) <- c("date","total_steps")
head(steps)
```

```
##         date total_steps
## 1 2012-10-02         126
## 2 2012-10-03       11352
## 3 2012-10-04       12116
## 4 2012-10-05       13294
## 5 2012-10-06       15420
## 6 2012-10-07       11015
```

Now use the summary() function to get the mean and median total number of steps taken each day.


```r
summary(steps$total_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```


Finally we create a histogram of the total number of steps taken each day.



```r
hist(steps$total_steps,breaks=8, main = "Histogram of the total number of steps taken each day",xlab="Total Number of Steps",ylab = "Number of Days",col="red")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

###Part 2: What is the average daily activity pattern?

We will create a time series plot of the 5 minute intervals and the average number of steps taken (averaged across all days).  

First, we create a dataframe consisting only of the number of steps taken and the time interval.


```r
activity2 <- select(activity1, -date)
```

Now we need to find the average number of steps taken over each time interval. So, we first change the interval variable into a factor variable. Then use tapply.


```r
activity2$interval <- as.factor(activity2$interval)
avg_step <- tapply(activity2$step, activity2$interval, mean)
```

Now we make our plot.


```r
xnames <- names(avg_step)
plot(avg_step, type = "l", xlab = "time interval", xaxt = "n", ylab = "average number of steps", main = "Average number of steps taken over each time interval")
axis(1, at=1:length(xnames), labels = xnames)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

To end this section we want to determine the 5 minute interval that contains the maximum number of steps. We can tell from our plot that the interval is just under 850. The exact time interval is 835, as shown below.


```r
which.max(avg_step)
```

```
## 835 
## 104
```


### Imputing Missing Values

We first figure that there are 2304 missing values. All missing values are in the step variable.


```r
sum(is.na(activity))
```

```
## [1] 2304
```

```r
sum(is.na(activity$step))
```

```
## [1] 2304
```

I want to replace all missing values with the average number of steps taken over the corresponding time interval. (So, replace with what we found in previous part.)

Create a *data frame* of the averages of the time intervals.


```r
avg_step <- aggregate(activity2$step, list(activity2$interval),mean)
names(avg_step) <- c("time_interval","avg_step")
head(avg_step)
```

```
##   time_interval  avg_step
## 1             0 1.7169811
## 2             5 0.3396226
## 3            10 0.1320755
## 4            15 0.1509434
## 5            20 0.0754717
## 6            25 2.0943396
```

Glue this data frame to the original activity data frame.


```r
merged_data <- merge(activity, avg_step, by.x="interval",by.y="time_interval",all=TRUE)
merged_data <- arrange(merged_data,date)
head(merged_data)
```

```
##   interval steps       date  avg_step
## 1        0    NA 2012-10-01 1.7169811
## 2        5    NA 2012-10-01 0.3396226
## 3       10    NA 2012-10-01 0.1320755
## 4       15    NA 2012-10-01 0.1509434
## 5       20    NA 2012-10-01 0.0754717
## 6       25    NA 2012-10-01 2.0943396
```

Now replace all missing values in the steps column with the corresponding value in the avg_step column.


```r
stepsNA <- is.na(merged_data$steps)
merged_data$steps[stepsNA] <- merged_data$avg_step[stepsNA]
head(merged_data)
```

```
##   interval     steps       date  avg_step
## 1        0 1.7169811 2012-10-01 1.7169811
## 2        5 0.3396226 2012-10-01 0.3396226
## 3       10 0.1320755 2012-10-01 0.1320755
## 4       15 0.1509434 2012-10-01 0.1509434
## 5       20 0.0754717 2012-10-01 0.0754717
## 6       25 2.0943396 2012-10-01 2.0943396
```

Now find the total number of steps taken each day.


```r
new_total_steps <- aggregate(merged_data$steps, list(merged_data$date), sum)
names(new_total_steps) <- c("date","total_steps")
head(new_total_steps)
```

```
##         date total_steps
## 1 2012-10-01    10766.19
## 2 2012-10-02      126.00
## 3 2012-10-03    11352.00
## 4 2012-10-04    12116.00
## 5 2012-10-05    13294.00
## 6 2012-10-06    15420.00
```

Find the mean and median number of steps taken each day.


```r
summary(new_total_steps$total_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

Finally, create a histogram.


```r
hist(new_total_steps$total_steps,breaks=8, main = "Histogram of the total number of steps taken each day",xlab="Total Number of Steps",ylab = "Number of Days",col="blue")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png) 

This histogram is similar than the one in Part 1. It seems that imputing the missing values (as opposed to just deleting them) squishes the data together a bit more. But, that's about it for this data set.

###Part 4: Are there differences in patterns between the weekends and weekdays?

First we figure out what days are weekends. Then we create a column in the merged_data data frame that gives weekend vs weekday.


```r
merged_data <- mutate(merged_data, day = weekdays(as.Date(date)))
head(merged_data)
```

```
##   interval     steps       date  avg_step    day
## 1        0 1.7169811 2012-10-01 1.7169811 Monday
## 2        5 0.3396226 2012-10-01 0.3396226 Monday
## 3       10 0.1320755 2012-10-01 0.1320755 Monday
## 4       15 0.1509434 2012-10-01 0.1509434 Monday
## 5       20 0.0754717 2012-10-01 0.0754717 Monday
## 6       25 2.0943396 2012-10-01 2.0943396 Monday
```

```r
days <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
day_type <- c("Weekday","Weekday","Weekday","Weekday","Weekday", "Weekend","Weekend")
day_frame <- data.frame(days,day_type)
day_frame
```

```
##        days day_type
## 1    Monday  Weekday
## 2   Tuesday  Weekday
## 3 Wednesday  Weekday
## 4  Thursday  Weekday
## 5    Friday  Weekday
## 6  Saturday  Weekend
## 7    Sunday  Weekend
```

```r
merged_data <- merge(merged_data, day_frame, by.x="day", by.y="days",all=TRUE)
merged_data <- arrange(merged_data,date)
```


We need the average steps over each interval for both weekends and weekdays. So, I am going to split the merged_data data frame over each. Find the average number of steps over each interval for each type of day and then glue things back together.


```r
weekday <- filter(merged_data, day_type == "Weekday")
weekend <- filter(merged_data, day_type == "Weekend")
avg_weekday <- aggregate(weekday$steps, list(weekday$interval),mean)
names(avg_weekday) <- c("interval","avg_step")
avg_weekday <- mutate(avg_weekday, day_type="Weekday")
head(avg_weekday)
```

```
##   interval   avg_step day_type
## 1        0 2.25115304  Weekday
## 2        5 0.44528302  Weekday
## 3       10 0.17316562  Weekday
## 4       15 0.19790356  Weekday
## 5       20 0.09895178  Weekday
## 6       25 1.59035639  Weekday
```


```r
avg_weekend <- aggregate(weekend$steps, list(weekend$interval),mean)
names(avg_weekend) <- c("interval","avg_step")
avg_weekend <- mutate(avg_weekend, day_type="Weekend")
head(avg_weekend)
```

```
##   interval    avg_step day_type
## 1        0 0.214622642  Weekend
## 2        5 0.042452830  Weekend
## 3       10 0.016509434  Weekend
## 4       15 0.018867925  Weekend
## 5       20 0.009433962  Weekend
## 6       25 3.511792453  Weekend
```

Gluing avgweekend to avgweekday


```r
avg_by_type <- rbind(avg_weekday,avg_weekend)
avg_by_type$day_type <- as.factor(avg_by_type$day_type)
```
Now we are ready to make a plot!


```r
library(ggplot2)
g <- ggplot(avg_by_type, aes(interval,avg_step))
g + geom_line() + facet_grid(.~day_type)+xlab("time interval") + ylab("average number of steps")
```

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21-1.png) 

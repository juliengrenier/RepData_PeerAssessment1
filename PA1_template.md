# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
original_data <- read.csv('activity.csv')
data <- na.omit(original_data)
```


## What is mean total number of steps taken per day?

```r
total_steps_per_day <- aggregate(data$steps, by=list(data$date), FUN=sum)[[2]]
hist(total_steps_per_day, main="Histogram of total steps per day", xlab="Number of steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
steps_mean <- mean(total_steps_per_day)
steps_median <- median(total_steps_per_day)
```
The mean is 10766.1887 and the median is 10765.

## What is the average daily activity pattern?

```r
avg_steps_per_interval <- aggregate(data$steps, by=list(data$interval), FUN=mean)
names(avg_steps_per_interval) <- c('interval', 'avg_steps')
plot(avg_steps_per_interval$interval, avg_steps_per_interval$avg_steps, type="l", main="Time series of avg. steps per 5-minutes interval", xlab="Interval", ylab="Avg. Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

#### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avg_steps_per_interval[avg_steps_per_interval$avg_steps == max(avg_steps_per_interval$avg_steps),1]
```

```
## [1] 835
```

## Imputing missing values

#### Total number of missing data

```r
num_missing_data <- nrow(original_data) - nrow(data)
num_missing_data
```

```
## [1] 2304
```

#### Filling NA with 5-minute interval mean

```r
missing_values <- original_data[is.na(original_data$steps),]
missing_values_replacement <- transform(missing_values, steps= na.omit(avg_steps_per_interval[avg_steps_per_interval$interval == missing_values$interval,])$avg_steps)
original_data[is.na(original_data$steps),] <- missing_values_replacement
```

#### Plot new histogram

```r
total_steps_per_day2 <- aggregate(original_data$steps, by=list(original_data$date), FUN=sum)[[2]]
hist(total_steps_per_day2, main="Histogram of total steps per day", xlab="Number of steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

```r
steps_mean2 <- mean(total_steps_per_day2)
steps_median2 <- median(total_steps_per_day2)
```
The mean is 10766.1887 and the median is 10766.1887.

After replacing the missing values with their interval average we notice that it had no effect on the mean but it did change the median from 10765 to 10766.1887.



## Are there differences in activity patterns between weekdays and weekends?

### Adding weekday information to the data.frame

```r
original_data <- transform(original_data, date=as.Date(date))
original_data_with_weekdays <- transform(original_data, weekday=weekdays(date))
original_data_with_weekdays <- transform(original_data_with_weekdays, is_weekend=original_data_with_weekdays$weekday %in% c('Sunday', 'Saturday'))
```

### Finding average number of steps for weekdays

```r
weekdays_data <- original_data_with_weekdays[original_data_with_weekdays$is_weekend==FALSE,]
avg_weekdays_steps_per_interval <- aggregate(weekdays_data$steps, by=list(weekdays_data$interval), FUN=mean)
avg_weekdays_steps_per_interval$is_weekend = FALSE
names(avg_weekdays_steps_per_interval) <- c('interval', 'avg_steps', 'is_weekend')
```

### Finding average number of steps for weekend

```r
weekend_data <- original_data_with_weekdays[original_data_with_weekdays$is_weekend==TRUE,]
avg_weekend_steps_per_interval <- aggregate(weekend_data$steps, by=list(weekend_data$interval), FUN=mean)
avg_weekend_steps_per_interval$is_weekend = TRUE
names(avg_weekend_steps_per_interval) <- c('interval', 'avg_steps', 'is_weekend')
```

### Merge them back together

```r
avg_steps <- rbind(avg_weekdays_steps_per_interval, avg_weekend_steps_per_interval)
```

### Plotting the result using lattice

```r
library(lattice)
xyplot(avg_steps$avg_steps ~ avg_steps$interval|avg_steps$is_weekend, type="l", layout=c(1,2), strip=strip.custom(factor.levels=c( 'weekday', 'weekend')), xlab="Interval", ylab="Number of steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

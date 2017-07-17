Reproducible Research: Peer Assessment 1
=======================================

Loading and Processing the data


```r
data <- read.csv("activity.csv")
```

What is the mean total number of steps taken in a day?


```r
stepsEachDay <- tapply(data$steps, data$date, FUN = sum, na.rm = T)
hist(stepsEachDay, xlab = "Number of Steps per Day")
```

![Alt text](https://github.com/bridey397/RepData_PeerAssessment1/blob/master/unnamed-chunk-2-1.png)

```r
mean(stepsEachDay)
```

```
## [1] 9354.23
```

```r
median(stepsEachDay)
```

```
## [1] 10395
```

What is the average daily activity pattern?


```r
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")
```

![plot of chunk unnamed-chunk-3](https://github.com/bridey397/RepData_PeerAssessment1/blob/master/unnamed-chunk-3-1.png)

```r
averages[which.max(averages$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

Imputing missing values


```r
missingValues <- is.na(data$steps)
table(missingValues)
```

```
## missingValues
## FALSE  TRUE 
## 15264  2304
```

```r
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
  
filledSteps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
hist(filledSteps, xlab = "Total number of steps taken each day")
```

![plot of chunk unnamed-chunk-4](https://github.com/bridey397/RepData_PeerAssessment1/blob/master/unnamed-chunk-4-1.png)

```r
mean(filledSteps)
```

```
## [1] 10766.19
```

```r
median(filledSteps)
```

```
## [1] 10766.19
```

By imputing the values for the steps, the mean and the median values of the data both increased. 

Are there differences in activity patterns between weekdays and weekends?


```r
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)

averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```

![plot of chunk unnamed-chunk-5](https://github.com/bridey397/RepData_PeerAssessment1/blob/master/unnamed-chunk-5-1.png)


#Reproducible Research: Peer Assessment 1

##Loading and preprocessing the data


```r
unzip("activity.zip")
data <- read.csv("activity.csv")
```
#What is the mean total number of steps taken per day?

```r
StepsPerDay <- tapply(data$steps, data$date, sum, na.rm = T)
hist(StepsPerDay, ylim = range(0:30), main = "Total number of steps per day", xlab = "Steps per day", ylab = "Frequency")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
print(mean_StepsPerDay <- mean(StepsPerDay))
```

```
## [1] 9354
```

```r
print(median_StepsPerDay <- median(StepsPerDay))
```

```
## [1] 10395
```
##What is the average daily activity pattern?

```r
StepsPerInterval <- tapply(data$steps, as.factor(data$interval), mean, na.rm = T)

plot(levels(as.factor(data$interval)), StepsPerInterval, type = "l", xlab = "5 Min interval (hhmm)", ylab = "Average number of steps", main = "Average number of steps per interval", ylim = range(0:250), xlim = range(0:2400))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

##Which 5-minute interval (in format (h)hmm), on average across all the days in the dataset, contains the maximum number of steps? This is calculated as follows:

```r
names(StepsPerInterval)[which(StepsPerInterval == max(StepsPerInterval))]
```

```
## [1] "835"
```
##Replace missing values


```r
print(NumberOfNAs <- sum(is.na(data$steps)))
```

```
## [1] 2304
```

```r
source("replaceNA.R")
replaceNA()

StepsPerDay_noNA <- tapply(data_noNA$steps, data_noNA$date, sum, na.rm = T)
par(mfcol = c(1, 2))

hist(StepsPerDay, ylim = range(0:40), main = "Total number of steps per day (with NA)", xlab = "Steps per day", ylab = "Frequency")
hist(StepsPerDay_noNA, ylim = range(0:40), main = "Total number of steps per day (NA's replaced)", xlab = "Steps per day", ylab = "Frequency")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

```r
print(paste("Mean:", (mean_StepsPerDay_noNA <- mean(StepsPerDay_noNA)), sep = " "))
```

```
## [1] "Mean: 10766.1886792453"
```

```r
print(paste("Median:", (median_StepsPerDay_noNA <- median(StepsPerDay_noNA)), sep = " "))
```

```
## [1] "Median: 10766.1886792453"
```
##Are there differences in activity patterns between weekdays and weekends?


```r
data_noNA$date <- as.character(data_noNA$date)
data_noNA$date <- as.Date(data_noNA$date)

weekdays <- (weekdays(data_noNA$date))

data_noNA$weekdays <- as.factor(weekdays)

levels(data_noNA$weekdays) <- c("weekday", "weekday", "weekday", "weekday", "weekday","weekend", "weekend")

which_weekdays <- data_noNA$weekdays == "weekday"  ## subset weekday rows
which_weekends <- data_noNA$weekdays == "weekend"  ## subset weekend rows

StepsPerInterval_weekday <- tapply(data_noNA$steps[which_weekdays], as.factor(data$interval[which_weekdays]),mean, na.rm = T)
StepsPerInterval_weekend <- tapply(data_noNA$steps[which_weekends], as.factor(data$interval[which_weekends]),mean, na.rm = T)

par(mfcol = c(2, 1))

plot(levels(as.factor(data_noNA$interval)), StepsPerInterval_weekday, type = "l", xlab = "5 Min interval (hhmm)", ylab = "Average number of steps", main = "Average number of steps per weekday interval", ylim = range(0:250), xlim = range(0:2400))

plot(levels(as.factor(data_noNA$interval)), StepsPerInterval_weekend, type = "l", xlab = "5 Min interval (hhmm)", ylab = "Average number of steps", main = "Average number of steps per weekend interval", ylim = range(0:250), xlim = range(0:2400))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

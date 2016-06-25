#Reproducible Research: Peer Assessment 1

## Loading and processing the Data
- Loading the data from the downloaded csv file

```r
activity <- read.csv("~/Coursera/activity.csv", colClasses = c("numeric", "Date", "numeric"))
attach(activity)
```

```
## The following objects are masked from activity (pos = 3):
## 
##     date, interval, steps
```

```
## The following objects are masked from activity (pos = 10):
## 
##     date, interval, steps
```

- Loading the required packages

```r
library(knitr)
library(lattice)
library(lubridate)
library (ggplot2)
library (reshape2)
library(plyr)
library(dplyr)
```

- Process the data into a format suitable for analysis

```r
activity$date <- ymd(activity$date)
```

## 1. What is the mean total number of steps taken per day?

### For this part of the assignment, you can ignore the missing values in the dataset.

- Calculate the total number of steps taken per day


```r
steps_taken <- aggregate(steps ~ date, activity, sum)
```

### If you do not understand the difference between a histogram and a barplot, research the difference between them. 
- Make a histogram of the total number of steps taken each day.

```r
hist(steps_taken$steps, breaks = 25, main = "Total number of steps taken each day", xlab = "Steps", col = "orange")
```

<img src="PA1_template_files/figure-html/hist steps-1.png" width="672" />

- Calculate and report the mean and median of the total number of steps taken per day

```r
stepmean <- mean(steps_taken$steps)
stepmedian <- median(steps_taken$steps)
```
- Mean

```r
stepmean
```

```
## [1] 10766.19
```
- Median

```r
stepmedian
```

```
## [1] 10765
```


## 2. What is the average daily activity pattern?

- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
daily <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
dailymelt <- melt (daily)
names(dailymelt) <- c("interval", "avg")
nrow (dailymelt)
```

```
## [1] 288
```

```r
plot(avg ~ interval, data = dailymelt, type = "l", main = "Daily Activity Pattern")
```

<img src="PA1_template_files/figure-html/pattern-1.png" width="672" />


- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
dailymelt[dailymelt$avg == max(dailymelt$avg), ]
```

```
##     interval      avg
## 104      835 206.1698
```

## 3. Imputing missing values

### Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum (is.na (activity))
```

```
## [1] 2304
```

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

- Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
mean(activity$steps, na.rm = TRUE)

merged_NAs <- activity
merged_NAs$steps [is.na(merged_NAs$steps)] <- mean(merged_NAs$steps, na.rm = TRUE)
colSums(is.na(merged_NAs))
```

```r
merged_NAs <- tapply(merged_NAs$steps, merged_NAs$date, sum)
melted <- melt(merged_NAs)
names(melted) <- c("Date", "Steps_taken")
```

- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
hist(melted$Steps_taken, main = "Total of Steps Taken per Day", xlab = "Total number of steps per day", 
     ylab = "Frequency", col = "blue", breaks = 25)
```

<img src="PA1_template_files/figure-html/hist with no missing values-1.png" width="672" />

## 4 Are there differences in activity patterns between weekdays and weekends?
### For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

- Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
merged_NAs <- activity
merged_NAs$steps [is.na(merged_NAs$steps)] <- mean(merged_NAs$steps, na.rm = TRUE)
colSums(is.na(merged_NAs))
```

```
##    steps     date interval 
##        0        0        0
```

```r
merged_NAs$weekdays <- weekdays(merged_NAs$date)
merged_NAs$weeks[(merged_NAs$weekdays == "Saturday" | merged_NAs$weekdays == "Sunday")] <- "weekend"
merged_NAs$weeks[!(merged_NAs$weekdays == "Saturday" | merged_NAs$weekdays == "Sunday")] <- "weekdays"

weeks_ <- ddply(merged_NAs, c("interval", "weeks"), function (x) apply(x[1], 2, mean))
```

- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
xyplot(steps ~ interval | weeks, data = weeks_, type = "l", xlab = "Interval", 
       ylab = "Number of steps", layout = c(1, 2))
```

<img src="PA1_template_files/figure-html/time series plot-1.png" width="672" />

rmarkdown::render("PA1_template.Rmd", clean=FALSE)



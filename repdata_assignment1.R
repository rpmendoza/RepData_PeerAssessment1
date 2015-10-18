## Author: Rolando Mendoza

## load dplyr package which will be used later
require(dplyr)


## Loading and preprocessing the data
unzip("repdata-data-activity.zip")
actDF <- read.csv(file="activity.csv", header=TRUE)
## convert "date" column to date format
actDF$date <- as.Date(actDF$date, format="%Y-%m-%d")



## What is mean total number of steps taken per day?
totalSteps <- aggregate(cbind(steps) ~ date, data=actDF, FUN=sum)
## create histogram (not a barplot!!) of the total number of steps taken each day
hist(totalSteps$steps, breaks=25, col="steel blue", xlab="Total Steps per Day", main="Histogram of Steps per Day")
## calculate and report the mean and median of the total # of steps taken per day
meanTotalSteps <- mean(totalSteps$steps, na.rm=TRUE)
medianTotalSteps <- median(totalSteps$steps, na.rm=TRUE)


## What is the average daily activity pattern?
interval <- aggregate(actDF$steps, by=list(interval=actDF$interval), na.rm=TRUE, FUN=mean)
## Make a time series plot (type="l") for the 5-min intervals (x-axis) and the average # of steps taken, 
## averaged across all days (y-axis)
plot(y=interval$x, x=interval$interval, col="steel blue", type="l", main="Daily Activity Pattern")
## Which 5-minute interval, in average across all the days, contains the max number of steps?
maxInterval <- interval[which.max(interval$x),]


## Imputing missing values
## Calculate and report the total # of missing values in the dataset (NA's)
totalNAs <- sum(is.na(actDF$steps))
## For the NA's enter the mean/median for the 5-minute interval(data fix)
meanTotalInterval <- tapply(actDF$steps, actDF$interval, mean, na.rm=TRUE)
## Create a new dataset with the NA's filled in
noNADF <- actDF
allNAs <- is.na(noNADF$steps)
noNADF$steps[allNAs] <- meanTotalInterval[as.character(noNADF$interval[allNAs])]
## Make a histogram of the total # of steps taken each day and then calculate the mean and median total # of steps
## taken per day.
totalStepsNoNA <- aggregate(cbind(steps) ~ date, data=noNADF, FUN=sum)
hist(totalStepsNoNA$steps, breaks=25, col="steel blue", xlab="Total Steps per Day", main="Histogram of Steps per Day")
## Do these values differ from the estimates from first part? What is the impact of imputting missing 
## data on the estimates of the total daily number of steps?
meanTotalStepsNoNA <- mean(totalStepsNoNA$steps, na.rm=TRUE)
medianTotalStepsNoNA <- median(totalStepsNoNA$steps, na.rm=TRUE)
meanDiff <- meanTotalStepsNoNA - meanTotalSteps
medianDiff <- medianTotalStepsNoNA - medianTotalSteps



## Are there differences in activity patterns between weekdays and weekends?
## hint: use weekdays() function. Use the filled-in dataset.
## Create a new factor variable in the dataset with two levels: weekday and weekend
noNADF <- mutate(noNADF, week=weekdays(date))
noNADF <- noNADF %>% mutate(typeOfWeek = ifelse(week=="Saturday" | week=="Sunday","weekend", "weekday"))
## Make a panel plot containing a time series plot (type="l") of the 5-min interval (x-axis) and the average
## number of steps taken, averaged accross all weekday days of weekend days (y-axis).
noNADFWeekend <- subset(noNADF, typeOfWeek == "weekend")
noNADFWeekday <- subset(noNADF, typeOfWeek == "weekday")
intervalWeekend <- aggregate(noNADFWeekend$steps, by=list(interval=noNADFWeekend$interval), na.rm=TRUE, FUN=mean)
intervalWeekday <- aggregate(noNADFWeekday$steps, by=list(interval=noNADFWeekday$interval), na.rm=TRUE, FUN=mean)
plot(y=intervalWeekend$x, x=intervalWeekend$interval, col="steel blue", type="l", main="Weekend Activity Pattern")
plot(y=intervalWeekday$x, x=intervalWeekday$interval, col="steel blue", type="l", main="Weekday Activity Pattern")

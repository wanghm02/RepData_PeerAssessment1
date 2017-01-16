# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(ggplot2)
library(xtable)
data<-read.csv("activity.csv")
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


## What is mean total number of steps taken per day?

```r
## rm NAs
cdata <- data[complete.cases(data),]
datastepsday<-aggregate(cdata$steps,by=list(days=cdata$date),sum)

hist(datastepsday$x,main="Histogram steps taken in a day",xlab="Steps taken in a date",breaks = 25)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

### calculate and report the mean and median total number of steps taken in a day

```r
        meansteps<-mean(datastepsday$x)
        mediansteps<-median(datastepsday$x)

        meansteps<-formatC(meansteps, digits = 1, format = "f")
        mediansteps<-formatC(mediansteps, digits = 1, format = "f")    
```

The mean is 10766.2 and medium is 10765.0

## What is the average daily activity pattern
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the 
average number of steps taken, averaged across all days (y-axis)


```r
   stepsinterval<-aggregate(cdata$steps,by=list(interval=cdata$interval),mean)
   head(stepsinterval)
```

```
##   interval         x
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
   ggplot(stepsinterval,aes(interval,x))+geom_line()+ylab("Average steps")+xlab("5 minute interval over 24 hours")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
Which 5-minute interval, on average across all the days in the dataset, contains the 
maximum number of steps?


```r
   nummaxsteps<-stepsinterval[which.max(stepsinterval$x),1]
   maxsteps<-stepsinterval[which.max(stepsinterval$x),2]
```
At the 5-minute interval from 835 to 840, the average number of steps is the maximum with the number of steps of 206.1698113 

## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
        missing<-nrow(data)-nrow(cdata)
```

There are 2304 rows missing informaiton.

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

*I used the mean for a particular interval to fill in the missing NA values. *


```r
   rowNAs<-which(is.na(data$steps))
   for (i in rowNAs) {
        datarow<-data[i,]
        avg<-stepsinterval[which(stepsinterval$interval==datarow$interval),]$x
        data$steps[i]<-avg
   }

        datastepsday<-aggregate(data$steps,by=list(days=data$date),sum)
        hist(datastepsday$x,main="Histogram steps taken in a day",xlab="Steps taken in a date",breaks = 25)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
        meansteps<-mean(datastepsday$x)
        mediansteps<-median(datastepsday$x)        

        meansteps<-formatC(meansteps, digits = 1, format = "f")
        mediansteps<-formatC(mediansteps, digits = 1, format = "f")        
```
The mean is 10766.2 and medium is 10766.2

## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day


```r
        data$day<-weekdays(as.Date(data$date))
        head(data$day)
```

```
## [1] "Monday" "Monday" "Monday" "Monday" "Monday" "Monday"
```

```r
##        weekdays<-c("Monday","Tuesday","Wednesday","Thursday","Friday")
        data$day[data$day=="Monday"] <-"Weekday"
        data$day[data$day=="Tuesday"] <-"Weekday"
        data$day[data$day=="Wednesday"] <-"Weekday"
        data$day[data$day=="Thursday"] <-"Weekday"
        data$day[data$day=="Friday"] <-"Weekday"
        
##        weekends<-c("Saturday","Sunday")
        data$day[data$day=="Saturday"] <-"Weekend"
        data$day[data$day=="Sunday"] <-"Weekend"        
        
        data<-transform(data,day=factor(day))
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
        datastepsday<-aggregate(steps~interval+day,data=data,mean)
        g<-ggplot(datastepsday,aes(x=interval,y=steps))
        g+geom_line()+facet_grid(day~.)+xlab("Interval")+ylab("Number of steps")+ggtitle("Number of steps on weekdays vs weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

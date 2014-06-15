Peer Assessment 1: Reproducible Research
========================================================

Loading the data:


```r
activity<- read.csv("activity.csv", as.is=T)
```

Calculate mean total number of steps taken per day. For this part of the 
assignment, you can ignore the missing values in the dataset.
Make a histogram of the total number of steps taken each day



```r
totalspd<-tapply(activity$steps, activity$date, sum)
hist(totalspd)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

Calculate and report the mean and median total number of steps taken per day


```r
totalspd_mean<-tapply(activity$steps, activity$date, mean, na.rm=T)

totalspd_median<-tapply(activity$steps, activity$date, median, na.rm=T)
```

What is the average daily activity pattern?

```r
steps_per_int_mean<-tapply(activity$steps, activity$interval, mean, na.rm=T)
```


Which 5-minute interval, on average across all the days in the dataset, 
contains the maximum number of steps?

```r
max_steps <- which.max(steps_per_int_mean)
```
Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
#calculate and report how many rows have NAs
x <-complete.cases(activity)
total_complete <- sum(x)
total_incomplete <- nrow(activity)-total_complete
#print(total_incomplete)

#fill in NAs with interval average and create a new dataset
activity2 <-activity
naactivity <- is.na(activity2[,1])

for (i in 1:61){
       # print(i)
#        print(steps_per_int_mean[i])
        plan <- rep(FALSE,nrow(activity2))
        plan[(288*(i-1)+1):(288*i)] <- naactivity[(288*(i-1)+1):(288*i)]
        activity2[plan,1] <- steps_per_int_mean   
        
}
#print(activity2)

#calculate and report mean and median total number of steps taken per day

totalspd_mean2<-tapply(activity2$steps, activity2$date, mean, na.rm=T)
#print(totalspd_mean2)
totalspd_median2<-tapply(activity2$steps, activity2$date, median, na.rm=T)
#print(totalspd_median2)

#create new factor (weekday and weekend) for original dataset
dayname <-weekdays(as.Date(activity$date))
for (i in 1:nrow(activity)){
        if (dayname[i]=="Saturday"| dayname[i]=="Sunday"){
                dayname[i]<-"Weekend"
        }
        else {dayname[i]<-"Weekday"}
        
}
#print(dayname)

activity3<-cbind(activity,dayname=as.factor(dayname))
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


```r
plot(cars)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


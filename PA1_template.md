# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
#create connection to zip file
con<-unz("activity.zip","activity.csv")
# read csv from connection
data<-read.csv(con,na.strings = "NA",colClasses=c("numeric","Date","numeric"))
```

## What is mean total number of steps taken per day?

```r
library (dplyr,warn.conflicts=FALSE)
data_day<-data %>% group_by(date) %>% 
        summarise(steps.sum=sum(steps,na.rm = TRUE))

steps_mean=mean(data_day$steps.sum)
steps_median=median(data_day$steps.sum)
```

Mean total number of steps taken per day = **9354.2295082**  
Median total number of steps taken per day = **1.0395\times 10^{4}**

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?

setwd("D:/Documents and Settings/tnjunge/Documents/R/repdata%2Fdata%2Factivity")
##Code for reading in the dataset and/or processing the data
activity<-read.csv('D:/Documents and Settings/tnjunge/Documents/R/repdata%2Fdata%2Factivity/activity.csv')
##Code for pre- processing the data.Change the date into dateformat 
activity$date <- as.Date(activity$date)

##Question:Histogram of the total number of steps taken each day
##Step1:Calculate total number of steps taken each day
library(dplyr)
stepsperday <- activity %>% filter(!is.na(steps)) %>% group_by(date) %>% summarize(steps = sum(steps)) 
head(stepsperday)

##step2:Histogram of the total number of steps taken each day.n/b :Using ggplot
library(ggplot2)
hist(stepsperday$steps,main="Histogram of total steps per day",xlab = "Total steps per day" ,ylab = "Frequency",
     col="blue",border="green")

##Mean and median number of steps taken each day
meanstepperday<-mean(stepsperday$steps,na.rm = T)
medianstepsperday<-median(stepsperday$steps,na.rm = T)

##Time series plot of the average number of steps taken
##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
##and the average number of steps taken, averaged across all days (y-axis)
stepsper5min <- activity %>% filter(!is.na(steps)) %>% group_by(interval) %>% summarize(steps = mean(steps)) 

plot(stepsper5min$interval, stepsper5min$steps, type="l",xlab="interval",ylab="steps",col="blue")

##Which 5-minute interval, on average across all the days in the dataset,
##contains the maximum number of steps?
 stepsper5min[which.max(stepsper5min$steps),]

##Calculate and report the total number of missing values in the dataset
##(i.e. the total number of rows with NAs)
sum(is.na(activity$steps))

##Devise a strategy for filling in all of the missing values in the dataset.N/B we use the number of steps per 5min interval.
activity2 <- activity
Nas <- is.na(activity2$steps)
averageinterval<- tapply(activity2$steps, activity2$interval, mean, na.rm=TRUE )
activity2$steps[Nas] <- averageinterval[as.character(activity2$interval[Nas])]
sum(is.na(activity2$steps))

##Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day
stepsperday_activity2 <- activity2 %>% filter(!is.na(steps)) %>% group_by(date) %>% summarize(steps = sum(steps))
hist(stepsperday_activity2$steps,main="Histogram of total steps per day_activty2",xlab = "Total steps per day" ,ylab = "Frequency",
     col="blue",border="green")
##Mean and median number of steps taken each day/Activity2;No Nas
meanstepperday_activity2<-mean(stepsperday_activity2$steps,na.rm = T)
medianstepsperday_activity2<-median(stepsperday_activity2$steps,na.rm = T)

##The  mean of the dataset steps per day was the same with  Nas replaced or removed which was 10766.19
##The median was slightly different with dataset with Nas replaced at 10766.19 and with Nas removed at 10765

##Are there differences in activity patterns between weekdays and weekends?
activity2 <- mutate(activity2, weektype = ifelse(weekdays(activity2$date) == "Saturday" | weekdays(activity2$date) == "Sunday", "weekend", "weekday"))
activity2$weektype <- as.factor(activity2$weektype)
head(activity2)

interval_activity2 <- activity2 %>% group_by(interval, weektype) %>% summarise(steps = mean(steps))
panelplot <- ggplot(interval_activity2, aes(x=interval, y=steps, color = weektype)) +
  geom_line() +
  facet_wrap(~weektype, ncol = 1, nrow=2)
print(panelplot)

##the subject is more active durring the weekday than weekend

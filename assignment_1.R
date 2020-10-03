##Load Dataset
unzip(zipfile="repdata_data_activity.zip")
data <- read.csv("activity.csv")

##Histogram of steps by day
library(magrittr)
library(dplyr)
databydate <- data %>% select(date, steps) %>% group_by(date) %>% summarize(tsteps= sum(steps)) %>%na.omit()
hist(databydate$tsteps, xlab = "Total daily Steps",main="Histogram of Total Steps by day", breaks = 20)

##Calculate Mean and Median of Total steps by day
mean(databydate$tsteps)
median(databydate$tsteps)

##Time series Plot of avg no. of steps
library(ggplot2)
databyinterval <- data%>% select(interval, steps) %>% na.omit() %>% group_by(interval) %>% summarize(tsteps= mean(steps)) 
ggplot(databyinterval, aes(x=interval, y=tsteps))+ geom_line()

##5-minute interval 
databyinterval[which(databyinterval$tsteps== max(databyinterval$tsteps)),]

##Calculating missing values
missing <- is.na(data$steps)
# How many missing
table(missing)

##Replace each missing value with the mean value of its 5-minute interval
replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
meandata <- data%>% group_by(interval) %>% mutate(steps= replacewithmean(steps))
head(meandata)
FullSummedDataByDay <- aggregate(meandata$steps, by=list(meandata$date), sum)

##histogram of the total number of steps taken each day
names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"
head(FullSummedDataByDay,15)

summary(FullSummedDataByDay)

hist(FullSummedDataByDay$totalsteps, xlab = "Steps", ylab = "Frequency", main = "Total Daily Steps", breaks = 20)

##Compare the mean and median of Old and New data
oldmean <- mean(databydate$tsteps, na.rm = TRUE)
newmean <- mean(FullSummedDataByDay$totalsteps)

# Old mean and New mean
oldmean
newmean

oldmedian <- median(databydate$tsteps, na.rm = TRUE)
newmedian <- median(FullSummedDataByDay$totalsteps)

# Old median and New median
oldmedian
newmedian

##differences in activity patterns between weekdays and weekends
meandata$date <- as.Date(meandata$date)
meandata$weekday <- weekdays(meandata$date)
meandata$weekend <- ifelse(meandata$weekday=="Saturday" | meandata$weekday=="Sunday", "Weekend", "Weekday" )
library(ggplot2)
meandataweekendweekday <- aggregate(meandata$steps , by= list(meandata$weekend, meandata$interval), na.omit(mean))
names(meandataweekendweekday) <- c("weekend", "interval", "steps")

ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
  facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
  ggtitle("Comparison of Average Number of Steps in Each Interval")
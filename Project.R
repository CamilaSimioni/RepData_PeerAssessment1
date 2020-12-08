
library("data.table")
#Set a file

setwd("C:/Users/Hand 07/Desktop/Curso/Curso_5/Week_2/Project/RepData_PeerAssessment1")

arquivo <- "file:///C:/Users/Hand%2007/Downloads/repdata_data_activity.zip"

download.file(arquivo, destfile = "./repdata_data_activity.zip", method="curl")

unzip(zipfile = "./repdata_data_activity.zip", exdir = "./Pasta_2")

data <- read.csv("./Pasta_2/activity.csv", header = TRUE)
head(data)

# What is mean total number of steps taken per day?

#1. Calculate total number of steps taken each day

library(magrittr)
library(dplyr)

date <- data %>% select(date, steps) %>% group_by(date) %>% summarize(tsteps= sum(steps)) %>%na.omit()
hist(date$tsteps, xlab = "Total daily Steps",main="Histogram of Total Steps by day", breaks = 20)


#3. Calculate and report the mean and median of the total number of steps taken per day

mean(date$tsteps)

median(date$tsteps)

#what is the average daily activity pattern?

#1. Time series plot

library(ggplot2)

interval <- data%>% select(interval, steps) %>% na.omit() %>% group_by(interval) %>% summarize(tsteps= mean(steps)) 
ggplot(interval, aes(x=interval, y=tsteps))+ geom_line()


#2.The 5-minute interval that, on average, contains the maximum number of steps

interval[which(interval$tsteps== max(interval$tsteps)),]


#   Imputing missing values

#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


# generate listing of NA's
missingVals <- sum(is.na(data))

missingVals


#2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

library(magrittr)
library(dplyr)

replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
m_data <- data%>% group_by(interval) %>% mutate(steps= replacewithmean(steps))
head(m_data)

#4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day

FullSummedDataByDay <- aggregate(m_data$steps, by=list(m_data$date), sum)

names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"
head(FullSummedDataByDay,15)


summary(FullSummedDataByDay)

hist(FullSummedDataByDay$totalsteps, xlab = "Steps", ylab = "Frequency", main = "Total Daily Steps", breaks = 20)


oldmean <- mean(date$tsteps, na.rm = TRUE)
newmean <- mean(FullSummedDataByDay$totalsteps)
# Old mean and New mean
oldmean


newmean


oldmedian <- median(date$tsteps, na.rm = TRUE)
newmedian <- median(FullSummedDataByDay$totalsteps)
# Old median and New median
oldmedian

newmedian

#Are there differences in activity patterns between weekdays and weekends?

m_data$date <- as.Date(m_data$date)
m_data$weekday <- weekdays(m_data$date)
m_data$weekend <- ifelse(m_data$weekday=="Saturday" | m_data$weekday=="Sunday", "Weekend", "Weekday" )


library(ggplot2)
dataweek <- aggregate(m_data$steps , by= list(m_data$weekend, m_data$interval), na.omit(mean))
names(dataweek) <- c("weekend", "interval", "steps")

ggplot(dataweek, aes(x=interval, y=steps, color=weekend)) + geom_line()+
  facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
  ggtitle("Comparison of Average Number of Steps in Each Interval")


#Load the libraries
library(ggplot2)
#install.packages("Hmisc")
library(Hmisc)

##########################################################
# Loading and preprocessing the data
#########################################################
setwd("D:\\Certificering\\Coursera Data Science Specialization (R)\\5. Coursera Reproducible research\\Week 2\\Assignment")

# Clear the environment
rm(list = ls())

Activity <- read.csv("activity.csv", head = TRUE, sep = ",")

#Data preparation
Activity$date <- as.Date(Activity$date)
Activity$steps <- as.numeric(Activity$steps)

## Remove the NA.
ActivityWNA <- Activity[complete.cases(Activity),]

# some data exploration
str(ActivityWNA)
dim(ActivityWNA) 
summary(ActivityWNA)
head(ActivityWNA, 10)
names(ActivityWNA)
pairs(ActivityWNA)

ActivityWNAPerDay <- aggregate(steps ~ date, ActivityWNA, sum)

## add descriptive variable names
names(ActivityWNAPerDay)[2] <- "Sumsteps"

##########################################################
# What is mean total number of steps taken per day?
#########################################################
ActivityByDayMean <- mean(ActivityWNAPerDay$Sumsteps)
ActivityByDayMedian <- median(ActivityWNAPerDay$Sumsteps)

print(paste("The Mean number of steps per day is : ", round(ActivityByDayMean,1)))
print(paste("The Median number of steps per day is :",round(ActivityByDayMedian,1)))

hist(
  ActivityWNAPerDay$Sumsteps,
  main = "Total number of steps taken aach day",
  col = "green", 
  xlab = "steps",
  breaks = 25
)

abline(v = mean(ActivityByDayMean), lty = 5, col = "blue")
abline(v = median(ActivityByDayMedian), lty = 5, col = "red")  

##########################################################
# What is the average daily activity pattern?
#########################################################
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)

ActivityWNAPerinterval <- aggregate(steps ~ interval, ActivityWNA, sum)
head(ActivityWNAPerinterval)

plot(ActivityWNAPerinterval, type = "l", col = "red", 
     main ="Average number of steps interval all days",
     xlab = "Interval per 5 minutes",  
     ylab = "Mean number of steps")

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxIntervalSteps <-ActivityWNAPerinterval[ActivityWNAPerinterval == max(ActivityWNAPerinterval$steps)]
ActivityWNAPerinterval[ActivityWNAPerinterval$steps == maxIntervalSteps,]

##########################################################
# Imputing missing values
#########################################################

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
NumberOfNAs <- sum(is.na(Activity$steps))
NumberOfNAs

# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, 
# or the mean for that 5-minute interval, etc.


# Create a new dataset that is equal to the original dataset but with the missing data filled in.
ActivityImputed <- Activity
ActivityImputed$steps <- impute(Activity$steps, fun=mean)

# Make a histogram of the total number of steps taken each day and Calculate and report the mean 
# and median total number of steps taken per day. Do these values differ from the estimates 
# from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
ActivityImputedinterval <- aggregate(steps ~ interval, ActivityImputed, sum)
head(ActivityImputedinterval)

summary(Activity)
summary(ActivityImputed)
#NA has disappeared, mean is the the same.

hist(
  Activity$steps,
  main = "Total number of steps taken aach day",
  col = "green", 
  xlab = "steps",
  breaks = 25
)

hist(
  ActivityImputedinterval$steps,
  main = "Total number of steps taken aach day",
  col = "green", 
  xlab = "steps",
  breaks = 25
)


##########################################################
# Are there differences in activity patterns between weekdays and weekends?
#########################################################
#Add column day 
ActivityImputed$day<-weekdays(ActivityImputed$date) 
ActivityImputed


ActivityImputed <- transform(ActivityImputed, day = ifelse(day %in% c("zondag","zaterdag"), "Weekend", "Week"))

head (ActivityImputed, 10000)
summary(ActivityImputed)

ActivityImputed <- aggregate(steps ~ interval + day, data=ActivityImputed, mean)

ggplot(ActivityImputed, aes(interval, steps, color = day)) + 
  geom_line() + 
  facet_grid(day ~ .) +
  xlab("Interval") + 
  ylab("Steps")


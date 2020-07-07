## ---- echo=TRUE---------------------------------------------------------------
unlink("data", recursive=TRUE)
dir.create("data")
unzip("activity.zip", exdir="data")

activity <- read.csv(file.path("data", "activity.csv"))

dim(activity)
head(activity)


## ---- echo=TRUE---------------------------------------------------------------
activityWithoutNa <- subset(activity, !is.na(activity$steps))
dim(activityWithoutNa)


## ---- echo=TRUE---------------------------------------------------------------
totalStepsPerDay <- tapply(activityWithoutNa$steps, activityWithoutNa$date, sum)

totalStepsPerDay


## ---- echo=TRUE---------------------------------------------------------------

barplot(totalStepsPerDay, xlab = "Date", ylab = "Total steps per day", main = "Total number of steps per day")


## ---- echo=TRUE---------------------------------------------------------------
meanStepsPerDay <- mean(totalStepsPerDay)
medianStepsPerDay <- median(totalStepsPerDay)

meanStepsPerDay
medianStepsPerDay


## ---- echo=TRUE---------------------------------------------------------------
meanStepsPerInterval <- tapply(activityWithoutNa$steps, activityWithoutNa$interval, mean)

plot(x = names(meanStepsPerInterval), y = meanStepsPerInterval, type="l", xlab = "hour of day", ylab="average steps taken", main = "Average steps / time interval (5 minutes)")


## ---- echo=TRUE---------------------------------------------------------------
whichIsMax <- which.max(meanStepsPerInterval)

names(whichIsMax)


## ---- echo=TRUE---------------------------------------------------------------
numberOfNa <- sum(is.na(activity$steps))

numberOfNa


## ---- echo=TRUE---------------------------------------------------------------
suppressWarnings(library(dplyr))
meanDataFrame <- data.frame(interval=names(meanStepsPerInterval), mean=meanStepsPerInterval)
imputedActivity <- merge(x=activity, y=meanDataFrame) %>% mutate(steps = ifelse(is.na(steps), mean, steps))

dim(imputedActivity)
head(imputedActivity)



## ---- echo=TRUE---------------------------------------------------------------
imputedTotalStepsPerDay <- tapply(activityWithoutNa$steps, activityWithoutNa$date, sum)

barplot(imputedTotalStepsPerDay, xlab = "Date", ylab = "Total steps per day", main = "Total number of steps per day")


## ---- echo=TRUE---------------------------------------------------------------
meanStepsPerDay <- mean(imputedTotalStepsPerDay)
medianStepsPerDay <- median(imputedTotalStepsPerDay)
meanStepsPerDay
medianStepsPerDay


## ---- echo=TRUE---------------------------------------------------------------
print("table here");


## ---- echo=TRUE---------------------------------------------------------------
iaWithFactors <- imputedActivity %>% mutate(typeOfDay := as.factor(ifelse(as.POSIXlt(as.Date(date))$wday %in% c(0,6), "weekend", "weekday")))

head(iaWithFactors)

head(iaWithFactors$typeOfDay)


imputedTotalStepsPerDay <- tapply(activityWithoutNa$steps, activityWithoutNa$date, sum)


## ---- echo=TRUE---------------------------------------------------------------
suppressWarnings(library(ggplot2))


---
output:
  pdf_document: default
  html_document: default
  word_document: default
---
# Reproducible Research week 2: Assesment 1


## Read the data
```{r load data, echo=TRUE}
activitydata <- read.csv("activity.csv")

```

## What is mean total number of steps taken per day?
```{r mean, echo=TRUE}
library(dplyr)
library(ggplot2)
mean(activitydata$steps, na.rm = TRUE)

perday <- activitydata %>%
  group_by(date)%>%
  summarize(totalsteps = sum(steps))%>%
  na.omit

hist(perday$totalsteps, col = "yellow", main = "Total steps per day", xlab = "Total daily Steps", breaks = 20)

mean(na.omit(perday$totalsteps))
median(na.omit(perday$totalsteps))
```

## What is the average daily activity pattern?
```{r daily activity, echo=TRUE}
averagedailyact <- activitydata %>%
  select(interval, steps)%>%
  na.omit()%>%
  group_by(interval)%>%
  summarise(totalsteps = sum(steps))

ggplot(averagedailyact, aes(x = interval, y = totalsteps)) + 
  geom_line(col = "red") +
  ggtitle("Average daily activity pattern")

averagedailyact[which(averagedailyact$totalsteps == max(averagedailyact$totalsteps)),]
```

## Imputing missing values
```{r missing values, echo=TRUE}
missingValues <- sum(is.na(activitydata))
missingValues

replacewithMean <- function(x)
  replace(x, is.na(x), mean(x, na.rm = TRUE))

meanData <- activitydata%>% 
  group_by(interval)%>%
  mutate(steps = replacewithMean(steps))


perdaymeanData <- meanData %>%
  group_by(date)%>%
  summarize(totalsteps = sum(steps))

hist(perdaymeanData$totalsteps, col = "yellow", main = "Total steps per day", xlab = "Total daily Steps", breaks = 20)

summary(perdaymeanData)
mean(na.omit(perdaymeanData$totalsteps))
median(na.omit(perdaymeanData$totalsteps))
```

## Are there differences in activity patterns between weekdays and weekends?
```{r weekdays weekends, echo=TRUE}
meanData$date <- as.Date(meanData$date)
meanData$weekday <- weekdays(meanData$date)
meanData$weekend <- ifelse(meanData$weekday =="zaterdag" | 
                             meanData$weekday =="zondag", "Weekend", "Weekday")


meandataweekendweekday <- aggregate(meanData$steps , by= list(meanData$weekend, meanData$interval), na.omit(mean))
#give column names
names(meandataweekendweekday) <- c("weekend", "interval", "steps")

ggplot(meandataweekendweekday, aes(x=interval, y = steps, color = weekend)) + geom_line()+
  facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
  ggtitle("Comparison of Average Number of Steps in Each Interval")
```

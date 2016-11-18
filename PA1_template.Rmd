---
output: html_document
---
Counting Steps
=================================
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)
```


## Synopsis
In this project, we are analyzing data on step counts, finding the mean steps per day and over 5-minute intervals throughout many days. Finally, the average steps taken over 5-minute intervals on the weekends compared to the weekdays is observed.

## Loading the Data
```{r}
data<- read.table("activity.csv", sep=",",head=TRUE, na.strings="NA")
data$date<-as.Date(data$date,"%m/%d/%Y")
head(data)
dim(data)

nonas<-data[!is.na(data$steps),]
str(nonas)
```


## Mean Number of Steps Taken
```{r}
#Total Steps Per Day
totalSteps<- nonas%>%  
group_by(date)%>%
        summarise(tSteps = sum(steps))
totalSteps
```

```{r}
#Histogram
ggplot(totalSteps, aes(x = tSteps)) +
        geom_histogram(fill = "red") +
        labs(title = "Steps Per Day", x = "Total Steps", y = "Frequency")
```

```{r}
mean(totalSteps$tSteps)
median(totalSteps$tSteps)
```


## Average Daily Activity Pattern

```{r}
#Find average number of steps per interval
eachFive<- nonas%>%
        group_by(interval)%>%
        summarize(avg = mean(steps))
eachFive

g<-ggplot(eachFive,aes(x=interval,y=avg))
g<-g+geom_line(color="red",size=1)
g<-g+labs(title="Average Steps Each 5 min.",x="Interval", y="Average Steps")
g
```

```{r}
#Calculate maximum number of steps in an interval
maxSteps<-max(eachFive$avg)
maxSteps
eachFive[eachFive$avg==maxSteps,1]
```

The maximum number of steps in an interval was 206, and this occurred on the 835th interval.

##Imputing Missing Values
```{r}
#Number of NA's
sum(is.na(data$steps))

#Filling in the NA's with mean for the interval
replace<-data
nas<- is.na(data$steps)
avgSteps<- tapply(replace$steps, replace$interval, mean, na.rm=TRUE, simplify = TRUE)
replace$steps[nas] <- avgSteps[as.character(replace$interval[nas])]
head(replace)
```
```{r}
newData<- replace%>%
        group_by(date)%>%
        summarise(dailySteps = sum(steps, na.rm=TRUE))
head(newData)
```

```{r}
ggplot(newData, aes(x = dailySteps)) +
        geom_histogram(fill = "green") +
        labs(title = "Steps Per Day", x = "Total Steps", y = "Frequency")
```

```{r}
#Finding mean and median of this updated dataset
mean(newData$dailySteps)
median(newData$dailySteps)
```

##Activity Patterns on Weekdays vs. Weekends

```{r}
#Creating new factor variable with 2 levels: Weekend and Weekday
dataWeek<- replace%>%
        mutate(weektype= ifelse(weekdays(newData$date)=="Saturday" | weekdays(replace$date)=="Sunday", "Weekend", "Weekday"))
head(dataWeek)
```
```{r}
interval2<- dataWeek%>%
    group_by(interval, weektype)%>%
    summarise(avg2 = mean(steps, na.rm=TRUE))
head(interval2)

#Plotting the dataset by Weekday
gg<-ggplot(interval2,aes(x=interval, y=avg2))
gg<-gg+geom_line()
gg<-gg+labs(title = "Avg. Steps During Weekend/Weekday", x = "Interval", y = "Steps")
gg<-gg+facet_wrap(~weektype, ncol = 1, nrow=2)
gg
```
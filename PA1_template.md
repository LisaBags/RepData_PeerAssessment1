R Markdown Course Project - Reproducible Research week 2
========================================================
## Loading and Pre-processing the Data

The working directory is set to the folder containing the data, which has been extracted to the file 'activity.csv'.

```{r}
setwd("C:/Users/Lisa/Desktop/data science/Activity")
```

The csv data file is read into the working directory.  The second column's values are converted to date class, and the other 2 remain their original class.

```{r}
activity <- read.csv("activity.csv", colClasses= c(NA, "Date", NA))
```

## Mean number of steps taken per day
For this part of the analysis, I have ignored missing values (NAs).

1. Calculate the total number of steps taken per day
```{r}
steps_day <- aggregate(steps~date, data=activity, FUN = sum)
```

2. Create a histogram of the data using ggplot2 package
```{r}
library(ggplot2)
qplot(steps_day$steps, geom="histogram", binwidth=1000, main= "Histogram of number of steps taken per day for 2 months", xlab="steps per day", ylab="frequency")
```

3. Calculate and report the mean and median of the data
```{r}
mean_steps <- mean(steps_day$steps, na.rm=TRUE)
median_steps <- median(steps_day$steps, na.rm=TRUE)
mean_steps
median_steps
```
The mean number of steps is 10766.19 and the median number of steps is 10765
        
## Average Daily Activity Pattern
For this part of the analysis, I made a time series plot of each 5-minute interval and the average number of steps taken across all days.
1. Calculate the average number of steps taken during each 5 minute interval and create a time-series plot
```{r}
each_interval <- aggregate(steps ~ interval, data=activity, FUN = mean)
qplot(each_interval$interval, each_interval$steps, geom="line", xlab="interval", ylab="steps", main="Average number of steps taken during each 5 minute interval")
```

2. Which interval contains the maximum number of steps?
```{r}
each_interval[which.max(each_interval$steps), ]
```
Interval 835 contains the maximum number of steps, which is 206.

## Imputing Missing Values
Missing values can introduce bias into a dataset.

1. First, I'll calculate the number of rows with missing data (NA)
```{r}
sum(is.na(activity$steps))
```
There are 2304 rows with missing data

2. The days with missing data (NA) will be filled in with the mean number of steps taken during that same interval on the other days (as shown in the data table, each_interval)

3. Here, I've created a new dataset called 'full_activity', which is the same as the original 'activity' dataset, except the missing NA values have been filled in (as described above)
```{r}
full_activity <- activity
full_activity$steps[is.na(full_activity$steps)] <- each_interval$steps[match(full_activity$interval, each_interval$interval)][which(is.na(full_activity$steps))]
```

4. Now, I use the new dataset to recalculate the mean number of steps taken per day, saved as 'full_steps' and plot a histogram of this new filled in data
```{r}
full_steps <- aggregate(steps~date, data=full_activity, FUN = sum)
qplot(full_steps$steps, geom="histogram", binwidth=1000, main= "Histogram of number of steps taken per day with imputed NA values", xlab="steps per day", ylab="frequency")
```

Here's the new mean and median number of steps taken per day with the filled-in dataset  
```{r}
mean_full_steps <- mean(full_steps$steps)
median_full_steps <- median(full_steps$steps)
mean_full_steps
median_full_steps
```
The mean and median number of steps from the filled in data set is the same at 10766.19
The mean values do not differ between the missing dataset and the filled-in dataset, however, the median is slightly lower for the missing dataset because less values were included in this calculation and the median number in the original dataset is an integer. 
These data show that the impact is minimal to null and imputing data with the average value of similarly calculated data do not impact the results
        
## Differences in Activity Paterns between Weekdays and Weekends

1. For this part of the analysis, I'll duplicate 'full_activity' and add a factor column which will categorize each date as 'weekday' or 'weekend' 
```{r}
library(plyr)
full_days <- mutate(full_activity, wd=weekdays(full_activity$date))
full_days$wd <- ifelse(full_days$wd %in% c("Saturday","Sunday"), "weekend", "weekday")
full_days$wd <- as.factor(full_days$wd)
```

2. Next, I'll plot 2 time-series plots which show the average number of steps taken every 5 minutes on weekdays vs. weekends during a 2 month period.

First, I generate a dataset from this data called days_interval, then plot weekday vs. weekend data
```{r}
days_interval <- aggregate(steps ~ interval+wd, data=full_days, FUN = mean)
qplot(interval, steps, data=days_interval, facets= ~ wd, geom="line", xlab="interval", ylab="steps", main="Mean number of steps taken on weekdays vs. weekends for each interval")
```

The data suggest a difference in the average number of steps taken on weekdays vs. weekends



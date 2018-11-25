---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Load relevant libraries

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(reshape)
```

```
## 
## Attaching package: 'reshape'
```

```
## The following object is masked from 'package:dplyr':
## 
##     rename
```

```
## The following object is masked from 'package:lubridate':
## 
##     stamp
```

```r
library(ggplot2)
```


## Loading the data


```r
setwd("/Users/florianaubert/Documents/Coursera/Class5 - Reproducible Research")
data <- read.csv("activity.csv")
datadf <- as.data.frame(data)
datadf$date <- ymd(datadf$date)
str(datadf)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## Question 1: What is mean total number of steps taken per day?

### Prepare dataset

```r
data_1 <- datadf %>% 
    select(date, steps) %>% 
    group_by(date) %>% 
    summarise(total_steps = sum(steps, na.rm = TRUE)) %>% 
    ungroup
```

### Create histogram

```r
hist(data_1$total_steps,
     main = "Histogram of total steps per day",
     xlab = "Total steps per day")
```

![](PA1_template_files/figure-html/histogram-1.png)<!-- -->

### Compute mean and median steps per day

```r
mean_steps <- as.integer(round(mean(data_1$total_steps),0))
median_steps <- as.integer(round(median(data_1$total_steps),0))
```
On average, the subject has been doing 9354 per day. But the median was 10395.

## Question 2: What is the average daily activity pattern?

### Prepare dataset

```r
data_2 <- datadf %>% 
    select(interval, steps) %>% 
    group_by(interval) %>% 
    summarise(total_steps = sum(steps, na.rm = TRUE),
              mean_steps = mean(steps, na.rm = TRUE)) %>% 
    ungroup
```

### Create time-serie chart

```r
with(data_2,
     plot(
         interval,
         mean_steps,
         type = "l",
         ylab = "Average number of steps (for all days)"))
```

![](PA1_template_files/figure-html/plot-1.png)<!-- -->

```r
highest_int_df <- data_2 %>% 
    filter(mean_steps == max(mean_steps))
highest_int <- as.integer(round(highest_int_df$interval),0)
highest_steps <- as.integer(round(highest_int_df$mean_steps),0)
```
On average across all the days in the dataset, interval 835 contains the maximum number of steps (206)

## Question 3: Imputing missing values

### Identifying missing values

```r
na_vector <- as.data.frame(is.na(data$steps))
colnames(na_vector) <- "is_na"
na_vector$occurence <- 1
na_summary <- na_vector %>% 
    group_by(is_na) %>% 
    summarise(count_na = sum(occurence)) %>% 
    ungroup
na_summary
```

```
## # A tibble: 2 x 2
##   is_na count_na
##   <lgl>    <dbl>
## 1 FALSE    15264
## 2 TRUE      2304
```

### Replace missing values with the mean for this interval

```r
# We know that data_2 has already the mean for each interval, so we will left join the main dataset on data_2 to bring in a new column the average for this interval
data_new <- data %>%
    left_join(data_2) %>% 
    mutate(steps_adj = ifelse(is.na(steps),
                              mean_steps,
                              steps)) %>% 
    select(date, interval, steps_adj)
```

```
## Joining, by = "interval"
```

```r
# Compute total steps per day
data_new_1 <- data_new %>%
    group_by(date) %>% 
    summarise(total_steps_adj = sum(steps_adj))
```


### Create histogram

```r
hist(data_new_1$total_steps_adj,
     main = paste("Histogram of total steps per day","(adjusted for interval mean when NA)",sep="\n"),
     xlab = "Total adjusted steps per day")
```

![](PA1_template_files/figure-html/histogram2-1.png)<!-- -->

### Compute mean and median steps per day

```r
mean_steps_adj <- as.integer(round(mean(data_new_1$total_steps_adj),0))
median_steps_adj <- as.integer(round(median(data_new_1$total_steps_adj),0))
```
Adjusting NA values by replacing them with the average for the respective interval gives a new average of steps per day (10766 vs. 9354) and a new median of 10766 vs. 10395.

## Question 4: Are there differences in activity patterns between weekdays and weekends?

```r
data_new$date <- as.Date(data_new$date)
data_new$weekdays <- weekdays(data_new$date)
data_new <- data_new %>% 
    mutate(weekend = ifelse(weekdays == "Samedi" |
                                weekdays == "Dimanche",
                            "weekend",
                            "weekday"))
# Melt dataset on the variables weekend and weekday 
data_new_cast <- cast(data_new, interval ~ weekend, value = "steps_adj", fun = "mean")
data_new_melt <- melt(data_new_cast,
                      id.vars = "interval",
                      measure.vars = c("weekday", "weekend"))
```
### Create plot

```r
g <- ggplot(data_new_melt, aes(interval, value))
g + geom_line() + facet_grid(weekend~.) + labs(y="Average steps adjusted", title = "Comparing Average Steps per interval \n between weekends and weekdays")
```

![](PA1_template_files/figure-html/plot2-1.png)<!-- -->


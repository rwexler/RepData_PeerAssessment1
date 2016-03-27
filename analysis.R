analysis <- function() {
  
  library(lubridate)
  library(ggplot2)
  library(lattice)
  
  ##### Loading and preprocessing the data
  
  download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
                destfile = "activity.zip")
  unzip(zipfile = "activity.zip")
  activity <- read.csv(file = "activity.csv")
 
  ##### Mean total number of steps taken per day
  
  # Total number of steps taken per day
  total_steps_day <- aggregate(x = activity$steps, by = list(activity$date), FUN = sum, na.rm = TRUE)
  names(total_steps_day) <- c("Date", "TotalSteps")
  
  # Mean and median of the total number of steps taken per day
  mean_total_steps_day <- mean(x = total_steps_day$TotalSteps)
  median_total_steps_day <- median(x = total_steps_day$TotalSteps)
  cuts1 <- data.frame(Summary = "Mean", vals = mean_total_steps_day)
  cuts2 <- data.frame(Summary = "Median", vals = median_total_steps_day)
  cuts <- rbind(cuts1, cuts2)
  print(noquote("What are the mean and median of the total number of steps taken per day?"))
  write.table(cuts, row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  # Histogram of the total number of steps taken each day
  ggplot(data = total_steps_day, mapping = aes(total_steps_day$TotalSteps))+geom_histogram(binwidth = 1000)+
    labs(x="Total Number of Steps Taken Per Day", y="Count")+
    geom_vline(data=cuts, aes(xintercept=vals, linetype=Summary, colour = Summary), show.legend = TRUE)+
    theme(legend.position=c(0.8, 0.8))+ggsave("total_steps_day.png")
  
  ##### Average daily activity pattern
  
  # Time series plot of the 5-minute interval and the average number of steps taken, averaged across all days
  average_steps_interval <- aggregate(x = activity$steps, by = list(activity$interval), FUN = mean, na.rm = TRUE)
  names(average_steps_interval) <- c("Interval", "MeanSteps")
  png("time_series_plot.png")
  with(data = average_steps_interval, expr = plot(x = Interval, y = MeanSteps, type = "l", 
                                                  ylab = "Average Number of Steps Taken"))
  
  # 5-minute interval, on average across all the days in the dataset, which contains the maximum number of steps
  max_steps_interval = average_steps_interval$Interval[average_steps_interval$MeanSteps == 
                                                         max(average_steps_interval$MeanSteps)]
  abline(v = max_steps_interval, col = "blue", lwd = 2)
  legend(x = 200, y = 200, legend = max_steps_interval, bty = "n", text.col = "blue")
  dev.off()
  print(noquote("Which 5-minute interval, on average across all the days in the dataset,")) 
  print(noquote("contains the maximum number of steps?"))
  print(max_steps_interval)
  
  ##### Imputing missing values
  
  # Total number of missing values in the dataset
  number_missing_values <- sum(is.na(activity$steps))
  print(noquote("Total number of missing values in the dataset:"))
  print(number_missing_values)
 
  # Use the mean for that 5-minute interval and create a new dataset that is equal to the original dataset but with 
  # the missing data filled in.
  for (i in 1:nrow(activity)) {
    if (is.na(activity$steps[i]) == TRUE) {
      interval_temp = activity$interval[i]
      mean_steps <- average_steps_interval$MeanSteps[average_steps_interval$Interval == interval_temp]
      activity$steps[i] <- mean_steps
    }
  }
  
  # Total number of steps taken per day
  total_steps_day_imputed <- aggregate(x = activity$steps, by = list(activity$date), FUN = sum, na.rm = TRUE)
  names(total_steps_day_imputed) <- c("Date", "TotalSteps")
  
  # Mean and median of the total number of steps taken per day
  mean_total_steps_day_imputed <- mean(x = total_steps_day_imputed$TotalSteps)
  median_total_steps_day_imputed <- median(x = total_steps_day_imputed$TotalSteps)
  cuts1_imputed <- data.frame(Summary = "Mean", vals = mean_total_steps_day_imputed)
  cuts2_imputed <- data.frame(Summary = "Median", vals = median_total_steps_day_imputed)
  cuts_imputed <- rbind(cuts1_imputed, cuts2_imputed)
  print(noquote("What are the mean and median of the total number of steps taken per day after imputing"))
  print(noquote("missing values?"))
  write.table(cuts_imputed, row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  # Histogram of the total number of steps taken each day
  ggplot(data = total_steps_day_imputed, mapping = aes(total_steps_day_imputed$TotalSteps))+
    geom_histogram(binwidth = 1000)+
    labs(x="Total Number of Steps Taken Per Day", y="Count")+
    geom_vline(data=cuts_imputed, aes(xintercept=vals, linetype=Summary, colour = Summary), show.legend = TRUE)+
    theme(legend.position=c(0.8, 0.8))+ggsave("total_steps_day_imputed.png")
  
  print(noquote("Do these values differ from the estimates from the first part of the assignment?"))
  print(noquote(""))
  print(noquote("Yes"))
  print(noquote(""))
  print(noquote("What is the impact of imputing missing data on the estimates of the total daily number of steps?"))
  print(noquote(""))
  print(noquote("Both the mean and median increase and the histrogram is more Gaussian"))
  
  ##### Differences in activity patterns between weekdays and weekends
  
  # Create a new factor variable in the dataset indicating whether a given date is a "weekday" or "weekend" day
  activity$wday[wday(activity$date) >= 2 & wday(activity$date) <= 6] <- "weekday"
  activity$wday[wday(activity$date) == 1 | wday(activity$date) == 7] <- "weekend"
  
  # Panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, 
  # averaged across all weekday days or weekend days
  average_steps_interval_wday <- aggregate(x = activity$steps, by = list(activity$interval, activity$wday), 
                                           FUN = mean, na.rm = TRUE)
  names(average_steps_interval_wday) <- c("Interval", "wday", "MeanSteps")
  trellis.device(device="png", filename="time_series_plot_weekend_weekday.png")
  panel_plot <- xyplot(MeanSteps ~ Interval | wday, data = average_steps_interval_wday, type = "l", 
                       ylab = "Average Number of Steps Taken", layout = c(1,2))
  print(panel_plot)
  dev.off()
  
}
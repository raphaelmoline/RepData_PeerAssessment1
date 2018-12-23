## working code to answer questions

unzip("activity.zip")
data <- read.csv("activity.csv",stringsAsFactors = FALSE)
data$date <- as.Date.character(data$date)
str(data)

data_day <- split(data, data$date)
steps_day_total <- sapply(data_day, function(x) {sum(x$steps)})
hist(steps_day_total)

steps_day_mean <- sapply(data_day, function(x) {mean(x$steps)})
steps_day_median <- sapply(data_day, function(x) {median(x$steps)})

days_scale <- sapply(data_day, function(x) {max(x$date)})
time_serie <- cbind(days_scale, steps_day_mean)
plot(time_serie[,1], time_serie[,2], type = "l") # change format

data_interval <- split(data, data$interval)
steps_interval_mean <- sapply(data_interval, function(x) {mean(x$steps, na.rm = TRUE)})

which.max(steps_interval_mean) # value and index returned

data2 <- data
for (i in 1:length(data$steps)) {
    if (is.na(data$steps[i])) {
        ref <- match(data$interval[i], names(steps_interval_mean))
        data2$steps[i] <- steps_interval_mean[ref]
    }
}

data_day2 <- split(data2, data2$date)
steps_day_total2 <- sapply(data_day2, function(x) {sum(x$steps)})
hist(steps_day_total2)


data$weekdays <- ifelse(weekdays(data$date, abbreviate = TRUE)=="Sat" | weekdays(data$date, abbreviate = TRUE)=="Sun",0,1)

data_interval_wday <- split(data[data$weekdays==1,], data[data$weekdays==1,3])
steps_interval_mean_wday <- sapply(data_interval_wday, function(x) {mean(x$steps, na.rm = TRUE)})

data_interval_wkend <- split(data[data$weekdays==0,], data[data$weekdays==0,3])
steps_interval_mean_wkend <- sapply(data_interval_wkend, function(x) {mean(x$steps, na.rm = TRUE)})

par(mfrow=c(2,1))
plot(as.numeric(names(steps_interval_mean_wkend)),steps_interval_mean_wkend, type = "l", main = "# of steps per interval on weekend", xlab = "", ylab = "", ylim = c(0,200))
plot(as.numeric(names(steps_interval_mean_wday)),steps_interval_mean_wday, type = "l", main = "# of steps per interval on weekday",xlab = "", ylab = "", ylim = c(0,200))


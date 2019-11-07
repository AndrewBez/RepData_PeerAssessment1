act <- read.csv('./activity/activity.csv',stringsAsFactors = FALSE)

act$date <- as.POSIXct(act$date,format="%Y-%m-%d")

act$interval <- sprintf("%04.f",act$interval)
act$interval_time <- as.POSIXlt(act$interval,format="%H%M")
act$interval_time <- substr(act$interval_time,12,16)



dailysum <- aggregate(steps ~ date,data=act,sum)
# plot(dailysum$date, dailysum$steps, type = 'h', xlab = 'Date', ylab = 'Total Steps per Day')
hist(dailysum$steps, breaks = 10, xlab = 'Total Steps per Day')
meansteps <- round(mean(dailysum$steps))
mediansteps <- round(median(dailysum$steps))



timemean <- aggregate(steps ~ interval,data = act,mean)
plot(timemean$interval,timemean$steps, type = 'l',xlab = 'Time of Day',ylab = 'Average Number of Steps')
activ_time <- timemean$interval[timemean$steps==max(timemean$steps)]



miss_steps <- sum(is.na(act$steps))

act1 <- merge(act, timemean,by = 'interval', all.x = TRUE)
act1$steps <- ifelse(is.na(act1$steps.x), act1$steps.y, act1$steps.x)
act_nona <- act1[,-c(2,5)]

dailysum_nona <- aggregate(steps ~ date,data=act_nona,sum)
hist(dailysum_nona $steps, breaks = 10, xlab = 'Total Steps per Day')
meansteps_nona  <- round(mean(dailysum_nona $steps))
mediansteps_nona  <- round(median(dailysum_nona $steps))



act_nona$day <- ifelse(weekdays(act_nona$date) %in% c('Saturday','Sunday'),'Weekend','Weekday')
act_nona$day <- as.factor(act_nona$day)

timemean_wday <- aggregate(steps ~ interval, data = act_nona[act_nona$day=='Weekday',],mean)
timemean_wend <- aggregate(steps ~ interval, data = act_nona[act_nona$day=='Weekend',],mean)
par(mfcol=c(2,1))
plot(timemean_wday$interval,timemean_wday$steps,type = 'l',col='red',xlab = 'Time of Day',ylab = 'Average Number of Steps',ylim = c(-10,250))
title('Weekday')
plot(timemean_wend$interval,timemean_wend$steps,type = 'l',col='red',xlab = 'Time of Day',ylab = 'Average Number of Steps',ylim = c(-10,250))
title('Weekend')
dev.off()

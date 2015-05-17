###### Set Up ######
# 1.
setwd("~/Training/Data")
dat <- read.table("Activity.csv", header = T, sep = ",", as.is= TRUE)
# 2.
dat$interval <- as.factor(dat$interval)
dat$date <- as.Date(dat$date, format="%m/%d/%Y")
dat$day <- weekdays(dat$date) #Adds day of week
dat$weekday <- ifelse(weekdays(dat$date)=="Saturday" | weekdays(dat$date)=="Sunday", "weekend", "weekday") #Adds weekend/weekday column
dat_wday <- subset(dat, dat$weekday == "weekday") # creates a weekday subset dataframe
dat_wend <- subset(dat, dat$weekday == "weekend") # creates a weekend subset dataframe

###### Steps per day ######
# 1.
steps_per_day <- list() #creates empty list
for (i in 1:length(unique(dat$date))){ #counts total steps per day
  steps_per_day <- cbind(steps_per_day, sum(dat$steps[dat$date==(unique(dat$date))[i]]))
}
steps_per_day <- sapply(steps_per_day, as.numeric)  #converts to numeric
# 2.
hist(steps_per_day, breaks= length(unique(dat$date))) #creates a histogram
#3. 
mean(steps_per_day, na.rm=T)
median(steps_per_day, na.rm=T)

###### Average daily activity pattern ######
# 1.
steps_per_int <- list() #creates empty list
for (i in 1:length(unique(dat$interval))){ #counts total steps per interval
  steps_per_int <- cbind(steps_per_int,sum(dat$steps[dat$interval==as.character((unique(dat$interval))[i])],na.rm=T))
}
steps_per_int <- sapply(steps_per_int, as.numeric)  #converts to numeric
steps_per_int <- steps_per_int/length(unique(dat$date))
plot(steps_per_int, unique(dat$intervals), type="l", xlab= "", ylab="") # creates a plot of steps per interval
mtext("Steps", side=2, line=2.75, cex=1.2, lwd=4)
mtext("Time Interval", side=1, line=3, cex=1.2)
max_steps<- max(steps_per_int, na.rm=T) #Caluculates the max steps
match(max_steps,steps_per_int)

###### Imputing Missing Values
#1
summary(dat)[7,1] #returns the number of intervals that have NA's
#2
dat_imputed <- dat # creates copy of dat to imput values on
avg_step_wday_int <- mean(dat_wday$steps, na.rm=T) # average weekday int
avg_step_wend_int <- mean(dat_wend$steps, na.rm=T) # average weekend int
# 3
#if(dat_imputed$weekday=="weekday"){
#  dat_imputed$steps[is.na(dat_imputed$steps)] <-avg_step_wday_int
#  } else {
#  dat_imputed$steps[is.na(dat_imputed$steps)] <-avg_step_wend_int
#}

ifelse(dat_imputed$steps=="Saturday" | dat_imputed$day=="Sunday",
       dat_imputed$steps[is.na(dat_imputed$steps)] <- avg_step_wday_int,
       dat_imputed$steps[is.na(dat_imputed$steps)] <- avg_step_wend_int)


###### Differences between weekend and weekday panel plot ######

wday_steps_per_int <- list() #creates empty list
for (i in 1:length(unique(dat_wday$interval))){ #counts avg steps per interval
  wday_steps_per_int <- cbind(wday_steps_per_int,sum(dat_wday$steps[dat_wday$interval==as.character((unique(dat_wday$interval))[i])],na.rm=T))
}
wday_steps_per_int <- sapply(wday_steps_per_int, as.numeric)  #converts to numeric
wday_steps_per_int <- wday_steps_per_int/length(unique(dat_wday$date))
wday_avgs <- data.frame(unique(dat_wday$interval), wday_steps_per_int, "weekday")

wend_steps_per_int <- list() #creates empty list
for (i in 1:length(unique(dat_wend$interval))){ #counts avg steps per interval
  wend_steps_per_int <- cbind(wend_steps_per_int,sum(dat_wend$steps[dat_wend$interval==as.character((unique(dat_wend$interval))[i])],na.rm=T))
}
wend_steps_per_int <- sapply(wend_steps_per_int, as.numeric)  #converts to numeric
wend_steps_per_int <- wend_steps_per_int/length(unique(dat_wend$date))
wend_avgs <- data.frame(unique(dat_wend$interval), wend_steps_per_int, "weekend")
avgs <- rbind(wday_avgs, wend_avgs)
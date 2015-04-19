# task 1
data <- read.csv('activity.csv')

library(dplyr)

df <- data %>%
        group_by(date) %>%
        summarize(ttl_steps = sum(steps, na.rm = T))

hist(df$ttl_steps, breaks = 20, xlab = "total number of steps", main = 'Total number of steps per day')

summary(df)[c(4,3),2]

#task 2

df2 <- data %>%
        group_by(interval) %>%
        summarize(ttl_steps = mean(steps, na.rm = T))

plot(x = df2$interval, y = df2$ttl_steps, type ="l", xlab= "interval", ylab = "average namer of steps taken per interval")

df2[df2$ttl_steps == max(df2$ttl_steps), ]

#task 3

sum(is.na(data$steps))

data2 <- data

for (i in 1:nrow(data2)) {
        if (is.na(data2[i,1]) == T)
                data2[i,1] <- df2[df2$interval == data2[i,3],2]        
}

ndf <- test %>%
        group_by(date) %>%
        summarize(ttl_steps = sum(steps, na.rm = T))

hist(ndf$ttl_steps, breaks = 20)

summary(ndf)[c(4,3),2]

#task 4
Sys.setlocale("LC_TIME", "English")

data2 <- data %>%
        mutate(weekday = weekdays(as.Date(date), abbreviate = T))

for (i in 1:nrow(data2)) {
        if (data2[i,4] %in% c("Mon","Tue","Wed","Thu","Fri"))
                data2[i,4] <- "wd"
        else data2[i,4] <- "we"
        
}

test <- data2 %>%
        group_by(weekday, interval) %>%
        summarize(mean_steps = mean(steps, na.rm = T))

library(ggplot2)

ggplot(data = test, aes(x = interval, y = mean_steps)) + facet_wrap(~weekday, nrow = 2, ncol = 1) + geom_line()


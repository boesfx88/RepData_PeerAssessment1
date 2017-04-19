This is the histogram of the total number of steps taken each day
```{r}
data <-read.csv("./activity/activity.csv")
library(plyr);library(dplyr)
library(tidyr)
dat <-spread(data,interval,steps)
row.names(dat) <- dat[,1]
dat$date <- NULL

dat2 <- dat
dat2$sum <- apply(dat,1,sum)
hist(dat2$sum, xlab="Total number of steps taken per day", main="")
```

This is mean of steps taken each day
```{r}
data <-read.csv("./activity/activity.csv")
library(plyr);library(dplyr)
library(tidyr)
dat <-spread(data,interval,steps)
row.names(dat) <- dat[,1]
dat$date <- NULL

dat2 <- dat
dat2$sum <- apply(dat,1,sum)
dat2$mean <- apply(dat,1,mean)
```

This is median number of steps taken each day
```{r}
data <-read.csv("./activity/activity.csv")
library(plyr);library(dplyr)
library(tidyr)
dat <-spread(data,interval,steps)
row.names(dat) <- dat[,1]
dat$date <- NULL

dat2 <- dat
dat2$sum <- apply(dat,1,sum)
dat2$median <- apply(dat,1,median)
```
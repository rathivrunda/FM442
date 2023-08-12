#Exam No. 44514

library(reshape2)
library(lubridate)
library(moments)
library(zoo)
library(tseries)

data <- read.csv('HW1.csv')

#adjusting price, date to correct format and returns compounded
data$Adjusted_PRC <- data$PRC / data$CFACPR
data$date <- ymd(data$date)
data$Adjusted_RET <- log(1 + data$RET)

# make a data frame for prices and normalizing prices
PRC <- dcast(data, date ~ PERMNO, value.var = "Adjusted_PRC")
names(PRC) <- c("date", "AMZN", "TSLA")
PRC$normAMZN <- PRC$AMZN / PRC$AMZN[1]
PRC$normTSLA <- PRC$TSLA / PRC$TSLA[1]

#plot normalized prices with legend
matplot(PRC$date, PRC[,4:5], type = "l", main = "Normalized Prices for our stocks", xlab= "Date",
        ylab = "Price", lty = 1)

legend("topleft", legend=names(PRC[,2:3]), col=c(1, 2), lty=1)

# creating data frame for returns
RET <- dcast(data, date ~ PERMNO, value.var = "Adjusted_RET")
names(RET) <- c("date", "AMZN", "TSLA")


# plotting returns side by side
par(mfrow = c(1,2))

for (i in c(2, 3)) { 
  plot(RET$date, RET[,i], type = "l", ylab = "Returns", xlab = "Date",
       main = paste("Returns for", names(RET)[i]), ylim =c(-0.2, 0.2))
}



# sample stats
mean(RET$AMZN)
sd(RET$AMZN)
min(RET$AMZN)
max(RET$AMZN)
skewness(RET$AMZN)
kurtosis(RET$AMZN)
acf(RET$AMZN, 1, plot=FALSE)
acf(RET$AMZN ^2, 1, plot=FALSE)
jarque.bera.test(RET$AMZN)
Box.test (RET$AMZN, lag =20, type =c ("Ljung-Box"))
Box.test (RET$AMZN^2, lag =20, type =c ("Ljung-Box"))

mean(RET$TSLA)
sd(RET$TSLA)
min(RET$TSLA)
max(RET$TSLA)
skewness(RET$TSLA)
kurtosis(RET$TSLA)
acf(RET$TSLA, 1, plot=FALSE)
acf(RET$TSLA ^2, 1, plot=FALSE)
jarque.bera.test(RET$TSLA)
Box.test (RET$TSLA, lag =20, type =c ("Ljung-Box"))
Box.test (RET$TSLA^2, lag =20, type =c ("Ljung-Box"))


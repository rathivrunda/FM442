#Exam No. 44514

library(reshape2)
library(lubridate)
library(moments)
library(zoo)
library(tseries)
library(rugarch)
library(tseries)

data <- read.csv('HW1.csv')

#adjusting price, date to correct format and returns compounded
data$Adjusted_PRC <- data$PRC / data$CFACPR
data$date <- ymd(data$date)
data$Adjusted_RET <- log(1 + data$RET)

# make a data frame for prices and normalizing prices
RET <- dcast(data, date ~ PERMNO, value.var = "Adjusted_RET")
names(RET) <- c("date", "AMZN", "TSLA")
y <- RET$AMZN


default_spec <- ugarchspec()
# Fit the model to the data using ugarchfit
default_garch <- ugarchfit(spec = default_spec, data = y)

#parameters
mu = coef(default_garch)[1]
alpha =coef(default_garch)[5]
beta = coef(default_garch)[6]
omega = coef(default_garch)[4]

est_window = length(y)
S =1000
p= 0.05
portfolio= 1

#garch equation
Y_t = y[length(y)]

sigma_t = coredata(sigma(default_garch))[length(y)]

sigma_new = omega + alpha * (Y_t ^2) + beta * sigma_t

Y_new = rnorm(S, mu, sqrt(sigma_new))

# Sort the values in y using sort()

VaR = vector("numeric", S)
ES = vector("numeric", S)


for (i in 100: S){
  ys <- sort(Y_new[1:i])
  
  # Round up, since the above number is not an integer!
  M<- ceiling(i*p)

  # Scaling for the portfolio value
  VaR[i] <- ys[M] * portfolio


  ES[i] <- mean(ys[1:M]) * portfolio
}


matplot(1:S, cbind(Y_new, VaR, ES), type=c("p", "l", "l"), 
        pch = 19, lty = 1, lwd = 2, cex= 0.5, 
        xlab =" No. of Simulations",
        ylab = "Simulated Returns",
        main = "VaR and ES predictions with number of simulations")

legend("topleft", inset=.05, c("VaR","ES"),
       lty=c(1, 1), cex=0.6,col=c(2, 3))

VaR_analytical = mu + sqrt(sigma_new) * qnorm(p)
#Exam No. 44514

setwd('C:/Coursework/FM442')

library(reshape2)
library(lubridate)
library(moments)
library(zoo)
library(tseries)
library(rugarch)
library(rmgarch)
#portfolio weights

MSFTSt = 1
IBMSt = 1
MSFTOpt = 1
IBMOpt = 1

#Option details

MSFTOptT = 5
IBMOptT = 3


data<- read.csv("indproject.csv")

#adjusting price, date to correct format and returns compounded
data$Adjusted_PRC <- data$PRC / data$CFACPR
data$date <- ymd(data$date)
data$Adjusted_RET <- log(1 + data$RET)

# make a data frame for prices and reurns
PRC <- dcast(data, date ~ PERMNO, value.var = "Adjusted_PRC")
names(PRC) <- c("date", "MSFT", "IBM")
PRC$normMSFT <- PRC$MSFT / PRC$MSFT[1]
PRC$normIBM <- PRC$IBM / PRC$IBM[1]

#plot normalized prices with legend
matplot(PRC$date, PRC[,2:3], type = "l", main = "Prices for our stocks", xlab= "Date",
        ylab = "Price", lty = 1)
legend("topleft", legend=names(PRC[,2:3]), col=c(1, 2), lty=1)


RET <- dcast(data, date ~ PERMNO, value.var = "Adjusted_RET")
names(RET) <- c("date", "MSFT", "IBM")




#plot(RET$date, RET$MSFT)

#more specs

EW = 2000
TW = 2800
SimSize = 300

Sigmas <- array(numeric(), c(2, 2, TW))

for (i in 2764:TW){
  # Extract the returns for MSFT and IBM
  y <-RET[c("MSFT", "IBM")]
  y <- as.matrix(y)
  y <- y[i:(i+EW-1), ]
  
  
  
  # DCC model --------------------------------------------------------------------
  
  # Specify the default univariate GARCH model with no mean
  xspec <- ugarchspec(
    mean.model = list(armaOrder = c(0,0), include.mean = FALSE)
  )
  
  # Replicate it into a multispec() element
  uspec <- multispec(replicate(2, xspec))
  
  # Define the specification for the DCC model
  spec <- dccspec(
    # GARCH specification
    uspec = uspec,
    
    # DCC specification
    dccOrder = c(1,1),
    
    # Distribution, here multivariate normal
    distribution = "mvnorm"
  )
  
  # Fit the specification to the data
  res <- dccfit(spec, data = y)
  
  # Extracting the Sigma matrix
  Sigmas[,,i] <- res@mfit$H[,,EW]
  
  
}

Sigmasdf <- as.data.frame.array(Sigmas[,,1:2800])

save(Sigmasdf, file="Sigma.Rdata")

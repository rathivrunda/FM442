#Exam No. 44514

setwd('C:/Coursework/FM442')

library(reshape2)
library(lubridate)
library(moments)
library(zoo)
library(tseries)
library(rugarch)
library(rmgarch)
library(qrmtools)
library(MASS)

#portfolio weights

MSFTSt = 5
IBMSt = 4
MSFTOpt = 100
IBMOpt = 50

#Option details

MSFTOpt_T = 0.5
IBMOpt_T = 1
rfr = 0.08
#K_MSFT = 50
#K_IBM = 100





data<- read.csv("indproject.csv")

#adjusting price, date to correct format and returns compounded
data$Adjusted_PRC <- data$PRC / data$CFACPR
data$date <- ymd(data$date)
data$Adjusted_RET <- log(1 + data$RET)

# make a data frame for prices and reurns
PRC <- dcast(data, date ~ PERMNO, value.var = "Adjusted_PRC")
names(PRC) <- c("date", "MSFT", "IBM")

RET <- dcast(data, date ~ PERMNO, value.var = "Adjusted_RET")
names(RET) <- c("date", "MSFT", "IBM")


# specifications
EW = 2000
TW = 2800
SimSize = 100
quant = SimSize * 0.05
Portfolio_value = array(numeric(), TW)
uc_sigma_msft = array(numeric(), TW)
uc_sigma_ibm = array(numeric(), TW)

# creating an array to store unconditional variance in yearly returns for BS equation
for (i in 1:TW){
  # Extract the returns for MSFT and IBM
  y <-RET[c("MSFT", "IBM")]
  y <- as.matrix(y)
  x <- y[i:(i+EW-1), ]
  uc_sigma_msft[i] = sqrt(250) * sd(x[,1])
  uc_sigma_ibm[i] = sqrt(250) * sd(x[,2])
  #sqrt(250) * sd(x[,2])

}


#the ACTUAL calculation

Violations = array(numeric(), TW)
for (i in 1:TW) {
  
  #strike rates
  K_MSFT = PRC$MSFT[i+EW-1]*0.9
  K_IBM = PRC$IBM[i+EW-1] *0.9
  
  #calculate portfolio value on last day of estimation window
  
  OptMsft_i = Black_Scholes(t = 1/365, S = PRC$MSFT[i+EW-1], r = rfr, sigma =uc_sigma_msft[i], K= K_MSFT, MSFTOpt_T, "call")
  OptIBM_i = Black_Scholes(t = (1/365), S = PRC$IBM[i+EW-1], r = rfr, sigma = uc_sigma_ibm[i], K= K_IBM, T=IBMOpt_T, "call")
  Portfolio_value[i] = MSFTSt * PRC$MSFT[i+EW-1] + IBMSt * PRC$IBM[i+EW-1] + IBMOpt * OptIBM_i + MSFTOpt * OptMsft_i
   
  PRC_nextday = array(numeric(), c(4, SimSize))
  Port_nextday = array(numeric(), SimSize)
  
  Sigma = as.matrix(Sigmasdf[ , (2*i-1):(2*i)])
  for (j in 1:SimSize){
    # log normal correction
    PRC_nextday[1, j] = exp(mvrnorm(1,c(0, 0),Sigma)[1] - 0.5 * Sigma[1,1] + log(PRC$MSFT[i+EW-1]))
    PRC_nextday[2, j] = exp(mvrnorm(1,c(0, 0),Sigma)[2] - 0.5 * Sigma[2,2] + log(PRC$IBM[i+EW-1]))
    PRC_nextday[3, j] = Black_Scholes(t = 2/365, PRC_nextday[1, j], r = rfr, sigma =uc_sigma_msft[i], K= K_MSFT, MSFTOpt_T, "call")
    PRC_nextday[4, j] = Black_Scholes(t=2/365, PRC_nextday[2, j], r = rfr, sigma = uc_sigma_ibm[i], K= K_IBM, T=IBMOpt_T, "call")
    Port_nextday[j] = MSFTSt * PRC_nextday[1, j] + IBMSt * PRC_nextday[2, j] + MSFTOpt * PRC_nextday[3, j] + IBMOpt * PRC_nextday[4, j]
    
  }
  
  #calculate VaR by sorting the array of simulations (subtracting the previous day prices to get returns)
  Ret_nextday = sort(Port_nextday-Portfolio_value[i])
  
  VaR = Ret_nextday[quant]
  
  #calculate portfolio value based on actual price 
  OptMsft_i = Black_Scholes(t = 2/365, S = PRC$MSFT[i+EW], r = rfr, sigma =uc_sigma_msft[i], K= K_MSFT, MSFTOpt_T,  "call")
  OptIBM_i  = Black_Scholes(t = (2/365), S = PRC$IBM[i+EW], r = rfr, sigma = uc_sigma_ibm[i], K= K_IBM, T=IBMOpt_T, "call")
  Act_Port_nextday = MSFTSt * PRC$MSFT[i+EW] + IBMSt * PRC$IBM[i+EW] + IBMOpt * OptIBM_i + MSFTOpt * OptMsft_i
  
  #get returns 
  Act_Ret_nextday = Act_Port_nextday - Portfolio_value[i]
  
  #compare to see if VaR is violated
  if (VaR > Act_Ret_nextday){
    Violations[i] = 1
  } else{
    Violations[i] = 0
  }
  
}

#create plots
OptIBM = array(numeric(), TW)
OptMsft = array(numeric(), TW)
for (i in 1:TW){
  
  #strike rates
  K_MSFT = PRC$MSFT[i+EW-1] * 0.9
  K_IBM = PRC$IBM[i+EW-1] *0.9
  
  OptMsft[i] = MSFTOpt* Black_Scholes(t = 1/365, S = PRC$MSFT[i+EW-1], r = rfr, sigma =uc_sigma_msft[i], K= K_MSFT, MSFTOpt_T, "call")
  OptIBM[i] = IBMOpt * Black_Scholes(t = (1/365), S = PRC$IBM[i+EW-1], r = rfr, sigma = uc_sigma_ibm[i], K= K_IBM, T=IBMOpt_T, "call")
  Portfolio_value[i] = MSFTSt * PRC$MSFT[i+EW-1] + IBMSt * PRC$IBM[i+EW-1] + OptIBM[i] + OptMsft[i]
}

matplot(PRC$date[(EW):(EW+TW-1)], cbind(MSFTSt * PRC$MSFT[(EW):(EW+TW-1)], IBMSt *PRC$IBM[(EW):(EW+TW-1)], OptMsft, OptIBM, Portfolio_value), type = "l", main = "Test Portfolio values", xlab= "Date",
        ylab = "Price", lty = 1)
legend("topleft", legend= c("MSFT", "IBM", "MSFT Option", "IBM Option", "Portfolio Value"), col=c(1, 2, 3, 4, 5), lty=1)

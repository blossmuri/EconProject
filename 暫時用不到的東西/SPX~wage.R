###
install.packages("dplyr") ; install.packages("lmtest") ; install.packages("vars") ; install.packages("readxl")
library(lmtest) ; library(dplyr) ; library(ggplot2) ; library(vars) ; library(readxl)

SPX <- read_excel("data/SPX.xlsx")
Wage <- read_excel("data/wage.xlsx")
options(digits = 4)

WagY <- na.omit(Wage$YoY)
WagM <- na.omit(Wage$MoM)
SpxY <- na.omit(SPX$YoY)

WagdY <- na.omit(Wage$dYoY)
WagdM <- na.omit(Wage$dMoM)
SpxdY <- na.omit(SPX$dYoY)

####

wagY <- ts(rev(scale(WagY[1:60])), start = c(2006, 2), frequency = 12)
spxY <- ts(rev(scale(SpxY[1:60])), start = c(2006, 2), frequency = 12)
plot(wagY, main = "wagY" ,  xlab = "Time", ylab = "Value",  col = "blue") ; lines(spxY, col = "black")


wagdY <- ts(rev(scale(WagdY[1:60])), start = c(2006, 2), frequency = 12)
plot(wagdY, main = "wagdY" ,  xlab = "Time", ylab = "Value",col = "blue") ; lines(spxY,col = "black" )


wagdM <- ts(rev(scale(WagdM[1:60])), start = c(2006, 2), frequency = 12)
plot(wagdM, main = "wagdM" ,  xlab = "Time", ylab = "Value",col = "blue") ; lines(spxY, col = "black")


plot(WagdY[1:60],SpxY[1:60],
     xlab = "WagdY", ylab = "SpxY", main = SW)

###

ccf_result <- ccf(wagdY, spxY , lag.max = 10, plot = FALSE)

max_corr <- max(ccf_result$acf)
lag_index <- which(ccf_result$acf == max_corr)
lag_time <- as.numeric(ccf_result$lag[lag_index])

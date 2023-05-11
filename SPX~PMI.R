### 嘗試分別在5年,10年,20年,30年的情況，利用ccf & granger's causality test，找出PMI是SPX多少個月的領先指標 

install.packages("dplyr") ; install.packages("lmtest")
library(lmtest) ; library(dplyr) ; library(ggplot2)

PMI <- read.csv("data/PMI.csv")
SPX <- read.csv("data/SPX.csv")

SPXplot <- ts(SPX$PercentChange[1:120], start = c(2013, 3), frequency = 12)
PMIplot <- ts(PMI$PercentChange[1:120], start = c(2013, 3), frequency = 12)
plot(SPXplot, main = "SPX and PMI Series", xlab = "Time", ylab = "Value")
lines(PMIplot, col = "blue")

M <- matrix(nrow = 5, ncol = 4)
colnames(M) <- c("5y", "10y", "20y", "30y")
rownames(M) <- c("ccf.lag", "max.corr", "OLS.B","R-squared","Granger.p")



b = 1

for (y in c(60,120,240,360)){ a = 1
  
  # test 1. Cross-Correlation 分析
  PMIseries <- as.vector(PMI$PercentChange[1:y]) 
  SPXseries <- as.vector(SPX$PercentChange[1:y])
  
  PMI_normalized <- c(scale(PMIseries))
  SPX_normalized <- c(scale(SPXseries))
  
  ccf_result <- ccf(PMI_normalized, SPX_normalized, lag.max = 12,plot=FALSE)
  
  max_corr <- max(ccf_result$acf)
  lag_index <- which(ccf_result$acf == max_corr)
  lag_time <- as.numeric(ccf_result$lag[lag_index])
  
  M[a,b] = lag_time
  a = a+1
  M[a,b] = max_corr
  a = a+1

  # test 2. lm(SPX~PMI)的迴歸係數
  if(lag_time >= 0){
    L=1+lag_time ; U=y+lag_time
    PMI_subset <- as.vector(PMI$PercentChange[L:U]) 
    SPX_subset <- SPXseries[1 : y]
    OLS <- summary(lm(SPX_subset ~ PMI_subset))}
  
  else{
    OLS$coefficients[2]="NA" ; OLS$adj.r.squared="NA"
  }

  M[a,b] = OLS$coefficients[2]
  a = a+1
  M[a,b] = OLS$adj.r.squared
  a = a+1
  
  # test 3. Granger's causality
  if(lag_time>=0){
    G <- grangertest(PMIseries ~ SPXseries,order=lag_time)}
    
  else{
    G$Pr[2]="NA"
  }
    
  M[a,b] = G$Pr[2]
  b = b+1

}

M


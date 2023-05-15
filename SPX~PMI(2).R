### 嘗試分別在2018-2023年,2013-2018年,2008-2013年,2003-2008,1998-2003年的情況，
#利用ccf & granger's causality test，找出PMI是SPX多少個月的領先指標 

install.packages("dplyr") ; install.packages("lmtest") ; install.packages("vars")
library(lmtest) ; library(dplyr) ; library(ggplot2) ; library(vars)

PMI <- read.csv("data/PMI.csv")
SPX <- read.csv("data/SPX.csv")

SPXplot <- ts(SPX$YoY[1:120], start = c(2013, 3), frequency = 12)
PMIplot <- ts(PMI$YoY[1:120], start = c(2013, 3), frequency = 12)
plot(SPXplot, main = "SPX and PMI Series", xlab = "Time", ylab = "Value")
lines(PMIplot, col = "blue")

M <- matrix(nrow = 5, ncol = 5)
colnames(M) <- c("2018-2023", "2013-2018", "2008-2013", "2003-2008","1998-2003")
rownames(M) <- c("ccf.lag-month", "ccf.max-corr", "OLS.B1","OLS.R-squared","Granger.p-value")



b = 1 

for (i in c(1,2,3,4,5)){ a = 1 

  # test 1. Cross-Correlation 分析
  PMIseries <- as.vector(as.numeric(PMI$YoY[(1 + 60*(i - 1)) : (60*i)]))
  SPXseries <- as.vector(as.numeric(SPX$YoY[(1 + 60*(i - 1)) : (60*i)]))
  
  PMI_normalized <- c(scale(PMIseries))
  SPX_normalized <- c(scale(SPXseries))
  
  ccf_result <- ccf(PMI_normalized, SPX_normalized, lag.max = 12,plot = FALSE)
  
  max_corr <- max(ccf_result$acf)
  lag_index <- which(ccf_result$acf == max_corr)
  lag_time <- as.numeric(ccf_result$lag[lag_index])
  
  M[a, b] = lag_time
  a = a + 1
  M[a, b] = max_corr
  a = a + 1
  
  # test 2. lm(SPX ~ PMI)的迴歸係數
  if (lag_time >= 0) {
    PMIseries <- as.vector(as.numeric(PMI$YoY[ (1 + 60*(i - 1) + lag_time) : (60*i + lag_time  )]))
    SPXseries <- as.vector(as.numeric(SPX$YoY[(1 + 60*(i - 1)) : (60*i)]))
    OLS <- summary(lm(SPXseries ~ PMIseries))
  }
  
  else{
    OLS$coefficients[2] = "NA"
    OLS$adj.r.squared = "NA"
  }
  
  M[a, b] = OLS$coefficients[2]
  a = a + 1
  M[a, b] = OLS$adj.r.squared
  a = a + 1
  
  # test 3. Granger's causality
  if (lag_time >= 0) {
    
    data <- cbind(SPXseries, PMIseries) # 假設有兩個時間序列
    var_result <- VAR(data, p = 10, type = "const")  # 指定最大滯後期數為 10

    # 自動選擇滯後期數
    select_result <- VARselect(data, lag.max = 10, type = "const")

    G <- grangertest(PMIseries ~ SPXseries, order = select_result$selection[1])
  }
  
  else{
    G$Pr[2] = "NA"
  }
  
  M[a, b] = G$Pr[2]
  b = b + 1

}

M


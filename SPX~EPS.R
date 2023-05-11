### 載入套件
install.packages("dplyr")
install.packages("lmtest")
library(lmtest) 
library(dplyr)

# 定義變數
EPS <- read.csv("data/EPS.csv")
SPX <- read.csv("data/SPX.csv")

EPSseries <- as.vector(EPS$PercentChange[1:120]) 
SPXseries <- as.vector(SPX$PercentChange[1:120])


# test 1. Cross-Correlation 分析

EPS_normalized <- c(scale(EPSseries))
SPX_normalized <- c(scale(SPXseries))

ccf_result <- ccf(EPS_normalized, SPX_normalized, lag.max = 12,plot=TRUE)

max_corr <- max(ccf_result$acf)
lag_index <- which(ccf_result$acf == max_corr)
lag_time <- ccf_result$lag[lag_index]

print(ccf_result)
cat("EPS領先SPX",lag_time,"個星期","\n")
cat("Max correlation:", max_corr,"\n")



# test 2. lm(SPX~PMI)的迴歸係數

Z <- numeric(12)
for (i in 0:11) {
  PMI_subset <- PMIseries[(i+1):120]
  SPX_subset <- SPXseries[1:(120-i)]
  OLS <- lm(SPX_subset ~ PMI_subset)
  Z[i] <- OLS$coefficients[2]
}

PMI_subset <- PMIseries[(which.max(Z)+1):120]
SPX_subset <- SPXseries[1:(120-which.max(Z))]
summary(lm(SPX_subset ~ PMI_subset))
print(cat("PMI領先SPX", which.max(Z), "個月"))


# test 3. Granger 因果性檢驗

granger_test <- grangertest(PMIseries ~ SPXseries,order=which.max(Z))
print(granger_test)

### 載入套件
install.packages("dplyr")
install.packages("lmtest")
library(lmtest) 
library(dplyr)

# 定義變數
AAII <- read.csv("data/AAIIBull.csv")
SPX <- read.csv("data/SPXweek.csv")

AAIIseries <- as.vector(AAII$PercentChange[1:120]) 
SPXseries <- as.vector(SPX$PercentChange[1:120])


# test 1. Cross-Correlation 分析

AAII_normalized <- c(scale(AAIIseries)) #標準化數據
SPX_normalized <- c(scale(SPXseries))

ccf_result <- ccf(SPX_normalized, AAII_normalized, lag.max = 3, plot=TRUE) 

max_corr <- max(ccf_result$acf)
lag_index <- which(ccf_result$acf == max_corr)
lag_time <- ccf_result$lag[lag_index]

print(ccf_result)
cat("AAIIBear領先SPX",lag_time,"個星期","\n")
cat("Max correlation:", max_corr,"\n")


# test 2. lm(SPX~AAII)的迴歸係數

Z <- numeric(12)
for (i in 0:11) {
  AAII_subset <- AAIIseries[(i+1):120]
  SPX_subset <- SPXseries[1:(120-i)]
  OLS <- lm(SPX_subset ~ AAII_subset)
  Z[i] <- OLS$coefficients[2]
}

AAII_subset <- AAIIseries[(which.max(Z)+1):120]
SPX_subset <- SPXseries[1:(120-which.max(Z))]
summary(lm(SPX_subset ~ AAII_subset))
OLS <- summary(lm(SPX_subset ~ AAII_subset)) ; OLS


cat("AAII領先SPX", which.max(Z),"個星期","/n")
cat("迴歸係數為",OLS$coefficients[2])

# test 3. Granger 因果性檢驗

granger_test <- grangertest(AAIIseries ~ SPXseries,order=which.max(Z))
print(granger_test)



### 載入套件

### 定義變數
PMI <- read.csv("data/PMI.csv")
SPX <- read.csv("data/SPX.csv")

PMIseries <- as.vector(PMI$percentChange)
SPXseries <- as.vector(SPX$percentChange)


# 將兩個序列標準化
PMI_normalized <- c(scale(PMIseries))
SPX_normalized <- c(scale(SPXseries))

# 計算交叉相關值
ccf_result <- ccf(PMI_normalized, SPX_normalized, lag.max = 150,plot=TRUE)

# 找到最大的交叉相關值及其對應的延遲時間
max_corr <- max(ccf_result$acf)
lag_index <- which(ccf_result$acf == max_corr)
lag_time <- ccf_result$lag[lag_index]

# 輸出最大的交叉相關值及其對應的延遲時間
cat("Max correlation:", max_corr, "\n")
cat("Lag time:", lag_time, "\n")

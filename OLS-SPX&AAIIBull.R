### 載入套件
install.packages("dplyr")
library(dplyr)

# 定義變數
AAII <- read.csv("data/AAIIBull.csv")
SPX <- read.csv("data/SPXweek.csv")

AAIIseries <- as.vector(AAII$PercentChange[1:120]) 
SPXseries <- as.vector(SPX$PercentChange[1:120])


# 使用迴圈進行迴歸分析
Z <- numeric(12)
for (i in 0:11) {
  AAII_subset <- AAIIseries[(i+1):120]
  SPX_subset <- SPXseries[1:(120-i)]
  OLS <- lm(SPX_subset ~ AAII_subset)
  Z[i] <- OLS$coefficients[2]
}

# 輸出結果
print(paste("AAII領先SPX", which.max(Z), "個月"))

AAII_subset <- AAIIseries[(which.max(Z)+1):120]
SPX_subset <- SPXseries[1:(120-which.max(Z))]
summary(lm(SPX_subset ~ AAII_subset))

##


# 定義dummy變數
AAIIseries <- as.vector(AAII$dummy[1:120]) 
SPXseries <- as.vector(SPX$dummy[1:120])

# 使用迴圈進行迴歸分析
Z <- numeric(12)
for (i in 0:11) {
  AAII_subset <- AAIIseries[(i+1):120]
  SPX_subset <- SPXseries[1:(120-i)]
  OLS <- lm(SPX_subset ~ AAII_subset)
  Z[i] <- OLS$coefficients[2]
}

# 輸出結果
print(paste("AAII領先SPX", which.max(Z), "個月"))

AAII_subset <- AAIIseries[(which.max(Z)+1):120]
SPX_subset <- SPXseries[1:(120-which.max(Z))]
summary(lm(SPX_subset ~ AAII_subset))


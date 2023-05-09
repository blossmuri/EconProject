### 載入套件
install.packages("dplyr")
library(dplyr)

# 定義變數
PMI <- read.csv("data/PMI.csv")
SPX <- read.csv("data/SPX.csv")

PMIseries <- as.vector(PMI$PercentChange[1:120]) 
SPXseries <- as.vector(SPX$PercentChange[1:120])


# 使用迴圈進行迴歸分析
Z <- numeric(12)
for (i in 0:11) {
  PMI_subset <- PMIseries[(i+1):120]
  SPX_subset <- SPXseries[1:(120-i)]
  OLS <- lm(SPX_subset ~ PMI_subset)
  Z[i] <- OLS$coefficients[2]
}

# 輸出結果
print(paste("PMI領先SPX", which.max(Z), "個月"))

PMI_subset <- PMIseries[(which.max(Z)+1):120]
SPX_subset <- SPXseries[1:(120-which.max(Z))]
summary(lm(SPX_subset ~ PMI_subset))

##


# 定義dummy變數
PMIseries <- as.vector(PMI$dummy[1:120]) 
SPXseries <- as.vector(SPX$dummy[1:120])

# 使用迴圈進行迴歸分析
Z <- numeric(12)
for (i in 0:11) {
  PMI_subset <- PMIseries[(i+1):120]
  SPX_subset <- SPXseries[1:(120-i)]
  OLS <- lm(SPX_subset ~ PMI_subset)
  Z[i] <- OLS$coefficients[2]
}

# 輸出結果
print(paste("PMI領先SPX", which.max(Z), "個月"))

PMI_subset <- PMIseries[(which.max(Z)+1):120]
SPX_subset <- SPXseries[1:(120-which.max(Z))]
summary(lm(SPX_subset ~ PMI_subset))


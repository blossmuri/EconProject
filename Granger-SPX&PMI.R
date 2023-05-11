### 載入lmtest套件
install.packages("lmtest")
library(lmtest)  
library(dplyr)

# 定義變數
PMI <- read.csv("data/PMI.csv")
SPX <- read.csv("data/SPX.csv")

PMIseries <- ts(PMI$PercentChange[1:120])
SPXseries <- ts(SPX$PercentChange[1:120])

# 執行 Granger 因果性檢驗
granger_test <- grangertest(PMIseries ~ SPXseries,order=5)
print(granger_test)


## test2

PMIseries <- ts(PMI$dummy[1:120])
SPXseries <- ts(SPX$dummy[1:120])

# 執行 Granger 因果性檢驗
granger_test <- grangertest(PMIseries ~ SPXseries)
print(granger_test)

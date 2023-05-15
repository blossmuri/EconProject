# 嘗試分別在2018-2023年,2013-2018年,2008-2013年,2003-2008,1998-2003年的情況，
# 利用ccf & granger's causality test，找出PMI是SPX多少個月的領先指標 

install.packages("dplyr") ; install.packages("lmtest") ; install.packages("vars")
library(lmtest) ; library(dplyr) ; library(ggplot2) ; library(vars)

PMI <- read.csv("data/PMI.csv")
SPX <- read.csv("data/SPX.csv")
options(digits = 4)

############################ 

M <- matrix(nrow = 5, ncol = 5)
colnames(M) <- c("2018-2023", "2013-2018", "2008-2013", "2003-2008","1998-2003")
rownames(M) <- c("ccf.lag-month", "ccf.max-corr", "OLS.B1","OLS.R-squared","Granger.p-value")

b = 1 

for (i in c(1,2,3,4,5)){ a = 1 

SPXplot <- ts(rev(SPX$MoM[(1 + 60*(i - 1)) : (60*i)]), start = c((2023-5*i), 3), frequency = 12)
PMIplot <- ts(rev(PMI$MoM[(1 + 60*(i - 1)) : (60*i)]), start = c((2023-5*i), 3), frequency = 12)

plot(SPXplot, main = "SPX and PMI Series", ylim= c(-20, 25), xlab = "Time", ylab = "% Change") ; lines(PMIplot, col = "blue")

plot(SPX$YoY[(1 + 60*(i - 1)) : (60*i)],PMI$YoY[(1 + 60*(i - 1)) : (60*i)],
     xlab = "SPX", ylab = "PMI", main = paste((2023-5*i),"~", (2023-5*(i-1) )))

# test 1. Cross-Correlation 分析
PMIseries <- as.vector(as.numeric(PMI$MoM[(1 + 60*(i - 1)) : (60*i)]))
SPXseries <- as.vector(as.numeric(SPX$MoM[(1 + 60*(i - 1)) : (60*i)]))

ccf_result <- ccf(PMIseries, SPXseries, lag.max = 6, plot = FALSE)

max_corr <- max(ccf_result$acf)
lag_index <- which(ccf_result$acf == max_corr)
lag_time <- as.numeric(ccf_result$lag[lag_index])

M[a,b] = lag_time
a = a + 1
M[a,b] = max_corr 
a = a + 1

# test 2. lm(SPX ~ PMI)的迴歸係數
if (lag_time >= 0) {
  PMIseries <- as.vector(as.numeric(PMI$MoM[ (1 + 60*(i - 1) + lag_time) : (60*i + lag_time  )]))
  SPXseries <- as.vector(as.numeric(SPX$MoM[(1 + 60*(i - 1)) : (60*i)]))
  OLS <- summary(lm(SPXseries ~ PMIseries))
}

else{
  OLS$coefficients[2] = 0
  OLS$adj.r.squared = 0
}

M[a,b] = OLS$coefficients[2]
a = a + 1
M[a,b] = OLS$adj.r.squared
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
  G$Pr[2] = 0
}

M[a,b] = G$Pr[2]
b = b + 1

}


##########################


N <- matrix(nrow = 5, ncol = 5)
colnames(N) <- c("2018-2023", "2013-2023", "2008-2023", "2003-2023","1998-2023")
rownames(N) <- c("ccf.lag-month", "ccf.max-corr", "OLS.B1","OLS.R-squared","Granger.p-value")

b = 1

for (i in c(60,120,180,240,300)){ a = 1 

# test 1. Cross-Correlation 分析
PMIseries <- as.vector(as.numeric(PMI$MoM[1:i]))
SPXseries <- as.vector(as.numeric(SPX$MoM[1:i]))
ccf_result <- ccf( PMIseries,  SPXseries, lag.max = 6 , plot = FALSE)

max_corr <- max(ccf_result$acf)
lag_index <- which(ccf_result$acf == max_corr)
lag_time <- as.numeric(ccf_result$lag[lag_index])

N[a,b] = lag_time
a = a + 1
N[a,b] = max_corr
a = a + 1

# test 2. lm(SPX~PMI)的迴歸係數

if(lag_time >= 0){
  
  L = 1 + lag_time ; U = i + lag_time 
  PMIseries <- as.vector(as.numeric(PMI$MoM[ L : U ])) 
  SPXseries <- as.vector(as.numeric(SPX$MoM[ 1 : i ]))
  OLS <- summary(lm(SPXseries ~ PMIseries)) 
  
}

else{
  
  OLS$coefficients[2] = 0 ; OLS$adj.r.squared = 0
  
}

N[a,b] = OLS$coefficients[2]
a = a + 1 
N[a,b] = OLS$adj.r.squared
a = a + 1

# test 3. Granger's causality

if(lag_time >= 0){
  
  data <- cbind(SPXseries, PMIseries) # 假設有兩個時間序列
  var_result <- VAR(data, p = 10, type = "const")  # 指定最大滯後期數為 10
  
  # 自動選擇滯後期數
  select_result <- VARselect(data, lag.max = 10, type = "const")
  
  G <- grangertest(PMIseries ~ SPXseries, order = select_result$selection[1])
  
}

else{
  
  G$Pr[2] = 0
  
}

N[a,b] = G$Pr[2]
b = b + 1

}

##################

SPXplot <- ts(rev(SPX$MoM[1:300]), start = c(1998, 3), frequency = 12)
PMIplot <- ts(rev(PMI$MoM[1:300]), start = c(1998, 3), frequency = 12)
plot(SPXplot, main = "SPX and PMI Series",ylim= c(-20, 25), xlab = "Time", ylab = "Value") ; lines(PMIplot, col = "blue")

M ; N


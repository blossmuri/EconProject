# 以每五年為一個跨度，from 1983 - 2003
# 嘗試利用ccf & granger's causality test，判斷PMI作為SPX領先指標是否有統計上顯著性 

#我把滯後期數改成用ccf得出的lag_time
#原本用var.select得出來的感覺像是optimized的結果，雖然會有不錯的p value，但看經濟上的解釋沒有道理。因此granger test 跟 OLS  都改用ccf的lag_time來回歸。

install.packages("dplyr") ; install.packages("lmtest") ; install.packages("vars") ; install.packages("readxl")
library(lmtest) ; library(dplyr) ; library(ggplot2) ; library(vars) ; library(readxl)

PMI <- read.csv("data/PMI.csv")
SPX <- read_excel("data/SPX.xlsx")
options(digits = 5)
Table <- list()

############################ Each 5-years


for (i in c(1,2,3,4,5,6,7,8)){  j <- paste((2023-5*i),"~",(2023-5*(i-1)))

  Table[[j]] <- list() 
  
  for (l in 1:4){
    Table[[j]][l] <- list()
  }
  
  SPXplot <- ts(rev(SPX$YoY[(1 + 60*(i - 1)) : (60*i)]), start = c((2023-5*i), 3), frequency = 12)
  PMIplot <- ts(rev(PMI$YoY[(1 + 60*(i - 1)) : (60*i)]), start = c((2023-5*i), 3), frequency = 12)
  
  plot(SPXplot, main = "SPX and PMI Series", ylim= c(-45, 60), xlab = "Time", ylab = "% Change") ; lines(PMIplot, col = "blue")
  
  plot(SPX$YoY[(1 + 60*(i - 1)) : (60*i)],PMI$YoY[(1 + 60*(i - 1)) : (60*i)],
       xlab = "SPX", ylab = "PMI", main = paste((2023-5*i),"~", (2023-5*(i-1) )))
  

  # test 1. Cross-Correlation analysis
  
  PMIseries <- as.vector(as.numeric(PMI$YoY[(1 + 60*(i - 1)) : (60*i)]))
  SPXseries <- as.vector(as.numeric(SPX$YoY[(1 + 60*(i - 1)) : (60*i)]))
  
  ccf_result <- ccf(PMIseries, SPXseries, lag.max = 6, plot = FALSE)
  
  max_corr <- max(ccf_result$acf)   
  lag_index <- which(ccf_result$acf == max_corr)  
  lag_time <- as.numeric(ccf_result$lag[lag_index])   
  
  Table[[j]][1] <- list(c(max_corr,lag_time))
  
  
  # test 2. Granger's causality test
  
  if (lag_time > 0) {
    
     PMIseries <- as.vector(as.numeric(PMI$YoY[(1 + 60*(i - 1)) : (60*i)]))
     SPXseries <- as.vector(as.numeric(SPX$YoY[(2 + 60*(i - 1)) : (60*i+1)]))
    
     dat <- data.frame(Y = SPXseries , X = PMIseries)

     G1 <- list(grangertest( Y ~ X , order = (lag_time+1) , data = dat))
     G2 <- "Not applicable"
  
    
  } else if (lag_time == 0){ 
    
     PMIseries <- as.vector(as.numeric(PMI$YoY[(1 + 60*(i - 1)) : (60*i)]))
     SPXseries <- as.vector(as.numeric(SPX$YoY[(2 + 60*(i - 1)) : (60*i+1)]))
  
     dat <- data.frame(Y = SPXseries , X = PMIseries)
    
     G1 <- list(grangertest( Y ~ X , order = (lag_time+1)  , data = dat))
     
     
     PMIseries <- as.vector(as.numeric(PMI$YoY[(1 + 60*(i - 1)) : (60*i)]))
     SPXseries <- as.vector(as.numeric(SPX$YoY[(1 + 60*(i - 1)) : (60*i)]))
     
     dat <- data.frame(Y = PMIseries , X = SPXseries)
     
     G2 <- list(grangertest( Y ~ X , order = (lag_time+1)  , data = dat))
     
  
  } else { 
    
    PMIseries <- as.vector(as.numeric(PMI$YoY[(1 + 60*(i - 1)) : (60*i)]))
    SPXseries <- as.vector(as.numeric(SPX$YoY[(1 + 60*(i - 1)) : (60*i)]))
    
    dat <- data.frame(Y = PMIseries , X = SPXseries)
    
    G1 <- "Not applicable"
    
    G2 <- list(grangertest( Y ~ X , order = (abs(lag_time)+1)  , data = dat))
     
     
  }
  
  Table[[j]][2] <- G1
  Table[[j]][3] <- G2
  
  
  # test 3. OLS model
  
  if (lag_time >= 0) {
    
    
    Y = as.vector(as.numeric(SPX$YoY[(1 + 60*(i - 1)) : (60*i)]))
    
    X <- matrix(0 , nrow = 60, ncol = (lag_time + 1) )
    
    for (k in 1:60) {
      
      PMIsubset <- as.vector(as.numeric(PMI$YoY[( 60*(i - 1) + k) : (60*(i-1) + k + (lag_time))]))
      X[k, ] <-  PMIsubset
      
    }
    
    OLS <- lm( Y ~ X)
   
  } else {
  
    OLS <- "Not applicable"
    
  }
  
  Table[[j]][4] <- list(summary(OLS))
  
}

############################ ALL YEARS


PMIseries <- as.vector(as.numeric(PMI$YoY[1:480]))
SPXseries <- as.vector(as.numeric(SPX$YoY[1:480]))

j <- paste(1983,"~",2023)

Table[[j]] <- list() 

for (l in 1:4){
  Table[[j]][l] <- list()
}

#Test 1.

ccf_result <- ccf( PMIseries,  SPXseries, lag.max = 6 , plot = FALSE)

max_corr <- max(ccf_result$acf)
lag_index <- which(ccf_result$acf == max_corr)
lag_time <- as.numeric(ccf_result$lag[lag_index])

Table[[j]][1] <- list(c(max_corr,lag_time))

# Test 2.

if(lag_time >= 0){
  
  
  PMIseries <- as.vector(as.numeric(PMI$YoY[1:480]))
  SPXseries <- as.vector(as.numeric(SPX$YoY[2:481]))
  
  dat <- data.frame(Y = SPXseries, X = PMIseries)
  
  G1 <- list(grangertest( Y ~ X , order = (lag_time + 1) , data = dat))
  G2 <- "Not applicable"
  
  
} else if (lag_time == 0){ 
  
  PMIseries <- as.vector(as.numeric(PMI$YoY[1:480]))
  SPXseries <- as.vector(as.numeric(SPX$YoY[2:481]))
  
  dat <- data.frame(Y = SPXseries , X = PMIseries)
  
  G1 <- list(grangertest( Y ~ X , order = (lag_time+1)  , data = dat))
  
  
  PMIseries <- as.vector(as.numeric(PMI$YoY[1:480]))
  SPXseries <- as.vector(as.numeric(SPX$YoY[1:480]))
  
  dat <- data.frame(Y = PMIseries , X = SPXseries)
  
  G2 <- list(grangertest( Y ~ X , order = (lag_time+1)  , data = dat))
  
  
} else { 
  
  PMIseries <- as.vector(as.numeric(PMI$YoY[1:480]))
  SPXseries <- as.vector(as.numeric(SPX$YoY[1:480]))
  
  dat <- data.frame(Y = PMIseries , X = SPXseries)
  
  G1 <- "Not applicable"
  
  G2 <- list(grangertest( Y ~ X , order = (abs(lag_time)+1)  , data = dat))
  
  
}

Table[[j]][2] <- G1
Table[[j]][3] <- G2


# test 3. OLS model

if (lag_time >= 0) {
  
  
  Y = as.vector(as.numeric(SPX$YoY[1:480]))
  
  X <- matrix(0 , nrow = 480 , ncol = (lag_time + 1) )
  
  for (k in 1:480) {
    
    PMIsubset <- as.vector(as.numeric(PMI$YoY[k : k + (lag_time)]))
    X[k, ] <-  PMIsubset
    
  }
  
  OLS <- lm( Y ~ X)
  
} else {
  
  OLS <- "Not applicable"
  
}

Table[[j]][4] <- list(summary(OLS))



############################ Result


Table
SPXplot <- ts(rev(SPX$YoY[1:480]), start = c(1983, 4), frequency = 12)
PMIplot <- ts(rev(PMI$YoY[1:480]), start = c(1983, 4), frequency = 12)
plot(SPXplot, main = "SPX and PMI Series",ylim= c(-45, 60), xlab = "Time", ylab = "Value") ; lines(PMIplot, col = "blue")


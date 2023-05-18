# 嘗試分別在2018-2023年,2013-2018年,2008-2013年,2003-2008,1998-2003年的情況，
# 利用ccf & granger's causality test，找出PMI是SPX多少個月的領先指標 

install.packages("dplyr") ; install.packages("lmtest") ; install.packages("vars") ; install.packages("readxl")
library(lmtest) ; library(dplyr) ; library(ggplot2) ; library(vars) ; library(readxl)

PMI <- read.csv("data/PMI.csv")
SPX <- read.csv("data/SPX.csv")
options(digits = 4)
Table <- list()

############################ Each 5-years


for (i in c(1,2,3,4,5)){  j <- paste((2023-5*i),"~",(2023-5*(i-1)))

  Table[[j]] <- list() 
  
  for (l in 1:3){
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
  
  if (lag_time >= 0) {
    
    dat <- data.frame(Y = SPXseries, X = PMIseries)

    G <- list(grangertest( Y ~ X , order = (lag_time + 1) , data = dat))
    
  } else {
  
    G = "NA"
    
  }
  
  Table[[j]][2] <- G
  
  
  # test 3. OLS model
  
  if (lag_time >= 0) {
    
    
    Y = SPXseries <- as.vector(as.numeric(SPX$YoY[(1 + 60*(i - 1)) : (60*i)]))
    
    X <- matrix(0 , nrow = 60, ncol = (lag_time + 1) )
    
    for (k in 1:60) {
      
      PMIsubset <- as.vector(as.numeric(PMI$YoY[( 60*(i - 1) + k) : (60*(i-1) + k + (lag_time))]))
      X[k, ] <-  PMIsubset
      
    }
    
    OLS <- lm( Y ~ X)
   
  } else {
  
    OLS <- "NA"
    
  }
  
  Table[[j]][3] <- list(summary(OLS))
  
}

############################ ALL YEARS


PMIseries <- as.vector(as.numeric(PMI$YoY[1:300]))
SPXseries <- as.vector(as.numeric(SPX$YoY[1:300]))
j <- paste(1998,"~",2023)
Table[[j]] <- list() 
for (l in 1:3){
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
  
  dat <- data.frame(Y = SPXseries, X = PMIseries)
  
  G <- list(grangertest( Y ~ X , order = (lag_time + 1) , data = dat))
  
  
} else {

  G = "NA"
  
}

Table[[6]][2] = G

# test 3.

if(lag_time >= 0){
  
  L = 1 + lag_time ; U = 300 + lag_time 
  X <- as.vector(as.numeric(PMI$YoY[ L : U ])) 
  Y <- as.vector(as.numeric(SPX$YoY[ 1 : 300]))
  OLS <- lm(Y ~ X) 
  
} else{

  OLS = "NA"
  
}

Table[[6]][3] = list(summary(OLS))


############################ Result


Table
SPXplot <- ts(rev(SPX$YoY[1:300]), start = c(1998, 3), frequency = 12)
PMIplot <- ts(rev(PMI$YoY[1:300]), start = c(1998, 3), frequency = 12)
plot(SPXplot, main = "SPX and PMI Series",ylim= c(-45, 60), xlab = "Time", ylab = "Value") ; lines(PMIplot, col = "blue")


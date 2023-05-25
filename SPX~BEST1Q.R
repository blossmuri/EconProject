# 嘗試分別在2018-2023年,2013-2018年,2008-2013年,2003-2008,1998-2003年的情況，
# 利用ccf & granger's causality test，找出Foward eps estimate是SPX多少個月的領先指標 

install.packages("dplyr") ; install.packages("lmtest") ; install.packages("vars") ; install.packages("readxl")
library(lmtest) ; library(dplyr) ; library(ggplot2) ; library(vars) ; library(readxl)

FPE <- read_excel("data/BEST.xlsx")
SPX <- read_excel("data/SPX.xlsx")
options(digits = 5)
Table <- list()

############################ Each 5-years


for (i in c(1,2,3,4,5)){  j <- paste((2023-5*i),"~",(2023-5*(i-1)))

  Table[[j]] <- list() 
  
  for (l in 1:3){
    Table[[j]][l] <- list()
  }
  
  SPXplot <- ts(rev(SPX$YoY[(1 + 60*(i - 1)) : (60*i)]), start = c((2023-5*i), 4), frequency = 12)
  FPEplot <- ts(rev(FPE$YoY1Q[(1 + 60*(i - 1)) : (60*i)]), start = c((2023-5*i), 4), frequency = 12)
  
  plot(SPXplot, main = "SPX and FPE Series", ylim= c(-45, 60), xlab = "Time", ylab = "% Change") ; lines(FPEplot, col = "blue")
  
  plot(SPX$YoY[(1 + 60*(i - 1)) : (60*i)],FPE$YoY1Q[(1 + 60*(i - 1)) : (60*i)],
       xlab = "SPX", ylab = "FPE1Q", main = paste((2023-5*i),"~", (2023-5*(i-1) )))
  
  
  # test 1. Cross-Correlation analysis
  
  FPEseries <- as.vector(as.numeric(FPE$YoY1Q[(1 + 60*(i - 1)) : (60*i)]))
  SPXseries <- as.vector(as.numeric(SPX$YoY[(1 + 60*(i - 1)) : (60*i)]))
  
  ccf_result <- ccf(FPEseries, SPXseries, lag.max = 6, plot = FALSE)
  
  max_corr <- max(ccf_result$acf)   
  lag_index <- which(ccf_result$acf == max_corr)  
  lag_time <- as.numeric(ccf_result$lag[lag_index])   
  
  Table[[j]][1] <- list(c(max_corr,lag_time))
  

  
  # test 2. Granger's causality test
  
  if (lag_time > 0) {
    
    FPEseries <- as.vector(as.numeric(FPE$YoY1Q[(1 + 60*(i - 1)) : (60*i)]))
    SPXseries <- as.vector(as.numeric(SPX$YoY[(2 + 60*(i - 1)) : (60*i+1)]))
    
    dat <- data.frame(Y = SPXseries , X = FPEseries)
    
    G1 <- list(grangertest( Y ~ X , order = (lag_time+1) , data = dat))
    G2 <- "NA"
    
    
  } else if (lag_time == 0){ 
    
    FPEseries <- as.vector(as.numeric(FPE$YoY1Q[(1 + 60*(i - 1)) : (60*i)]))
    SPXseries <- as.vector(as.numeric(SPX$YoY[(2 + 60*(i - 1)) : (60*i+1)]))
    
    dat <- data.frame(Y = SPXseries , X = FPEseries)
    
    G1 <- list(grangertest( Y ~ X , order = (lag_time+1)  , data = dat))
    
    
    FPEseries <- as.vector(as.numeric(FPE$YoY1Q[(1 + 60*(i - 1)) : (60*i)]))
    SPXseries <- as.vector(as.numeric(SPX$YoY[(1 + 60*(i - 1)) : (60*i)]))
    
    dat <- data.frame(Y = FPEseries , X = SPXseries)
    
    G2 <- list(grangertest( Y ~ X , order = (lag_time+1)  , data = dat))
    
    
  } else { 
    
    FPEseries <- as.vector(as.numeric(FPE$YoY1Q[(1 + 60*(i - 1)) : (60*i)]))
    SPXseries <- as.vector(as.numeric(SPX$YoY[(1 + 60*(i - 1)) : (60*i)]))
    
    dat <- data.frame(Y = FPEseries , X = SPXseries)
    
    G1 <- "NA"
    
    G2 <- list(grangertest( Y ~ X , order = (abs(lag_time)+1)  , data = dat))
    
    
  }
  
  Table[[j]][2] <- G1
  Table[[j]][3] <- G2
  
  
  
  # test 3. OLS model
  
  if (lag_time >= 0) {
    
    
    Y = SPXseries <- as.vector(as.numeric(SPX$YoY[(1 + 60*(i - 1)) : (60*i)]))
    
    X <- matrix(0 , nrow = 60, ncol = (lag_time + 1) )
    
    for (k in 1:60) {
      
      FPEsubset <- as.vector(as.numeric(FPE$YoY1Q[( 60*(i - 1) + k) : (60*(i-1) + k + (lag_time))]))
      X[k, ] <-  FPEsubset
      
    }
    
    OLS <- lm( Y ~ X)
    
  } else {
    
    OLS <- "NA"
    
  }
  
  Table[[j]][4] <- list(summary(OLS))

}

Table
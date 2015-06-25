
#' Creating data to test the Gini-function
#' @return A data.frame with test data for the function huvud that calculates gini
#' @export 
#' @details Just for testing! 
testdata <- function(){ 
  pid <- rep(1:10, each=2)## test
  ar <- rep(c(1:2), 10)
  region <- c(3,3,3,1,1,1,2,2,2,2,3,3,3,1,1,1,2,2,2,2)
  income <- c(rnorm(10,50,10),rnorm(10,150,10))
  
  data <- as.data.frame(cbind(pid, ar, region,income)) 
}

#' huvud function
#' This functions takes a data.frame with region and income and calculates Gini coefficients for every region 
#' @param data A data.frame with one column region and one column income
#' @return reg.gini A data.frame with columns: reg      gini      mean   quart1    median   quart3 no.cases
#' @export 
#' @author Erling Haggstrom Lundevaller
#' @details This functions takes a data.frame with region and income and calculates Gini coefficients for every region.
#'  Note that there must be collumns named region and income
huvud <- function(data = data){ ## gini<-huvud(data)
  data <- data[order(data$region, data$income),]  
  
  reg.gini <- as.data.frame(cbind(unique(data$region), rep(NA, length(unique(data$region))),rep(NA, length(unique(data$region))),rep(NA, length(unique(data$region))), rep(NA, length(unique(data$region))),rep(NA, length(unique(data$region))),rep(NA, length(unique(data$region)))))## Skapar container for regionernas ginis
  names(reg.gini) <- c("region", "gini", "mean", "quart1", "median", "quart3","no.cases")
   for(i in 1:length(reg.gini$region)){
    income <- data$income[data$region == reg.gini$region[i]]
    kumul.v <- kumul(vec=income)
    income.mean <- rep(mean(income), length(income))
    kumul.mean <- kumul(vec=income.mean)
    reg.gini$gini[i] <- 1-sum(kumul.v)/sum(kumul.mean)
    reg.gini$mean[i] <- mean(income)
    reg.gini[i, c("quart1", "median", "quart3")] <- quantile(income, c(0.25, 0.5, 0.75) ) 
    reg.gini$median[i] <- median(income)
    reg.gini$no.cases[i] <- length(income)
  }  
  reg.gini
}

#' kumul function
#' This function makes a cumulative vector
#' @author Erling Haggstrom Lundevaller
#' @export 
#' @param vec a sorted vector with incomes
#' @return kum.vec A cumulative vector of the same length as the input vec
#' @details Makes a cumulative vector
kumul <- function(vec){ 
  kum.vec <-rep(NA,length(vec))
  kum.vec[1] <- vec[1]
  for(i in 2:length(vec)){
    kum.vec[i] <- kum.vec[i-1] + vec[i]
  }
  kum.vec
}




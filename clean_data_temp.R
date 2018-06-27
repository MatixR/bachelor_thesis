##----------------------------------------------------------------------------------------------------------##
##Clean interpolated data in order to match dates.
##----------------------------------------------------------------------------------------------------------##
sample_index <- index(window(countries[[1]], start = "2002-01-01", end = "2018-05-01"))
sample_colnames <- colnames(countries[[1]])

extrapDates <- function(data_1)
{
  p <- length(sample_index) - length(data_1[,1])
  data_2 <- matrix(nrow = length(sample_index), ncol = length(sample_colnames))
  for (j in 1:length(data_1[1,])) 
  {
    d <- coredata(data_1[1:p,j])
    x_out <- c(1:p)
    x <- c((p+1):2*p)
    d2 <- approxExtrap(x = x, y = d, xout = x_out)$y
    data_2[,j] <- c(d2, data_1[,j])
  }
  return(data_2)
}

clean_data <- function(data_all)
{
  counter <- t(as.matrix(1:length(data_all)))
  
  for (i in counter) 
  {
    if (length(which(index(data_all[[i]]) == "2002-01-01")) > 0)
    {
      data_all[[i]] <- window(data_all[[i]], start = "2002-01-01", end = "2018-05-01")
    }
    else
    {
      c <- extrapDates(data_all[[i]])
      c <- as.zoo(c)
      colnames(c) <- sample_colnames
      index(c) <- sample_index
      data_all[[i]] <- c
    }
  }
  
  return(list(countries[[1]], countries[[2]], countries[[3]], countries[[4]], countries[[6]],
              countries[[6]], countries[[7]], countries[[8]], countries[[9]], countries[[10]]))
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##
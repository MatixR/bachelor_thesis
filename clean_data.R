##----------------------------------------------------------------------------------------------------------##
##Clean interpolated data in order to match dates.
##----------------------------------------------------------------------------------------------------------##
cleanData <- function(data_list, start_date, end_date)
{
  sample_index <- index(window(data_list[[1]], start = start_date, end = end_date))
  for (i in length(data_list))
  {
    data_list[[i]] <- window(data_list[[i]], start = start_date, end = end_date)
    index(data_list[[i]]) <- sample_index
  }
}

countries_main <- cleanData(countries_main, "1995-01-01", "2017-12-01")
countries_extension <- cleanData(countries_extension, "2002-01-01", "2017-12-01")

# extrapDates <- function(data_1)
# {
#   p <- length(sample_index) - length(data_1[,1])
#   data_2 <- matrix(nrow = length(sample_index), ncol = length(sample_colnames))
#   for (j in 1:length(data_1[1,])) 
#   {
#     d <- coredata(data_1[1:p,j])
#     x_out <- c(1:p)
#     x <- c((p+1):2*p)
#     d2 <- approxExtrap(x = x, y = d, xout = x_out)$y
#     data_2[,j] <- c(d2, data_1[,j])
#   }
#   return(data_2)
# }
# 
# clean_data <- function(data_all)
# {
#   counter <- t(as.matrix(1:length(data_all)))
# 
#   for (i in counter) 
#   {
#     if (length(which(index(data_all[[i]]) == "2000-01-01")) > 0)
#     {
#       data_all[[i]] <- window(data_all[[i]], start = "2000-01-01", end = "2018-05-01")
#     }
#     else
#     {
#       c <- extrapDates(data_all[[i]])
#       c <- as.zoo(c)
#       colnames(c) <- sample_colnames
#       index(c) <- sample_index
#       data_all[[i]] <- c
#     }
#   }
#   
#   return(data_all)
# }
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##
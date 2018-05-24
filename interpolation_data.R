##----------------------------------------------------------------------------------------------------------##
##Linear interpolation of data. 
##----------------------------------------------------------------------------------------------------------##
interpData <- function(data_1, x)
{
  counter <- 1:length(data_1[,1])
  result <- apply(counter, 1, lin_int <- function(i)
                  {
                    y <- data_1[i,]
                    return(t(spline(x = x, y = y, n = 40, xmin = 0.25, xmax = 10)$y))
                  })
  return(result)
}

# interpolate <- function(D, maturities)
# {
#   data_interp <- interpData(D, maturities)
#   
# }
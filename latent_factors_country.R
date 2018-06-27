##----------------------------------------------------------------------------------------------------------##
##Obtain country specific latent factors for level slope and curvature.
##----------------------------------------------------------------------------------------------------------##
regression_country <- function(data_c)
{
  counter <- t(as.matrix(1:length(data_c[,1])))
  tau <- strtoi(colnames(data_c))
  results <- apply(counter, 2, regression <- function(i)
  {
    data_t <- data_c[i,]
    lambda <- 0.064
    
    slope <- (1 - exp(1)^(-lambda * tau))/(lambda*tau)
    curvature <- slope - exp(1)^(-lambda * tau)
    
    fit <- lm(t(data_t) ~ 1 + slope + curvature)
    coef_fit <- t(as.matrix(coredata(coefficients(fit))))
    colnames(coef_fit) <- c("level", "slope", "curvature")
    res <- t(as.matrix(coredata(residuals(fit))))
    return(cbind(coef_fit, res))
  })
  col_names <- c("level", "slope", "curvature", tau)
  results <- t(results)
  colnames(results) <- col_names
  return(results)
}

retrieve_country_lf <- function(data_all)
{
  counter <- t(as.matrix(1:length(data_all)))
  resids <- list()
  factors <- list()
  
  for (i in counter) 
  {
    data_c <- data_all[[i]]
    result <- regression_country(data_c)
    factor_c <- as.zoo(result[,c(1:3)])
    res_c <- as.zoo(result[,c(4:length(result[1,]))])
    index(factor_c) <- index(data_c)
    index(res_c) <- index(data_c)
    resids[[i]] <- res_c
    factors[[i]] <- factor_c
  }
  
  return(list(Factors = factors, Res = resids))
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##
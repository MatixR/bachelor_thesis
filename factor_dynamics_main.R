##----------------------------------------------------------------------------------------------------------##
## Factor dynamics for the replication model.
##----------------------------------------------------------------------------------------------------------##
factor_dynamicsMain <- function(local_factors, global_factors)
{
  # Estimate equation 5, the AR(1) model of the global factors
  ARmodel_globalLevel <- arima(global_factors[,1], order = c(1, 0, 0), include.mean = FALSE)
  ARmodel_globalSlope <- arima(global_factors[,2], order = c(1, 0, 0), include.mean = FALSE)
  ARmodel_globalCurvature <- arima(global_factors[,3], order = c(1, 0, 0), include.mean = FALSE)
  ARmodel_global <- list(level = ARmodel_globalLevel, slope = ARmodel_globalSlope,
                         curvature = ARmodel_globalCurvature)
  
  # Estimate equations 6 and 7, the dependence of local factors on global factors and the importance 
  # of the idiosyncratic components of the country-specific factors.
  MAmodel_local <- lapply(as.matrix(1:length(local_factors$l[1,])), function(i)
  {
    MAlevel <- arima(local_factors$l[,i], order = c(0, 0, 1), xreg = global_factors[,1])
    MAslope <- arima(local_factors$s[,i], order = c(0, 0, 1), xreg = global_factors[,2])
    MAcurvature <- arima(local_factors$c[,i], order = c(0, 0, 1), xreg = global_factors[,3])
    
    return(list(level = MAlevel, slope = MAslope, curvature = MAcurvature))
  })
  
  return(list(ARmodel_global = ARmodel_global, MAmodel_local = MAmodel_local))
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##
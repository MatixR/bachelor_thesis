##----------------------------------------------------------------------------------------------------------##
## Factor dynamics for the extension model.
##----------------------------------------------------------------------------------------------------------##
factor_dynamicsExt <- function(local_factors, regional_factors, global_factors)
{
  # Estimate the AR(1) model of the global factors
  ARmodel_globalLevel <- arima(global_factors[,1], order = c(1, 0, 0), include.mean = FALSE)
  ARmodel_globalSlope <- arima(global_factors[,2], order = c(1, 0, 0), include.mean = FALSE)
  ARmodel_globalCurvature <- arima(global_factors[,3], order = c(1, 0, 0), include.mean = FALSE)
  ARmodel_global <- list(level = ARmodel_globalLevel, slope = ARmodel_globalSlope,
                         curvature = ARmodel_globalCurvature)
  
  # Estimate the dependence of the regional factors on the global factors and the idiosyncratic factors
  MAmodel_regionLevel <- arima(regional_factors[,1], 
                               order = c(0, 0, 1), xreg = global_factors[,1])
  MAmodel_regionSlope <- arima(regional_factors[,2], 
                               order = c(0, 0, 1), xreg = global_factors[,2])
  MAmodel_regionCurvature <- arima(regional_factors[,3], 
                               order = c(0, 0, 1), xreg = global_factors[,3])
  MAmodel_region <- list(level = MAmodel_regionLevel, slope = MAmodel_regionSlope, 
                         curvature = MAmodel_regionCurvature)
  
  # Estimate the dependence of the local factors on the regional factors and the idiosyncratic factors
  MAmodel_local <- lapply(as.matrix(1:length(local_factors$l[1,])), function(i)
  {
    MAlevel <- arima(local_factors$l[,i], order = c(0, 0, 1), xreg = regional_factors[,1])
    MAslope <- arima(local_factors$s[,i], order = c(0, 0, 1), xreg = regional_factors[,2])
    MAcurvature <- arima(local_factors$c[,i], order = c(0, 0, 1), xreg = regional_factors[,3])
    
    return(list(level = MAlevel, slope = MAslope, curvature = MAcurvature))
  })
  
  return(list(ARmodel_global = ARmodel_global, MAmodel_region = MAmodel_region, MAmodel_local = MAmodel_local))
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##
## Perform principal component analysis on factors to obtain global/regional factors.
##----------------------------------------------------------------------------------------------------------##
group_factors <- function(factor_list, v)
{
  local_level <- matrix(nrow = length(factor_list$Factors[[1]][,1]), ncol = length(v))
  local_slope <- matrix(nrow = length(factor_list$Factors[[1]][,1]), ncol = length(v))
  local_curvature <- matrix(nrow = length(factor_list$Factors[[1]][,1]), ncol = length(v))
  
  for (i in 1:length(v)) 
  {
    local_level[,i] <- factor_list$Factors[[(i+v[1]-1)]][,1]
    local_slope[,i] <- factor_list$Factors[[(i+v[1]-1)]][,2]
    local_curvature[,i] <- factor_list$Factors[[(i+v[1]-1)]][,3]
  }
  local_level <- as.xts(local_level, order.by = time(factor_list$Factors[[1]][,1]))
  local_slope <- as.xts(local_slope, order.by = time(factor_list$Factors[[1]][,1]))
  local_curvature <- as.xts(local_curvature, order.by = time(factor_list$Factors[[1]][,1]))
  
  return(list(l = local_level, s = local_slope, c = local_curvature))
}

pca <- function(data_list, v)
{
  local_factors <- group_factors(data_list, v)
  
  global_level <- as.xts(-prcomp(local_factors$l, center = TRUE, scale. = TRUE)$x[,1], 
                         order.by = time(data_list$Factors[[1]][,1]))
  global_slope <- as.xts(-prcomp(local_factors$s, center = TRUE, scale. = TRUE)$x[,1], 
                         order.by = time(data_list$Factors[[1]][,1]))
  global_curvature <- as.xts(-prcomp(local_factors$c, center = TRUE, scale. = TRUE)$x[,1], 
                         order.by = time(data_list$Factors[[1]][,1]))
  global_factors <- cbind(global_level, global_slope, global_curvature)
  colnames(global_factors) <- cbind("level", "slope", "curvature")
  return(list(global = global_factors, local = local_factors))
}

pca_region <- function(region_list)
{
  regional_level <- matrix(nrow = length(region_list[[1]][,1]), ncol = length(region_list))
  regional_slope <- matrix(nrow = length(region_list[[1]][,1]), ncol = length(region_list))
  regional_curvature <- matrix(nrow = length(region_list[[1]][,1]), ncol = length(region_list))
  
  for (i in 1:length(region_list)) 
  {
    regional_level[,i] <- region_list[[i]][,1]
    regional_slope[,i] <- region_list[[i]][,2]
    regional_curvature[,i] <- region_list[[i]][,3]
    
  }
  regional_factors <- list(l = regional_level, s = regional_slope, c = regional_curvature)
  
  global_level <- as.xts(-prcomp(regional_factors$l, center = TRUE, scale. = TRUE)$x[,1], 
                         order.by = time(region_list[[1]][,1]))
  global_slope <- as.xts(-prcomp(regional_factors$s, center = TRUE, scale. = TRUE)$x[,1], 
                         order.by = time(region_list[[1]][,1]))
  global_curvature <- as.xts(-prcomp(regional_factors$c, center = TRUE, scale. = TRUE)$x[,1], 
                             order.by = time(region_list[[1]][,1]))
  global_factors <- cbind(global_level, global_slope, global_curvature)
  colnames(global_factors) <- cbind("level", "slope", "curvature")
  return(global_factors)
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##
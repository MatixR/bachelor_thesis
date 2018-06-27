##----------------------------------------------------------------------------------------------------------##
## Main function.
##----------------------------------------------------------------------------------------------------------##
setwd("~/Documents/Repositories/bachelor_thesis/")
source("source_file.R")
source("latent_factors_country.R")
source("pca.R")
source("factor_dynamics_main.R")
source("factor_dynamics_extension.R")
source("forecasting.R")


# Obtain local latent factors for main and extension countries
main_lf <- retrieve_country_lf(countries_main)
extension_lf <- retrieve_country_lf(countries_extension)

# Perform pca to obtain regional and global latent factors
factorsMain <- pca(main_lf, 1:5)
northAmerican_factors <- pca(extension_lf, 1:2)
european_factors <- pca(extension_lf, 3:6)
asian_factors <- pca(extension_lf, 7:10)
global_factorsExt <- pca_region(list(northAmerican_factors$global, 
                                     european_factors$global, asian_factors$global))

# Obtain the factor dynamics for local, regional and global latent factors
factor_dynamics <- factor_dynamicsMain(factorsMain$local, factorsMain$global)
factor_dynamics_northAmerica <- factor_dynamicsExt(northAmerican_factors$local, 
                                                   northAmerican_factors$global, global_factorsExt)
factor_dynamics_europe <- factor_dynamicsExt(european_factors$local, 
                                             european_factors$global, global_factorsExt)
factor_dynamcis_asia <- factor_dynamicsExt(asian_factors$local, 
                                           asian_factors$global, global_factorsExt)

forecasts <- forecast(countries_extension, 120)
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##
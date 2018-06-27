##----------------------------------------------------------------------------------------------------------##
## Reads and transforms the data.
##----------------------------------------------------------------------------------------------------------##
# Import data and convert into time series format
countries_main <- list()
countries_extension <- list()

load("/Users/user/Documents/Repositories/bachelor_thesis/data_raw/raw_yield_curves.RData")
ca_raw <- ca_raw[,c(5, 2, 3, 4, 6, 1)]
uk_raw <- cbind(uk_raw[,1], uk_raw[,2:length(uk_raw[1,])])
de_raw <- cbind(de_raw[,1], de_raw[,2:length(de_raw[1,])])
us_raw <- us_raw[!is.na(us_raw[,1]),]
month <- function(x) format(x, "%Y-%m")
us_raw <- aggregate(us_raw, by=month, FUN=last)
jp_raw <- aggregate(jp_raw, by=month, FUN=last)
us_raw <- as.xts(us_raw, order.by = as.yearmon(format(time(us_raw)), "%Y-%m"))
us_raw <- cbind(us_raw[,1], us_raw[,2:length(us_raw[1,])])

countries_main[[1]] <- read.zoo("./data_raw/canada.csv", header = TRUE, sep = ",",format="%m/%d/%Y",index.column = 1)
countries_main[[2]] <- de_raw
countries_main[[3]] <- jp_raw
countries_main[[4]] <- uk_raw
countries_main[[5]] <- us_raw

countries_extension[[1]] <- read.zoo("./data_raw/canada.csv", header = TRUE, sep = ",",format="%m/%d/%Y",index.column = 1)
countries_extension[[2]] <- us_raw
countries_extension[[3]] <- de_raw
countries_extension[[4]] <- uk_raw
countries_extension[[5]] <- read.zoo("./data_raw/italy.csv", header = TRUE, sep = ",",format="%m/%d/%Y",index.column = 1)
countries_extension[[6]] <- read.zoo("./data_raw/france.csv", header = TRUE, sep = ",",format="%m/%d/%Y",index.column = 1)
countries_extension[[7]] <- read.zoo("./data_raw/singapore.csv", header = TRUE, sep = ",",format="%m/%d/%Y",index.column = 1)
countries_extension[[8]] <- read.zoo("./data_raw/south_korea.csv", header = TRUE, sep = ",",format="%m/%d/%Y",index.column = 1)
countries_extension[[9]] <- read.zoo("./data_raw/hong_kong.csv", header = TRUE, sep = ",",format="%m/%d/%Y",index.column = 1)
# countries_extension[[10]] <- read.zoo("./data_raw/philippines.csv", header = TRUE, sep = ",",format="%m/%d/%Y",index.column = 1)
countries_extension[[10]] <- jp_raw

# Interpolate data (17 maturities)
for (i in 1:length(countries_main)) 
{
  countries_main[[i]] <- interpolate(countries_main[[i]])
}

# Interpolate data (17 maturities)
for (i in 1:length(countries_extension)) 
{
  countries_extension[[i]] <- interpolate(countries_extension[[i]])
}

remove(ca_raw)
remove(de_raw)
remove(jp_raw)
remove(uk_raw)
remove(us_raw)

sample_index_main <- index(window(countries_main[[2]], start = "1995-01-01", end = "2017-12-01"))
sample_index_ext <- index(window(countries_extension[[2]], start = "2002-01-01", end = "2017-12-01"))

countries_main[[1]] <- window(countries_main[[1]], start = "1995-01-01", end = "2017-12-01")
countries_main[[2]] <- window(countries_main[[2]], start = "1995-01-01", end = "2017-12-01")
countries_main[[3]] <- window(countries_main[[3]], start = "1995", end = "2017-12-01")
countries_main[[4]] <- window(countries_main[[4]], start = "1995-01-01", end = "2017-12-31")
countries_main[[5]] <- window(countries_main[[5]], start = "1995-01-01", end = "2017-12-01")

countries_extension[[1]] <- window(countries_extension[[1]], start = "2002-01-01", end = "2017-12-01")
countries_extension[[2]] <- window(countries_extension[[2]], start = "2002-01-01", end = "2017-12-01")
countries_extension[[3]] <- window(countries_extension[[3]], start = "2002-01-01", end = "2017-12-01")
countries_extension[[4]] <- window(countries_extension[[4]], start = "2002-01-01", end = "2017-12-31")
countries_extension[[5]] <- window(countries_extension[[5]], start = "2002-01-01", end = "2017-12-01")
countries_extension[[6]] <- window(countries_extension[[6]], start = "2002-01-01", end = "2017-12-01")
countries_extension[[7]] <- window(countries_extension[[7]], start = "2002-01-01", end = "2017-12-01")
countries_extension[[8]] <- window(countries_extension[[8]], start = "2002-01-01", end = "2017-12-01")
countries_extension[[9]] <- window(countries_extension[[9]], start = "2002-01-01", end = "2017-12-01")
# countries_extension[[10]] <- window(countries_extension[[10]], start = "2002-01-01", end = "2017-12-01")
countries_extension[[10]] <- window(countries_extension[[10]], start = "2002", end = "2017-12-01")

for (i in 1:length(countries_main))
{
  index(countries_main[[i]]) <- sample_index_main
}

for (i in 1:length(countries_extension))
{
  index(countries_extension[[i]]) <- sample_index_ext
}

##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##
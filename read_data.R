##----------------------------------------------------------------------------------------------------------##
##Reads and transforms the data.
##----------------------------------------------------------------------------------------------------------##

# Import data and convert into time series format
south_korea <- read.zoo("./data_raw/south_korea.csv", header = TRUE, sep = ",",format="%d/%m/%Y",index.column = 1)

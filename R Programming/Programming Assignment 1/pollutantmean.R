pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  sumall <- 0
  nsum <- 0L
  
  for(cur in id) {
    filename <- sprintf("%s%s%03d.csv", directory, .Platform$file.sep, cur)
    data <- read.csv(filename)
    sumall <- sumall + sum(data[[pollutant]], na.rm = TRUE)
    nsum <- nsum + sum(!is.na(data[pollutant]))
  }
  sumall / nsum
}

# Error in FUN(X[[1L]], ...) : 
#   only defined on a data frame with all numeric variables

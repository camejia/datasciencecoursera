corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  df <- complete(directory)
  dfthresh <- df[df$nobs > threshold, ]
  cr <- numeric(nrow(dfthresh))
  if(nrow(dfthresh) > 0) {
    for(cur in 1:nrow(dfthresh)) {
      filename <- sprintf("%s%s%03d.csv", directory, .Platform$file.sep, dfthresh$id[cur])
      data <- read.csv(filename)
      completedata <- data[complete.cases(data), ]
      cr[cur] <- cor(completedata$sulfate, completedata$nitrate)
    }
  }
  cr
}

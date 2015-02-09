complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  # Allocate space for the nobs vector, which is an integer vector of the same length as the id vector
  nobs <- integer(length(id))
  # Loop over all of the input id values
  for(cur in 1:length(id)) {
    # Note that we need to map the one-based loop index cur back to the id that was passed in
    filename <- sprintf("%s%s%03d.csv", directory, .Platform$file.sep, id[cur])
    data <- read.csv(filename)
    # Calculate the number of complete cases for this id
    nobs[cur] <- sum(complete.cases(data))
  }
  # Put the result into an output data frame
  data.frame(id, nobs)
}

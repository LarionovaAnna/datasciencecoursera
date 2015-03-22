complete <- function(directory, id = 1:332) {
  df <- data.frame(id = numeric(), nobs = numeric())
  for (i in id) {
    
    if (i < 10) {
      filename = paste(c('./',directory, '/', '00', i, '.csv'),collapse='')
    }
    else if (i < 100) {
      filename = paste(c('./',directory, '/', '0', i, '.csv'),collapse='')
    }
    else {
      filename = paste(c('./',directory, '/', i, '.csv'),collapse='')
    }
    file <- read.csv(filename)
    numpol <- nrow(file[!rowSums(is.na(file[names(file)])), ])
    
    df <- rbind(df, data.frame(id = i, nobs = numpol))  
  }
    
  df
}
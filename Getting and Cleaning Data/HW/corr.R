corr <- function(directory, threshold = 0) {
  correlations <- c()
  for (i in 1:332) {
    
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
    if (numpol > threshold) {
      sub <- file[!rowSums(is.na(file[names(file)])), ]
      
      correlations <- c(correlations, cor(sub$nitrate, sub$sulfate)) 
      
    } 
  }
  correlations
  
}
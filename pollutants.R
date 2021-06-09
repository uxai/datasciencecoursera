# Get pollutantmean from a CSV file
# where the column name is specified by pollutant variable
#
# Each file is numbers 001.csv to 332.csv

pollutantmean <- function(directory, pollutant, id = 1:332) {
  val = 0
  tot = 0
  for(ids in id) {
    if(ids < 10) {
      pre <- "/00"
    } else if(ids < 100 && ids >= 10) {
      pre <- "/0"
    } else {
      pre <- "/"
    }
    filer <- paste(directory, pre, ids, ".csv", sep="")
    reading <- read.csv(filer, sep=",")
    x <- reading[[pollutant]]
    val <- val + sum(x, na.rm = TRUE)
    tot <- tot + sum(!is.na(x))
  }
  print(val/tot)
}

# Get the number of rows that have complete data from the CSV files

complete <- function(directory, id = 1:332) {
  df = data.frame(matrix(ncol = 2, nrow = 0))
  colnames(df) <- c("id", "nobs")
  for(ids in id) {
    if(ids < 10) {
      pre <- "/00"
    } else if(ids < 100 && ids >= 10) {
      pre <- "/0"
    } else {
      pre <- "/"
    }
    filer <- paste(directory, pre, ids, ".csv", sep="")
    reading <- read.csv(filer, sep=",")
    x <- sum(complete.cases(reading))
    df[nrow(df) + 1,] = c(ids, x)
  }
  return(df)
}

# Get the correlation of data between
# nitrate and sulfate in the data sets

corr <- function(directory, threshold = 0) {
  df <- complete(directory)
  results <- vector()
  for(l in 1:nrow(df)) {
    if(df[l,"nobs"] >= threshold && df[l,"nobs"] > 0) {
      
      if(l < 10) {
        pre <- "/00"
      } else if(l < 100 && l >= 10) {
        pre <- "/0"
      } else {
        pre <- "/"
      }
      
      filer <- paste(directory, pre, l, ".csv", sep="")
      reading <- read.csv(filer, sep=",")
      
      results <- c(results, cor(reading[["sulfate"]], reading[["nitrate"]], use = "complete.obs"))
    }
  }
  return(results)
}
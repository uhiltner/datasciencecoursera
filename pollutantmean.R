# Part 1----
# The function pollutantmean() returns the mean of the pollutant across all monitors list in the 'id' vector.
# It ignores missing values and does not round the result.
# The function pollutantmean() takes three arguments:
# directory = character string of length 1 to working directory.
# pollutant = character string of length 1 indicating the name of the pollutant, which we want to analyze. either "nitrate" or "sulfate"
# id = integer vector, indicating the monitor id numbers to be used.
pollutantmean <- function(directory, pollutant, id = 1:332){
  # import data----
  # load packages needed
  require(tidyverse)
  
  # files list containing all file names of observation data
  csvFiles <- dir(path= directory, pattern = ".csv") 
  # import all observation files to be used into a single data frame
  df <- csvFiles[id] %>% # assign to 'df' 
    map(read.csv) %>% # import data with function read_csv() from the readr package
    reduce(rbind) # reduce with rbind into one dataframe
  
  
  # select only the pollutant wanted and remove missing values
  dfTemp <- df[pollutant]
  dfTemp <- na.omit(dfTemp)
  # calculate arithmetic mean of column wanted
  mean(dfTemp[, pollutant])
  
 }

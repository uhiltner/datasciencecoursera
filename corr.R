
source("pollutantmean.R")

# Part 3----
# The function corr() takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. The function should return a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0.
# The function corr() takes two arguments:
# directory = character string of length 1 to working directory.
# threshold = numeric vector of length 1 indicating the number of completely measured observations required to compute the correlations between both pollutants. default value = 0.
# the function returnsa a numberic vector of correlations and does not round.
corr <- function(directory, threshold = 0){
  # import data----
  # load packages needed
  require(tidyverse)
  source("complete.R")
  
  dfTemp2 <- complete(directory) %>%
    # select number of completely observed cases above 'threshold'
    filter(NOBS >= threshold)
  
  # files list containing all file names of observation data
  csvFiles <- dir(path= directory, pattern = ".csv") 
  # import all observation files to be used into a single data frame
  df <- csvFiles %>% # assign to 'df' 
    map(read.csv) %>% # import data with function read_csv() from the readr package
    reduce(rbind) # reduce with rbind into one dataframe
  
  # take dataframe and assign new dataframe, then
  df %>%
    # select casses with measurements for both pollutants, then
    filter(!is.na(nitrate) & !is.na(sulfate) & ID %in% dfTemp2$ID) %>%
    # for each location ...
    group_by(ID) %>%
    # compute the correlations between both pollutants
    summarise(myCor = cor(sulfate, nitrate))
}


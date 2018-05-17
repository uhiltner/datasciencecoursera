# Part 2----
# The function complete() reads a directory full of files and reports the number of completely observed cases in each data file. The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases. 
# The function complete() takes two arguments:
# directory = character string of length 1 to working directory.
# id = integer vector, indicating the monitor id numbers to be used.
complete <- function(directory, id = 1:332){
  # import data----
  # load packages needed
  require(tidyverse)
  
  # files list containing all file names of observation data
  csvFiles <- dir(path= directory, pattern = ".csv") 
  # import all observation files to be used into a single data frame
  df <- csvFiles[id] %>% # assign to 'df' 
    map(read.csv) %>% # import data with function read_csv() from the readr package
    reduce(rbind) # reduce with rbind into one dataframe
  
  # take dataframe and assign new dataframe, then
  df %>%
    # select casses with measurements for both pollutants, then
    filter(!is.na(nitrate) & !is.na(sulfate)) %>%
    # for each monitor id ...
    group_by(ID) %>%
    # count complete cases.
    tally() %>%
    # assign new header to variables
    rename(NOBS = n)
} 

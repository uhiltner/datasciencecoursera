---
title: "R Programming Assignment 3"
output: html_notebook
author: 'Ulrike Hiltner'
date: '2018-06-30'
---

The zip file containing the data can be downloaded here: [data](https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip)
The repo for the specialization is this: [repo](https://github.com/uhiltner/datasciencecoursera)

## Header

```{r header, include=TRUE, echo=TRUE,  eval=FALSE}
# load required packages
library("data.table")
# get / set relative path to working directory
wdir <- getwd() # tell the current directory
# checking for and creating directories
if(!file.exists("inputData")){ # check to see if the directory exists
  dir.create("inputData") # create a directory if it does not exist
}
# files have been downloaded manually
matchPat_files <- ".csv"
dataPath <- paste0(wdir,"/inputData/")
# find all file names with pattern as given below:
fileNames <- list.files(dataPath, pattern = matchPat_files) 

```

## Part 1: Plot the 30-day mortality rates for heart attack

```{r outcome, include=T, echo=T, eval=F}
# import data
outcome <- data.table::fread(paste0(dataPath,fileNames[2]))
# coerce the column to be numeric 
# (You may get a warning about NAs being introduced; that is okay)
outcome[, (11) := lapply(.SD, as.numeric), .SDcols = (11)]
# plot a histogramm
outcome[, lapply(.SD, hist, 
                 xlab = "mortality",
                 ylab = "frequency",
                 main = "30-day death rates from heart attack",
                 col = "#B8E100"), 
        .SDcols = (11)]

```


# part 2: Finding the best hospital in a state ('best.R')

```{r best, include=T, echo=T}

best <- function(state, outcome) {
  ##  Load/install packages
  packagesUsed <- "data.table"
  # function checks for missing packages and ... 
  packages_needed <- function(x){
    for(i in x){
      # require() returns TRUE invisibly if it was able to load package
      if(!require( i , character.only = TRUE) ){
        #  If package was not able to be loaded then install
        install.packages(i, dependencies = TRUE)
        #  Load package after installing. 
        library(i, character.only = TRUE ) # library() will throw an exception if the install wasn't successful
      }
    }
  }
  # ... load/install them automatically.
  packages_needed(packagesUsed) 
  
  ## Read outcome data
  # get / set relative path to working directory
  wdir <- getwd()
  fileName <- "outcome-of-care-measures.csv"
  dataPath <- paste0(wdir,"/inputData/")
  # import data
  outcomeDT <- data.table::fread(paste0(dataPath,fileName))
  # make it nicer
  outcome <- tolower(outcome)
  # Column name is same as variable so changing it for readability
  stateChosen <- state 
  
  ## Check that state and outcome are valid
  if (!stateChosen %in% unique(outcomeDT[["State"]])) {
    stop("invalid state")
  }
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }
  
  # Renaming columns for readability
  setnames(outcomeDT, 
           tolower(sapply(colnames(outcomeDT), gsub, 
                          pattern = "^Hospital 30-Day Death \\(Mortality\\) Rates from ", 
                          replacement = "")))
  # Filter dt by state
  outcomeDT <- outcomeDT[state == stateChosen]
  # Columns indices to keep
  colIndices <- grep(paste0("hospital name|state|^", outcome), colnames(outcomeDT))
  # Remove data not needed 
  outcomeDT <- outcomeDT[, .SD, .SDcols = colIndices]
  # Determine classes of each column
  outcomeDT[, outcome] <- outcomeDT[, as.numeric(get(outcome))]
  # Removing NAs for numerical data
  outcomeDT <- outcomeDT[complete.cases(outcomeDT), ]
  # Sort for ranking
  outcomeDT <- outcomeDT[order(get(outcome), `hospital name`)]
  
  ## Return hospital name in that state with lowest 30-day death and rate
  return(outcomeDT[, "hospital name"][1])
  
} ## end best()

```


```{r testBest, include=TRUE, echo=T}
source("best.R")
# example output:
best("MD", "heart attack")

```

## 3 Ranking hospitals by outcome in a state ('rankhospital.R')

```{r rankhospital, include=T, echo=T}

rankhospital <- function(state, outcome, num = "best") {
  ##  Load/install packages
  packagesUsed <- "data.table"
  # function checks for missing packages and ... 
  packages_needed <- function(x){
    for(i in x){
      # require() returns TRUE invisibly if it was able to load package
      if(!require( i , character.only = TRUE) ){
        #  If package was not able to be loaded then install
        install.packages(i, dependencies = TRUE)
        #  Load package after installing. 
        library(i, character.only = TRUE ) # library() will throw an exception if the install wasn't successful
      }
    }
  }
  # ... load/install them automatically.
  packages_needed(packagesUsed) 
  
  ## Read outcome data
  # get / set relative path to working directory
  wdir <- getwd()
  fileName <- "outcome-of-care-measures.csv"
  dataPath <- paste0(wdir,"/inputData/")
  # import data
  outcomeDT <- data.table::fread(paste0(dataPath,fileName))
  # make it nicer
  outcome <- tolower(outcome)
  # Column name is same as variable so changing it for readability
  stateChosen <- state 
  
  ## Check that state and outcome are valid
  if (!stateChosen %in% unique(outcomeDT[["State"]])) {
    stop("invalid state")
  }
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }
  
  # Renaming columns for readability
  setnames(outcomeDT, 
           tolower(sapply(colnames(outcomeDT), gsub, 
                          pattern = "^Hospital 30-Day Death \\(Mortality\\) Rates from ", 
                          replacement = "")))
  # Filter dt by state
  outcomeDT <- outcomeDT[state == stateChosen]
  # Columns indices to keep
  colIndices <- grep(paste0("hospital name|state|^", outcome), colnames(outcomeDT))
  # Remove data not needed 
  outcomeDT <- outcomeDT[, .SD, .SDcols = colIndices]
  # Determine classes of each column
  outcomeDT[, outcome] <- outcomeDT[, as.numeric(get(outcome))]
  # Removing NAs for numerical data
  outcomeDT <- outcomeDT[complete.cases(outcomeDT), ]
  # Sort for ranking (top)
  outcomeDT <- outcomeDT[order(get(outcome), `hospital name`)]
  outcomeDT <- outcomeDT[, .(`hospital name` = `hospital name`, state = state, rate = get(outcome), Rank = .I)]
  
  ## Return hospital name in that state with the given rank 30-day death rate
  if (num == "best"){
    return(outcomeDT[1,`hospital name`])
  }
  if (num == "worst"){
    return(outcomeDT[.N,`hospital name`])
  }
  return(outcomeDT[num,`hospital name`])
  
} ## end rankhospital()

```

```{r testrankhospital, include=TRUE, echo=T}
source("rankhospital.R")
# example output:
rankhospital("MD", "heart attack", "worst")
rankhospital("TX", "heart failure", 4)

```

## 4 Ranking hospitals in all states ('rankall.R')

```{r rankall_old, include=T, echo=T}

rankall <- function(outcome, num = "best") {
  ##  Load/install packages
  packagesUsed <- "data.table"
  # function checks for missing packages and ... 
  packages_needed <- function(x){
    for(i in x){
      # require() returns TRUE invisibly if it was able to load package
      if(!require( i , character.only = TRUE) ){
        #  If package was not able to be loaded then install
        install.packages(i, dependencies = TRUE)
        #  Load package after installing. 
        library(i, character.only = TRUE ) # library() will throw an exception if the install wasn't successful
      }
    }
  }
  # ... load/install them automatically.
  packages_needed(packagesUsed) 
  
  ## Read outcome data
  # get / set relative path to working directory
  wdir <- getwd()
  fileName <- "outcome-of-care-measures.csv"
  dataPath <- paste0(wdir,"/inputData/")
  # import data
  outcomeDT <- data.table::fread(paste0(dataPath,fileName))
  # make it nicer
  outcome <- tolower(outcome)
 
  ## Check that outcome is valid
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }
  
  # Renaming columns for readability
  setnames(outcomeDT, 
           tolower(sapply(colnames(outcomeDT), gsub, 
                          pattern = "^Hospital 30-Day Death \\(Mortality\\) Rates from ", 
                          replacement = "")))
  # Columns indices to keep
  colIndices <- grep(paste0("hospital name|state|^", outcome), colnames(outcomeDT))
  # Remove data not needed 
  outcomeDT <- outcomeDT[, .SD, .SDcols = colIndices]
  # Determine classes of each column
  outcomeDT[, outcome] <- outcomeDT[, as.numeric(get(outcome))]
 
  ## For each state, find the hospital of the given rank
  ## Return hospital name in that state with the given rank 30-day death rate
  if (num == "best"){
    return(outcomeDT[order(state, get(outcome), `hospital name`), .(hospital = head(`hospital name`, 1)), by = state])
    }
  if (num == "worst"){
    return(outcomeDT[order(get(outcome), `hospital name`), .(hospital = tail(`hospital name`, 1)), by = state])
    }
  return(outcomeDT[order(state, get(outcome), `hospital name`),
                   head(.SD, num), by = state, .SDcols = c("hospital name")])
  
} ## end rankall()

```

```{r testrankall_old, include=TRUE, echo=T}
source("rankall.R")
# example output:
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)

```

```{r rankall, include=TRUE, echo=T}
rankall <- function(outcome, num = "best") {
  ##  Load/install packages
  packagesUsed <- "data.table"
  # function checks for missing packages and ... 
  packages_needed <- function(x){
    for(i in x){
      # require() returns TRUE invisibly if it was able to load package
      if(!require( i , character.only = TRUE) ){
        #  If package was not able to be loaded then install
        install.packages(i, dependencies = TRUE)
        #  Load package after installing. 
        library(i, character.only = TRUE ) # library() will throw an exception if the install wasn't successful
      }
    }
  }
  # ... load/install them automatically.
  packages_needed(packagesUsed) 
  
  ## Read outcome data
  # get / set relative path to working directory
  wdir <- getwd()
  fileName <- "outcome-of-care-measures.csv"
  dataPath <- paste0(wdir,"/inputData/")
  # import data
  data <- data.table::fread(paste0(dataPath,fileName))
  
  fd   <- as.data.frame(cbind(data[, 2],  # hospital
                              data[, 7],  # state
                              data[, 11],  # heart attack
                              data[, 17],  # heart failure
                              data[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  fd[, eval(outcome)] <- as.numeric(fd[, eval(outcome)])
  
  ## Check that state and outcome are valid
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if (is.numeric(num)) {
    by_state <- with(fd, split(fd, state))
    ordered  <- list()
    for (i in seq_along(by_state)){
      by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                           by_state[[i]][, "hospital"]), ]
      ordered[[i]]  <- c(by_state[[i]][num, "hospital"], by_state[[i]][, "state"][1])
    }
    result <- do.call(rbind, ordered)
    output <- as.data.frame(result, row.names = result[, 2], stringsAsFactors = FALSE)
    names(output) <- c("hospital", "state")
  } else if (!is.numeric(num)) {
    if (num == "best") {
      by_state <- with(fd, split(fd, state))
      ordered  <- list()
      for (i in seq_along(by_state)){
        by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                             by_state[[i]][, "hospital"]), ]
        ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, ordered)
      output <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(output) <- output[, 2]
    } else if (num == "worst") {
      by_state <- with(fd, split(fd, state))
      ordered  <- list()
      for (i in seq_along(by_state)){
        by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                             by_state[[i]][, "hospital"], 
                                             decreasing = TRUE), ]
        ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, ordered)
      output <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(output) <- output[, 2]
    } else {
      stop('invalid num')
    }
  }
  return(output)
}

```

```{r testrankall, include=TRUE, echo=T}
source("rankall.R")
# example output:
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)

```

```{r quizzweek4, include=F}
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)

```


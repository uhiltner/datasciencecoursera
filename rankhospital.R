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
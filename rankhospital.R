

## Constants
hospitalNameColumn <- 2
stateColumn <- 7
invalidOutcomeError <- "invalid outcome"

getRankedHospital <- function(outcomeData, state, outcomeColumnIndex, 
                              rank, isBest, isWorst)
{
  # Limit data to hospital outcomes for the current state
  isCurrentState <- outcomeData[, stateColumn] == state
  hospitalNames <- outcomeData[, hospitalNameColumn][isCurrentState]
  outcomes <- outcomeData[, outcomeColumnIndex][isCurrentState]
  
  # Convert outcomes from character to numeric (suppress warning of NAs introduced)
  outcomes <- suppressWarnings(as.numeric(outcomes)) 
  
  # Remove rows with missing outcome data
  good <- complete.cases(hospitalNames, outcomes)
  goodHospitalNames <- hospitalNames[good]
  goodOutcomes <- outcomes[good]
  
  # Rank the outcomes
  ranking <- order(goodOutcomes, goodHospitalNames)
  rankedHospitals <- rbind(goodOutcomes, goodHospitalNames)[,ranking]
  
  # remove row names so they don't show in final result
  rownames(rankedHospitals) <- c() 
  
  # Determine the hospital with the requested ranking and return it
  result <- NA
  if (isBest) result <- rankedHospitals[2,1]
  else if (isWorst) result <- rankedHospitals[2, ncol(rankedHospitals)]
  else if (rank > 0 && rank <= length(goodHospitalNames))
  {
    result <- rankedHospitals[2, rank]
  }
  result
}


# The function should check the validity of its arguments. If an invalid state value is passed to rankhospital,
# the function should throw an error via the stop function with the exact message \invalid state". If an invalid
# outcome value is passed to rankhospital, the function should throw an error via the stop function with
# the exact message \invalid outcome".
# Here is some sample output from the function.
# > source("rankhospital.R")
# > rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"
# > rankhospital("MD", "heart attack", "worst")
# 3
# [1] "HARFORD MEMORIAL HOSPITAL"
# > rankhospital("MN", "heart attack", 5000)
# [1] NA
rankhospital <- function(state, outcome, num = "best")
{
  # Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Validate state argument
  validStates <- unique(outcomeData[, stateColumn]) 
  isFound <- validStates == state
  valid <- FALSE
  for (i in 1:length(isFound))
  {
    if (isFound[i]) 
    {
      valid <- TRUE
      break
    }
  }
  if (!valid) stop("invalid state")
  
  ## Valid state argument
  
  # Validate outcome argument: these are the valid outcomes and their column indexes
  validOutcomes <- c("heart attack", "heart failure", "pneumonia")
  outcomeColumnIndexes <- c(11, 17, 23)
  
  isFound <- validOutcomes == outcome
  outcomeNum <- 0
  for (i in 1:length(isFound))
  {
    if (isFound[i]) 
    {
      outcomeNum <- i
      break
    }
  }
  if (outcomeNum < 1) stop(invalidOutcomeError)
  
  ## Valid outcome argument
  
  # Validate ranking argument: these are the valid outcomes and their column indexes
  isBest <- FALSE
  isWorst <- FALSE
  if (identical(num, "best")) isBest <- TRUE
  else if (identical(num, "worst")) isWorst <- TRUE
  else 
  {
    num <- suppressWarnings(as.integer(num))
    if (is.na(num) || 
        !is.integer(num) || num < 1) stop("invalid rank")
  }
    

  ## Valid rank argument
  
  # Return hospital name in that state with the specified outcome ranking
  getRankedHospital(outcomeData, state, 
                    outcomeColumnIndexes[outcomeNum], num, isBest, isWorst)
}

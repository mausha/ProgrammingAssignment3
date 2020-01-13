

## Constants
hospitalNameColumn <- 2
stateColumn <- 7
invalidOutcomeError <- "invalid outcome"

getBestHospital <- function(outcomeData, state, outcomeColumnIndex)
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

  # The best outcome is the minimum value
  bestOutcome <- min(goodOutcomes)

  # Get the names of hospitals with the best outcome and sort them to break ties.
  bestHospitalNames <- sort(goodHospitalNames[goodOutcomes == bestOutcome])
  if (length(bestHospitalNames) < 1) stop(invalidOutcomeError) # No outcome data

  # Return the first hospital in the list (this is how ties are broken)
  bestHospitalNames[1]
}


# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv le and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specied outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of \heart attack", \heart failure", or \pneumonia". Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.
# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the rst hospital in that set should be chosen (i.e. if hospitals \b", \c",
# and \f" are tied for best, then hospital \b" should be returned).
# The function should use the following template.
# best <- function(state, outcome) {
# ## Read outcome data
# ## Check that state and outcome are valid
# ## Return hospital name in that state with lowest 30-day death
# ## rate
# }
# The function should check the validity of its arguments. If an invalid state
# value is passed to best, the function should throw an error via the stop
# function with the exact message \invalid state". If an invalid outcome value
# is passed to best, the function should throw an error via the stop function
# with the exact message \invalid outcome". Here is some sample output from the
# function.
# > best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# > best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"
# > best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
# > best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"
# > best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state
# > best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome

  best <- function(state, outcome) 
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
    
    # Return hospital name in that state with lowest 30-day death rate
    getBestHospital(outcomeData, state, outcomeColumnIndexes[outcomeNum])
}

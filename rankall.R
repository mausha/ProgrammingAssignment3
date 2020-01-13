

## Constants
hospitalNameColumn <- 2
stateColumn <- 7
invalidOutcomeError <- "invalid outcome"

getRankedHospital <- function(hospitals, states, outcomes, state, 
                              rank, isBest, isWorst)
{
  # Limit data to hospital outcomes for the current state
  isCurrentState <- states == state
  hospitalNames <- hospitals[isCurrentState]
  outcomes <- outcomes[isCurrentState]

  # Rank the outcomes
  ranking <- order(outcomes, hospitalNames)
  result <- NA
  numHospitals <- length(hospitalNames)
  if (numHospitals > 0)
  {
    rankedHospitals <- rbind(outcomes, hospitalNames)
    if (numHospitals > 1) rankedHospitals <- rankedHospitals[,ranking]
    
    # remove row names so they don't show in final result
    rownames(rankedHospitals) <- c() 
    
    # Determine the hospital with the requested ranking and return it
    numRankedCols <- ncol(rankedHospitals)
    if (isBest) result <- rankedHospitals[2,1]
    else if (isWorst) result <- rankedHospitals[2, numRankedCols]
    else if (rank > 0 && rank <= length(hospitalNames)) result <- rankedHospitals[2, rank]
  }
  
  result
}

getAllHospitalsForRank <- function(outcomeData, outcomeColumnIndex, 
                                   rank, isBest, isWorst)
{
  # Data of interest
  states <- outcomeData[, stateColumn]
  uniqueStates <- sort(unique(states))
  hospitalNames <- outcomeData[, hospitalNameColumn]
  outcomes <- outcomeData[, outcomeColumnIndex]
  
  # Convert outcomes from character to numeric (suppress warning of NAs introduced)
  outcomes <- suppressWarnings(as.numeric(outcomes)) 
  
  # Remove rows with missing outcome data
  good <- complete.cases(hospitalNames, states, outcomes)
  goodHospitalNames <- hospitalNames[good]
  goodStates <- states[good]
  goodOutcomes <- outcomes[good]
  
  rankedHospitals <- c()
  for (state in uniqueStates)
  {
    rankedHospitals <- 
      c(rankedHospitals, 
        getRankedHospital(goodHospitalNames, goodStates, goodOutcomes, state, rank, isBest, isWorst))
  }
  
  # Create and return a data frame containing the ranked hospital data
  data.frame(hospital=rankedHospitals, state=uniqueStates)
}

# Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital rank-
#   ing (num). The function reads the outcome-of-care-measures.csv le and returns a 2-column data frame
# containing the hospital in each state that has the ranking specied in num. For example the function call
# rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
# are the best in their respective states for 30-day heart attack death rates. The function should return a value
# for every state (some may be NA). The rst column in the data frame is named hospital, which contains
# the hospital name, and the second column is named state, which contains the 2-character abbreviation for
# the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
# hospitals when deciding the rankings.
# Handling ties. The rankall function should handle ties in the 30-day mortality rates in the same way
# that the rankhospital function handles ties. 
#
# Here is some sample output:
# head(rankall("heart attack", 20), 10)
# hospital state
# AK <NA> AK
# AL D W MCMILLAN MEMORIAL HOSPITAL AL
# AR ARKANSAS METHODIST MEDICAL CENTER AR
# 4
# AZ JOHN C LINCOLN DEER VALLEY HOSPITAL AZ
# CA SHERMAN OAKS HOSPITAL CA
# CO SKY RIDGE MEDICAL CENTER CO
# CT MIDSTATE MEDICAL CENTER CT
# DC <NA> DC
# DE <NA> DE
# FL SOUTH FLORIDA BAPTIST HOSPITAL FL
# > tail(rankall("pneumonia", "worst"), 3)
# hospital state
# WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC WI
# WV PLATEAU MEDICAL CENTER WV
# WY NORTH BIG HORN HOSPITAL DISTRICT WY
# > tail(rankall("heart failure"), 10)
# hospital state
# TN WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL TN
# TX FORT DUNCAN MEDICAL CENTER TX
# UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER UT
# VA SENTARA POTOMAC HOSPITAL VA
# VI GOV JUAN F LUIS HOSPITAL & MEDICAL CTR VI
# VT SPRINGFIELD HOSPITAL VT
# WA HARBORVIEW MEDICAL CENTER WA
# WI AURORA ST LUKES MEDICAL CENTER WI
# WV FAIRMONT GENERAL HOSPITAL WV
# WY CHEYENNE VA MEDICAL CENTER WY
rankall <- function(outcome, num = "best")
{
  # Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

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
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  getAllHospitalsForRank(outcomeData, outcomeColumnIndexes[outcomeNum], 
                         num, isBest, isWorst)
}

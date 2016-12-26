# This program returns the hospital with the best (lowest) rate of outcome for a given state
#  It checks the file outcome-of-care-measure.csv for the outcomes 'heart attack',
# 'heart failure' and 'pnuemonia'

best <- function(state, outcome) {
## Read outcome data
outcome_df<-read.csv("outcome-of-care-measures.csv",colClasses ="character")
  
## Check that state and outcome are valid
  if (!(state %in% outcome_df[,7])){
  stop("invalid state")

  }
  if (!(outcome %in% c('heart attack','heart failure','pneumonia'))){
   stop("invalid outcome")
  }
 
## Return hospital name in that state with lowest 30-day death rate
## Determine column of dataframe to evaluate
  if(outcome=='heart attack') {col_index <- 11
  }  else if (outcome=='heart failure'){
    col_index <- 17
    
  } else 
    col_index <- 23
  
  ## Extract the relevant columns into new dataframe
  output_df <- data.frame(outcome_df[,2],outcome_df[,7],outcome_df[,col_index])
 
  ##Change names for convenience
  
  names(output_df) <- c("Hospital","State_Col","Outcome_Col")
  
  
  ## Create state subset
  state_df <- output_df[(output_df$State_Col == state) & (output_df$Outcome_Col!='Not Available'),]

  ##Remove NA values
  statedf_noNA <- na.omit(state_df)
  
  
    
  ## Change outcome to numeric
  state_df$Outcome_Col <- as.numeric(as.character(state_df$Outcome_Col))
  
  
  
  ##state_df <- state_df[order(state_df$Hospital),]
  state_df <- state_df[order(state_df$Outcome_Col,state_df$Hospital),]
  
  
  return(as.character(state_df[1,1]))

  


}

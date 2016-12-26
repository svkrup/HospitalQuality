rankall <- function(outcome, num="best") {
  
  ## Read outcome data
  outcome_df<-read.csv("outcome-of-care-measures.csv",colClasses ="character")
  
  
  if (!(outcome %in% c('heart attack','heart failure','pneumonia'))){
    stop("invalid outcome")
  }
  
  # check if num is valid
  if(((is.character(num)) &  (num!='best') & (num!='worst'))|(num<1)) {
    stop("invalid num")
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
  
  #print(names(output_df))
  
  ## Remove 'Not Available' values
  output_df <- output_df[output_df$Outcome_Col!='Not Available',]
  
  ##Remove NA values
  output_df <- na.omit(output_df)
  
  
  
  ## Change outcome to numeric
  output_df$Outcome_Col <- as.numeric(as.character(output_df$Outcome_Col))
  
  # Define gethospital function
  gethospital <- function() {
  
  
 # return(names()[1,1])

  }
  
    
  output_df <- output_df[order(output_df$State_Col,output_df$Outcome_Col, output_df$Hospital),]
  
  
  ## Split output_df by state
  state_df <- split(output_df,output_df$State_Col)
  #print (state_df)
  #print(names(state_df))
  #print(state_df. [1,1])
  state_vec <- sapply(state_df,function(data){
    if(num=="best") {
      num=1
    } else if (num=="worst") 
      num=length(data$Outcome_Col)  
     
    data$Hospital[num]})
  
  states <- names(state_vec)
  hosp <- state_vec
  return_df <- data.frame(hospital=hosp,state=states,row.names=states)
  return(return_df)
  #  print(state_vec)
  
  
  # Return the hospital with the row value=num
  #return(state_list)
  
  
  
}
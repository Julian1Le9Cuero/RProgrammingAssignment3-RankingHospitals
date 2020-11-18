##  Ranking hospitals by outcome in a state
read_outcome_data <- function(){
    setwd("C:/Users/julia/OneDrive/Escritorio")
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcome
}

rankhospital <- function(state, outcome, num){
    data <- read_outcome_data()
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    # Check if it's a valid outcome
    if(!outcome %in% valid_outcomes){
        stop("invalid outcome")
    }
    
    # Check if it's a valid state
    if(!state %in% data$State){
        stop("invalid state")
    }
    
    #Convert columns to numeric
    data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    
    # Remove missing values 
    data_no_na <- data[complete.cases(data),]
    # Get hospitals by state
    state_hospitals <- data_no_na[data_no_na$State == state,]
    # Initialize rank for final result 
    hospitals <- character()
    rates <- numeric()
 
    #Check the disease provided name and if matches the outcome 
    if(outcome == "heart attack"){
        sorted <- state_hospitals[order(state_hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, state_hospitals$Hospital.Name),]
            # Only allow unique values 
        rates <- sorted$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
        hospitals<- sorted$Hospital.Name
    }
    
    if(outcome == "heart failure"){
        sorted <- state_hospitals[order(state_hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, state_hospitals$Hospital.Name),]
        # Only allow unique values 
        rates <- sorted$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
        hospitals <- sorted$Hospital.Name
    }
    
    if(outcome == "pneumonia"){
        sorted <- state_hospitals[order(state_hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, state_hospitals$Hospital.Name),]
        # Only allow unique values 
        rates <- sorted$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
        hospitals <- sorted$Hospital.Name
    }
  
    ranking <- data.frame(Hospital.Name = hospitals, Rate=rates, Rank= seq_len(length(rates)))
    
    if(num == 'best'){
       return(ranking[ranking$Rank == 1,]$Hospital.Name)
    } else if(num == 'worst'){
       return(ranking[ranking$Rank == nrow(ranking),]$Hospital.Name)
    } else if(num > nrow(ranking) || num < 0){
        return(NA)
    } else {
        return(ranking[ranking$Rank == num,]$Hospital.Name)
    }
}
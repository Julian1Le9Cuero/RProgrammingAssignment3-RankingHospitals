##Finding the best hospital in a state
read_outcome_data <- function(){
    setwd("C:/Users/julia/OneDrive/Escritorio")
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcome
}

best <- function(state, outcome){
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
    
    data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    
    # Remove missing values 
    data_no_na <- data[complete.cases(data),]
    hospital <- ''
    
    
    #Check the disease provided name and if matches the outcome 
    if(outcome == "heart attack"){
        # Get state and best hospital
        state_hospitals <- data_no_na[data_no_na$State == state,]
        hospital <- state_hospitals[state_hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min(state_hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]$Hospital.Name
        
    }
    
    if(outcome == "heart failure"){
        # Get state and best hospital
        state_hospitals <- data_no_na[data_no_na$State == state,]
        hospital <- state_hospitals[state_hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min(state_hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]$Hospital.Name
    }
    
    if(outcome == "pneumonia"){
        # Get state and best hospital
        state_hospitals <- data_no_na[data_no_na$State == state,]
        hospital <- state_hospitals[state_hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == min(state_hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]$Hospital.Name
    }
    
    hospital
}

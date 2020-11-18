#Ranking hospitals in all states
read_outcome_data <- function(){
    setwd("C:/Users/julia/OneDrive/Escritorio")
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcome
}


rankall <- function(outcome, num="best"){
    data <- read_outcome_data()
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    # Check if it's a valid outcome
    if(!outcome %in% valid_outcomes){
        stop("invalid outcome")
    }
    
    count <- 1
    rank <- 1
    current_state <- ''
    ranks <- numeric()
    result <- ''
    
    if(outcome == "heart attack"){
        data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
        data_no_na <- data[complete.cases(data),]
        sorted <- data_no_na[order(data_no_na$State, data_no_na$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, data_no_na$Hospital.Name),]
        
        #Create ranks by state
        for(state in sorted$State){
            if(state != current_state){
                current_state <- state 
                rank <- 1
            }
            ranks[count] <- rank
            rank <- rank + 1
            count <- count + 1
        }
        ranking <- data.frame(Rank = ranks, hospital = sorted$Hospital.Name, state = sorted$State)
        result <- ranking 
    }
    
    if(outcome == "heart failure"){
        data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
        data_no_na <- data[complete.cases(data),]
        sorted <- data_no_na[order(data_no_na$State, data_no_na$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, data_no_na$Hospital.Name),]
        
        #Create ranks by state
        for(state in sorted$State){
            if(state != current_state){
                current_state <- state 
                rank <- 1
            }
            ranks[count] <- rank
            rank <- rank + 1
            count <- count + 1
        }
        
        ranking <- data.frame(Rank = ranks, hospital = sorted$Hospital.Name, state = sorted$State)
        result <- ranking 
    }
    
    if(outcome == "pneumonia"){
        data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
        data_no_na <- data[complete.cases(data),]
        sorted <- data_no_na[order(data_no_na$State, data_no_na$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, data_no_na$Hospital.Name),]
        
        #Create ranks by state
        for(state in sorted$State){
            if(state != current_state){
                current_state <- state 
                rank <- 1
            }
            ranks[count] <- rank
            rank <- rank + 1
            count <- count + 1
        }
        
        ranking <- data.frame(Rank = ranks, hospital = sorted$Hospital.Name, state = sorted$State)
        result <- ranking 
    }
    
    states <- sort(unique(data$State))
    hospitals <- character()
    
    if(num == "best"){
        for(state in seq_len(length(states))){
            hospital <- result[result$state == states[state] & result$Rank == 1,]$hospital
            
            if(length(hospital) == 0){
                hospitals[state] <- NA
            }else{
                hospitals[state] <- hospital 
            }
        }
        return(data.frame(hospital = hospitals, state = states))
    } else if(num == "worst"){
        for(state in seq_len(length(states))){
            # Group by state 
            worst <- result[result$state == states[state],]
            
            #Get worst 
            hospital <- worst[worst$Rank == max(worst$Rank),]$hospital
            
            if(length(hospital) == 0){
                hospitals[state] <- NA
            }else{
                hospitals[state] <- hospital 
            }
        }
        return(data.frame(hospital = hospitals, state = states))
    } else if (num > 0 & num < nrow(result)){
        for(state in seq_len(length(states))){
            hospital <- result[result$state == states[state] & result$Rank == num,]$hospital
            if(length(hospital) == 0){
                hospitals[state] <- NA
            }else{
                hospitals[state] <- hospital 
            }
        }
        return(data.frame(hospital = hospitals, state = states))
    } else {
        stop("Invalid num")
    }
}

tail(rankall("heart failure"), 10)
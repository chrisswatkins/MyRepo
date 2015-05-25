best <- function (in_state, in_outcome) {
      
        ## Read outcome data
        outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")        
      

        ## State is col7
        ## Hospital.name is col2
        ## "heart attack" motality is col 11
        ## "heart failure" motality is col 17        
        ## "pneumonia" motality is col 23

        ## Check that in_state is valid
        
        ValidState <- unique(outcome$State)  ## Obtain unique list of values from outcome$State
        good_state <- as.logical("FALSE")  ## initialize state status
        
        ## Loop through list of valid states looking for match

        for (i in seq_along(ValidState))  { 
                
                if (in_state == ValidState[i]) {
                        good_state <- as.logical("TRUE")
                        break
                        }

                }  ## end for loop
        
        if (!good_state){stop("invalid state")} else {print("valid state")}
        
        ## Check that in_outcome is valid        

        ValidOutcome <- c("heart attack", "heart failure", "pneumonia")  ## Obtain unique list of values from outcome$State
        
        good_outcome <- as.logical("FALSE")  ## initialize outcome status
        
        ## Loop through list of valid states looking for match
        
        for (i in seq_along(ValidOutcome))  { 
                
                if (in_outcome == ValidOutcome[i]) {
                        good_outcome <- as.logical("TRUE")
                        break
                }
                
        }  ## end for loop
        
        if (!good_outcome){stop("invalid outcome")} else {print("valid outcome")}
        
        
        
        ## Return Hospital name in that state with the lowest 30-day death rate
        
        
}
        
        
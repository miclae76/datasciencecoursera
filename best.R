best <- function(state, outcome) {
        
        ## Validate the outcome string
        possible.outcomes = c("heart attack", "heart failure", "pneumonia")
        if( toupper(outcome) %in% toupper(possible.outcomes) == FALSE ) stop("invalid outcome")
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Filter and simplify the column names
        data <- data[c(2, 7, 11, 17, 23)]
        colnames(data) <- c("Hosp.Name","State","heart attack", "heart failure", "pneumonia")
        
        
        ## Validate the state string
        possible.states <- unique(data[, 2])
        if( toupper(state) %in% toupper(possible.states) == FALSE ) stop("invalid state")
        
        ## Filter rows against parameters	
        data <- data[data$State==toupper(state) & data[outcome] != 'Not Available', ]
        index <- which.min(data[, outcome])
        
        ## Return hospital name in that state with lowest 30-day death rate
        data[index, ]$Hosp.Name
}
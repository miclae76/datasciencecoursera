rankhospital <- function(state, outcome, num) {
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
        
        ## Validate the num value
        if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")
        
        ## Grab only rows with our state value    
        data <- data[data$State==state & data[outcome] != 'Not Available', ]
        
        ## Order the data
        data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
        ##data <- data[order(data$name, decreasing = FALSE), ]
        data <- data[order(data[outcome], data$Hosp.Name) , ]
        
        ## Process the num argument
        if( num == "best" ) {
                index <- which.min(data[, outcome])
        } else if( num == "worst" ) {
                index <- which.max(data[, outcome])
        } else {
                index <- num
        }
        
        ## Return hospital name in that state with lowest 30-day death rate
        data[index, ]$Hosp.Name
}
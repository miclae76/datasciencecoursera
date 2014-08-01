rankall <- function(outcome, num) {
        
        ## Validate the outcome string
        possible.outcomes = c("heart attack", "heart failure", "pneumonia")
        if( toupper(outcome) %in% toupper(possible.outcomes) == FALSE ) stop("invalid outcome")
        
        ## Validate the num value
        if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")
        
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data <- data[c(2, 7, 11, 17, 23)]
        colnames(data) <- c("Hosp.Name","State","heart attack", "heart failure", "pneumonia")
        
        
        #subset the data to search by state
        data <- data[data[outcome] != 'Not Available', ]
        data_state <- split(data[, c("Hosp.Name", "State", outcome)], data$State)
        
        
        
        # Search for one state
        rank_hosp_by_state <- function(state_data, num) {
                state_data[outcome] <- as.data.frame(sapply(state_data[outcome], as.numeric))
                data_state_sorted <- order(state_data[outcome], state_data$Hosp.Name, na.last = NA)
                
                ## Process the num argument
                if( num == "best" ) {
                        state_data$Hosp.Name[data_state_sorted[1]]
                } else if( num == "worst" ) {
                        state_data$Hosp.Name[data_state_sorted[length(data_state_sorted)]]
                } else {
                        state_data$Hosp.Name[data_state_sorted[num]]
                }
        }
        
        # build results set
        temp <- lapply(data_state, rank_hosp_by_state, num)
        data.frame(hospital = unlist(temp), state = names(temp), row.names = names(temp))
}
        
        

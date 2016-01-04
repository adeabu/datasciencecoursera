
rankhospital <- function(state, outcome, num = "best"){
        counterState <- 0
        
        outcomeFile <- read.csv("outcome-of-care-measures.csv", 
                                na.strings = "Not Available", 
                                stringsAsFactors = FALSE)
        #REMEMBER Check if state is valid...
        
        okStates <- c(outcomeFile[, 7])
        for(i in 1:length(okStates)){
                if(state == okStates[i]){
                        counterState <- counterState + 1
                }
        }
        if(counterState == 0){
                stop("invalid state")   
        }
        
        outcomeFile <- subset(outcomeFile, outcomeFile[, 7] == state,
                              select = c(2,7,11,17,23))
        if(outcome == "heart attack") {
                x <- 3
        } else if(outcome == "heart failure") {
                x <- 4
        }
        else if(outcome == "pneumonia") {
                x <- 5
        }else {
                stop("invalid outcome")
        }
        
        outcomeFile <- subset(outcomeFile, outcomeFile[,x] != "NA",
                              select = c(1,x))
        outcomeFile <- outcomeFile[order(outcomeFile[,2]),]
        
        if(num == "best") {
                y <- 1
        } else if (num == "worst"){
                y <- nrow(outcomeFile)
        } else if(num < 0 | num > nrow(outcomeFile)){
                x <- FALSE
        } else{
                y <- num
        }
        
        if(x == FALSE){
                NA
        }else{
                for(i in 1:nrow(outcomeFile)){
                        if(outcomeFile[i,2] == outcomeFile[y,2]){
                                y <- i
                                break
                        }
                }
                outcomeFile[y,1]
        }
}

##best() that take two arguments: the 2-character 
##abbreviated name of a state and an outcome name. 

##The function reads the outcome-of-care-measures.csv file and returns 
##a character vector with 
##the name of the hospital that has the best 30-day mortality
##for the specified outcome in that state. 

##The outcomes can be one of 
##“heart attack”, “heart failure”, or “pneumonia”. Hospitals that do 
##not have data on a particular outcome are excluded from the 
##set of hospitals when deciding the rankings.

##If there is a tie for the best hospital for a given 
##outcome, then the hospital names are orted in alphabetical order 
##and the first hospital in that set is be chosen (i.e. if hospitals 
##“b”, “c”, and “f” are tied for best, then hospital “b” is 
##returned).

best <- function(state, outcome){
        
        #Variables are defined
        counterState <- 0
        outcomeFile <- NULL
        okStates <- NULL
        stateOutcome <- NULL
        bestResult <- 0
        bestState <- NULL
        
        #File with data is read
        outcomeFile <- read.csv("outcome-of-care-measures.csv", 
                                colClasses = "character")
        
        
        #Check that state is valid
        okStates <- c(outcomeFile[, 7])
        for(i in 1:length(okStates)){
                if(state == okStates[i]){
                        counterState <- counterState + 1
                }
        }
        if(counterState == 0){
                stop("invalid state")   
        }
        
        #Check which outcome is chosen. Checks which hospitals are
        #the best ranked for the chosen outcome and returns best
        #hospital. If invalid outcome, error is returned.
        if(outcome == "heart attack"){
                stateOutcome <- subset(outcomeFile, 
                                       outcomeFile$State == state &
                                               outcomeFile[,11] 
                                       != "Not Available")
                stateOutcome[,11] <- as.numeric(stateOutcome[,11])
                bestResult <- min(na.omit(stateOutcome[,11]))
                bestState <- subset(stateOutcome, 
                                    stateOutcome[,11] == bestResult)
                bestState <- sort(bestState)
                bestState$Hospital.Name
        } else if(outcome == "pneumonia"){
                stateOutcome <- subset(outcomeFile, 
                                       outcomeFile$State == state &
                                               outcomeFile[,23] 
                                       != "Not Available")
                stateOutcome[,23] <- as.numeric(stateOutcome[,23])
                bestResult <- min(na.omit(stateOutcome[,23]))
                bestState <- subset(stateOutcome, 
                                    stateOutcome[,23] == bestResult)
                bestState <- sort(bestState)
                bestState$Hospital.Name
        } else if(outcome == "heart failure"){
                stateOutcome <- subset(outcomeFile, 
                                       outcomeFile$State == state &
                                               outcomeFile[,17] 
                                       != "Not Available")
                stateOutcome[,17] <- as.numeric(stateOutcome[,17])
                bestResult <- min(na.omit(stateOutcome[,17]))
                bestState <- subset(stateOutcome, 
                                    stateOutcome[,17] == bestResult)
                bestState <- sort(bestState)
                bestState$Hospital.Name
        } else{
                stop("invalid outcome")
        }
}
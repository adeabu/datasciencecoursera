rankall <- function(outcome, num = "best"){
        
        #Read file. Specify that "Not Available" should be regarded as NA
        #and that strings should be read as strings and not factors. Read
        #into variable "df"
        df <- read.csv("outcome-of-care-measures.csv", 
                       na.strings = "Not Available", 
                       stringsAsFactors = FALSE)
        
        #Create subset with the 5 columns (hospital, state, heart attack,
        #heart failure and pneumonia) that we are interested in.
        df <- subset(df, select = c(2,7,11,17,23))
        
        #Read what outcome that has been passed by the user. Depending on which
        #outcome the variable "x" gets the corresponding index value. If none
        #of the outcomes have been passed, an error message is shown.
        if(outcome == "heart attack") {
                x <- 3
        } else if(outcome == "heart failure") {
                x <- 4
        } else if(outcome == "pneumonia") {
                x <- 5
        } else {
                stop("invalid outcome")
        }
        
        #Subset df depending on what outcome that have been passed (and what
        #corresponding value x has been given). df will constitute of three 
        #columns (hospital, state, outcome).
        df <- subset(df, select = c(1,2,x))
        
        #Rename columns for easier display and to make the data easier to work
        #with.
        colnames(df) <- c("hospital", "state", "outcome")
        
        #Split df inte a list of hospitals and outcome values depending on 
        #state. Splitdf will be a list containing dataframes where every state
        #will have it's own dataframe with hospitals and outcome values.
        splitdf <- split(df, df$state)
        
        #Order every dataframe in the list according to outcome value from
        #low to high. NA's will be last on the list. Output contained in 
        #the list of dataframes "splitrank".
        splitrank <- lapply(splitdf, function(y) y[order(y$outcome, y$hospital),])
        
        #If user specifies "worst", the hospitals with the worst available 
        #outcomes will display after NA's are removed by using na.omit() and
        #tail(). From the list "q", the character vectors hospital and state
        #are created.
        
        #If user specifies ranking this will be used to get the corresponding
        #hospitals for each state. If user specifies "best", the first hospital
        #will be displayed. 
        if(num == "worst"){
                splitrank <- lapply(splitrank, na.omit)
                q <- lapply(splitrank, tail, 1)
                hospital <- sapply(q, function(spl) spl[[1]])
                state <- sapply(q, function(spl) spl[[2]])
        }else{
                if(num == "best") {
                        z <- 1
                } else {
                        z <- num
                }
                
                #Depending on what ranking the user have specified a list with
                #dataframes with only the corresponding hospital and state is 
                #created.
                q <- lapply(splitrank, "[", z, 1:2)
                hospital <- sapply(q, function(spl) spl[[1]])
                state <- sapply(q, function(spl) spl[[2]])
        }
        
        #If NA is returned for hospital, NA is also returned for state. But we
        #do know the state as the rowname defines state. This for-loop gives
        #the value in state column the index name the value in state column
        #is NA.
        for(i in 1:length(state)){
                if(is.na(state[i])){
                        state[i] <- names(state[i])
                }
        }
        
        #Create a dataframe from the vectors hospital and state
        data.frame(hospital, state)
}

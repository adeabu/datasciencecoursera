rankall <- function(outcome, num = "best"){
        
        
        df <- read.csv("outcome-of-care-measures.csv", 
                       na.strings = "Not Available", 
                       stringsAsFactors = FALSE)
        
        df <- subset(df, select = c(2,7,11,17,23))
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
        
        df <- subset(df, select = c(1,2,x))
        colnames(df) <- c("hospital", "state", "outcome")
        
        splitdf <- split(df, df$state)
        splitrank <- lapply(splitdf, function(y) y[order(y$outcome, y$hospital),])
        
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
                
                q <- lapply(splitrank, "[", z, 1:2)
                hospital <- sapply(q, function(spl) spl[[1]])
                state <- sapply(q, function(spl) spl[[2]])
        }

        for(i in 1:length(state)){
                if(is.na(state[i])){
                        state[i] <- names(state[i])
                }
        }
        data.frame(c(as.data.frame.AsIs(hospital), as.data.frame.AsIs(state)))
}

#replaceNA.R
replaceNA <- function() {
        
        data_noNA <- data
        
        for (i in 1:length(data_noNA$steps)) {
                
                if (is.na(data_noNA$steps[i])) {
                        
                        interval <- as.character(data_noNA$interval[i])
                        intervalmean <- StepsPerInterval[interval]
                        
                        data_noNA$steps[i] <- intervalmean
                        
                } else {
                }
                
        }
        
        data_noNA <<- data_noNA
}
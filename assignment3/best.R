best <- function(state,outcome){

	data <- read.csv('../outcome-of-care-measures.csv',colClasses='character')

	death.cause <- c('heart attack', 'heart failure', 'pneumonia')	# causes of death
	
	if(!state %in% data$State){
			stop('invalid state')						# errors handling
			break
	}
	else if(!outcome %in% death.cause){
			stop('invalid outcome')
			break
	}
	else {
		options(warn=-1)					# warnings switched off (for NAs)
		data[,11] <- as.numeric(data[,11])
		data[,17] <- as.numeric(data[,17])
		data[,23] <- as.numeric(data[,23])
				
		subset.state <- subset(data,State == state)
		index.cause <- switch(which(death.cause == outcome),11,17,23)
		                      # choosing correct column
		
		mindex <- which(subset.state[,index.cause] == 
		                  min(subset.state[,index.cause],na.rm = TRUE))
		
		options(warn=0)				# switch back warnings
		
		sort(subset.state[mindex,]$Hospital.Name)[1]
		                      # only 1st from the sorted hospitals
	}
}
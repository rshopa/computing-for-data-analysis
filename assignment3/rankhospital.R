rankhospital <- function(state, outcome, num = 'best') {
	
	## Read outcome data

	data <- read.csv('../outcome-of-care-measures.csv',colClasses='character')

	death.cause <- c('heart attack', 'heart failure', 'pneumonia')
						# causes of death

	## Check that state and outcome are valid

	if(!state %in% data$State){
			stop('invalid state')			# errors handling
			break
	}
	else if(!outcome %in% death.cause){
			stop('invalid outcome')
			break
	}
	else {

		options(warn = -1)			# warnings switched off
	  
		data[,11] <- as.numeric(data[,11])
		data[,17] <- as.numeric(data[,17])
		data[,23] <- as.numeric(data[,23])

			# choosing correct state and index of outcome

		subset.state <- subset(data,State == state)

			# choosing index of a particular outcome

		index.cause <- switch(which(death.cause == outcome),11,17,23)

			# reordered subset by outcome rate
			# and its length 

		choose.order <- order(subset.state[,index.cause],subset.state$Hospital.Name,na.last = NA)
		ordered.subset <- subset.state[choose.order,]
		ordered.length <- length(ordered.subset[,index.cause])

			# choosing correct num (as numerical)

		num_char <- c('best','worst')		# character cases of 'num'
		if(num %in% num_char){
			num <- switch(which(num_char == num),1,ordered.length)
		}
		else if(class(num) != 'numeric'){
			stop('invalid num')		# error in 'num' handling
			break
		}
		
	## Return hospital name in that state with the given rank
	## 30-day death rate

		options(warn=0)	# switch back warnings

		ordered.subset$Hospital.Name[num]
	}
}
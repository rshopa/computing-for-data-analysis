rankall <- function(outcome, num = 'best') {
	
	## Read outcome data

	data <- read.csv('../outcome-of-care-measures.csv',colClasses='character')

	death.cause <- c('heart attack', 'heart failure', 'pneumonia')
	                # causes of death
	
	num_char <- c('best','worst')					# character cases of 'num'

	## Check whether outcome and num are valid

	if(!outcome %in% death.cause){
			stop('invalid outcome')
			break
	}
	else if(class(num) != 'numeric' && !num %in% num_char){
			stop('invalid num')		# error in 'num' handling
			break
	}
	else {

		options(warn = -1)			# warnings switched off
		
		data[,11] <- as.numeric(data[,11])
		data[,17] <- as.numeric(data[,17])
		data[,23] <- as.numeric(data[,23])

			# split by state

		splitted <- split(data,data$State)

			# choosing index of a particular outcome

		index.cause <- switch(which(death.cause == outcome),11,17,23)

			# reordered subset by outcome rate
			# and its length 

		Hospitals <- character(0)		# hospitals
		States <- character(0)

		for (state in names(splitted)){

			choose.order <- order(splitted[[state]][,index.cause],splitted[[state]]$Hospital.Name,na.last = NA)
			ordered.subset <- splitted[[state]][choose.order,]
			ordered.length <- length(ordered.subset[,index.cause])

				# temp_num - temporary 'num'-value for particular state

			if(num %in% num_char){
		      	temp_num <- switch(which(num_char == num),1,ordered.length)
			}
			else temp_num <- num
			
			Hospitals <- c(Hospitals, ordered.subset$Hospital.Name[temp_num])
			States <- c(States, state)
		}

		options(warn=0)	# switch back warnings
		
			## Return a data frame with the hospital names and the
			## (abbreviated) state name

		data.frame(hospital = Hospitals,state = States,row.names = States)
	}
}
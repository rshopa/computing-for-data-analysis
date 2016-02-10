complete <- function(directory, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files
	
	nobs <- vector('numeric', length = 0) 
	
	for(i in id) {
		
		path <- file.path(directory,paste0(sprintf('%03i',as.numeric(i)),'.csv'))
		data <- read.csv(path)
		nobs <- c(nobs,sum(complete.cases(data)))
	
	}

	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used


	return(data.frame(id, nobs))


	## Return a data frame of the form:
	## id nobs
	## 1 117
	## 2 1041
	## ...
	## where 'id' is the monitor ID number and 'nobs' is the
	## number of complete cases
}
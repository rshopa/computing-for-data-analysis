corr <- function(directory, threshold = 0) {

	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	correlations <- vector('numeric', length = 0)

	for(f in list.files(directory)){
			
		path <- file.path(directory,f)
		data <- read.csv(path)

			# read the data from 'id.csv'
		
		if(sum(complete.cases(data)) >= threshold){
			single_corr <- cor(data$sulfate,data$nitrate,use = 'pairwise.complete.obs')
			correlations <- c(correlations, single_corr)

			# append every new correlation between sulfate & nitrate,
			# where complete.cases >= threshold 
		}		
	}

	## 'threshold' is a numeric vector of length 1 indicating the
	## number of completely observed observations (on all
	## variables) required to compute the correlation between
	## nitrate and sulfate; the default is 0

	return(correlations[!is.na(correlations)])

	## Return a numeric vector of correlations, removing NA's

}
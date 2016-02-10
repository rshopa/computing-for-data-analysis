count <- function(cause = NULL) {
	
	## Check that "cause" is non-NULL; else throw error
	## Check that specific "cause" is allowed; else throw error

	causes <- c('asphyxiation', 'blunt force', 'other', 'shooting', 'stabbing', 'unknown')

	if(length(cause) == 0){
		stop('NULL cause')		# errors handling
		break
	}	
	else if(!cause %in% causes){
		stop('invalid cause')		# errors handling
		break
	}
	
	## Read "homicides.txt" data file

	homicides <- readLines('../homicides.txt')

	## Extract causes of death

	r <- regexpr('<dd>[Cc]ause:(.*?)</dd>',homicides)
	causes_tags <- tolower(regmatches(homicides,r))			# all found <dd> tags to lowercase
	raw_causes <- gsub('<dd>cause: |</dd>','',causes_tags)	# 'cleaned' causes
		
	## Return integer containing count of homicides for that cause

	as.numeric(table(raw_causes)[cause])		# convert to integer
}
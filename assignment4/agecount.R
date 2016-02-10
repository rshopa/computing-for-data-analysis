agecount <- function(age = NULL) {

## Check that "age" is non-NULL; else throw error

	if(length(age) == 0){
		stop('age is not specified')		# errors handling
		break
	}	

## Read "homicides.txt" data file

	homicides <- readLines('../homicides.txt')

## Extract ages of victims; ignore records where no age is
## given

	matches <- grep(paste0(' ',age,' ([Yy]ear|[Yy]ears) old'),homicides)

## Return integer containing count of homicides for that age

	length(matches)
}
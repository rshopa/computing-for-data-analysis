###################################
##
##  Assignment 1 (Quiz)
##
###################################

# Q1:
# What are the column names of the dataset?

library(datasets)

names(airquality)
colnames(airquality)

# Q2:
# Extract the first 2 rows of the data frame and print them to the console.
# What does the output look like?

head(airquality,2)
airquality[1:2,]

# Q3:
# How many observations (i.e. rows) are in this data frame?

dim(airquality)[1]
nrow(airquality)

# Q4:
# Extract the last 2 rows of the data frame and print them to the console.
# What does the output look like?

tail(airquality,2)
airquality[(nrow(airquality)-1):nrow(airquality),]

# Q5:
# What is the value of Ozone in the 47th row?

airquality$Ozone[47]
airquality[47,'Ozone']
airquality[['Ozone']][47]

# Q6:
# How many missing values are in the Ozone column of this data frame?

length(airquality$Ozone[!complete.cases(airquality$Ozone)])
length(airquality$Ozone[is.na(airquality$Ozone)])
length(airquality$Ozone[airquality$Ozone == 'NA'])

# Q7:
# What is the mean of the Ozone column in this dataset?
# Exclude missing values (coded as NA) from this calculation.

mean(airquality$Ozone, na.rm = T)
mean(airquality$Ozone[!is.na(airquality$Ozone)])

# Q8:
# Extract the subset of rows of the data frame where Ozone values
# are above 31 and Temp values are above 90.
# What is the mean of Solar.R in this subset?

sub <- subset(airquality, Ozone > 31 & Temp > 90)
sub <- airquality[airquality$Ozone > 31 & airquality$Temp > 90,]
mean(sub$Solar.R, na.rm = T)
mean(sub$Solar.R[complete.cases(sub$Solar.R)])

# Q9:
# What is the data type of the "Month" column in the dataset?

class(airquality$Month)
sapply(airquality,class)['Month']

# Q10:
# What is the mean of "Temp" when "Month" is equal to 6?

data.june <- airquality[airquality$Month == 6,]
mean(data.june$Temp, na.rm = T)

mean(airquality[airquality$Month == 6,'Temp'],na.rm = T)
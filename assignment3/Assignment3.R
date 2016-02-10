setwd('../Assignment3')

outcome <- read.csv('../outcome-of-care-measures.csv',colClasses = 'character')

##################################################
# Plot the 30-day mortality rates for heart attack

outcome[,11] <- as.numeric(outcome[,11])

hist(outcome[,11],
     main = 'Heart Attack 30-Day Death Rate', xlab='30-Day Death Rate')

##################################################################
# Plot the 30-day mortality rates for heart attack, heart failure,
# and pneumonia

# 1. Identify which columns of the data frame contain
# the 30-day death rate from heart attack, heart failure,
# and pneumonia.

names(outcome)[11]
rates <- grep("^Hospital.30.Day.Death..Mortality..Rates.from(.*)",names(outcome))

# 2. Coerce these columns to be numeric using the as.numeric function as above.
# You may receive warnings about NAs but that is okay.

outcome[,rates] <- data.frame(sapply(outcome[,rates],as.numeric))

# 3. Make histograms of the death rates for each outcome
# and put the histograms on the same plot window.
# This can be done by running par(mfrow = c(3, 1)) before calling hist.
# This sets the plot window to have 3 rows and 1 column.

par(mfrow = c(3,1))

# > rates [1] 11 17 23 - indices of each death rate
# 4. For each plot (there should be three plots, one for each outcome)
# make sure the x-axis label is “30-day Death Rate”.

# 5. For each plot, set the title of the plot to be the outcome
# (i.e. heart attack, heart failure, or pneumonia).

hist(outcome[,11],xlab = '30-Day Death Rate', main = 'Heart Attack')
hist(outcome[,17],xlab = '30-Day Death Rate', main = 'Heart Failure')
hist(outcome[,23],xlab = '30-Day Death Rate', main = 'Pneumonia')

# 6. Each time you call hist, a new plot is constructed using the data
# to be plotted. However, this makes it difficult to compare histograms
# across outcomes. Set all of the histograms to have the same numerical
# range on the x-axis by using the xlim argument. You can calculate
# the range of a vector of numbers by using the range function.

range(outcome[,11],na.rm=T)
range(outcome[,17],na.rm=T)
range(outcome[,23],na.rm=T)

# examined ranges span over x = (6.7,21.9)

hist(outcome[,11],xlab = '30-Day Death Rate', main = 'Heart Attack',xlim=c(6.7,21.9))
hist(outcome[,17],xlab = '30-Day Death Rate', main = 'Heart Failure',xlim=c(6.7,21.9))
hist(outcome[,23],xlab = '30-Day Death Rate', main = 'Pneumonia',xlim=c(6.7,21.9))

# Try the following variations on this plot:
#
# 2. Using the median and the abline function, draw a vertical line
# on each histogram at the location of the median for that outcome.
# 3. In the title of each histogram, put in parentheses the mean death rate
# by adding (X ?? =??) where ?? is the actual mean for that outcome.
# 4. Add a smooth density estimate on top of the histogram.
# To do this you need to use the density function and you need
# to set prob=TRUE when calling hist.

hist(outcome[,11],probability=T,xlab = '30-Day Death Rate',
     main = substitute('Heart Attack (' * bar(X) == m *')',
     list(m = mean(outcome[,11],na.rm = T))),xlim=c(6.7,21.9))
lines(density(outcome[,11],na.rm=T),lwd=2,col='blue')
abline(v = median(outcome[,11],na.rm = T),col='red',lwd = 2)

hist(outcome[,17],probability=T,xlab = '30-Day Death Rate',
     main = substitute('Heart Failure (' * bar(X) == m *')',
     list(m = mean(outcome[,17],na.rm = T))),xlim=c(6.7,21.9))
lines(density(outcome[,17],na.rm=T),lwd=2,col='blue')
abline(v = median(outcome[,17],na.rm = T),col='red',lwd = 2)

hist(outcome[,23],probability=T,xlab = '30-Day Death Rate',
     main = substitute('Pneumonia (' * bar(X) == m *')',
     list(m = mean(outcome[,23],na.rm = T))),xlim=c(6.7,21.9))
lines(density(outcome[,23],na.rm=T),lwd=2,col='blue')
abline(v = median(outcome[,23],na.rm = T),col='red',lwd = 2)


####################################
# Plot 30-day death rates by state

# Subset the original dataset and exclude states that contain
# less than 20 hospitals (2 mechanisms are proposed - outcome2 and 3)

outcome2 <- subset(outcome,State %in% names(table(State)[table(State)>=20]))
outcome3 <- subset(outcome,(table(State)>=20)[State])

table(outcome$State)
table(outcome2$State)
table(outcome3$State)

# A basic boxplot

par(mfrow = c(1,1))

death <- outcome2[,11]
state <- outcome2$State
boxplot(death ~ state)

# Add the following aspects to the plot
# 1. Set the y-axis label to say “30-day Death Rate”
# 2. Set the title of the plot to be “Heart Attack 30-day Death Rate by State”
# 3. Set the x- and y-axis tick labels to be perpendicular
# to the axis so that the abbreviated names of all the states
# will appear on the plot. Use the par function to set this.
# 4. Challenge: Sort the states by their median 30-day death rate
# and plot the boxplots in order of their median rate.
# Note that the boxplot function also accepts a list as its first argument
# in addition to a formula.

# sorted states step-like marked as stairs with their values
stairs <- reorder(state, death, median, na.rm = T)
stairs_num <- table(outcome2$State)[outcome2$State]

boxplot(death ~ stairs,ylab = '30-day Death Rate',
        main = 'Heart Attack 30-day Death Rate by State',
        las = 2,cex.axis = 0.7)
# 1. Shrink the x-axis tick labels so that the abbreviated state names
# do not overlap each other
# 2. Challenge: Alter the x-axis tick labels so that they include
# the number of hospitals in that state in parentheses.
# For example, the label for the state of Connecticut would be CT (32).

# for the axis function set the boxplot option xaxt to be “n”.

boxplot(death ~ stairs,ylab='30-day Death Rate',
        main = 'Heart Attack 30-day Death Rate by State',
        las = 2,xaxt = 'n',cex.axis = 0.7)
        # las 2 means always perpendicular to the axis

axis(1,unique(stairs),paste0(unique(stairs),' (',
                      unique(stairs_num),')'), cex.axis = 0.7,las = 2)


#################################################
# Plot 30-day death rates and numbers of patients

outcome <- read.csv('../outcome-of-care-measures.csv',colClasses = 'character')
hospital <- read.csv("../hospital-data.csv", colClasses = "character")
outcome.hospital <- merge(outcome,hospital,by = 'Provider.Number')

# the relevant variables 

death <- as.numeric(outcome.hospital[,11]) # Heart attack outcome
npatient <- as.numeric(outcome.hospital[,15])
owner <- factor(outcome.hospital$Hospital.Ownership)

# 1. Use the xyplot function in the lattice package to make a plot
# of the relationship between 30-day death rate for heart attack
# versus the number of patients seen. The number of patients should be on
# the x-axis. Make sure you run library(lattice) before calling xyplot.
# 2. Set the x-axis label to be “Number of Patients Seen”
# 3. Set the y-axis label to be “30-day Death Rate”
# 4. Set the title of the plot to be “Heart Attack 30-day Death Rate
# by Ownership”
# 5. In each panel of the plot, add a linear regression line
# highlighting the relationship between number of patients seen
# and the death rate. Use the panel.lmline function for this.

library(lattice)
xyplot(death~npatient|owner)

xyplot(death~npatient|owner,xlab = 'Number of Patients Seen',
       ylab = '30-day Death Rate',
       main = 'Heart Attack 30-day Death Rate by Ownership',
       panel = function(x,y,...){
         panel.xyplot(x,y,...)
         panel.lmline(x,y)
       })

######################################
# Finding the best hospital in a state

source('best.R')

# main test

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")

# test for errors

best("BB", "heart attack")
best("NY", "hert attack")

#########################################
# Ranking hospitals by outcome in a state

source('rankhospital.R')

# main test

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)

# test for errors

rankhospital("MN", "heart attack", '5000')
rankhospital("MN", "hert attack", 5000)
rankhospital("MZ", "heart attack", "worst")


#################################
# Ranking hospitals in all states

source('rankall.R')

# main test

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

# test for errors

tail(rankall("pneumon", "worst"))
head(rankall("heart failure", "10"))

homicides <- readLines("../homicides.txt")

# How many of each cause of homicide?

source('count.R')
count("other")
num <- count("unknown")
print(num)

# Ages of homicide victims

source('agecount.R')
agecount(3)
num <- agecount(21)
print(num)
# test getmonitor-output

source('getmonitor.R')
data <- getmonitor(1, "specdata")
head(data)
data <- getmonitor(101, "specdata", TRUE)
head(data)
data <- getmonitor("200", "specdata", TRUE)

# test complete-output

source('complete.R')
complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)

# test corr-output

source('corr.R')
cr <- corr("specdata", 150)
head(cr)
summary(cr)
cr <- corr("specdata", 400)
head(cr)
summary(cr)
cr <- corr("specdata", 5000)
summary(cr)
length(cr)
cr <- corr("specdata")
summary(cr)
length(cr)
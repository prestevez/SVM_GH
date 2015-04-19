## Runfile.R

# Run this file to execute

source("01_Setup.R")
source("02_Read_data.R")
source("Functions.R")
source("reset.sink.R")

logfile <- file("results.txt")

setwd(dirText)

# Initialise sink

sink(logfile, append=TRUE, type=c("output", "message"))

setwd(mainWD)
source("03_Analysis_Results.R", echo=TRUE, max.deparse.length=10000)

# To modify analysis parameters, modify the values in the 03_Analysis_Results.R file

# Ends sink
sink()

sink.reset()

## End of Runfile.R


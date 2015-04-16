## Run file

# Run this file to execute

source("01_Setup.R")
source("02_Read_data.R")
source("Functions.R")

logfile <- file("results.txt")

setwd(dirText)

# Initialise sink

sink(logfile, append=TRUE, type=c("output", "message"))


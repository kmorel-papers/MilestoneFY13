
# The results are hand-gathered from HPCToolkit using the PAPI_TOT_CYC clock


# To get the timings in seconds, we have to divide by the clockrate
clockrate = 2600e6;   

# Describe the columns of the output data.  In this case the transfer and wait cols are 0
cat("ranks, compute,  viz,  transfer,  wait\n")

cat("% --- pipe-3 ----------\n")

# Read the data 
xx <- read.csv("pipe-3-results-cycles.csv", comment.char="%")

# The time column is the total time for the application.  We need 
# to separate the time spent only doing CTH computation. 
compute <- (xx$time - (xx$init + xx$viz))/clockrate

writeLines(paste(xx$ranks, ", ", compute, ", ", xx$viz/clockrate, ", 0.0, 0.0"))


# pipe-4
cat("% --- pipe-4 ----------\n")
xx <- read.csv("pipe-4-results-cycles.csv", comment.char="%")
compute <- (xx$time - (xx$init + xx$viz))/clockrate
writeLines(paste(xx$ranks, ", ", compute, ", ", xx$viz/clockrate, ", 0.0, 0.0"))


cat("% --- pipe-5 ----------\n")
xx <- read.csv("pipe-5-results-cycles.csv", comment.char="%")
compute <- (xx$time - (xx$init + xx$viz))/clockrate
writeLines(paste(xx$ranks, ", ", compute, ", ", xx$viz/clockrate, ", 0.0, 0.0"))

cat("% --- pipe-6 ----------\n")
xx <- read.csv("pipe-6-results-cycles.csv", comment.char="%")
compute <- (xx$time - (xx$init + xx$viz))/clockrate
writeLines(paste(xx$ranks, ", ", compute, ", ", xx$viz/clockrate, ", 0.0, 0.0"))

cat("% --- pipe-7 ----------\n")
xx <- read.csv("pipe-7-results-cycles.csv", comment.char="%")
compute <- (xx$time - (xx$init + xx$viz))/clockrate
writeLines(paste(xx$ranks, ", ", compute, ", ", xx$viz/clockrate, ", 0.0, 0.0"))


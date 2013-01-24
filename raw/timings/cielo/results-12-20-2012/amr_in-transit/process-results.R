
# The results are hand-gathered from HPCToolkit using the PAPI_TOT_CYC clock


# To get the timings in seconds, we have to divide by the clockrate
clockrate = 2600e6;   

# Describe the columns of the output data.  In this case the transfer and wait cols are 0
cat("ranks, compute,  viz,  transfer,  wait\n")

cat("% --- pipe-3 ----------\n")
xx <- read.csv("pipe-3-results.csv", comment.char="%")
compute <- xx$time - (xx$xfer + xx$wait)
writeLines(paste(xx$ranks, ", ", compute, ", 0.0, ", xx$xfer, ", ", xx$wait ))


cat("% --- pipe-4 ----------\n")
xx <- read.csv("pipe-4-results.csv", comment.char="%")
compute <- xx$time - (xx$xfer + xx$wait)
writeLines(paste(xx$ranks, ", ", compute, ", 0.0, ", xx$xfer, ", ", xx$wait ))


cat("% --- pipe-5 ----------\n")
xx <- read.csv("pipe-5-results.csv", comment.char="%")
compute <- xx$time - (xx$xfer + xx$wait)
writeLines(paste(xx$ranks, ", ", compute, ", 0.0, ", xx$xfer, ", ", xx$wait ))

cat("% --- pipe-6 ----------\n")
xx <- read.csv("pipe-6-results.csv", comment.char="%")
compute <- xx$time - (xx$xfer + xx$wait)
writeLines(paste(xx$ranks, ", ", compute, ", 0.0, ", xx$xfer, ", ", xx$wait ))

cat("% --- pipe-7 ----------\n")
xx <- read.csv("pipe-7-results.csv", comment.char="%")
compute <- xx$time - (xx$xfer + xx$wait)
writeLines(paste(xx$ranks, ", ", compute, ", 0.0, ", xx$xfer, ", ", xx$wait ))


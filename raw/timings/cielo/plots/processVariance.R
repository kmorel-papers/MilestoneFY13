library(stringr)
library(ggplot2)


###########################################################
# Construct a data record from the client_timings files
#
# Read the input files from each directory.  We assume that
# each directory has a number of results stored in files with
# the name "client_timings*.csv".
###########################################################
processClientVariance <- function(dir) {

	files <- list.files(path=dir, pattern="client_timings.*csv", full.names=TRUE)
	numfiles <- length(files)

	my.data <- do.call("cbind.data.frame", lapply(files, read.csv, comment.char="%"))

	ncols <- length(my.data)/numfiles
	nrows <- length(my.data$ranks)

	# Get number of blocks and number of cores
	cores <- my.data$ranks
	allocated_blocks <- my.data$allocated_blocks
	active_blocks <- my.data$active_blocks

	# Calculate the indicies of CTH and viz columns
	cthindices <- c(0:(numfiles-1))*ncols + 6
	vizindices <- c(0:(numfiles-1))*ncols + 7
	syncmdindices <- c(0:(numfiles-1))*ncols + 8
	syncdindices <- c(0:(numfiles-1))*ncols + 9
	waitindices <- c(0:(numfiles-1))*ncols + 11

	data <- cbind.data.frame(cycle=my.data$cycle,
			cth_mean=apply(FUN=mean, MARGIN=1, my.data[cthindices]),
			cth_sd=apply(FUN=sd, MARGIN=1, my.data[cthindices]),
			xfer_mean=apply(FUN=mean, MARGIN=1, (my.data[syncmdindices]+my.data[syncdindices])),
			xfer_sd=apply(FUN=sd, MARGIN=1, (my.data[syncmdindices]+my.data[syncdindices])),
			wait_mean=apply(FUN=mean, MARGIN=1, my.data[waitindices]),
			wait_sd=apply(FUN=sd, MARGIN=1, my.data[waitindices]))

	return (data)
}


###########################################################
# Construct a data record from the client_timings files
#
# Read the input files from each directory.  We assume that
# each directory has a number of results stored in files with
# the name "client_timings*.csv".
###########################################################
processExtractedVariance <- function(dir) {

	files <- list.files(path=dir, pattern="ext.*csv", full.names=TRUE)
	numfiles <- length(files)

	my.data <- do.call("cbind.data.frame", lapply(files, read.csv, comment.char="%"))

	# Calculate the indicies of CTH and viz columns
	cthindices <- c(0:(numfiles-1))*3 + 2
	vizindices <- c(0:(numfiles-1))*3 + 3

	data <- cbind.data.frame(cycle=my.data$cycle,
			cth_mean=apply(FUN=mean, MARGIN=1, my.data[cthindices]),
			cth_sd=apply(FUN=sd, MARGIN=1, my.data[cthindices]),
			viz_mean=apply(FUN=mean, MARGIN=1, my.data[vizindices]),
			viz_sd=apply(FUN=sd, MARGIN=1, my.data[vizindices]))

	return (data)
}


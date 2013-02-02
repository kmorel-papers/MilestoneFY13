library(stringr)
library(ggplot2)


###########################################################
# Construct a data record from the client_timings files
#
# Read the input files from each directory.  We assume that
# each directory has a number of results stored in files with
# the name "client_timings*.csv".
###########################################################
processClientTimings <- function(directories, pattern="client_timings*.csv") {

	data <- {}
# For each directory, gather statistics on each experiment
	for (dir in directories) {

		print(dir)

		files <- list.files(path=dir, pattern=pattern, full.names=TRUE)
		numfiles <- length(files)

		my.data <- do.call("cbind", lapply(files, read.csv, comment.char="%"))

		# Extract the levels of refinement from the directory name, we assume the
		# name is */pipe-[levels]-*
		tmp <- str_replace(dir, pattern=".*pipe-", replacement="pipe-")
		levels <- as.numeric(unlist(str_split(tmp, "-"))[2])

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

		# This is the average time spent doing CTH/viz in a 10-cycle period
		cthmean <- mean(colMeans(my.data[cthindices]))
		ctherr <- sd(colMeans(my.data[cthindices]))
		cthrate <- mean(allocated_blocks[2:nrows]/rowMeans(my.data[cthindices])[2:nrows])
		cthrateerr <- sd(allocated_blocks[2:nrows]/rowMeans(my.data[cthindices])[2:nrows])

		vizmean <- mean(colMeans(my.data[vizindices]))
		vizerr <- sd(colMeans(my.data[vizindices]))
		vizrate <- mean(allocated_blocks[2:nrows]/rowMeans(my.data[vizindices])[2:nrows])
		vizrateerr <- sd(allocated_blocks[2:nrows]/rowMeans(my.data[vizindices])[2:nrows])

		syncmdmean <- mean(colMeans(my.data[syncmdindices]))
		syncmderr <- sd(colMeans(my.data[syncmdindices]))
		syncmdrate <- mean(allocated_blocks[2:nrows]/rowMeans(my.data[syncmdindices])[2:nrows])
		syncmdrateerr <- sd(allocated_blocks[2:nrows]/rowMeans(my.data[syncmdindices])[2:nrows])

		syncdmean <- mean(colMeans(my.data[syncdindices]))
		syncderr <- sd(colMeans(my.data[syncdindices]))
		syncdrate <- mean(allocated_blocks[2:nrows]/rowMeans(my.data[syncdindices])[2:nrows])
		syncdrateerr <- sd(allocated_blocks[2:nrows]/rowMeans(my.data[syncdindices])[2:nrows])

		waitmean <- mean(colMeans(my.data[waitindices]))
		waiterr <- sd(colMeans(my.data[waitindices]))
		waitrate <- mean(allocated_blocks[2:nrows]/rowMeans(my.data[waitindices])[2:nrows])
		waitrateerr <- sd(allocated_blocks[2:nrows]/rowMeans(my.data[waitindices])[2:nrows])

		row <- cbind(levels=levels, cores=mean(cores), blocks=mean(allocated_blocks),
				cthmean=cthmean, ctherr=ctherr, cthrate=cthrate, cthrateerr=cthrateerr,
				vizmean=vizmean, vizerr=vizerr, vizrate=vizrate, vizrateerr=vizrateerr,
				syncmdmean=syncmdmean, syncmderr=syncmderr, syncmdrate=syncmdrate, syncmdrateerr=syncmdrateerr,
				syncdmean=syncdmean, syncderr=syncderr, syncdrate=syncdrate, syncdrateerr=syncdrateerr,
				waitmean=waitmean, waiterr=waiterr, waitrate=waitrate, waitrateerr=waitrateerr)
		data <- as.data.frame(rbind(data, row))
	}

	return (data)
}


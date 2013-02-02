library(stringr)
library(ggplot2)


###########################################################
# Construct a data record from the extracted_timings files
#
# Read the input files from each directory.  We assume that
# each directory has a number of results stored in files with
# the name "extracted_timings*.csv".
###########################################################
processExtractedTimings <- function(directories) {

	data <- {}
# For each directory, gather statistics on each experiment
	for (dir in directories) {

		print(dir)

		files <- list.files(path=dir, pattern="ext.*csv", full.names=TRUE)
		numfiles <- length(files)

		my.data <- do.call("cbind", lapply(files, read.csv, comment.char="%"))

		# Extract the levels of refinement from the directory name, we assume the
		# name is */pipe-[levels]-*
		tmp <- str_replace(dir, pattern=".*pipe-", replacement="pipe-")
		levels <- as.numeric(unlist(str_split(tmp, "-"))[2])

		# The number of blocks represents the mean allocated blocks
		if (levels == 3) {
			numblocks <- 1146
		} else if (levels == 4) {
			numblocks <- 5080
		} else if (levels == 5) {
			numblocks <- 32965
		} else if (levels == 6) {
			numblocks <- 218362
		} else if (levels == 7) {
			numblocks <- 1498445
		} else if (levels == 8) {
			numblocks <- 10728933
		}



		# Extract the number of clients from the directory name
		np <- as.numeric(unlist(str_split(tmp, "-"))[3])

		# Calculate the indicies of CTH and viz columns
		cthindices <- c(0:(numfiles-1))*3 + 2
		vizindices <- c(0:(numfiles-1))*3 + 3

		# This is the average time spent doing CTH/viz in a 10-cycle period
		totalmean <- mean(colMeans(my.data[cthindices] + my.data[vizindices]))
		totalerr <- sd(colMeans(my.data[cthindices] + my.data[vizindices]))
		totalrate <- mean(numblocks/colMeans(my.data[cthindices] + my.data[vizindices]))
		totalrateerr <- sd(numblocks/colMeans(my.data[cthindices] + my.data[vizindices]))

		cthmean <- mean(colMeans(my.data[cthindices]))
		ctherr <- sd(colMeans(my.data[cthindices]))
		cthrate <- mean(numblocks/colMeans(my.data[cthindices]))
		cthrateerr <- sd(numblocks/colMeans(my.data[cthindices]))

		vizmean <- mean(colMeans(my.data[vizindices]))
		vizerr <- sd(colMeans(my.data[vizindices]))
		vizrate <- mean(numblocks/colMeans(my.data[vizindices]))
		vizrateerr <- sd(numblocks/colMeans(my.data[vizindices]))



		row <- cbind(levels=levels, cores=np,
				totalmean=totalmean, totalerr=totalerr, totalrate=totalrate, totalrateerr=totalrateerr,
				cthmean=cthmean, ctherr=ctherr, cthrate=cthrate, cthrateerr=cthrateerr,
				vizmean=vizmean, vizerr=vizerr, vizrate=vizrate, vizrateerr=vizrateerr)
		data <- as.data.frame(rbind(data, row))
	}

	return (data)
}




library(stringr)
library(ggplot2)


###########################################################
# Construct a data record from the client_timings files
#
# Read the input files from each directory.  We assume that
# each directory has a number of results stored in files with
# the name "client_timings*.csv".
###########################################################
processClientTimings <- function(directories) {

	data <- {}
# For each directory, gather statistics on each experiment
	for (dir in directories) {

		print(dir)

		files <- list.files(path=dir, pattern="client_timings.*csv", full.names=TRUE)
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
		cthrate <- mean(active_blocks[2:nrows]/rowMeans(my.data[cthindices])[2:nrows])
		cthrateerr <- sd(active_blocks[2:nrows]/rowMeans(my.data[cthindices])[2:nrows])

		vizmean <- mean(colMeans(my.data[vizindices]))
		vizerr <- sd(colMeans(my.data[vizindices]))
		vizrate <- mean(active_blocks[2:nrows]/rowMeans(my.data[vizindices])[2:nrows])
		vizrateerr <- sd(active_blocks[2:nrows]/rowMeans(my.data[vizindices])[2:nrows])

		syncmdmean <- mean(colMeans(my.data[syncmdindices]))
		syncmderr <- sd(colMeans(my.data[syncmdindices]))
		syncmdrate <- mean(active_blocks[2:nrows]/rowMeans(my.data[syncmdindices])[2:nrows])
		syncmdrateerr <- sd(active_blocks[2:nrows]/rowMeans(my.data[syncmdindices])[2:nrows])

		syncdmean <- mean(colMeans(my.data[syncdindices]))
		syncderr <- sd(colMeans(my.data[syncdindices]))
		syncdrate <- mean(active_blocks[2:nrows]/rowMeans(my.data[syncdindices])[2:nrows])
		syncdrateerr <- sd(active_blocks[2:nrows]/rowMeans(my.data[syncdindices])[2:nrows])

		waitmean <- mean(colMeans(my.data[waitindices]))
		waiterr <- sd(colMeans(my.data[waitindices]))
		waitrate <- mean(active_blocks[2:nrows]/rowMeans(my.data[waitindices])[2:nrows])
		waitrateerr <- sd(active_blocks[2:nrows]/rowMeans(my.data[waitindices])[2:nrows])

		row <- cbind(levels=levels, cores=mean(cores), blocks=mean(active_blocks),
				cthmean=cthmean, ctherr=ctherr, cthrate=cthrate, cthrateerr=cthrateerr,
				vizmean=vizmean, vizerr=vizerr, vizrate=vizrate, vizrateerr=vizrateerr,
				syncmdmean=syncmdmean, syncmderr=syncmderr, syncmdrate=syncmdrate, syncmdrateerr=syncmdrateerr,
				syncdmean=syncdmean, syncderr=syncderr, syncdrate=syncdrate, syncdrateerr=syncdrateerr,
				waitmean=waitmean, waiterr=waiterr, waitrate=waitrate, waitrateerr=waitrateerr)
		data <- as.data.frame(rbind(data, row))
	}

	return (data)
}

change_level_labels <- function(data) {
	# Need to do this to make the legend look right
	data$levels[data$levels == 3] <- "1k blocks"
	data$levels[data$levels == 4] <- "5k blocks"
	data$levels[data$levels == 5] <- "33k blocks"
	data$levels[data$levels == 6] <- "219k blocks"
	data$levels[data$levels == 7] <- "1.5m blocks"

	return (data)
}

# Find the directories that have data
intransit_dirs <- list.files(path="amr_in-transit-hpctoolkit", full.names=TRUE, pattern="pipe-.*", recursive=FALSE)
intransit_inc_dirs <- list.files(path="amr_in-transit-inclusive", full.names=TRUE, pattern="pipe-.*", recursive=FALSE)

# extract the variance data
intransit_data <- extract_variance_data(directories = intransit_dirs)
intransit_inc_data <- extract_variance_data(directories = intransit_inc_dirs)

# change the labels
intransit_data <- change_level_labels(intransit_data)
intransit_inc_data <- change_level_labels(intransit_inc_data)

# Set linetypes, colors, and shapes

myc <- c("indianred1", "dodgerblue", "forestgreen", "goldenrod1")   # colors
mys <- c(18,16,15,17)  # shapes
myl <- c(4,1,2,3)  # linetype

#=================== Bar plots showing CTH + Viz ==================

counts <- cbind.data.frame(CTH=intransit_data$cthmean, Viz=intransit_data$vizmean)
barplot(t(counts),
		names=intransit_data$cores,
		las=2,
		ylim=c(0,250),
		legend.text=c("CTH", "Viz"),
		col=myc[1:2],
		xlab="Client Ranks",
		ylab="Time (sec)",
		main="In-Transit Timings (128 extra nodes)")

text(2.4, 150, "33k blocks")
text(7.5, 150, "219k blocks")
text(12.8, 150, "1.5m blocks")

abline(v=4.9,col="gray25",lty=3)
abline(v=9.7,col="gray25",lty=3)

#-------

counts <- cbind.data.frame(CTH=intransit_data$cthmean,
		syncd=intransit_data$syncdmean,
		wait=intransit_data$waitmean)
barplot(t(counts),
		names=intransit_data$cores,
		las=2,
		ylim=c(0,250),
		legend.text=c("CTH", "Xfer Data", "Wait"),
		col=myc[c(1,3,4)],
		xlab="Client Ranks",
		ylab="Time (sec)",
		main="In-Transit Timings (128 extra nodes)")

text(2.4, 150, "33k blocks")
text(7.5, 150, "219k blocks")
text(12.8, 150, "1.5m blocks")

abline(v=4.9,col="gray25",lty=3)
abline(v=9.7,col="gray25",lty=3)


#-------

counts <- cbind.data.frame(CTH=intransit_inc_data$cthmean, Viz=intransit_inc_data$vizmean)
barplot(t(counts),
		names=intransit_inc_data$cores,
		las=2,
		ylim=c(0,250),
		legend.text=c("CTH", "Viz"),
		col=myc[1:2],
		xlab="Client Ranks",
		ylab="Time (sec)",
		main="In-Transit Timings (100 internal nodes)")

text(2.4, 150, "33k blocks")
text(7.5, 150, "219k blocks")
text(12.8, 150, "1.5m blocks")

abline(v=4.9,col="gray25",lty=3)
abline(v=9.7,col="gray25",lty=3)



#-------


counts <- cbind.data.frame(CTH=intransit_inc_data$cthmean,
		syncd=intransit_inc_data$syncdmean,
		wait=intransit_inc_data$waitmean)
barplot(t(counts),
		names=intransit_inc_data$cores,
		las=2,
		ylim=c(0,250),
		legend.text=c("CTH", "Xfer Data", "Wait"),
		col=myc[c(1,3,4)],
		xlab="Client Ranks",
		ylab="Time (sec)",
		main="In-Transit Timings (100 internal nodes)")

text(2.4, 150, "33k blocks")
text(7.5, 150, "219k blocks")
text(12.8, 150, "1.5m blocks")

abline(v=4.9,col="gray25",lty=3)
abline(v=9.7,col="gray25",lty=3)




#------------
# Large data set only
#------------

pdf("intransit-large.pdf", width=4.2, height=4)


d1 <- intransit_inc_data$cthmean[intransit_inc_data$levels=="1.5m blocks"]
d2 <- intransit_inc_data$syncdmean[intransit_inc_data$levels=="1.5m blocks"]
d3 <- intransit_inc_data$waitmean[intransit_inc_data$levels=="1.5m blocks"]

names <- intransit_inc_data$cores[intransit_inc_data$levels=="1.5m blocks"]

counts <- cbind.data.frame(CTH=d1, syncd=d2, wait=d3)

barplot(t(counts),
		names=names,
		las=2,
		ylim=c(0,250),
		legend.text=c("CTH", "Xfer Data", "Wait"),
		col=myc[c(1,3,4)],
		xlab="",
		ylab="Time (sec)",
		main="In-Transit Timings (100 internal nodes)")

#------------
# Large data set only
#------------

d1 <- intransit_data$cthmean[intransit_data$levels=="1.5m blocks"]
d2 <- intransit_data$syncdmean[intransit_data$levels=="1.5m blocks"]
d3 <- intransit_data$waitmean[intransit_data$levels=="1.5m blocks"]

names <- intransit_data$cores[intransit_data$levels=="1.5m blocks"]

counts <- cbind.data.frame(CTH=d1, syncd=d2, wait=d3)

barplot(t(counts),
		names=names,
		las=2,
		ylim=c(0,250),
		legend.text=c("CTH", "Xfer Data", "Wait"),
		col=myc[c(1,3,4)],
		xlab="",
		ylab="Time (sec)",
		main="In-Transit Timings (128 extra nodes)")


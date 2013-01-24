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

		if (levels == 3) {
			numblocks <- 1146
		} else if (levels == 4) {
			numblocks <- 5080
		} else if (levels == 5) {
			numblocks <- 33094
		} else if (levels == 6) {
			numblocks <- 218510
		} else if (levels == 7) {
			numblocks <- 1498866
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
insitu_unopt_dirs <- list.files(path="amr_in-situ-unopt", full.names=TRUE, pattern="pipe-.*", recursive=FALSE)
insitu_opt_dirs <- list.files(path="amr_in-situ-hpctoolkit", full.names=TRUE, pattern="pipe-.*", recursive=FALSE)
intransit_dirs <- list.files(path="amr_in-transit-hpctoolkit", full.names=TRUE, pattern="pipe-.*", recursive=FALSE)
intransit_inc_dirs <- list.files(path="amr_in-transit-inclusive", full.names=TRUE, pattern="pipe-.*", recursive=FALSE)

# extract the variance data
insitu_unopt_data <- extract_variance_data(directories=insitu_unopt_dirs)
insitu_opt_data <- extract_variance_data(directories=insitu_opt_dirs)
intransit_data <- extract_variance_data(directories = intransit_dirs)
intransit_inc_data <- extract_variance_data(directories = intransit_inc_dirs)

# change the labels
insitu_unopt_data <- change_level_labels(insitu_unopt_data)
insitu_opt_data <- change_level_labels(insitu_opt_data)
intransit_data <- change_level_labels(intransit_data)
intransit_inc_data <- change_level_labels(intransit_inc_data)

#=================== Comparision of In-situ and In-transit ==================

# take only the large data set
d1 <- insitu_unopt_data[insitu_unopt_data$levels == "1.5m blocks",]
d2 <- insitu_opt_data[insitu_opt_data$levels == "1.5m blocks",]
d3 <- intransit_data[intransit_data$levels == "1.5m blocks",]
d4 <- intransit_inc_data[intransit_inc_data$levels == "1.5m blocks",]

# change group names
d1$levels <- "In-Situ "
d2$levels <- "In-Situ (tuned)"
d3$levels <- "In-Transit (128 extra nodes)"
d4$levels <- "In-Transit (100 internal nodes)"

myc <- c("goldenrod1", "indianred1", "dodgerblue", "forestgreen" )   # colors
mys <- c(18,16,15,17)  # shapes
myl <- c(4,1,2,3)  # linetype

# -- Without in-situ tuned ---

#pdf("unopt-all.pdf", width=12, height=4)



data <- rbind.data.frame(d1, d3, d4)
names(data)[1] <- "Experiments"

# Compare untuned experiments
ggplot(data, aes(x=cores, y=totalmean, ymin=0)) +
		geom_line(size=1, aes(linetype=Experiments, colour=Experiments))  +
		geom_point(size=4.0, aes(shape=Experiments, colour=Experiments)) +
		geom_errorbar(aes(ymin=totalmean-totalerr, ymax=totalmean+totalerr, colour=Experiments), width=.05) +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_manual(values=myc[2:4]) +
		scale_shape_manual(values=mys[2:4]) +
		scale_linetype_manual(values=myl[2:4]) +
		ggtitle("Execution Time for 10 Cycles (1.5m blocks)") +
		xlab("Client Ranks") + ylab("Time (sec)") +
		theme(legend.position=c(0.6, 0.8))

                #theme_bw() +


# -- With in-situ tuned ---

data <- rbind.data.frame(d1, d2, d3, d4)
names(data)[1] <- "Experiments"

# Compare all experiments
# Compare untuned experiments
ggplot(data, aes(x=cores, y=totalmean, ymin=0)) +
		geom_line(size=1, aes(linetype=Experiments, colour=Experiments))  +
		geom_point(size=4.0, aes(shape=Experiments, colour=Experiments)) +
	 	geom_errorbar(aes(ymin=totalmean-totalerr, ymax=totalmean+totalerr, colour=Experiments), width=.15) +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_manual(values=myc) +
		scale_shape_manual(values=mys) +
		scale_linetype_manual(values=myl) +
		ggtitle("Execution Time for 10 Cycles (1.5m blocks)") +
		xlab("Client Ranks") + ylab("Time (sec)") +
		theme(legend.position=c(0.6, 0.8))

#=================== Bar plots showing CTH + Viz ==================

counts <- cbind.data.frame(CTH=insitu_opt_data$cthmean, Viz=insitu_opt_data$vizmean)
barplot(t(counts),
		names=insitu_opt_data$cores,
		las=2,
		ylim=c(0,250),
		legend.text=c("CTH", "Viz"),
		col=myc[2:3],
		xlab="Client Ranks",
		ylab="Time (sec)",
		main="CTH and Visualization Time for In-Situ (tuned)")

text(2, 150, "33k blocks")
text(7, 150, "219k blocks")
text(13, 150, "1.5m blocks")

counts <- cbind.data.frame(CTH=insitu_unopt_data$cthmean, Viz=insitu_unopt_data$vizmean)
barplot(t(counts),
		names=insitu_unopt_data$cores,
		las=2,
		ylim=c(0,250),
		legend.text=c("CTH", "Viz"),
		col=myc[2:3],
		xlab="Client Ranks",
		ylab="Time (sec)",
		main="CTH and Visualization Time for In-Situ (untuned)")

text(2, 150, "33k blocks")
text(7, 150, "219k blocks")
text(13, 150, "1.5m blocks")

counts <- cbind.data.frame(CTH=intransit_data$cthmean, Viz=intransit_data$vizmean)
barplot(t(counts),
		names=intransit_data$cores,
		beside=FALSE,
		las=2,
		#ylim=c(0,250),
		legend.text=c("CTH", "Viz"),
		col=myc[2:3],
		xlab="Client Ranks",
		ylab="Time (sec)",
		main="CTH and Visualization Time for In-Transit (128 extra nodes)")

text(2, 150, "33k blocks")
text(7, 150, "219k blocks")
text(13, 150, "1.5m blocks")

counts <- cbind.data.frame(CTH=intransit_inc_data$cthmean, Viz=intransit_inc_data$vizmean)
barplot(t(counts),
		names=intransit_inc_data$cores,
		beside=FALSE,
		las=2,
		ylim=c(0,250),
		legend.text=c("CTH", "Viz"),
		col=myc[2:3],
		xlab="Client Ranks",
		ylab="Time (sec)",
		main="CTH and Visualization Time for In-Transit (100 internal nodes)")

text(2, 150, "33k blocks")
text(7, 150, "219k blocks")
text(13, 150, "1.5m blocks")


# Only large portion
#pdf("insitu-large.pdf", width=4.2, height=4)

d1 <- insitu_unopt_data$cthmean[insitu_unopt_data$levels=="1.5m blocks"]
d2 <- insitu_unopt_data$vizmean[insitu_unopt_data$levels=="1.5m blocks"]

names <- insitu_unopt_data$cores[insitu_unopt_data$levels=="1.5m blocks"]

counts <- cbind.data.frame(CTH=d1, Viz=d2)
barplot(t(counts),
		names=names,
		las=2,
		ylim=c(0,250),
		legend.text=c("CTH", "Viz"),
		col=myc[2:3],
		xlab="",
		ylab="Time (sec)",
		main="In-Situ Timings")


#=================== Stack plots showing CTH + Viz ==================



#ggplot(d2, aes(cores, fill=cthmean)) +
		#geom_bar(position="stack") +
		#geom_area(aes(y=cthmean), fill='green') +
		#geom_ribbon(aes(ymin=cthmean, ymax=(cthmean+vizmean)),  fill='blue') +
		#scale_x_continuous(breaks=2^(2:16), trans='log2')



#=================== CTH Only ==================


# CTH only (exclude all viz components)
ggplot(insitu_opt_data, aes(x=cores, y=cthmean, ymin=0, colour=levels, shape=levels)) +
		geom_line(size=1)  +
		geom_point(size=2.5) +
		geom_errorbar(aes(ymin=cthmean-ctherr, ymax=cthmean+ctherr), width=.2) +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_discrete(name="Data Sets") + scale_shape_discrete(name="Data Sets") +
		ggtitle("CTH Execution Time for 10 cycles (no viz)") +
		xlab("Client Ranks") + ylab("Seconds") +
		theme(legend.position=c(0.1, 0.8))

# CTH block scaling rate only (exclude all viz components)
ggplot(insitu_opt_data, aes(x=cores, y=cthrate, ymin=0, colour=levels, shape=levels)) +
		geom_line(size=1)  +
		geom_point(size=2.5) +
		geom_errorbar(aes(ymin=cthrate-cthrateerr, ymax=cthrate+cthrateerr), width=.2) +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_discrete(name="Data Sets") + scale_shape_discrete(name="Data Sets") +
		ggtitle("CTH Processing Rate (no viz) ") +
		xlab("Client Ranks") + ylab("Blocks/Second") +
		theme(legend.position=c(0.1, 0.8))

#=================== In-Situ Untuned Results ==================

# In-situ unopt (viz only)
ggplot(insitu_unopt_data, aes(x=cores, y=vizmean, ymin=0, colour=levels, shape=levels)) +
		geom_line(size=1)  +
		geom_point(size=2.5) +
		geom_errorbar(aes(ymin=vizmean-vizerr, ymax=vizmean+vizerr), width=.2) +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_discrete(name="Data Sets") + scale_shape_discrete(name="Data Sets") +
		ggtitle("In-Situ Visualization Time (untuned, no cth)") +
		xlab("Client Ranks") + ylab("Seconds") +
		theme(legend.position=c(0.1, 0.8))

# In-situ unopt scaling rate (viz only)
ggplot(insitu_unopt_data, aes(x=cores, y=vizrate, ymin=0, colour=levels, shape=levels)) +
		geom_line(size=1)  +
		geom_point(size=2.5) +
                theme_bw() +
		geom_errorbar(aes(ymin=vizrate-vizrateerr, ymax=vizrate+vizrateerr), width=.2) +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_discrete(name="Data Sets") + scale_shape_discrete(name="Data Sets") +
		#ggtitle("In-Situ Visualization Processing Rate (untuned, no cth)") +
		ggtitle("Fragment Detection Processing Rate") +
		xlab("Client Ranks") + ylab("Blocks/Second") +
		theme(legend.position=c(0.8, 0.2),legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))

# In-situ unopt (viz+cth)
ggplot(insitu_unopt_data, aes(x=cores, y=totalmean, ymin=0, colour=levels, shape=levels)) +
		geom_line(size=1)  +
		geom_point(size=2.5) +
		geom_errorbar(aes(ymin=totalmean-totalerr, ymax=totalmean+totalerr), width=.2) +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_discrete(name="Data Sets") + scale_shape_discrete(name="Data Sets") +
		ggtitle("In-Situ CTH + Visualization Time (untuned)") +
		xlab("Client Ranks") + ylab("Seconds") +
		theme(legend.position=c(0.2, 0.8))

# In-situ unopt scaling (viz+cth)
ggplot(insitu_unopt_data, aes(x=cores, y=totalrate, ymin=0, colour=levels, shape=levels)) +
		geom_line(size=1)  +
		geom_point(size=2.5) +
		geom_errorbar(aes(ymin=totalrate-totalrateerr, ymax=totalrate+totalrateerr), width=.2) +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_discrete(name="Data Sets") + scale_shape_discrete(name="Data Sets") +
		ggtitle("In-Situ CTH + Visualization Processing Rate (untuned)") +
		xlab("Client Ranks") + ylab("Blocks/Second") +
		theme(legend.position=c(0.2, 0.8))


#=================== In-Situ Tuned Results ==================

# In-situ opt (viz only)
ggplot(insitu_opt_data, aes(x=cores, y=vizmean, ymin=0, colour=levels, shape=levels)) +
		geom_line(size=1)  +
		geom_point(size=2.5) +
		geom_errorbar(aes(ymin=vizmean-vizerr, ymax=vizmean+vizerr), width=.2) +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_discrete(name="Data Sets") + scale_shape_discrete(name="Data Sets") +
		ggtitle("In-Situ Vizualization Time (tuned, no cth, 10 cycles)") +
		xlab("Client Ranks") + ylab("Time (sec)") +
		theme(legend.position=c(0.2, 0.8))

# In-situ opt (viz only)
ggplot(insitu_opt_data, aes(x=cores, y=vizrate, ymin=0, colour=levels, shape=levels)) +
		geom_line(size=1)  +
		geom_point(size=2.5) +
		geom_errorbar(aes(ymin=vizrate-vizrateerr, ymax=vizrate+vizrateerr), width=.2) +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_discrete(name="Data Sets") + scale_shape_discrete(name="Data Sets") +
		ggtitle("In-Situ Vizualization Processing Rate (tuned, no cth)") +
		xlab("Client Ranks") + ylab("Blocks/Second") +
		theme(legend.position=c(0.2, 0.8))

# In-situ opt (viz+cth)
ggplot(insitu_opt_data, aes(x=cores, y=totalmean, ymin=0, colour=levels, shape=levels)) +
		geom_line(size=1)  +
		geom_point(size=2.5) +
		geom_errorbar(aes(ymin=totalmean-totalerr, ymax=totalmean+totalerr), width=.2) +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_discrete(name="Data Sets") + scale_shape_discrete(name="Data Sets") +
		ggtitle("In-Situ CTH + Vizualization (tuned)") +
		xlab("Client Ranks") + ylab("Time (sec)") +
		theme(legend.position=c(0.2, 0.8))

ggplot(insitu_opt_data, aes(x=cores, y=totalrate, ymin=0, colour=levels, shape=levels)) +
		geom_line(size=1)  +
		geom_point(size=2.5) +
		geom_errorbar(aes(ymin=totalrate-totalrateerr, ymax=totalrate+totalrateerr), width=.2) +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_discrete(name="Data Sets") + scale_shape_discrete(name="Data Sets") +
		ggtitle("In-Situ CTH + Vizualization Processing Rate (tuned)") +
		xlab("Client Ranks") + ylab("Blocks/Second") +
		theme(legend.position=c(0.2, 0.8))

#=================== In-Transit Results ==================

# In-transit viz (no cth)  -- xfer + wait
ggplot(intransit_data, aes(x=cores, y=vizmean, colour=levels, shape=levels)) +
		geom_line(size=1)  +
		geom_point(size=2.5) +
		geom_errorbar(aes(ymin=vizmean-vizerr, ymax=vizmean+vizerr), width=.2) +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_discrete(name="Data Sets") + scale_shape_discrete(name="Data Sets") +
		ggtitle("In-Transit Client Vizualization (xfer + wait)") +
		xlab("Client Ranks") + ylab("Time (sec)") +
		theme(legend.position=c(0.2, 0.8))

# In-transit cth + viz  -- xfer + wait
ggplot(intransit_data, aes(x=cores, y=totalmean, colour=levels, shape=levels)) +
		geom_line(size=1)  +
		geom_point(size=2.5) +
		geom_errorbar(aes(ymin=totalmean-totalerr, ymax=totalmean+totalerr), width=.2) +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_discrete(name="Data Sets") + scale_shape_discrete(name="Data Sets") +
		ggtitle("In-Transit CTH + Vizualization") +
		xlab("Client Ranks") + ylab("Time (sec)") +
		theme(legend.position=c(0.2, 0.8))

ggplot(intransit_data, aes(x=cores, y=totalrate, colour=levels, shape=levels)) +
		geom_line(size=1)  +
		geom_point(size=2.5) +
		geom_errorbar(aes(ymin=totalrate-totalrateerr, ymax=totalrate+totalrateerr), width=.2) +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_discrete(name="Data Sets") + scale_shape_discrete(name="Data Sets") +
		ggtitle("In-Transit CTH + Vizualization Effective Processing Rate") +
		xlab("Client Ranks") + ylab("Blocks/Second") +
		theme(legend.position=c(0.2, 0.8))




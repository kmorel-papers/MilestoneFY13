library(stringr)
library(ggplot2)


###########################################################
# Create a plot for client_timing*.csv file
###########################################################
plotClientSeries <- function(data, colors=heat.colors, title="", ymax=0) {


	# convert to a plot that we can use to generate area plots of CTH, XFer, and Wait
	# Need columns for cycle, operation (e.g., CTH), and time
	cth_data <- cbind.data.frame(cycle=data$cycle, time=data$cth, Operation="CTH")
	xfer_data <- cbind.data.frame(cycle=data$cycle, time=(data$viz_sync_md + data$viz_sync_data), Operation="Xfer")
	wait_data <- cbind.data.frame(cycle=data$cycle, time=data$viz_wait, Operation="Wait")

	if (ymax==0) {
		ymax <- max(cth_data$time + xfer_data$time + wait_data$time)
	}

	area_data <- rbind.data.frame(cth_data, xfer_data, wait_data)

	p <- ggplot(area_data, aes(x=cycle, y=time, ymax=250, fill=Operation))

	p <- p + geom_area()
	p <- p + scale_y_continuous(limits=c(0,ymax))
	p <- p + scale_fill_manual(values=colors)
	p <- p + theme_bw()
	p <- p + theme(legend.position=c(0.8,0.25)) + ggtitle(title)
	p <- p + xlab("Cycle") + ylab("Time (sec)")

	print(p)
}

###########################################################
# Create a plot for extracted*.csv file
###########################################################
plotExtractedSeries <- function(data, title="", colors, ymin=0, ymax=0) {


	# convert to a plot that we can use to generate area plots of CTH, XFer, and Wait
	# Need columns for cycle, operation (e.g., CTH), and time
	cth_data <- cbind.data.frame(cycle=data$cycle, time=data$cth, Operation="CTH")
	viz_data <- cbind.data.frame(cycle=data$cycle, time=(data$viz), Operation="Viz")

	area_data <- rbind.data.frame(cth_data, viz_data)

	if (ymax==0) {
		ymax <- max(cth_data$time + viz_data$time)
	}

	#title <- paste("In-Situ Cycle Timings for",
	#		data$ranks[1]," Ranks (",
	#		round(mean(data$active_blocks)), "blocks )\n\n", file)

	#p <- ggplot(area_data, aes(x=cycle, y=time, group=Operation, fill=Operation))
	p <- ggplot(area_data, aes(x=cycle, y=time, fill=Operation))


	p <- p + geom_area()
	p <- p + scale_fill_manual(values=colors)
	p <- p + theme_bw()
	p <- p + theme(legend.position=c(0.8,0.25)) + ggtitle(title)
	p <- p + scale_y_continuous(limits=c(0,ymax))
	p <- p + xlab("Cycle") + ylab("Time (sec)")

	print(p)
}


library(stringr)
library(ggplot2)


###########################################################
# Create a plot for client_timing*.csv file
###########################################################
plotClientSeries <- function(files, colors=heat.colors, title="") {

	# For each directory, gather statistics on each experiment
	for (file in files) {

		data <- read.csv(file, comment.char="%")

		# convert to a plot that we can use to generate area plots of CTH, XFer, and Wait
		# Need columns for cycle, operation (e.g., CTH), and time
		cth_data <- cbind.data.frame(cycle=data$cycle, time=data$cth, Operation="CTH")
		xfer_data <- cbind.data.frame(cycle=data$cycle, time=(data$viz_sync_md + data$viz_sync_data), Operation="Xfer")
		wait_data <- cbind.data.frame(cycle=data$cycle, time=data$viz_wait, Operation="Wait")

		area_data <- rbind.data.frame(cth_data, xfer_data, wait_data)

		p <- ggplot(area_data, aes(x=cycle, y=time, fill=Operation))

		p <- p + geom_area()
		p <- p + scale_fill_manual(values=colors)
		p <- p + theme(legend.position=c(0.8,0.3)) + ggtitle(title)
		p <- p + xlab("Cycle") + ylab("Time (sec)")

		print(p)
	}
}


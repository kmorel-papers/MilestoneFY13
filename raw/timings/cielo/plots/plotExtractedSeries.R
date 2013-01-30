library(stringr)
library(ggplot2)


source ("pvspyPlotDefaults.R")

###########################################################
# Create a plot for client_timing*.csv file
###########################################################
plotExtractedSeries <- function(file, title="", colors) {

	data <- read.csv(file, comment.char="%")

	# convert to a plot that we can use to generate area plots of CTH, XFer, and Wait
	# Need columns for cycle, operation (e.g., CTH), and time
	cth_data <- cbind.data.frame(cycle=data$cycle, time=data$cth, Operation="CTH")
	viz_data <- cbind.data.frame(cycle=data$cycle, time=(data$viz), Operation="Viz")

	area_data <- rbind.data.frame(cth_data, viz_data)

	#title <- paste("In-Situ Cycle Timings for",
	#		data$ranks[1]," Ranks (",
	#		round(mean(data$active_blocks)), "blocks )\n\n", file)

	#p <- ggplot(area_data, aes(x=cycle, y=time, group=Operation, fill=Operation))
	p <- ggplot(area_data, aes(x=cycle, y=time, fill=Operation))

	p <- p + geom_area()
	p <- p + scale_fill_manual(values=colors)
	p <- p + theme(legend.position=c(0.8,0.3)) + ggtitle(title)
	p <- p + xlab("Cycle") + ylab("Time (sec)")

	print(p)
}

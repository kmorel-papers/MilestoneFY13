# The Overlay plots plot all the runs from a single set of experiments
# on top of one another.  The goal is to show variance.

library("ggplot2")

# insitu_opt_dirs, insitu_unopt_dirs, intransit_dirs, intransit_inclusive_dirs
source("pvspyDirs.R")
source("pvspyPlotDefaults.R")


write_pdf <- TRUE


data_index <- 10

# Get list of extractedTimings files (we're only interested in plotting CTH variance)
files <- list.files(path=insitu_opt_dirs[data_index], pattern="extracted.*csv",full.names=TRUE)

data <- {}
for (i in 1:length(files)) {
   dd <- read.csv(files[i], comment.char="%")
   dd$Experiment <- as.character(i)
   data <- rbind.data.frame(data, dd)
}

# Insitu CTH Plot
color <- pvspy_colors[3]
plot <- ggplot(data, aes(x=cycle, y=cth, ymin=0, ymax=160, group=Experiment))
plot <- plot + geom_line(size=pvspy_line_size,aes(linetype=Experiment, colour=Experiment))
plot <- plot + geom_point(size=pvspy_point_size, aes(shape=Experiment, colour=Experiment))
plot <- plot + scale_color_manual(values=pvspy_colors[1:length(files)])
plot <- plot + scale_shape_manual(values=pvspy_shapes[1:length(files)])
plot <- plot + scale_linetype_manual(values=pvspy_lines[1:length(files)])
plot <- plot + xlab("Cycle") + ylab("Time (min)")
plot <- plot + theme_bw()
plot <- plot + guides(fill=guide_legend(nrow=4))
plot <- plot + theme(legend.position=c(0.5, 0.2),legend.key=element_rect(color="white"),legend.background=element_rect(color="black"),legend.direction='horizontal')


if (write_pdf == TRUE) {
	pdf(file="in-situ-opt-cth-overlay.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot



source("pvspyPlotDefaults.R")
source("processVariance.R")

# insitu_opt_dirs, insitu_unopt_dirs, intransit_dirs, intransit_inclusive_dirs
source("pvspyDirs.R")

write_pdf <- TRUE


data_index <- 10


# Insitu CTH Plot
color <- pvspy_colors[3]
data <- processExtractedVariance(insitu_opt_dirs[data_index])
#data <- data[!data$cycle==0,]  # remove cycle=0 from plot
plot <- ggplot(data, aes(x=cycle, y=cth_mean, ymin=0))
plot <- plot + geom_line(size=pvspy_line_size, linetype=1, color=color)
plot <- plot + geom_point(size=pvspy_point_size, fill="white", color=color, shape=pvspy_shapes[1])
plot <- plot + geom_errorbar(aes(ymin=cth_mean-cth_sd, ymax=cth_mean+cth_sd), color=color)
plot <- plot + theme_bw()
plot <- plot + xlab("Cycle") + ylab("Time (sec)")

if (write_pdf == TRUE) {
	pdf(file="in-situ-opt-cth-variance.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot


# Insitu refined Viz Plot
color <- pvspy_colors[4]
data <- processExtractedVariance(insitu_opt_dirs[data_index])
plot <- ggplot(data, aes(x=cycle, y=viz_mean, ymin=0))
plot <- plot + geom_line(size=pvspy_line_size, linetype=1, color=color)
plot <- plot + geom_point(size=pvspy_point_size, fill="white", color=color, shape=pvspy_shapes[2])
plot <- plot + geom_errorbar(aes(ymin=viz_mean-viz_sd, ymax=viz_mean+viz_sd), color=color)
plot <- plot + theme_bw()
plot <- plot + xlab("Cycle") + ylab("Time (sec)")
if (write_pdf == TRUE) {
	pdf(file="in-situ-opt-viz-variance.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot

# Insitu baseline Viz Plot
color <- pvspy_colors[4]
data <- processExtractedVariance(insitu_unopt_dirs[data_index])
plot <- ggplot(data, aes(x=cycle, y=viz_mean, ymin=0))
plot <- plot + geom_line(size=pvspy_line_size, linetype=1, color=color)
plot <- plot + geom_point(size=pvspy_point_size, fill="white", color=color, shape=pvspy_shapes[2])
plot <- plot + geom_errorbar(aes(ymin=viz_mean-viz_sd, ymax=viz_mean+viz_sd), color=color)
plot <- plot + theme_bw()
plot <- plot + xlab("Cycle") + ylab("Time (sec)")
if (write_pdf == TRUE) {
	pdf(file="in-situ-unopt-viz-variance.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot

# In-transit Extra Xfer Plot
color <- pvspy_colors[6]
data <- processClientVariance(intransit_extra_dirs[data_index])
#data <- data[!data$cycle==0,]  # remove cycle=0 from plot
plot <- ggplot(data, aes(x=cycle, y=xfer_mean, ymin=0, ymax=25))
plot <- plot + geom_line(size=pvspy_line_size, linetype=1, color=color)
plot <- plot + geom_point(size=pvspy_point_size, fill="white", color=color, shape=pvspy_shapes[3])
plot <- plot + geom_errorbar(aes(ymin=xfer_mean-xfer_sd, ymax=xfer_mean+xfer_sd), color=color)
plot <- plot + theme_bw()
plot <- plot + xlab("Cycle") + ylab("Time (sec)")

if (write_pdf == TRUE) {
	pdf(file="in-transit-extra-xfer-variance.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot

# In-transit extra wait Plot
color <- pvspy_colors[7]
data <- processClientVariance(intransit_extra_dirs[data_index])
data <- data[!data$cycle==0,]  # remove cycle=0 from plot
plot <- ggplot(data, aes(x=cycle, y=wait_mean, ymin=0, ymax=70))
plot <- plot + geom_line(size=pvspy_line_size, linetype=1, color=color)
plot <- plot + geom_point(size=pvspy_point_size, fill="white", color=color, shape=pvspy_shapes[4])
plot <- plot + geom_errorbar(aes(ymin=wait_mean-wait_sd, ymax=wait_mean+wait_sd), color=color)
plot <- plot + theme_bw()
plot <- plot + xlab("Cycle") + ylab("Time (sec)")

if (write_pdf == TRUE) {
	pdf(file="in-transit-extra-wait-variance.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot

# In-transit Inclusive Xfer Plot
color <- pvspy_colors[6]
data <- processClientVariance(intransit_inclusive_dirs[data_index])
#data <- data[!data$cycle==0,]  # remove cycle=0 from plot
plot <- ggplot(data, aes(x=cycle, y=xfer_mean, ymin=0, ymax=25))
plot <- plot + geom_line(size=pvspy_line_size, linetype=1, color=color)
plot <- plot + geom_point(size=pvspy_point_size, fill="white", color=color, shape=pvspy_shapes[3])
plot <- plot + geom_errorbar(aes(ymin=xfer_mean-xfer_sd, ymax=xfer_mean+xfer_sd), color=color)
plot <- plot + theme_bw()
plot <- plot + xlab("Cycle") + ylab("Time (sec)")

if (write_pdf == TRUE) {
	pdf(file="in-transit-inclusive-xfer-variance.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot

# In-transit extra wait Plot
color <- pvspy_colors[7]
data <- processClientVariance(intransit_inclusive_dirs[data_index])
data <- data[!data$cycle==0,]  # remove cycle=0 from plot
plot <- ggplot(data, aes(x=cycle, y=wait_mean, ymin=0, ymax=70))
plot <- plot + geom_line(size=pvspy_line_size, linetype=1, color=color)
plot <- plot + geom_point(size=pvspy_point_size, fill="white", color=color, shape=pvspy_shapes[4])
plot <- plot + geom_errorbar(aes(ymin=wait_mean-wait_sd, ymax=wait_mean+wait_sd), color=color)
plot <- plot + theme_bw()
plot <- plot + xlab("Cycle") + ylab("Time (sec)")

if (write_pdf == TRUE) {
	pdf(file="in-transit-inclusive-wait-variance.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot

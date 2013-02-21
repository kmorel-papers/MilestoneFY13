#################################################
# Generate plots that show total runtime
#
# Unfortunately, we didn't capture total runtime for all the
# plots.   I would like to use HPCToolkit data, but we only
# captured HPCToolkit data for in-situ (optimized) and
# in-transit (extra nodes).   We use HPCToolkit runs
# to extract timings for amrini and viz_init (pvspy_fil),
# then we used client_timings*.csv and extracted*.csv to
# get CTH and viz costs during the cycle calculations.
#
#################################################

library(stringr)
library(ggplot2)

# Load the function for processing HPCToolkit results
source("processHPCTimings.R")
source("processExtractedTimings.R")
source("processClientTimings.R")

# Load plot defaults (pvspy_colors, pvspy_shapes, pvspy_lines)
source("pvspyPlotDefaults.R")
source("pvspyDirs.R")


# Override the plot height and width
pvspy_plot_height <- 4
pvspy_plot_width <- 5

with_title <- FALSE
write_pdf <- TRUE



#################################################
# Extract Data From Result Files (only the results up to 32K)
#################################################


# paths to hpctoolkit experiments
#insitu_opt_dirs <- list.files(path="../amr_in-situ-hpctoolkit", full.names=TRUE, pattern="pipe-.*", recursive=FALSE)[1:12]

#intransit_dirs <- list.files(path="../amr_in-transit-hpctoolkit", full.names=TRUE, pattern="pipe-.*", recursive=FALSE)[1:12]
#spyplot_file_dirs <- list.files(path="../amr-file-hpctoolkit", full.names=TRUE, pattern="pipe-.*", recursive=FALSE)[1:12]


# I will soon have hpctoolkit dirs for these as well
#intransit_inclusive_dirs <- list.files(path="../amr_in-transit-inclusive", full.names=TRUE, pattern="pipe-.*", recursive=FALSE)[1:12]
#insitu_unopt_dirs <- list.files(path="../amr_in-situ-unopt", full.names=TRUE, pattern="pipe-.*", recursive=FALSE)[1:12]



##########  In-Situ Unopt Processing Rate   #####################

data <- processExtractedTimings(insitu_unopt_dirs)

data$complete <- data$vizrate
data$complete_err <- data$vizrateerr

data$levels[data$levels == 5] <- "33k blocks"
data$levels[data$levels == 6] <- "220k blocks"
data$levels[data$levels == 7] <- "1.5m blocks"

names(data)[1] <- "Dataset"

plot <- ggplot(data, aes(x=cores, y=complete/60, ymax=200, ymin=0, colour=Dataset, linetype=Dataset, shape=Dataset))

plot <- plot + geom_line(size=pvspy_line_size)
plot <- plot + geom_point(size=pvspy_point_size, fill="white")
plot <- plot + geom_errorbar(aes(ymin=(complete-complete_err)/60, ymax=(complete+complete_err)/60), width=pvspy_error_width, linetype=1)
plot <- plot + scale_x_continuous(breaks=2^(2:16), trans='log2')
plot <- plot + scale_shape_manual(values=pvspy_shapes)
plot <- plot + scale_color_manual(values=pvspy_colors)
plot <- plot + scale_linetype_manual(values=pvspy_lines)
plot <- plot + theme_bw()
plot <- plot + theme(legend.position=c(0.15, 0.83),legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))
plot <- plot + xlab("Client Ranks") + ylab("Blocks/Second")

if (with_title == TRUE) {
   plot <- plot + ggtitle("In Situ (Baseline) Processing Rate")
}

if (write_pdf == TRUE){
   pdf(file="in-situ-unopt-rate.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot



##########  In-Situ opt processing rate #####################

data <- processExtractedTimings(insitu_opt_dirs)

data$complete <- data$vizrate
data$complete_err <- data$vizrateerr

data$levels[data$levels == 5] <- "33k blocks"
data$levels[data$levels == 6] <- "220k blocks"
data$levels[data$levels == 7] <- "1.5m blocks"

names(data)[1] <- "Dataset"

plot <- ggplot(data, aes(x=cores, y=complete/60, ymax=200, ymin=0, colour=Dataset, linetype=Dataset, shape=Dataset))

plot <- plot + geom_line(size=pvspy_line_size)
plot <- plot + geom_point(size=pvspy_point_size, fill="white")
plot <- plot + geom_errorbar(aes(ymin=(complete-complete_err)/60, ymax=(complete+complete_err)/60), width=pvspy_error_width, linetype=1)
plot <- plot + scale_x_continuous(breaks=2^(2:16), trans='log2')
plot <- plot + scale_shape_manual(values=pvspy_shapes)
plot <- plot + scale_color_manual(values=pvspy_colors)
plot <- plot + scale_linetype_manual(values=pvspy_lines)
plot <- plot + theme_bw()
plot <- plot + theme(legend.position=c(0.15, 0.83),legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))
plot <- plot + xlab("Client Ranks") + ylab("Blocks/Second")

if (with_title == TRUE) {
	plot <- plot + ggtitle("In Situ (Refined) Processing Rate")
}

if (write_pdf == TRUE){
	pdf(file="in-situ-opt-rate.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot



##########  In-transit extra  processing rate #####################

data <- processExtractedTimings(intransit_extra_dirs)

data$complete <- data$vizrate
data$complete_err <- data$vizrateerr

data$levels[data$levels == 5] <- "33k blocks"
data$levels[data$levels == 6] <- "220k blocks"
data$levels[data$levels == 7] <- "1.5m blocks"

names(data)[1] <- "Dataset"

plot <- ggplot(data, aes(x=cores, y=complete/60, ymax=200, ymin=0, colour=Dataset, linetype=Dataset, shape=Dataset))

plot <- plot + geom_line(size=pvspy_line_size)
plot <- plot + geom_point(size=pvspy_point_size, fill="white")
plot <- plot + geom_errorbar(aes(ymin=(complete-complete_err)/60, ymax=(complete+complete_err)/60), width=pvspy_error_width, linetype=1)
plot <- plot + scale_x_continuous(breaks=2^(2:16), trans='log2')
plot <- plot + scale_shape_manual(values=pvspy_shapes)
plot <- plot + scale_color_manual(values=pvspy_colors)
plot <- plot + scale_linetype_manual(values=pvspy_lines)
plot <- plot + theme_bw()
plot <- plot + theme(legend.position=c(0.15, 0.83),legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))
plot <- plot + xlab("Client Ranks") + ylab("Blocks/Second")

if (with_title == TRUE) {
	plot <- plot + ggtitle("In Situ (Refined) Processing Rate")
}

if (write_pdf == TRUE){
	pdf(file="in-transit-extra-rate.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot




##########  In-Situ opt processing rate #####################

data <- processExtractedTimings(intransit_inclusive_dirs)

data$complete <- data$vizrate
data$complete_err <- data$vizrateerr

data$levels[data$levels == 5] <- "33k blocks"
data$levels[data$levels == 6] <- "220k blocks"
data$levels[data$levels == 7] <- "1.5m blocks"

names(data)[1] <- "Dataset"

plot <- ggplot(data, aes(x=cores, y=complete/60, ymax=200, ymin=0, colour=Dataset, linetype=Dataset, shape=Dataset))

plot <- plot + geom_line(size=pvspy_line_size)
plot <- plot + geom_point(size=pvspy_point_size, fill="white")
plot <- plot + geom_errorbar(aes(ymin=(complete-complete_err)/60, ymax=(complete+complete_err)/60), width=pvspy_error_width, linetype=1)
plot <- plot + scale_x_continuous(breaks=2^(2:16), trans='log2')
plot <- plot + scale_shape_manual(values=pvspy_shapes)
plot <- plot + scale_color_manual(values=pvspy_colors)
plot <- plot + scale_linetype_manual(values=pvspy_lines)
plot <- plot + theme_bw()
plot <- plot + theme(legend.position=c(0.15, 0.83),legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))
plot <- plot + xlab("Client Ranks") + ylab("Blocks/Second")

if (with_title == TRUE) {
	plot <- plot + ggtitle("In Transit (inclusive) Processing Rate")
}

if (write_pdf == TRUE){
	pdf(file="in-transit-inclusive-rate.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot



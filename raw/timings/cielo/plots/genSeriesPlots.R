
source("pvspyPlotDefaults.R")
source("plotSeries.R")

write_pdf <- TRUE

insitu_opt_file <- "../amr_in-situ-hpctoolkit/pipe-7-08192/extracted_timings-e692651.csv"
insitu_unopt_file <- "../amr_in-situ-unopt/pipe-7-08192/extracted_timings-e630828.csv"
intransit_inclusive_file <- "../amr_in-transit-inclusive/pipe-7-08192/client_timings-701627.csv"
intransit_extra_file <- "../amr_in-transit-hpctoolkit/pipe-7-08192/client_timings-701628.csv"

ymax <- 150

if (write_pdf == TRUE){
	pdf(file="in-situ-opt-series.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
insitu_opt_data <- read.csv(insitu_opt_file, comment.char="%")
plotExtractedSeries(insitu_opt_data, colors=pvspy_colors[c(3,4)], ymax=ymax)

if (write_pdf == TRUE){
	pdf(file="in-situ-unopt-series.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
insitu_unopt_data <- read.csv(insitu_unopt_file, comment.char="%")
plotExtractedSeries(insitu_unopt_data, colors=pvspy_colors[c(3,4)], ymax=ymax)

if (write_pdf == TRUE){
	pdf(file="in-transit-inclusive-series.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
intransit_inclusuve_data <- read.csv(intransit_inclusive_file, comment.char="%")
plotClientSeries(intransit_inclusuve_data, colors=pvspy_colors[c(3,6,7)], ymax=ymax)

if (write_pdf == TRUE){
	pdf(file="in-transit-extra-series.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
intransit_extra_data <- read.csv(intransit_extra_file, comment.char="%")
plotClientSeries(intransit_extra_data, colors=pvspy_colors[c(3,6,7)], ymax=ymax)


# Plot the number of blocks
if (write_pdf == TRUE) {
	pdf(file="active-blocks-series.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot(x=intransit_extra_data$cycle, y=intransit_extra_data$active_blocks/1.0e6,
		xlab="Cycle", ylab="Blocks (millons)")
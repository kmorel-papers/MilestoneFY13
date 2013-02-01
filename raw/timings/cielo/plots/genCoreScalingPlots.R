#################################################
# Generate plots that show core scaling performance
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

pvspy_plot_width <- 4
pvspy_plot_height <- 4

with_title <- FALSE
write_pdf <- TRUE


# Use HPCToolkit data to get initialization timings
dirs <- list.files(path="../amr_in-situ-hpctoolkit", full.names=TRUE, pattern="pipe-.*", recursive=FALSE)[1:12]
insitu_hpc_data <- processHPCClient(directories=dirs)

# CTH Initialization
amrini_secs <- insitu_hpc_data$amrini_mean / 1e6   # convert to seconds (same for insitu and in-transit)
amrini_err <- insitu_hpc_data$amrini_err / 1e6   # convert to seconds (same for insitu and in-transit)


# Now get the data for the 2, 4, 8 core runs
dirs <- list.files(path="../amr_in-transit", full.names=TRUE, pattern="pipe-.*", recursive=FALSE)[1:12]

data2 <- processClientTimings(directories=dirs, pattern="client_timings-02.csv")
data4 <- processClientTimings(directories=dirs, pattern="client_timings-04.csv")
data8 <- processClientTimings(directories=dirs, pattern="client_timings-08.csv")



##########   TOTAL TIME  #####################

# Add a total time
data2$complete <- data2$cthmean*50 + data2$vizmean*51 + amrini_secs
data4$complete <- data4$cthmean*50 + data4$vizmean*51 + amrini_secs
data8$complete <- data8$cthmean*50 + data8$vizmean*51 + amrini_secs

###### Small data set  ######################

d2 <- data2[data2$levels == "5",]
d4 <- data4[data4$levels == "5",]
d8 <- data8[data8$levels == "5",]

# change group names
d2$levels <- " 4 cores (2/node)"
d4$levels <- " 8 cores (4/node)"
d8$levels <- "16 cores (8/node)"


data <- rbind.data.frame(d2, d4, d8)

data <- data[!(data$cores==128),]
name <- "Service Size"

# Compare untuned experiments
plot <- ggplot(data, aes(x=cores, y=complete/60, ymin=0)) +
	geom_line(size=pvspy_line_size, aes(linetype=levels, colour=levels))  +
	geom_point(size=pvspy_point_size, aes(shape=levels, colour=levels), fill="white") +
	scale_x_continuous(breaks=2^(2:16), trans='log2') +
	scale_color_manual(name=name, values=pvspy_colors[1:5]) +
	scale_shape_manual(name=name, values=pvspy_shapes[1:5]) +
	scale_linetype_manual(name=name, values=pvspy_lines[1:5]) +
	xlab("Client Ranks") + ylab("Time (min)") +
	theme_bw() +
	theme(legend.position=c(0.24, 0.2),legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))

if (with_title==TRUE) {
   plot <- plot + ggtitle("Total Execution Time for 500 Cycles (1.5m blocks)")
}

if (write_pdf == TRUE) {
   pdf(file="in-transit-33k-total.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot


###### Medium data set   ######################

d2 <- data2[data2$levels == "6",]
d4 <- data4[data4$levels == "6",]
d8 <- data8[data8$levels == "6",]

# change group names
d2$levels <- " 32 cores (2/node)"
d4$levels <- " 64 cores (4/node)"
d8$levels <- "128 cores (8/node)"


data <- rbind.data.frame(d2, d4, d8)

data <- data[!(data$cores==128),]
name <- "Service Size"

# Compare untuned experiments
plot <- ggplot(data, aes(x=cores, y=complete/60, ymin=0)) +
		geom_line(size=pvspy_line_size, aes(linetype=levels, colour=levels))  +
		geom_point(size=pvspy_point_size, aes(shape=levels, colour=levels), fill="white") +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_manual(name=name, values=pvspy_colors[1:5]) +
		scale_shape_manual(name=name, values=pvspy_shapes[1:5]) +
		scale_linetype_manual(name=name, values=pvspy_lines[1:5]) +
		xlab("Client Ranks") + ylab("Time (min)") +
		theme_bw() +
		theme(legend.position=c(0.25, 0.2),legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))

if (with_title==TRUE) {
	plot <- plot + ggtitle("Total Execution Time for 500 Cycles (1.5m blocks)")
}

if (write_pdf == TRUE) {
	pdf(file="in-transit-218k-total.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot


###### Large data set   ######################

d2 <- data2[data2$levels == "7",]
d4 <- data4[data4$levels == "7",]
d8 <- data8[data8$levels == "7",]

# change group names
d2$levels <- " 256  cores (2/node)"
d4$levels <- " 512  cores (4/node)"
d8$levels <- "1024 cores (8/node)"


data <- rbind.data.frame(d2, d4, d8)
name <- "Service Size"


# Compare untuned experiments
plot <- ggplot(data, aes(x=cores, y=complete/60, ymin=0)) +
		geom_line(size=pvspy_line_size, aes(linetype=levels, colour=levels))  +
		geom_point(size=pvspy_point_size, aes(shape=levels, colour=levels), fill="white") +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_manual(name=name, values=pvspy_colors[1:5]) +
		scale_shape_manual(name=name, values=pvspy_shapes[1:5]) +
		scale_linetype_manual(name=name, values=pvspy_lines[1:5]) +
		xlab("Client Ranks") + ylab("Time (min)") +
		theme_bw() +
		theme(legend.position=c(0.27, 0.2),legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))

if (with_title==TRUE) {
	plot <- plot + ggtitle("Total Execution Time for 500 Cycles (1.5m blocks)")
}

if (write_pdf == TRUE) {
	pdf(file="in-transit-1m-total.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot


##########   TRANSFER TIME  #####################

# Add a total time
data2$complete <- (data2$syncmdmean + data2$syncdmean)*51
data4$complete <- (data4$syncmdmean + data2$syncdmean)*51
data8$complete <- (data8$syncmdmean + data2$syncdmean)*51



###### Small data set  ######################

d2 <- data2[data2$levels == "5",]
d4 <- data4[data4$levels == "5",]
d8 <- data8[data8$levels == "5",]

# change group names
d2$levels <- " 4 cores (2/node)"
d4$levels <- " 8 cores (4/node)"
d8$levels <- "16 cores (8/node)"


data <- rbind.data.frame(d2, d4, d8)

data <- data[!(data$cores==128),]
name <- "Service Size"

# Compare untuned experiments
plot <- ggplot(data, aes(x=cores, y=complete/60, ymin=0)) +
		geom_line(size=pvspy_line_size, aes(linetype=levels, colour=levels))  +
		geom_point(size=pvspy_point_size, aes(shape=levels, colour=levels), fill="white") +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_manual(name=name, values=pvspy_colors[1:5]) +
		scale_shape_manual(name=name, values=pvspy_shapes[1:5]) +
		scale_linetype_manual(name=name, values=pvspy_lines[1:5]) +
		xlab("Client Ranks") + ylab("Time (min)") +
		theme_bw() +
		theme(legend.position=c(0.24, 0.2),legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))

if (with_title==TRUE) {
	plot <- plot + ggtitle("Total Execution Time for 500 Cycles (1.5m blocks)")
}

if (write_pdf == TRUE) {
	pdf(file="in-transit-33k-xfer.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot


###### Medium data set   ######################

d2 <- data2[data2$levels == "6",]
d4 <- data4[data4$levels == "6",]
d8 <- data8[data8$levels == "6",]

# change group names
d2$levels <- " 32 cores (2/node)"
d4$levels <- " 64 cores (4/node)"
d8$levels <- "128 cores (8/node)"


data <- rbind.data.frame(d2, d4, d8)

data <- data[!(data$cores==128),]
name <- "Service Size"

# Compare untuned experiments
plot <- ggplot(data, aes(x=cores, y=complete/60, ymin=0)) +
		geom_line(size=pvspy_line_size, aes(linetype=levels, colour=levels))  +
		geom_point(size=pvspy_point_size, aes(shape=levels, colour=levels), fill="white") +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_manual(name=name, values=pvspy_colors[1:5]) +
		scale_shape_manual(name=name, values=pvspy_shapes[1:5]) +
		scale_linetype_manual(name=name, values=pvspy_lines[1:5]) +
		xlab("Client Ranks") + ylab("Time (min)") +
		theme_bw() +
		theme(legend.position=c(0.25, 0.2),legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))

if (with_title==TRUE) {
	plot <- plot + ggtitle("Total Execution Time for 500 Cycles (1.5m blocks)")
}

if (write_pdf == TRUE) {
	pdf(file="in-transit-218k-xfer.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot


###### Large data set   ######################

d2 <- data2[data2$levels == "7",]
d4 <- data4[data4$levels == "7",]
d8 <- data8[data8$levels == "7",]

# change group names
d2$levels <- " 256  cores (2/node)"
d4$levels <- " 512  cores (4/node)"
d8$levels <- "1024 cores (8/node)"


data <- rbind.data.frame(d2, d4, d8)
name <- "Service Size"


# Compare untuned experiments
plot <- ggplot(data, aes(x=cores, y=complete/60, ymin=0)) +
		geom_line(size=pvspy_line_size, aes(linetype=levels, colour=levels))  +
		geom_point(size=pvspy_point_size, aes(shape=levels, colour=levels), fill="white") +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_manual(name=name, values=pvspy_colors[1:5]) +
		scale_shape_manual(name=name, values=pvspy_shapes[1:5]) +
		scale_linetype_manual(name=name, values=pvspy_lines[1:5]) +
		xlab("Client Ranks") + ylab("Time (min)") +
		theme_bw() +
		theme(legend.position=c(0.26, 0.2),legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))

if (with_title==TRUE) {
	plot <- plot + ggtitle("Total Execution Time for 500 Cycles (1.5m blocks)")
}

if (write_pdf == TRUE) {
	pdf(file="in-transit-1m-xfer.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot



##########   WAIT TIME  #####################

# Add a total time
data2$complete <- data2$waitmean*51
data4$complete <- data4$waitmean*51
data8$complete <- data8$waitmean*51



###### Small data set  ######################

d2 <- data2[data2$levels == "5",]
d4 <- data4[data4$levels == "5",]
d8 <- data8[data8$levels == "5",]

# change group names
d2$levels <- " 4 cores (2/node)"
d4$levels <- " 8 cores (4/node)"
d8$levels <- "16 cores (8/node)"


data <- rbind.data.frame(d2, d4, d8)

data <- data[!(data$cores==128),]
name <- "Service Size"

# Compare untuned experiments
plot <- ggplot(data, aes(x=cores, y=complete/60, ymin=0)) +
		geom_line(size=pvspy_line_size, aes(linetype=levels, colour=levels))  +
		geom_point(size=pvspy_point_size, aes(shape=levels, colour=levels), fill="white") +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_manual(name=name, values=pvspy_colors[1:5]) +
		scale_shape_manual(name=name, values=pvspy_shapes[1:5]) +
		scale_linetype_manual(name=name, values=pvspy_lines[1:5]) +
		xlab("Client Ranks") + ylab("Time (min)") +
		theme_bw() +
		theme(legend.position=c(0.24, 0.8),legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))

if (with_title==TRUE) {
	plot <- plot + ggtitle("Total Execution Time for 500 Cycles (1.5m blocks)")
}

if (write_pdf == TRUE) {
	pdf(file="in-transit-33k-wait.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot


###### Medium data set   ######################

d2 <- data2[data2$levels == "6",]
d4 <- data4[data4$levels == "6",]
d8 <- data8[data8$levels == "6",]

# change group names
d2$levels <- " 32 cores (2/node)"
d4$levels <- " 64 cores (4/node)"
d8$levels <- "128 cores (8/node)"


data <- rbind.data.frame(d2, d4, d8)

data <- data[!(data$cores==128),]
name <- "Service Size"

# Compare untuned experiments
plot <- ggplot(data, aes(x=cores, y=complete/60, ymin=0)) +
		geom_line(size=pvspy_line_size, aes(linetype=levels, colour=levels))  +
		geom_point(size=pvspy_point_size, aes(shape=levels, colour=levels), fill="white") +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_manual(name=name, values=pvspy_colors[1:5]) +
		scale_shape_manual(name=name, values=pvspy_shapes[1:5]) +
		scale_linetype_manual(name=name, values=pvspy_lines[1:5]) +
		xlab("Client Ranks") + ylab("Time (min)") +
		theme_bw() +
		theme(legend.position=c(0.25, 0.8),legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))

if (with_title==TRUE) {
	plot <- plot + ggtitle("Total Execution Time for 500 Cycles (1.5m blocks)")
}

if (write_pdf == TRUE) {
	pdf(file="in-transit-218k-wait.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot


###### Large data set   ######################

d2 <- data2[data2$levels == "7",]
d4 <- data4[data4$levels == "7",]
d8 <- data8[data8$levels == "7",]

# change group names
d2$levels <- " 256  cores (2/node)"
d4$levels <- " 512  cores (4/node)"
d8$levels <- "1024 cores (8/node)"


data <- rbind.data.frame(d2, d4, d8)
name <- "Service Size"


# Compare untuned experiments
plot <- ggplot(data, aes(x=cores, y=complete/60, ymin=0)) +
		geom_line(size=pvspy_line_size, aes(linetype=levels, colour=levels))  +
		geom_point(size=pvspy_point_size, aes(shape=levels, colour=levels), fill="white") +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_manual(name=name, values=pvspy_colors[1:5]) +
		scale_shape_manual(name=name, values=pvspy_shapes[1:5]) +
		scale_linetype_manual(name=name, values=pvspy_lines[1:5]) +
		xlab("Client Ranks") + ylab("Time (min)") +
		theme_bw() +
		theme(legend.position=c(0.26, 0.8),legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))

if (with_title==TRUE) {
	plot <- plot + ggtitle("Total Execution Time for 500 Cycles (1.5m blocks)")
}

if (write_pdf == TRUE) {
	pdf(file="in-transit-1m-wait.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot



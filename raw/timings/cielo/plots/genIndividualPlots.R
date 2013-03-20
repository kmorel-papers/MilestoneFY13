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

# insitu_opt_dirs, insitu_unopt_dirs, intransit_extra_dirs, intransit_inclusive_dirs
source("pvspyDirs.R")

# Override the plot height and width
pvspy_plot_height <- 4
pvspy_plot_width <- 5

with_title <- FALSE
write_pdf <- TRUE


#################################################
# Extract Data From Result Files (only the results up to 32K)
#################################################


# Use HPCToolkit data to get initialization timings
insitu_hpc_data <- processHPCClient(directories=insitu_opt_dirs)
intransit_hpc_data <- processHPCClient(directories = intransit_extra_dirs)
amr_file_hpc_data <- processHPCClient(directories = spyplot_file_dirs)

# CTH Initialization
amrini_secs <- insitu_hpc_data$amrini_mean / 1e6   # convert to seconds (same for insitu and in-transit)
amrini_err <- insitu_hpc_data$amrini_err / 1e6   # convert to seconds (same for insitu and in-transit)

# Insitu viz Initialization
insitu_viz_init_secs <- insitu_hpc_data$viz_init_mean / 1e6
insitu_viz_init_err <- insitu_hpc_data$viz_init_err / 1e6

# Intransit viz Initialization
intransit_viz_init_secs <- intransit_hpc_data$viz_init_mean / 1e6
intransit_viz_init_err <- intransit_hpc_data$viz_init_err / 1e6


# Use extracted timings for in-situ (when we have HPCTOOLKIT data for all data, we will not do this)
insitu_opt_data <- processExtractedTimings(insitu_opt_dirs)
insitu_unopt_data <- processExtractedTimings(insitu_unopt_dirs)
intransit_data <- processExtractedTimings(intransit_extra_dirs)
intransit_inclusive_data <- processExtractedTimings(intransit_inclusive_dirs)
amr_file_data <- processExtractedTimings(spyplot_file_dirs)

# Use extracted timings for in-situ




##########  Line Plot of In-Situ Unopt   #####################

data <- processExtractedTimings(insitu_unopt_dirs)

data$complete <- (data$totalmean)*51 + amrini_secs + insitu_viz_init_secs
data$complete_err <- (data$totalerr*51) + amrini_err + insitu_viz_init_err

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
plot <- plot + xlab("Client Ranks") + ylab("Time (min)")

if (with_title == TRUE) {
   plot <- plot + ggtitle("In Situ (Baseline): Total Execution Time for 500 Cycles")
}

if (write_pdf == TRUE){
   pdf(file="in-situ-unopt-line.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot


##########  Bar Plot of In-Situ Unopt   #####################

data <- processExtractedTimings(insitu_unopt_dirs)

names <- data$cores

d1 <- amrini_secs
d2 <- insitu_viz_init_secs
d3 <- (data$cthmean)*51
d4 <- (data$vizmean)*51

counts <- cbind.data.frame(d1, d2, d3, d4)/60

if (write_pdf == TRUE){
   pdf(file="in-situ-unopt-bar.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}

barplot(t(counts),
        names=names,
        las=2,
        col=pvspy_colors[1:4],
        ylim=c(0,200),
        xlab="",
        ylab="Time (min)")

if (with_title == TRUE) {
   title(main="In-Situ Total Time (500 cycles)")
}

text(2.4, 140, "33k blocks")
text(7.5, 140, "219k blocks")
text(12.8, 140, "1.5m blocks")

abline(v=4.9,col="gray25",lty=3)
abline(v=9.7,col="gray25",lty=3)

labels = c("CTH Init", "Viz Init", "CTH", "Viz")
legend("topleft", ncol=2, labels, bg="white", inset=.01, fill=pvspy_colors[1:4], cex=0.8)



# Just the large dataset (FIX WITH HPCTOOLKIT DATA)

names <- data$cores[data$levels == 7]

d1 <- insitu_hpc_data$amrini_mean[data$levels == 7]/1e6
d2 <- insitu_hpc_data$viz_init_mean[data$levels == 7]/ 1e6
d3 <- data$cthmean[data$levels==7]*51
d4 <- data$vizmean[data$levels==7]*51

counts <- cbind.data.frame(d1, d2, d3, d4)/60

if (write_pdf == TRUE) {
   pdf(file="in-situ-unopt-large.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}

barplot(t(counts),
        names=names,
        las=2,
        col=pvspy_colors[1:4],
        ylim=c(0,200),
        xlab="",
        ylab="Time (min)")

if (with_title == TRUE) {
   title(main="In-Situ Total Time\n(Baseline, 500 cycles, 1.5m Blocks)")
}

labels = c("CTH Init", "Viz Init", "CTH", "Viz")
legend("topright", ncol=2, labels, bg="white", inset=.01, fill=pvspy_colors[1:4], cex=0.8)


##########  Line Plot of In-Situ opt   #####################

# timings in ms
data <- processHPCClient(directories=insitu_opt_dirs)


data$levels[data$levels == 5] <- "33k blocks"
data$levels[data$levels == 6] <- "220k blocks"
data$levels[data$levels == 7] <- "1.5m blocks"

names(data)[1] <- "Dataset"

plot <- ggplot(data, aes(x=cores, y=total_mean/(60e6), ymax=200, ymin=0, colour=Dataset, linetype=Dataset, shape=Dataset))

plot <- plot + geom_line(size=pvspy_line_size)
plot <- plot + geom_point(size=pvspy_point_size, fill="white")
plot <- plot + geom_errorbar(aes(ymin=(total_mean-total_err)/60e6, ymax=(total_mean+total_err)/60e6), width=pvspy_error_width, linetype=1)
plot <- plot + scale_x_continuous(breaks=2^(2:16), trans='log2')
plot <- plot + scale_shape_manual(values=pvspy_shapes)
plot <- plot + scale_color_manual(values=pvspy_colors)
plot <- plot + scale_linetype_manual(values=pvspy_lines)
plot <- plot + theme_bw()
plot <- plot + theme(legend.position=c(0.15, 0.83),legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))
plot <- plot + xlab("Client Ranks") + ylab("Time (min)")

if (with_title == TRUE) {
   plot <- plot + ggtitle("In Situ (Refined): Total Execution Time for 500 Cycles")
}

if (write_pdf == TRUE) {
   pdf(file="in-situ-opt-line.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot



##########  Bar Plot of In-Situ Opt   #####################

data <- processHPCClient(insitu_opt_dirs)

names <- data$cores

d1 <- data$amrini_mean/1e6
d2 <- data$viz_init_mean/1e6
d3 <- data$cth_mean/1e6
d4 <- data$viz_mean/1e6

counts <- cbind.data.frame(d1, d2, d3, d4)/60

if (write_pdf == TRUE) {
   pdf(file="in-situ-opt-bar.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}

barplot(t(counts),
        names=names,
        las=2,
        col=pvspy_colors[1:4],
        ylim=c(0,200),
        xlab="",
        ylab="Time (min)")

if (with_title == TRUE) {
   title(main="In-Situ Total Time (Refined, 500 cycles)")
}

text(2.4, 140, "33k blocks")
text(7.5, 140, "219k blocks")
text(12.8, 140, "1.5m blocks")

abline(v=4.9,col="gray25",lty=3)
abline(v=9.7,col="gray25",lty=3)

labels = c("CTH Init", "Viz Init", "CTH", "Viz")
legend("topleft", ncol=2, labels, bg="white", inset=.01, fill=pvspy_colors[1:4], cex=0.8)



# Just the large dataset

names <- data$cores[data$levels == 7]

d1 <- data$amrini_mean[data$levels == 7]/1e6
d2 <- data$viz_init_mean[data$levels == 7]/ 1e6
d3 <- data$cth_mean[data$levels==7]/1e6
d4 <- data$viz_mean[data$levels==7]/1e6

counts <- cbind.data.frame(d1, d2, d3, d4)/60

if (write_pdf == TRUE) {
   pdf(file="in-situ-opt-large.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}

barplot(t(counts),
        names=names,
        las=2,
        col=pvspy_colors[1:4],
        ylim=c(0,200),
        xlab="",
        ylab="Time (min)")

if (with_title == TRUE) {
   title(main="In-Situ Total Time\n(Refined, 500 cycles, 1.5m Blocks)")
}

labels = c("CTH Init", "Viz Init", "CTH", "Viz")
legend("topright", ncol=2, labels, bg="white", inset=.01, fill=pvspy_colors[1:4], cex=0.8)


##########  Line Plot of In-Transit (extra)   #####################

# timings in ms
data <- processHPCClient(directories=intransit_extra_dirs)


data$levels[data$levels == 5] <- "33k blocks (2 extra nodes)"
data$levels[data$levels == 6] <- "220k blocks (16 extra nodes)"
data$levels[data$levels == 7] <- "1.5m blocks (128 extra nodes)"

names(data)[1] <- "Dataset"

plot <- ggplot(data, aes(x=cores, y=total_mean/(60e6), ymax=200, ymin=0, colour=Dataset, linetype=Dataset, shape=Dataset))

plot <- plot + geom_line(size=pvspy_line_size)
plot <- plot + geom_point(size=pvspy_point_size, fill="white")
plot <- plot + geom_errorbar(aes(ymin=(total_mean-total_err)/60e6, ymax=(total_mean+total_err)/60e6), width=pvspy_error_width, linetype=1)
plot <- plot + scale_x_continuous(breaks=2^(2:16), trans='log2')
plot <- plot + scale_shape_manual(values=pvspy_shapes)
plot <- plot + scale_color_manual(values=pvspy_colors)
plot <- plot + scale_linetype_manual(values=pvspy_lines)
plot <- plot + theme_bw()
plot <- plot + theme(legend.position=c(0.3,0.83), legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))
plot <- plot + xlab("Client Ranks") + ylab("Time (min)")

if (with_title == TRUE) {
   plot <- plot + ggtitle("In Transit (128 extra nodes): Total Execution Time for 500 Cycles")
}

if (write_pdf == TRUE) {
   pdf(file="in-transit-extra-line.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot



##########  Bar Plot of In-Transit (128 extra nodes)   #####################

# Use client timings to get in-transit data
data <- processClientTimings(intransit_extra_dirs)
data <- processHPCClient(intransit_extra_dirs)

names <- data$cores

d1 <- data$amrini_mean/1e6
d2 <- data$viz_init_mean/1e6
d3 <- data$cth_mean/1e6
# nothing for viz
d5 <- data$viz_sync_md_mean/1e6
d6 <- data$viz_sync_data_mean/1e6
d7 <- data$viz_wait_mean/1e6

counts <- cbind.data.frame(d1, d3, d6, d7)/60

if (write_pdf == TRUE) {
   pdf(file="in-transit-extra-bar.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}

barplot(t(counts),
        names=names,
        las=2,
        col=pvspy_colors[c(1,3, 6,7)],
        ylim=c(0,200),
        xlab="",
        ylab="Time (min)")

if (with_title) {
   title(main="In-Transit Total Time \n(128 extra nodes, 500 cycles)")
}


text(2.4, 140, "33k blocks")
text(7.5, 140, "219k blocks")
text(12.8, 140, "1.5m blocks")

abline(v=4.9,col="gray25",lty=3)
abline(v=9.7,col="gray25",lty=3)

labels = c("CTH Init", "CTH", "Xfer Data", "Wait")
legend("topleft", ncol=2, labels, bg="white", inset=.01, fill=pvspy_colors[c(1,3, 6,7)], cex=0.8)


# Just the large dataset

names <- data$cores[data$levels == 7]


d1 <- data$amrini_mean[data$levels == 7]/1e6
d2 <- data$viz_init_mean[data$levels == 7]/ 1e6
d3 <- data$cth_mean[data$levels == 7]/1e6
# nothing for viz
d5 <- data$viz_sync_md_mean[data$levels == 7]/1e6
d6 <- data$viz_sync_data_mean[data$levels == 7]/1e6
d7 <- data$viz_wait_mean[data$levels == 7]/1e6

counts <- cbind.data.frame(d1, d3, d6, d7)/60

if (write_pdf == TRUE) {
   pdf(file="in-transit-extra-large.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}

barplot(t(counts),
        names=names,
        las=2,
        col=pvspy_colors[c(1,3,6,7)],
        ylim=c(0,200),
        xlab="",
        ylab="Time (min)")

if (with_title) {
   title(main="In-Transit Total Time\n(128 extra nodes, 500 cycles, 1.5m Blocks)")
}

labels = c("CTH Init", "CTH", "Xfer", "Wait")
legend("topright", ncol=2, labels, bg="white", inset=.01, fill=pvspy_colors[c(1,3,6,7)], cex=0.8)



##########  Line Plot of In-Transit Inclusive   #####################

data <- processExtractedTimings(intransit_inclusive_dirs)

data$complete <- (data$totalmean)*51 + amrini_secs + insitu_viz_init_secs
data$complete_err <- (data$totalerr*51) + amrini_err + insitu_viz_init_err

data$levels[data$levels == 5] <- "33k blocks (2 internal nodes)"
data$levels[data$levels == 6] <- "220k blocks (16 internal nodes)"
data$levels[data$levels == 7] <- "1.5m blocks (100 internal nodes)"

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
#plot <- plot + theme(legend.position=c(0.1, 0.9),legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))
plot <- plot + theme(legend.position=c(0.3,0.83), legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))
plot <- plot + xlab("Client Ranks") + ylab("Time (min)")

if (with_title == TRUE) {
   plot <- plot + ggtitle("In Transit: Total Execution Time for 500 Cycles")
}

if (write_pdf == TRUE) {
   pdf(file="in-transit-inclusive-line.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot


##########  Bar Plot of In-Transit Inclusive Opt   #####################

# Use client timings to get in-transit data
intransit_inclusive_data <- processClientTimings(intransit_inclusive_dirs)

names <- intransit_inclusive_data$cores

d1 <- amrini_secs
d2 <- intransit_viz_init_secs
d3 <- (intransit_inclusive_data$cthmean)*51
# nothing for d4 (viz)
d5 <- (intransit_inclusive_data$syncmdmean)*51
d6 <- (intransit_inclusive_data$syncdmean)*51
d7 <- (intransit_inclusive_data$waitmean)*51

counts <- cbind.data.frame(d1, d3, d6, d7)/60

if (write_pdf == TRUE) {
   pdf(file="in-transit-inclusive-bar.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}

barplot(t(counts),
        names=names,
        las=2,
        col=pvspy_colors[c(1,3,6,7)],
        ylim=c(0,200),
        xlab="",
        ylab="Time (min)")

if (with_title == TRUE) {
   title(main="In-Transit Total Time\n(100 internal nodes, 500 cycles)")
}

text(2.4, 140, "33k blocks")
text(7.5, 140, "219k blocks")
text(12.8, 140, "1.5m blocks")

abline(v=4.9,col="gray25",lty=3)
abline(v=9.7,col="gray25",lty=3)

labels = c("CTH Init", "CTH", "Xfer", "Wait")
legend("topleft", ncol=2, labels, bg="white", inset=.01, fill=pvspy_colors[c(1,3,6,7)], cex=0.8)

# Just the large dataset

names <- intransit_inclusive_data$cores[intransit_inclusive_data$levels == 7]

d1 <- insitu_hpc_data$amrini_mean[insitu_hpc_data$levels == 7]/1e6
d2 <- intransit_hpc_data$viz_init_mean[intransit_hpc_data$levels == 7]/ 1e6
d3 <- (intransit_inclusive_data$cthmean[intransit_inclusive_data$levels==7])*51
# nothing for d4 (viz)
d5 <- (intransit_inclusive_data$syncmdmean[intransit_inclusive_data$levels==7])*51
d6 <- (intransit_inclusive_data$syncdmean[intransit_inclusive_data$levels==7])*51
d7 <- (intransit_inclusive_data$waitmean[intransit_inclusive_data$levels==7])*51

counts <- cbind.data.frame(d1, d3, d6, d7)/60

if (write_pdf == TRUE) {
   pdf(file="in-transit-inclusive-large.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}

barplot(t(counts),
        names=names,
        las=2,
        col=pvspy_colors[c(1,3,6,7)],
        ylim=c(0,200),
        xlab="",
        ylab="Time (min)")

if (with_title == TRUE) {
   title(main="In-Transit Total Time\n(100 internal nodes, 500 cycles, 1.5m Blocks)")
}

labels = c("CTH Init", "CTH", "Xfer", "Wait")
legend("topright", ncol=2, labels, bg="white", inset=.01, fill=pvspy_colors[c(1,3,6,7)], cex=0.8)




##########  Line Plot of Spyplot file    #####################

# timings in ms
data <- processHPCClient(directories=spyplot_file_dirs)

## ------ MANUALLY ADD POST-PROCESSING TIME

# 183.21 secs for the 33k blocks
data$total_mean[data$levels == 5] <- data$total_mean[data$levels == 5] + 183.21 * 1e6
# 646.24 secs for 219k blocks
data$total_mean[data$levels == 6] <- data$total_mean[data$levels == 6] + 646.24 * 1e6
# 2576.43 secs for 1.5m blocks
data$total_mean[data$levels == 7] <- data$total_mean[data$levels == 7] + 2576.43 * 1e6

## ------


data$levels[data$levels == 5] <- "33k blocks"
data$levels[data$levels == 6] <- "220k blocks"
data$levels[data$levels == 7] <- "1.5m blocks"


names(data)[1] <- "Dataset"

plot <- ggplot(data, aes(x=cores, y=total_mean/(60e6), ymax=200, ymin=0, colour=Dataset, linetype=Dataset, shape=Dataset))

plot <- plot + geom_line(size=pvspy_line_size)
plot <- plot + geom_point(size=pvspy_point_size, fill="white")
plot <- plot + geom_errorbar(aes(ymin=(total_mean-total_err)/60e6, ymax=(total_mean+total_err)/60e6), width=pvspy_error_width, linetype=1)
plot <- plot + scale_x_continuous(breaks=2^(2:16), trans='log2')
plot <- plot + scale_shape_manual(values=pvspy_shapes)
plot <- plot + scale_color_manual(values=pvspy_colors)
plot <- plot + scale_linetype_manual(values=pvspy_lines)
plot <- plot + theme_bw()
plot <- plot + theme(legend.position=c(0.15, 0.83),legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))
plot <- plot + xlab("Client Ranks") + ylab("Time (min)")

if (with_title == TRUE) {
   plot <- plot + ggtitle("Disk-based post processing: Total Execution Time for 500 Cycles")
}

if (write_pdf == TRUE) {
   pdf(file="spyplot-file-line.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot




##########  Bar Plot of Spyplot-file    #####################

# Use client timings to get in-transit data
data <- processHPCClient(directories = spyplot_file_dirs)

names <- data$cores

## ------ MANUALLY ADD POST-PROCESSING TIME

# Timings from Red Sky
# 33k blocks, 3.59 sec avg, 183.21 sec total
# 219k blocks, 12.66 sec avg, 646.24 sec total
# 1.5m blocks, 51.51 sec avg, 2576.43 sec total.

# Timings from Cielo
# 33k, 3.28s avg, 194s  total
# 219k, 10.55s avg, 555s total
# 1.5m, 64.63s avg, 3232s total 

# 183.21 secs for the 33k blocks
data$pp_mean[data$levels == 5] <- 194 * 1e6
# 646.24 secs for 219k blocks
data$pp_mean[data$levels == 6] <- 555 * 1e6
# 2576.43 secs for 1.5m blocks
data$pp_mean[data$levels == 7] <- 3232 * 1e6

## ------

d1 <- (data$amrini_mean)/1e6  # seconds
# nothing for d2
d3 <- (data$cth_mean)/1e6
# nothing for d4 (viz)
# nothing for d5 (syncmd)
# nothing for d6 (syncd)
# nothing for d7 (wait)
d8 <- (data$spy_file_out_mean)/1e6
d9 <- (data$pp_mean)/1e6

counts <- cbind.data.frame(d1, d3, d8, d9)/60

if (write_pdf == TRUE) {
   pdf(file="spyplot-file-bar.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}

barplot(t(counts),
        names=names,
        las=2,
        col=pvspy_colors[c(1,3,8,4)],
        ylim=c(0,200),
        xlab="",
        ylab="Time (min)")

if (with_title == TRUE) {
   title(main="Disk-based post processing Total Time\n(500 cycles)")
}


text(2.4, 130, "33k blocks")
text(7.5, 130, "219k blocks")
text(12.8, 130, "1.5m blocks")

abline(v=4.9,col="gray25",lty=3)
abline(v=9.7,col="gray25",lty=3)

labels = c("CTH Init", "CTH", "I/O", "Viz")
legend("topleft", ncol=2, labels, bg="white", inset=.01, fill=pvspy_colors[c(1,3,8,4)], cex=0.8)

# Just the large dataset

if (write_pdf == TRUE) {
   pdf(file="spyplot-file-large.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}

names <- data$cores[data$levels == 7]

d1 <- data$amrini_mean[data$levels == 7]/1e6
# nothing for d2 (viz init)
d3 <- data$cth_mean[data$levels==7]/1e6
# nothing for d4 (viz)
# nothing for d5 (syncmd)
# nothing for d6 (syncd)
# nothing for d7 (wait)
d8 <- data$spy_file_out_mean[data$levels==7]/1e6
d9 <- (data$pp_mean[data$levels==7])/1e6


counts <- cbind.data.frame(d1, d3, d8, d9)/60

barplot(t(counts),
        names=names,
        las=2,
        col=pvspy_colors[c(1,3,8,4)],
        ylim=c(0,200),
        xlab="",
        ylab="Time (min)")

if (with_title == TRUE) {
   title(main="Spyplot File Total Time\n(500 cycles, 1.5m Blocks)")
}

labels = c("CTH Init", "CTH", "I/O", "Viz")
legend("topright", ncol=2, labels, bg="white", inset=.01, fill=pvspy_colors[c(1,3,8,4)], cex=0.8)



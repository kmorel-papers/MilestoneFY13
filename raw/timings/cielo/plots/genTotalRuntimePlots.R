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

# insitu_opt_dirs, insitu_unopt_dirs, intransit_dirs, intransit_inclusive_dirs
source("pvspyDirs.R")

pvspy_plot_width <- 7
pvspy_plot_height <- 5

with_title <- FALSE
write_pdf <- TRUE

#################################################
# Extract Data From Result Files (only the results up to 32K)
#################################################



# Use HPCToolkit data to get initialization timings
insitu_hpc_data <- processHPCClient(directories=insitu_opt_dirs)
intransit_hpc_data <- processHPCClient(directories = intransit_dirs)
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

# Use client timings to get in-transit data
#intransit_data <- processClientTimings(intransit_dirs)
#intransit_inclusive_data <- processClientTimings(intransit_inclusive_dirs)

# Use extracted timings for in-situ (when we have HPCTOOLKIT data for all data, we will not do this)
insitu_opt_data <- processExtractedTimings(insitu_opt_dirs)
insitu_unopt_data <- processExtractedTimings(insitu_unopt_dirs)
intransit_data <- processExtractedTimings(intransit_dirs)
intransit_inclusive_data <- processExtractedTimings(intransit_inclusive_dirs)

amr_file_data <- processExtractedTimings(spyplot_file_dirs)
amr_file_data <- processHPCClient(spyplot_file_dirs)

# Use extracted timings for in-situ


##########   PLOTS COMPARING ALL THE DATA #####################

# Set PDF size


# Convert data to similar format (complete timings, not average over 10 cycles, times in minutes)
d1 <- insitu_unopt_data
d1$complete <- (d1$totalmean)*51 + amrini_secs + insitu_viz_init_secs
d1$complete_err <- (d1$totalerr*51) + amrini_err + insitu_viz_init_err

d2 <- insitu_opt_data
d2$complete <- (d2$totalmean)*51 + amrini_secs + insitu_viz_init_secs
d2$complete_err <- (d2$totalerr*51) + amrini_err + insitu_viz_init_err

d3 <- intransit_data
d3$complete <- (d3$totalmean)*51 + amrini_secs + intransit_viz_init_secs
d3$complete_err <- (d3$totalerr*51) + amrini_err + intransit_viz_init_err

d4 <- intransit_inclusive_data
d4$complete <- (d4$totalmean)*51 + amrini_secs + intransit_viz_init_secs
d4$complete_err <- (d4$totalerr*51) + amrini_err + intransit_viz_init_err

#d5 <- amr_file_data
#d5$complete <- (d5$totalmean)*51 + amrini_secs + intransit_viz_init_secs
#d5$complete_err <- (d5$totalerr*51) + amrini_err + intransit_viz_init_err


d5 <- amr_file_data
d5$complete <- d5$total_mean/1e6
d5$complete_err <- d5$total_err/1e6

## ------ MANUALLY ADD POST-PROCESSING TIME

# 183.21 secs for the 33k blocks
d5$complete[d5$levels == 5] <- d5$complete[d5$levels == 5] + 183.21
# 646.24 secs for 219k blocks
d5$complete[d5$levels == 6] <- d5$complete[d5$levels == 6] + 646.24
# 2576.43 secs for 1.5m blocks
d5$complete[d5$levels == 7] <- d5$complete[d5$levels == 7] +  2576.43

## ------

# take only the large data set
d1 <- d1[d1$levels == "7",]
d2 <- d2[d2$levels == "7",]
d3 <- d3[d3$levels == "7",]
d4 <- d4[d4$levels == "7",]
d5 <- d5[d5$levels == "7",]

# use only the cores, complete, and complete_err cols
d1 <- cbind.data.frame(cores=d1$cores, complete=d1$complete, complete_err=d1$complete_err, levels=d1$levels)
d2 <- cbind.data.frame(cores=d2$cores, complete=d2$complete, complete_err=d2$complete_err, levels=d2$levels)
d3 <- cbind.data.frame(cores=d3$cores, complete=d3$complete, complete_err=d3$complete_err, levels=d3$levels)
d4 <- cbind.data.frame(cores=d4$cores, complete=d4$complete, complete_err=d4$complete_err, levels=d4$levels)
d5 <- cbind.data.frame(cores=d5$cores, complete=d5$complete, complete_err=d5$complete_err, levels=d5$levels)

# change group names
d1$levels <- "In Situ (baseline)"
d2$levels <- "In Situ (refined)"
d3$levels <- "In Transit (128 extra nodes)"
d4$levels <- "In Transit (100 internal nodes)"
d5$levels <- "Spyplot File"


data <- rbind.data.frame(d1, d2, d3, d4, d5)
names(data)[4] <- "Experiments"


# Compare untuned experiments
plot <- ggplot(data, aes(x=cores, y=complete/60, ymax=200, ymin=0)) +
	geom_line(size=pvspy_line_size, aes(linetype=Experiments, colour=Experiments))  +
	geom_errorbar(aes(ymin=(complete-complete_err)/60, ymax=(complete+complete_err)/60, colour=Experiments), width=pvspy_error_width) +
	geom_point(size=pvspy_point_size, aes(shape=Experiments, colour=Experiments), fill="white") +
	scale_x_continuous(breaks=2^(2:16), trans='log2') +
	scale_color_manual(values=pvspy_colors[1:5]) +
	scale_shape_manual(values=pvspy_shapes[1:5]) +
	scale_linetype_manual(values=pvspy_lines[1:5]) +
	xlab("Client Ranks") + ylab("Time (min)") +
	theme_bw() +
	theme(legend.position=c(0.8, 0.8),legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))

if (with_title==TRUE) {
   plot <- plot + ggtitle("Total Execution Time for 500 Cycles (1.5m blocks)")
}

if (write_pdf == TRUE) {
   pdf(file="total-runtime-all.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot

##########   PLOT WITHOUT Optimized  #####################

data <- rbind.data.frame(d1, d3, d4, d5)
names(data)[4] <- "Experiments"



# Compare untuned experiments
plot <- ggplot(data, aes(x=cores, y=complete/60, ymax=200, ymin=0)) +
	geom_line(size=pvspy_line_size, aes(linetype=Experiments, colour=Experiments))  +
	geom_errorbar(aes(ymin=(complete-complete_err)/60, ymax=(complete+complete_err)/60, colour=Experiments), width=pvspy_error_width) +
	geom_point(size=pvspy_point_size, fill="white", aes(shape=Experiments, colour=Experiments)) +
	scale_x_continuous(breaks=2^(2:16), trans='log2') +
	scale_color_manual(values=pvspy_colors[c(1,3,4,5)]) +
	scale_shape_manual(values=pvspy_shapes[c(1,3,4,5)]) +
	scale_linetype_manual(values=pvspy_lines[c(1,3,4,5)]) +
	xlab("Client Ranks") + ylab("Time (min)") +
	theme_bw() +
	theme(legend.position=c(0.8, 0.8),legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))

if (with_title == TRUE) {
   plot <- plot + ggtitle("Total Execution Time for 500 Cycles (1.5m blocks)")
}

if (write_pdf == TRUE) {
   pdf(file="total-runtime-noopt.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
plot

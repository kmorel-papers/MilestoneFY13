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


#################################################
# Extract Data From Result Files (only the results up to 32K)
#################################################


# paths to hpctoolkit experiments
insitu_opt_dirs <- list.files(path="../amr_in-situ-hpctoolkit", full.names=TRUE, pattern="pipe-.*", recursive=FALSE)[1:12]

intransit_dirs <- list.files(path="../amr_in-transit-hpctoolkit", full.names=TRUE, pattern="pipe-.*", recursive=FALSE)[1:12]
amr_file_dirs <- list.files(path="../amr-file-hpctoolkit", full.names=TRUE, pattern="pipe-.*", recursive=FALSE)[1:12]


# I will soon have hpctoolkit dirs for these as well
intransit_inclusive_dirs <- list.files(path="../amr_in-transit-inclusive", full.names=TRUE, pattern="pipe-.*", recursive=FALSE)[1:12]
insitu_unopt_dirs <- list.files(path="../amr_in-situ-unopt", full.names=TRUE, pattern="pipe-.*", recursive=FALSE)[1:12]



# Use HPCToolkit data to get initialization timings
insitu_hpc_data <- processHPCClient(directories=insitu_opt_dirs)
intransit_hpc_data <- processHPCClient(directories = intransit_dirs)
amr_file_hpc_data <- processHPCClient(directories = amr_file_dirs)

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
amr_file_data <- processExtractedTimings(amr_file_dirs)

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

d5 <- amr_file_data
d5$complete <- (d5$totalmean)*51 + amrini_secs + intransit_viz_init_secs
d5$complete_err <- (d5$totalerr*51) + amrini_err + intransit_viz_init_err

# take only the large data set
d1 <- d1[d1$levels == "7",]
d2 <- d2[d2$levels == "7",]
d3 <- d3[d3$levels == "7",]
d4 <- d4[d4$levels == "7",]
d5 <- d5[d5$levels == "7",]

# change group names
d1$levels <- "In-Situ (original)"
d2$levels <- "In-Situ (optimized)"
d3$levels <- "In-Transit (128 extra nodes)"
d4$levels <- "In-Transit (100 internal nodes)"
d5$levels <- "File"


data <- rbind.data.frame(d1, d2, d3, d4, d5)
names(data)[1] <- "Experiments"

pdf(file="total-runtime-all.pdf", height=pvspy_plot_height, width=pvspy_plot_width)

# Compare untuned experiments
ggplot(data, aes(x=cores, y=complete/60, ymin=0)) +
		geom_line(size=1, aes(linetype=Experiments, colour=Experiments))  +
		geom_errorbar(aes(ymin=(complete-complete_err)/60, ymax=(complete+complete_err)/60, colour=Experiments), width=.05) +
		geom_point(size=4.0, aes(shape=Experiments, colour=Experiments), fill="white") +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_manual(values=pvspy_colors[1:5]) +
		scale_shape_manual(values=pvspy_shapes[1:5]) +
		scale_linetype_manual(values=pvspy_lines[1:5]) +
		ggtitle("Total Execution Time for 500 Cycles (1.5m blocks)") +
		xlab("Client Ranks") + ylab("Time (min)") + 
                theme_bw() + 
                theme(legend.position=c(0.8, 0.8),legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))

dev.off()


##########   PLOT WITHOUT Optimized  #####################

data <- rbind.data.frame(d1, d3, d4, d5)
names(data)[1] <- "Experiments"


pdf(file="total-runtime-noopt.pdf", height=pvspy_plot_height, width=pvspy_plot_width)

# Compare untuned experiments
ggplot(data, aes(x=cores, y=complete/60, ymin=0)) +
		geom_line(size=1, aes(linetype=Experiments, colour=Experiments))  +
		geom_errorbar(aes(ymin=(complete-complete_err)/60, ymax=(complete+complete_err)/60, colour=Experiments), width=.05) +
		geom_point(size=4.0, fill="white", aes(shape=Experiments, colour=Experiments)) +
		scale_x_continuous(breaks=2^(2:16), trans='log2') +
		scale_color_manual(values=pvspy_colors[c(1,3,4,5)]) +
		scale_shape_manual(values=pvspy_shapes[c(1,3,4,5)]) +
		scale_linetype_manual(values=pvspy_lines[c(1,3,4,5)]) +
		ggtitle("Total Execution Time for 500 Cycles (1.5m blocks)") +
		xlab("Client Ranks") + ylab("Time (min)") + 
                theme_bw() + 
                theme(legend.position=c(0.8, 0.8),legend.key=element_rect(color="white"),legend.background=element_rect(color="black"))

dev.off()



##########  Bar Plot of In-Situ Unopt   #####################

insitu_unopt_data <- processExtractedTimings(insitu_unopt_dirs)

names <- insitu_unopt_data$cores

d1 <- amrini_secs 
d2 <- insitu_viz_init_secs
d3 <- (insitu_unopt_data$cthmean)*51 
d4 <- (insitu_unopt_data$vizmean)*51 

counts <- cbind.data.frame(d1, d2, d3, d4)/60

pdf(file="in-situ-unopt.pdf", height=pvspy_plot_height, width=pvspy_plot_width)

barplot(t(counts), 
        names=names, 
        las=2,
        #legend.text=c("CTH Init", "Viz Init", "CTH", "Viz"),
        #args.legend = list(x=2, y=210, inset=.05),
        #args.legend = list(x='top', horiz=TRUE, bg="white"),
        col=pvspy_colors[1:4],
        ylim=c(0,200),
        xlab="",
        ylab="Time (min)",
        main="In-Situ Total Time (500 cycles)")

text(2.4, 140, "33k blocks")
text(7.5, 140, "219k blocks")
text(12.8, 140, "1.5m blocks")

abline(v=4.9,col="gray25",lty=3)
abline(v=9.7,col="gray25",lty=3)

labels = c("CTH Init", "Viz Init", "CTH", "Viz")
legend("topleft", ncol=2, labels, bg="white", inset=.01, fill=pvspy_colors[1:4])

dev.off()


# Just the large dataset

names <- insitu_unopt_data$cores[intransit_inclusive_data$levels == 7]

d1 <- insitu_hpc_data$amrini_mean[insitu_hpc_data$levels == 7]/1e6
d2 <- insitu_hpc_data$viz_init_mean[insitu_hpc_data$levels == 7]/ 1e6
d3 <- (insitu_unopt_data$cthmean[intransit_inclusive_data$levels==7])*51 
d4 <- (insitu_unopt_data$vizmean[intransit_inclusive_data$levels==7])*51 

counts <- cbind.data.frame(d1, d2, d3, d4)/60

pdf(file="in-situ-unopt-large.pdf", height=pvspy_plot_height, width=pvspy_plot_width)

barplot(t(counts), 
        names=names, 
        las=2,
        #legend.text=c("CTH Init", "Viz Init", "CTH", "Viz"),
        col=pvspy_colors[1:4],
        ylim=c(0,200),
        xlab="",
        ylab="Time (min)",
        main="In-Situ Total Time\n(Unoptimized, 500 cycles, 1.5m Blocks)")

labels = c("CTH Init", "Viz Init", "CTH", "Viz")
legend("topright", ncol=2, labels, bg="white", inset=.01, fill=pvspy_colors[1:4])

dev.off()

##########  Bar Plot of In-Situ Opt   #####################

insitu_opt_data <- processExtractedTimings(insitu_opt_dirs)

names <- insitu_opt_data$cores

d1 <- amrini_secs 
d2 <- insitu_viz_init_secs
d3 <- (insitu_opt_data$cthmean)*51 
d4 <- (insitu_opt_data$vizmean)*51 

counts <- cbind.data.frame(d1, d2, d3, d4)/60

pdf(file="in-situ-opt.pdf", height=pvspy_plot_height, width=pvspy_plot_width)

barplot(t(counts), 
        names=names, 
        las=2,
        #legend.text=c("CTH Init", "Viz Init", "CTH", "Viz"),
        col=pvspy_colors[1:4],
        ylim=c(0,200),
        xlab="",
        ylab="Time (min)",
        main="In-Situ Total Time (Optimized, 500 cycles)")

text(2.4, 140, "33k blocks")
text(7.5, 140, "219k blocks")
text(12.8, 140, "1.5m blocks")

abline(v=4.9,col="gray25",lty=3)
abline(v=9.7,col="gray25",lty=3)

labels = c("CTH Init", "Viz Init", "CTH", "Viz")
legend("topleft", ncol=2, labels, bg="white", inset=.01, fill=pvspy_colors[1:4])

dev.off()


# Just the large dataset

names <- insitu_opt_data$cores[intransit_inclusive_data$levels == 7]

d1 <- insitu_hpc_data$amrini_mean[insitu_hpc_data$levels == 7]/1e6
d2 <- insitu_hpc_data$viz_init_mean[insitu_hpc_data$levels == 7]/ 1e6
d3 <- (insitu_opt_data$cthmean[intransit_inclusive_data$levels==7])*51 
d4 <- (insitu_opt_data$vizmean[intransit_inclusive_data$levels==7])*51 

counts <- cbind.data.frame(d1, d2, d3, d4)/60

pdf(file="in-situ-opt-large.pdf", height=pvspy_plot_height, width=pvspy_plot_width)

barplot(t(counts), 
        names=names, 
        las=2,
        #legend.text=c("CTH Init", "Viz Init", "CTH", "Viz"),
        col=pvspy_colors[1:4],
        ylim=c(0,200),
        xlab="",
        ylab="Time (min)",
        main="In-Situ Total Time\n(Optimized, 500 cycles, 1.5m Blocks)")

labels = c("CTH Init", "Viz Init", "CTH", "Viz")
legend("topright", ncol=2, labels, bg="white", inset=.01, fill=pvspy_colors[1:4])

dev.off()

##########  Bar Plot of In-Transit (128 extra nodes)   #####################

# Use client timings to get in-transit data
intransit_data <- processClientTimings(intransit_dirs)

names <- intransit_data$cores

d1 <- amrini_secs 
d2 <- intransit_viz_init_secs
d3 <- (intransit_data$cthmean)*51 
# nothing for viz
d5 <- (intransit_data$syncmdmean)*51  # too small to plot
d6 <- (intransit_data$syncdmean)*51 
d7 <- (intransit_data$waitmean)*51 

counts <- cbind.data.frame(d1, d3, d6, d7)/60

pdf(file="in-transit-extra.pdf", height=pvspy_plot_height, width=pvspy_plot_width)

barplot(t(counts), 
        names=names, 
        las=2,
        #legend.text=c("CTH Init", "CTH", "Xfer Data", "Wait"),
        col=pvspy_colors[c(1,3,6,7)],
        ylim=c(0,200),
        xlab="",
        ylab="Time (min)",
        main="In-Transit Total Time \n(128 extra nodes, 500 cycles)")


text(2.4, 140, "33k blocks")
text(7.5, 140, "219k blocks")
text(12.8, 140, "1.5m blocks")

abline(v=4.9,col="gray25",lty=3)
abline(v=9.7,col="gray25",lty=3)

labels = c("CTH Init", "CTH", "Xfer", "Wait")
legend("topleft", ncol=2, labels, bg="white", inset=.01, fill=pvspy_colors[c(1,3,6,7)])

# Just the large dataset

names <- intransit_data$cores[intransit_inclusive_data$levels == 7]


d1 <- insitu_hpc_data$amrini_mean[insitu_hpc_data$levels == 7]/1e6
d2 <- intransit_hpc_data$viz_init_mean[intransit_hpc_data$levels == 7]/ 1e6
d3 <- (intransit_data$cthmean[intransit_data$levels==7])*51 
# nothing for d4 (viz)
d5 <- (intransit_data$syncmdmean[intransit_data$levels==7])*51 
d6 <- (intransit_data$syncdmean[intransit_data$levels==7])*51 
d7 <- (intransit_data$waitmean[intransit_data$levels==7])*51 

counts <- cbind.data.frame(d1, d3, d6, d7)/60

pdf(file="in-transit-extra-large.pdf", height=pvspy_plot_height, width=pvspy_plot_width)

barplot(t(counts), 
        names=names, 
        las=2,
        #legend.text=c("CTH Init", "CTH", "Xfer Data", "Wait"),
        col=pvspy_colors[c(1,3,6,7)],
        ylim=c(0,200),
        xlab="",
        ylab="Time (min)",
        main="In-Transit Total Time\n(128 extra nodes, 500 cycles, 1.5m Blocks)")

labels = c("CTH Init", "CTH", "Xfer", "Wait")
legend("topright", ncol=2, labels, bg="white", inset=.01, fill=pvspy_colors[c(1,3,6,7)])

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

pdf(file="in-transit-inclusive.pdf", height=pvspy_plot_height, width=pvspy_plot_width)

barplot(t(counts), 
        names=names, 
        las=2,
        #legend.text=c("CTH Init", "CTH", "Xfer Data", "Wait"),
        col=pvspy_colors[c(1,3,6,7)],
        ylim=c(0,200),
        xlab="",
        ylab="Time (min)",
        main="In-Transit Total Time\n(100 internal nodes, 500 cycles)")

text(2.4, 140, "33k blocks")
text(7.5, 140, "219k blocks")
text(12.8, 140, "1.5m blocks")

abline(v=4.9,col="gray25",lty=3)
abline(v=9.7,col="gray25",lty=3)

labels = c("CTH Init", "CTH", "Xfer", "Wait")
legend("topleft", ncol=2, labels, bg="white", inset=.01, fill=pvspy_colors[c(1,3,6,7)])

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

pdf(file="in-transit-inclusive-large.pdf", height=pvspy_plot_height, width=pvspy_plot_width)

barplot(t(counts), 
        names=names, 
        las=2,
        #legend.text=c("CTH Init", "CTH", "Xfer Data", "Wait"),
        col=pvspy_colors[c(1,3,6,7)],
        ylim=c(0,200),
        xlab="",
        ylab="Time (min)",
        main="In-Transit Total Time\n(100 internal nodes, 500 cycles, 1.5m Blocks)")

labels = c("CTH Init", "CTH", "Xfer", "Wait")
legend("topright", ncol=2, labels, bg="white", inset=.01, fill=pvspy_colors[c(1,3,6,7)])



##########  Bar Plot of AMR-file    #####################

# Use client timings to get in-transit data
amr_file_hpc_data <- processHPCClient(directories = amr_file_dirs)

names <- amr_file_hpc_data$cores

d1 <- (amr_file_hpc_data$amrini_mean)/1e6  # seconds
d2 <- intransit_viz_init_secs
d3 <- (amr_file_hpc_data$cth_mean)/1e6 
# nothing for d4 (viz)
# nothing for d5 (syncmd)
# nothing for d6 (syncd)
# nothing for d7 (wait)
d8 <- (amr_file_hpc_data$spy_file_out_mean)/1e6

counts <- cbind.data.frame(d1, d3, d8)/60

pdf(file="amr-file.pdf", height=pvspy_plot_height, width=pvspy_plot_width)

barplot(t(counts), 
        names=names, 
        las=2,
        col=pvspy_colors[c(1,3,8)],
        ylim=c(0,200),
        xlab="",
        ylab="Time (min)",
        main="Spyplot File Total Time\n(500 cycles)")
        #args.legend = list(x='topleft'), 
        #legend.text = c("CTH Init", "CTH", "File Write"))



text(2.4, 120, "33k blocks")
text(7.5, 120, "219k blocks")
text(12.8, 120, "1.5m blocks")

abline(v=4.9,col="gray25",lty=3)
abline(v=9.7,col="gray25",lty=3)

labels = c("CTH Init", "CTH", "I/O")
legend("topleft", ncol=1, labels, bg="white", inset=.01, fill=pvspy_colors[c(1,3,8)])

# Just the large dataset

pdf(file="amr-file-large.pdf", height=pvspy_plot_height, width=pvspy_plot_width)

names <- amr_file_hpc_data$cores[amr_file_hpc_data$levels == 7]

d1 <- amr_file_hpc_data$amrini_mean[amr_file_hpc_data$levels == 7]/1e6
# nothing for d2 (viz init)
d3 <- (amr_file_hpc_data$cth_mean[amr_file_hpc_data$levels==7])/1e6
# nothing for d4 (viz)
# nothing for d5 (syncmd)
# nothing for d6 (syncd)
# nothing for d7 (wait)
d8 <- (amr_file_hpc_data$spy_file_out_mean[amr_file_hpc_data$levels==7])/1e6


counts <- cbind.data.frame(d1, d3, d8)/60

barplot(t(counts), 
        names=names, 
        las=2,
        #legend.text=c("CTH Init", "CTH", "File Write"),
        col=pvspy_colors[c(1,3,8)],
        ylim=c(0,200),
        xlab="",
        ylab="Time (min)",
        main="Spyplot File Total Time\n(500 cycles, 1.5m Blocks)")

labels = c("CTH Init", "CTH", "I/O")
legend("topright", ncol=1, labels, bg="white", inset=.01, fill=pvspy_colors[c(1,3,8)])



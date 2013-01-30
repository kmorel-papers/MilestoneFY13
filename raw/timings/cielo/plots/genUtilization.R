
source("pvspyPlotDefaults.R")

data <- read.csv("ccc3-usage.csv")

write_pdf <- TRUE


dates <- data$week[data$user == "Oldfield"]  # should be the same for both

# Total the hours from Oldfield and Fabian
ron_cpu <- data$cpu_hours[data$user == "Oldfield"]
ron_jobs <- data$jobs[data$user == "Oldfield"]
ron_wait <- data$wait[data$user == "Oldfield"]

nathan_cpu <- data$cpu_hours[data$user == "Fabian"]
nathan_jobs <- data$jobs[data$user == "Fabian"]
nathan_wait <- data$wait[data$user == "Fabian"]


# Convert the dates to something readable
year <- unlist(lapply(dates, substr, start=3, stop=4))
month <- unlist(lapply(dates, substr, start=5, stop=6))
day <- unlist(lapply(dates, substr, start=7, stop=8))
dates <- paste(month, day, year, sep="/")


###### CPU Utilization ###############################
colors <- pvspy_colors[c(1,2)]

# Separated by user
counts <- cbind.data.frame(ron_cpu, nathan_cpu)/1000

if (write_pdf == TRUE){ 
   pdf(file="utilization-cpu-individual.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
barplot(t(counts),
	names=dates, 
	las=2, 
	ylim=c(0,2000),
	ylab="CPU-hours (x1000)",
	col=colors)

labels <- c("Oldfield", "Fabian")
legend("topleft", ncol=1, labels, bg="white", inset=.01, fill=colors)

# total 
counts <- (ron_cpu + nathan_cpu)/1000
if (write_pdf == TRUE){ 
   pdf(file="utilization-cpu-total.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}
barplot(t(counts),
	names=dates, 
	las=2, 
	ylim=c(0,2000),
	ylab="CPU-hours (x1000)",
	col=colors)


###### Jobs ###############################
colors <- pvspy_colors[c(3,4)]
counts <- cbind.data.frame(ron_jobs, nathan_jobs)
if (write_pdf == TRUE){ 
   pdf(file="utilization-jobs-individual.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}

barplot(t(counts),
	names=dates, 
	las=2, 
	ylim=c(0,200),
        #beside=TRUE,
	ylab="Jobs",
	col=colors)

labels <- c("Oldfield", "Fabian")
legend("topleft", ncol=1, labels, bg="white", inset=.01, fill=colors)

# Total

colors <- pvspy_colors[c(5,6)]
counts <- (ron_jobs + nathan_jobs)
if (write_pdf == TRUE){ 
   pdf(file="utilization-jobs-total.pdf", height=pvspy_plot_height, width=pvspy_plot_width)
}

barplot(t(counts),
	names=dates, 
	las=2, 
	ylim=c(0,200),
	ylab="Jobs",
	col=colors)




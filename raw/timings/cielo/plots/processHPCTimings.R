library(stringr)
library(ggplot2)


###########################################################
# Construct a data record from the experiment.csv files
#
# Read the input files from each directory.  We assume that
# each directory has a number of results stored in files with
# the name "experiment.csv".
###########################################################
processHPCClient <- function(directories) {

	data <- {}

    # For each directory, gather statistics on each experiment
	for (dir in directories) {

		print(dir)

		files <- list.files(path=dir, pattern="experiment.csv", full.names=TRUE, recursive=TRUE)
		files <- files[grepl(pattern="mpicth", files)]  # Only use the client files

		numfiles <- length(files)

		all.data <- {}

		# Read all the data into a list
		#lines <- do.call("rbind", lapply(files, readLines))

		for (file in files) {

			# Remove the function parameters from the file, this screws up the csv
			lines <- sub(pattern="\\(.*\\)", replacement="", x=readLines(file))

			# Convert to a data.frame
			my.data <- read.csv(textConnection(lines))

			all.data <- rbind.data.frame(all.data, my.data)
		}

		# Initialize functions
		cth <- cbind.data.frame(Function="cth ", sum=0, mean=0, min=0, max=0)
		pvspy_fil <- cbind.data.frame(Function="pvspy_fil ", sum=0, mean=0, min=0, max=0)
		amrini <- cbind.data.frame(Function="amrini_ ", sum=0, mean=0, min=0, max=0)
		pvspy_viz <- cbind.data.frame(Function="pvspy_viz ", sum=0, mean=0, min=0, max=0)
		pvspy_viz_sync_data <- cbind.data.frame(Function="pvspy_viz_sync_data ", sum=0, mean=0, min=0, max=0)
		pvspy_viz_sync_md <- cbind.data.frame(Function="pvspy_viz_sync_metadata ", sum=0, mean=0, min=0, max=0)
		pvspy_viz_wait <- cbind.data.frame(Function="pvspy_viz_wait ", sum=0, mean=0, min=0, max=0)
		spy_file_out <- cbind.data.frame(Function="spy_file_out ", sum=0, mean=0, min=0, max=0)



		# total time
		if (length(grep("cth ", all.data$Function) > 0)) {
			cth <- all.data[grepl("cth ", all.data$Function),]
		}

		# init functions
		if (length(grep("amrini_ ", all.data$Function) > 0)) {
			amrini <- all.data[grepl("amrini_ ", all.data$Function),]
		}

		if (length(grep("pvspy_fil ", all.data$Function) > 0)) {
			pvspy_fil <- all.data[grepl("pvspy_fil ", all.data$Function),]
		}

		# analysis functions
		if (length(grep("pvspy_viz ", all.data$Function) > 0)) {
			pvspy_viz <- all.data[grepl("pvspy_viz ", all.data$Function),]  # in-situ
		}

		if (length(grep("pvspy_viz_sync_data ", all.data$Function) > 0)) {
			pvspy_viz_sync_data <- all.data[grepl("pvspy_viz_sync_data ", all.data$Function),]
		}

		if (length(grep("pvspy_viz_sync_metadata ", all.data$Function) > 0)) {
			pvspy_viz_sync_md <- all.data[grepl("pvspy_viz_sync_metadata ", all.data$Function),]
		}

		if (length(grep("pvspy_viz_wait ", all.data$Function) > 0)) {
			pvspy_viz_wait <- all.data[grepl("pvspy_viz_wait ", all.data$Function),]
		}


		# I/O functions
              
		if (length(grep("spy_file_out ", all.data$Function) > 0)) {
			spy_file_out <- all.data[grepl("spy_file_out ", all.data$Function),]
		}



		# Extract the levels of refinement from the directory name, we assume the
		# name is */pipe-[levels]-*
		tmp <- str_replace(dir, pattern=".*pipe-", replacement="pipe-")
		levels <- as.numeric(unlist(str_split(tmp, "-"))[2])

		if (levels == 3) {
			numblocks <- 1146
		} else if (levels == 4) {
			numblocks <- 5080
		} else if (levels == 5) {
			numblocks <- 33094
		} else if (levels == 6) {
			numblocks <- 218510
		} else if (levels == 7) {
			numblocks <- 1498866
		} else if (levels == 8) {
			numblocks <- 10728933
		}


		# Extract the number of clients from the directory name
		np <- as.numeric(unlist(str_split(tmp, "-"))[3])

		total_mean <- mean(cth$mean)
		total_err <- sd(cth$mean)

		amrini_mean <- mean(amrini$mean)
		amrini_err <- sd(amrini$mean)

		pvspy_fil_mean <- mean(pvspy_fil$mean)
		pvspy_fil_err <- sd(pvspy_fil$mean)

		pvspy_viz_mean <- mean(pvspy_viz$mean)
		pvspy_viz_err <- sd(pvspy_viz$mean)

		pvspy_viz_sync_data_mean <- mean(pvspy_viz_sync_data$mean)
		pvspy_viz_sync_data_err <- sd(pvspy_viz_sync_data$mean)

		pvspy_viz_sync_md_mean <- mean(pvspy_viz_sync_md$mean)
		pvspy_viz_sync_md_err <- sd(pvspy_viz_sync_md$mean)

		pvspy_viz_wait_mean <- mean(pvspy_viz_wait$mean)
		pvspy_viz_wait_err <- sd(pvspy_viz_wait$mean)

		spy_file_out_mean <- mean(spy_file_out$mean)
		spy_file_out_err <- sd(spy_file_out$mean)

		cth_mean <- mean(cth$mean - amrini$mean - pvspy_fil$mean - pvspy_viz$mean)
		cth_err <- sd(cth$mean - amrini$mean - pvspy_fil$mean - pvspy_viz$mean)


		row <- cbind.data.frame(levels=levels, cores=np,
				total_mean=total_mean, total_err=total_err,
				amrini_mean=amrini_mean, amrini_err=amrini_err,
				viz_init_mean=pvspy_fil_mean, viz_init_err=pvspy_fil_err,
				cth_mean=cth_mean, cth_err=cth_err,
				viz_mean=pvspy_viz_mean, viz_err=pvspy_viz_err,
				viz_sync_data_mean=pvspy_viz_sync_data_mean, viz_sync_data_err=pvspy_viz_sync_data_err,
				viz_sync_md_mean=pvspy_viz_sync_md_mean, viz_sync_md_err=pvspy_viz_sync_md_err,
				viz_wait_mean=pvspy_viz_wait_mean, viz_wait_err=pvspy_viz_wait_err,
				spy_file_out_mean=spy_file_out_mean, spy_file_out_err=spy_file_out_err)
		data <- as.data.frame(rbind(data, row))
	}

	return (data)
}



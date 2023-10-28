# Helper functions

library(lubridate)
library(readr)
library(dplyr)

# Small helper functions to make the various steps obvious in the log
if(!exists("LOGFILE")) LOGFILE <- ""
log_info <- function(msg, logfile = LOGFILE) {
    cat(format(Sys.time()), msg, "\n", file = logfile, append = TRUE)
}
log_warning <- function(msg, logfile = LOGFILE) {
    log_info(paste("WARNING:", msg), logfile = logfile)
}
new_section <- function(name, logfile = LOGFILE, root = ROOT) {
    log_info("")
    log_info("===================================================")
    log_info(name)
    list_directories(list(file.path(root, "Raw/"),
                          file.path(root, "L0/"),
                          file.path(root, "L1_normalize/"),
                          file.path(root, "L1/"),
                          file.path(root, "L2")),
                     list_files = TRUE, outfile = logfile)
}


# Copy quarto html output and log result
copy_output <- function(from, to, overwrite = TRUE) {
    if(file.copy(from, to, overwrite = overwrite)) {
        log_info(paste("html output copied to", outfile))
    } else {
        log_warning("Error copying html output")
    }
}


# Read a vector of CSV files with the same column structure, optionally
# removing them as we read, and bind data together. The read error count
# is returned as an attribute of the output
read_csv_group <- function(files, col_types = NULL,
                           remove_input_files = FALSE, quiet = FALSE, ...) {
    errors <- 0

    # Read in all files and bind data frames
    readf <- function(fn) {
        if(!quiet) message("\tReading ", basename(fn))
        x <- try(read_csv(fn, col_types = col_types, ...))
        if(!is.data.frame(x)) {
            errors <- errors + 1
            return(NULL)
        }
        if(remove_input_files) file.remove(fn)
        x
    }
    # Store the number of errors as an attribute of the data and return
    dat <- bind_rows(lapply(files, readf))
    attr(dat, "errors") <- errors
    dat
}

# File data into sub-folders based on what level data it is:
# L1_normalize outputs
#   Folders are site_year_month
#   Filenames are Site_logger_table_year_month
# L1 outputs
#   Folders are site_year
#   Filenames are site_timeperiod_L1
# L2 outputs
#   Folders are site_year
#   Filenames are site_timeperiod_table_L2

# The data (x) should be a data frame with a posixct 'TIMESTAMP' column
# This is used to split the data for sorting into <yyyy>_<mm> folders
# Returns a list of filenames written (names) and number of data lines (values)
write_to_folders <- function(x, root_dir, data_level, site,
                             logger, table, quiet = FALSE) {
    years <- year(x$TIMESTAMP)
    months <- sprintf("%02i", month(x$TIMESTAMP)) # add leading zero if needed

    lines_written <- list()
    for(y in unique(years)) {
        for(m in unique(months)) {

            # Isolate the data to write
            dat <- x[y == years & m == months,]
            stopifnot(nrow(dat) > 0) # this shouldn't happen

            # Construct folder and file names
            start <- min(dat$TIMESTAMP)
            end <- max(dat$TIMESTAMP)
            time_period <- paste(format(start, format = "%Y%m%d"),
                        format(end, format = "%Y%m%d"),
                        sep = "-")
            if(data_level == "L1_normalize") {
                folder <- file.path(root_dir, paste(site, y, m, sep = "_"))
                filename <- paste0(paste(logger, table, y, m, sep = "_"), ".csv")
            } else if(data_level == "L1") {
                folder <- file.path(root_dir, paste(site, y, sep = "_"))
                filename <- paste0(paste(site, time_period, data_level, sep = "_"), ".csv")
            } else if(data_level == "L2") {
                folder <- file.path(root_dir, paste(site, y, sep = "_"))
                filename <- paste0(paste(site, time_period, table, data_level, sep = "_"), ".csv")
            } else {
                stop("Unkown data_level ", data_level)
            }

            # Create folder, if needed
            if(!dir.exists(folder)) {
                if(!quiet) message("Creating ", basename(folder))
                if(!dir.create(folder)) {
                    stop("dir.create returned an error")
                }
            }

            # Write data
            if(!quiet) message("Writing ", nrow(dat), "/", nrow(x), " rows of data to ",
                               basename(folder), "/", filename)

            fn <- file.path(folder, filename)
            if(file.exists(fn)) message("NOTE: overwriting existing file")
            # We were using readr::write_csv for this but it was
            # randomly crashing on GA (Error in `vroom write()`: ! bad value)
            write.csv(dat, fn, row.names = FALSE)
            if(!file.exists(fn)) {
                stop("File ", fn, "was not written")
            }

            lines_written[[fn]] <- nrow(dat)
        } # for m
    } # for y
    invisible(lines_written)
}


# Reset the system by removing all intermediate files in L0, L1_normalize,
# L1, L2, and Logs folders
reset <- function(root = here::here("portage/data")) {
    message("root is ", root)
    items <- list.files(file.path(root, "L0/"), pattern = "*.csv",
                        full.names = TRUE)
    message("Removing ", length(items), " files in L0")
    lapply(items, file.remove)

    items <- list.files(file.path(root, "L1_normalize/"), pattern = "*.csv",
                        full.names = TRUE)
    message("Removing ", length(items), " files in L1_normalize")
    lapply(items, file.remove)

    items <- list.files(file.path(root, "L1/"), recursive = TRUE,
                        include.dirs = FALSE, full.names = TRUE)
    items <- items[basename(items) != "README.md"]
    message("Removing ", length(items), " files in L1")
    lapply(items, file.remove)
    items <- list.files(file.path(root, "L1/"), recursive = TRUE,
                        include.dirs = TRUE, full.names = TRUE)
    items <- items[basename(items) != "README.md"]
    message("Removing ", length(items), " directories in L1")
    lapply(items, file.remove)

    items <- list.files(file.path(root, "L2/"), recursive = TRUE,
                        include.dirs = FALSE, full.names = TRUE)
    items <- items[basename(items) != "README.md"]
    message("Removing ", length(items), " files in L2")
    lapply(items, file.remove)
    items <- list.files(file.path(root, "L2/"), recursive = TRUE,
                        include.dirs = TRUE, full.names = TRUE)
    items <- items[basename(items) != "README.md"]
    message("Removing ", length(items), " directories in L1a")
    lapply(items, file.remove)

    items <- list.files(file.path(root, "Logs/"), pattern = "(txt|html)$",
                        recursive = TRUE,
                        include.dirs = FALSE, full.names = TRUE)
    message("Removing ", length(items), " log files in Logs")
    lapply(items, file.remove)

    message("All done.")
}

# Print a nicely-formatted directory tree and its files
# Usage:
# list_directories(list("portage/Raw/", "portage/L0/", "portage/L1_normalize/",
#                       "portage/L1/", "portage/L2"))
list_directories <- function(dir_list, outfile = "", prefix = "",
                             pattern = NULL, list_files = TRUE) {

    for(d in dir_list) {
        # Print the directory name
        cat(paste0(prefix, "|\n"), file = outfile, append = TRUE)
        cat(paste0(prefix, "|- ", basename(d), "/"), "\n", file = outfile, append = TRUE)

        # As we list items, print a vertical pipe except for the last
        if(d == tail(dir_list, 1)) {
            thisprefix <- ""
        } else {
            thisprefix <- "|"
        }

        # Print files in this directory; track but don't print subdirectories
        files <- list.files(d, full.names = TRUE, pattern = pattern)
        subdirs <- list()
        filecount <- 0
        for(f in files) {
            if(dir.exists(f)) {
                subdirs[[f]] <- f
            } else {
                filecount <- filecount + 1
                if(list_files) cat(paste0(prefix, thisprefix, "\t|-"),
                                   basename(f), "\n",
                                   file = outfile, append = TRUE)
            }
        }
        if(!list_files) cat(paste0(prefix, thisprefix, "\t|- (", filecount, " file",
                                   ifelse(filecount == 1, "", "s"), ")\n"),
                            file = outfile, append = TRUE)

        # Now recurse for any subdirectories
        newprefix <- paste0(prefix, "|\t")
        list_directories(subdirs, outfile, prefix = newprefix, list_files = list_files)
    }
}

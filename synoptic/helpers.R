# Helper functions

library(lubridate)
library(readr)

# Constants used in this file and elsewhere in the system
GIT_COMMIT <- substr(system("git rev-parse HEAD", intern = TRUE), 1, 7)

NA_STRING_L1 <- "NA"
NA_STRING_L2 <- "-9999"

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
    # Warnings are not allowed here, as this usually means a column format
    # problem that we want to fix immediately
    oldwarn <- options()$warn
    options(warn = 2)

    # File-reading function
    readf <- function(fn, quiet, ...) {
        if(!quiet) message("\tReading ", basename(fn))
        x <- read_csv(fn, col_types = col_types, ...)
        if(remove_input_files) file.remove(fn)
        x
    }
    # Read all files, bind data frames, and return
    dat <- do.call("rbind", lapply(files, readf, quiet, ...))
    options(warn = oldwarn)
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

# The data (x) should be a data frame with a POSIXct 'TIMESTAMP' column
# This is used to split the data for sorting into <yyyy>_<mm> folders
# Returns a list of filenames written (names) and number of data lines (values)
write_to_folders <- function(x, root_dir, data_level, site,
                             logger, table, quiet = FALSE) {
    years <- year(x$TIMESTAMP)
    months <- sprintf("%02i", month(x$TIMESTAMP)) # add leading zero if needed

    lines_written <- list()
    for(y in unique(years)) {
        if(is.na(y)) {
            stop(data_level, " invalid year ", y)
        }

        for(m in unique(months)) {
            if(is.na(m)) {
                stop(data_level, " invalid month ", m)
            }

            # Isolate the data to write
            dat <- x[y == years & m == months,]
            if(!nrow(dat)) {
                message("No data for ", y, "_", m, " - skipping")
                next
            }

            # Construct folder and file names
            start <- min(dat$TIMESTAMP)
            end <- max(dat$TIMESTAMP)
            time_period <- paste(format(start, format = "%Y%m%d"),
                        format(end, format = "%Y%m%d"),
                        sep = "-")
            if(data_level == "L1_normalize") {
                folder <- file.path(root_dir, paste(site, y, m, sep = "_"))
                # A given month's data is usually split across two datalogger
                # files; add a short hash to end of filename to ensure we don't
                # overwrite anything that's already there
                short_hash <- substr(digest::digest(dat, algo = "md5"), 1, 4)
                filename <- paste0(paste(logger, table, y, m, short_hash, sep = "_"), ".csv")
                na_string <- NA_STRING_L1
            } else if(data_level == "L1") {
                folder <- file.path(root_dir, paste(site, y, sep = "_"))
                filename <- paste0(paste(site, time_period, data_level, sep = "_"), ".csv")
                na_string <- NA_STRING_L1
            } else if(data_level == "L2") {
                folder <- file.path(root_dir, paste(site, y, sep = "_"))
                filename <- paste0(paste(site, time_period, table, data_level, sep = "_"), ".csv")
                na_string <- NA_STRING_L2
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

            # Before writing, convert timestamp to character to ensure that observations
            # at midnight have seconds written correctly
            if(is.POSIXct(dat$TIMESTAMP)) {
                dat$TIMESTAMP <- format(dat$TIMESTAMP, "%Y-%m-%d %H:%M:%S")
            }

            # Write data
            if(!quiet) message("Writing ", nrow(dat), "/", nrow(x), " rows of data to ",
                               basename(folder), "/", filename)

            fn <- file.path(folder, filename)
            if(file.exists(fn)) message("\tNOTE: overwriting existing file")
            # We were using readr::write_csv for this but it was
            # randomly crashing on GA (Error in `vroom write()`: ! bad value)
            write.csv(dat, fn, row.names = FALSE, na = na_string)
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
reset <- function(root = here::here("synoptic/data_TEST")) {
    message("root is ", root)
    items <- list.files(file.path(root, "L0/"), pattern = "*.csv",
                        full.names = TRUE)
    message("Removing ", length(items), " files in L0")
    lapply(items, file.remove)

    items <- list.files(file.path(root, "L1_normalize/"), recursive = TRUE,
                        pattern = "*.csv",
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
# list_directories(list("synoptic/Raw/", "synoptic/L0/", "synoptic/L1_normalize/",
#                       "synoptic/L1/", "synoptic/L2"))
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

# Helper functions

library(lubridate)
library(readr)

# Constants used in this file and elsewhere in the system
GIT_COMMIT <- substr(system("git rev-parse HEAD", intern = TRUE), 1, 7)

# A date way far in the future, used by valid_entries()
MAX_DATE <- ymd_hms("2999-12-31 11:59:00")

# Data NA (not available) strings to use on writing
NA_STRING_L1 <- ""
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
read_csv_group <- function(files, col_types = NULL, quiet = FALSE, ...) {
    # Warnings are not allowed here, as this usually means a column format
    # problem that we want to fix immediately
    oldwarn <- options()$warn
    options(warn = 2)

    # File-reading function
    readf <- function(fn, quiet, ...) {
        if(!quiet) message("\tReading ", basename(fn))
        x <- read_csv(fn, col_types = col_types, ...)
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
write_to_folders <- function(x, root_dir, data_level, site, plot,
                             logger, table, version = "???",
                             quiet = FALSE, write_plots = TRUE) {
    years <- year(x$TIMESTAMP)
    months <- sprintf("%02i", month(x$TIMESTAMP)) # add leading zero if needed
    vversion <- paste0("v", version)

    lines_written <- list()
    nowyr <- year(Sys.Date())
    nowmo <- month(Sys.Date())
    for(y in unique(years)) {
        if(is.na(y)) {
            stop(data_level, " invalid year ", y)
        }

        for(m in unique(months)) {
            write_this_plot <- FALSE

            # Sanity checks
            if(is.na(m)) {
                stop(data_level, " invalid month ", m)
            }
            if(y > nowyr || (y == nowyr && m > nowmo)) {
                stop("I am being asked to write future data: ",
                     paste(site, logger, table, y, m))
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
                folder <- file.path(root_dir, paste(site, plot, y, m, sep = "_"))
                # A given month's data is usually split across two datalogger
                # files; add a short hash to end of filename to ensure we don't
                # overwrite anything that's already there
                short_hash <- substr(digest::digest(dat, algo = "md5"), 1, 4)
                filename <- paste0(paste(logger, table, y, m, short_hash, sep = "_"), ".csv")
                na_string <- NA_STRING_L1
            } else if(data_level == "L1") {
                folder <- file.path(root_dir, paste(site, y, sep = "_"))
                filename <- paste0(paste(site, plot, time_period, data_level, vversion, sep = "_"), ".csv")
                na_string <- NA_STRING_L1
                write_this_plot <- TRUE
                p <- ggplot(x, aes(TIMESTAMP, Value, group = paste(Instrument_ID, Sensor_ID))) +
                    geom_line() +
                    facet_wrap(~research_name, scales = "free") +
                    ggtitle(filename) +
                    theme(axis.text = element_text(size = 6),
                          strip.text = element_text(size = 8))
            } else if(data_level == "L2") {
                folder <- file.path(root_dir, paste(site, y, sep = "_"))
                filename <- paste0(paste(site, time_period, table, data_level, vversion, sep = "_"), ".csv")
                na_string <- NA_STRING_L2
            } else {
                stop("Unkown data_level ", data_level)
            }

            # Create folder, if needed
            if(!dir.exists(folder)) {
                if(!quiet) message("Creating ", basename(folder))
                if(!dir.create(folder, showWarnings = TRUE)) {
                    stop("dir.create returned error for ", folder)
                }
            }

            # Convert timestamp to character to ensure that observations
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

            # Write basic QA/QC plot
            if(write_plots && write_this_plot) {
                fn_p <- gsub("csv$", "pdf", fn)
                ggsave(fn_p, plot = p, width = 12, height = 8)
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

    remove_files_folders <- function(dir, pat = "(pdf|csv)$", rec = TRUE) {
        items <- list.files(file.path(root, dir), recursive = rec,
                            pattern = pat,
                            full.names = TRUE)
        message("Removing ", length(items), " files in ", dir)
        lapply(items, file.remove)
        items <- list.files(file.path(root, dir), recursive = rec,
                            include.dirs = TRUE, full.names = TRUE)
        # Don't remove READMEs or R files
        items <- items[basename(items) != "README.md"]
        items <- items[grep("\\.R$", items, invert = TRUE)]
        message("Removing ", length(items), " directories in ", dir)
        lapply(items, file.remove)
    }

    remove_files_folders("L0/")
    remove_files_folders("L1_normalize/")
    remove_files_folders("L1/")
    remove_files_folders("L2/")
    remove_files_folders("Logs/", pat = "(txt|html)$")

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


# The design links might not be stable over time; for example, if a tree
# dies, its sensor might get reassigned to a new tree. In this case the
# design_link table will have two entries, one for the old assignment and
# one for the new. We know which one to use by the "valid_through" column,
# which give a end date for a design link--or, most commonly, it will be
# empty (NA) indicating that there is no end date.
#
# So, when the design_link table is merged with a data table, if a reassignment
# has occurred, some data rows will get repeated with the different possible
# design links.
#
# This function uses the object (i.e. group identifier; typically, Logger+
# Table+Loggernet_variable), timestamp, and valid_through timestamps to identify
# which rows to keep (correct design_link assignment) and which to drop.
valid_entries <- function(objects, times, valid_through) {
    # Nothing to do if there are no valid_through entries
    if(all(is.na(valid_through))) return(rep(TRUE, length(objects)))

    # Any NA valid_through entries apply into the far future
    valid_through[is.na(valid_through)] <- MAX_DATE
    past_valid_time <- times > valid_through

    # Create a data frame to aggregate and then merge, below
    x <- data.frame(num = seq_along(objects), obj = objects, time = times, vu = valid_through)
    # Compute the minimum valid_through entry for each object and time that is
    # not past the valid_through point; this is the 'controlling' value
    y <- aggregate(vu ~ obj + time, data = x[!past_valid_time,], FUN = min)
    names(y)[3] <- "controlling"

    # Figure out controlling valid_through for each object/time
    z <- merge(x, y, all.x = TRUE)
    z <- z[order(z$num),] # ensure in correct original order
    # An NA controlling entry means there is none
    valids <- z$vu == z$controlling
    valids[is.na(valids)] <- FALSE

    valids
}

# Test code for valid_entries()
test_valid_entries <- function() {
    # Sample data. We have two objects (sensors) at time points 1:3
    test_data <- data.frame(obj = c(1, 1, 1, 2, 2, 2), time = c(1, 2, 3, 1, 2, 3))
    # Object 2 changes its design link after time 2
    test_dt <- data.frame(obj = c(1,2,2),
                          dl = c("A", "B", "C"),
                          valid_through = c(NA, 2, NA))
    # Merge the 'data' with the 'design link table'
    x <- merge(test_data, test_dt)
    # Call valid_entries. It figures out that all the object 1 entries should be
    # retained, but 1 of 2 entries in each timestep should be dropped for object 2.
    # This is because there are two design_table entries for it (see above); the
    # first ends at time point 2, and the second is indefinite after that.
    valid_entries(x$obj, x$time, x$valid_through)

    # No shifting objects
    ret <- valid_entries(c(1, 1, 1), c(1, 2, 3), c(NA, NA, NA))
    stopifnot(all(ret))
    # One object, shift time is never reached
    ret <- valid_entries(c(1, 1, 1, 1), c(1, 1, 2, 2), c(4, NA, 4, NA))
    stopifnot(ret == c(TRUE, FALSE, TRUE, FALSE))
    # One object, shift time is in the past
    ret <- valid_entries(c(1, 1, 1, 1), c(3, 3, 4, 4), c(2, NA, 2, NA))
    stopifnot(ret == c(FALSE, TRUE, FALSE, TRUE))
    # One object, shifts
    ret <- valid_entries(c(1, 1, 1, 1), c(2, 2, 3, 3), c(2, NA, 2, NA))
    stopifnot(ret == c(TRUE, FALSE, FALSE, TRUE))
    # One objects, shifts twice (valid_throughs at 1 and 2)
    ret <- valid_entries(objects = rep(1, 9),
                         times = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
                         valid_through = c(1, 2, NA, 1, 2, NA, 1, 2, NA))
    stopifnot(ret == c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE))
    # Two objects, only one shifts
    ret <- valid_entries(objects = c(1, 1, 1, 2, 2, 2, 2, 2, 2),
                         times = c(1, 2, 3, 1, 1, 2, 2, 3, 3),
                         valid_through = c(NA, NA, NA, 2, NA, 2, NA, 2, NA))
    stopifnot(ret == c(TRUE, TRUE, TRUE, # obj 1
                       TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)) # obj 2
    # There's a valid_through but no new entry
    ret <- valid_entries(objects = c(1, 1),
                         times = c(1, 2),
                         valid_through = c(1, 1))
    stopifnot(ret == c(TRUE, FALSE))
}
test_valid_entries()

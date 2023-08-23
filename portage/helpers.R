# Helper functions

# TODO: move into compasstools?

library(lubridate)
library(readr)


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
                          file.path(root, "L1a/"),
                          file.path(root, "L1b")),
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
                           remove_input_files = FALSE, quiet = FALSE) {
    errors <- 0

    # Read in all files and bind data frames
    readf <- function(fn) {
        if(!quiet) message("\tReading ", basename(fn))
        x <- try(read_csv(fn, col_types = col_types))
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

# File data into sub-folders based on logger and date
# The data should be a data frame with a 'TIMESTAMP' column
# Sort into <yyyy>_<mm>_<logger> folders, splitting apart as needed
# based on the timestamp
write_to_folders <- function(x, root_dir, data_level, site, logger, table, quiet = FALSE) {
    years <- year(x$TIMESTAMP)
    months <- sprintf("%02i", month(x$TIMESTAMP)) # add leading zero if needed

        for(y in unique(years)) {
            for(m in unique(months)) {

                # Construct folder name (<site>_<year>_<month>) and create if necessary
                folder <- file.path(root_dir, paste(site, y, m, sep = "_"))
                if(!dir.exists(folder)) {
                    # Create folder
                    if(!quiet) message("Creating ", basename(folder))
                    if(!dir.create(folder)) {
                        stop("dir.create returned an error")
                    }
                }

                # Isolate the data to write
                dat <- x[y == years & m == months,]
                stopifnot(nrow(dat) > 0) # this shouldn't happen

                # Construct filename and write the data
                if(data_level == "L1_normalize") {
                    # L1_normalize data: <logger>_<table>_<year>_<month>
                    filename <- paste0(paste(logger, table, y, m, sep = "_"), ".csv")
                } else if(data_level %in% c("L1b", "L1a")) {
                    # L1a and L1b data: <site>_<year>_<month>_<level> (and table if present)
                    if(missing(table)) {
                        filename <- paste0(paste(site, y, m, data_level, sep = "_"), ".csv")
                    } else {
                        filename <- paste0(paste(site, y, m, data_level, table, sep = "_"), ".csv")
                    }
                } else {
                    stop("Unkown data_level ", data_level)
                }

                if(!quiet) message("Writing ", nrow(dat), "/", nrow(x), " rows of data to ",
                                   basename(folder), "/", filename)

                fn <- file.path(folder, filename)
                if(file.exists(fn)) message("NOTE: overwriting existing file")
                readr::write_csv(dat, fn)
            }
    }

}


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

    items <- list.files(file.path(root, "L1a/"), recursive = TRUE,
                        include.dirs = FALSE, full.names = TRUE)
    items <- items[basename(items) != "README.md"]
    message("Removing ", length(items), " files in L1a")
    lapply(items, file.remove)
    items <- list.files(file.path(root, "L1a/"), recursive = TRUE,
                        include.dirs = TRUE, full.names = TRUE)
    items <- items[basename(items) != "README.md"]
    message("Removing ", length(items), " directories in L1a")
    lapply(items, file.remove)

    items <- list.files(file.path(root, "L1b/"), recursive = TRUE,
                        include.dirs = FALSE, full.names = TRUE)
    items <- items[basename(items) != "README.md"]
    message("Removing ", length(items), " files in L1b")
    lapply(items, file.remove)
    items <- list.files(file.path(root, "L1a/"), recursive = TRUE,
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

# Print a directory tree and its files
# Usage:
# list_directories(list("portage/Raw/", "portage/L0/", "portage/L1_normalize/",
#                       "portage/L1a/", "portage/L1b"))
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

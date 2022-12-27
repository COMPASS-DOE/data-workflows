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
new_section <- function(name, logfile = LOGFILE) {
    log_info("")
    log_info("===================================================")
    log_info(name)
    list_directories(list("portage/Raw/", "portage/L0/",
                          "portage/L1_normalize/", "portage/L1a/",
                          "portage/L1b"),
                     list_files = FALSE, outfile = logfile)
}


# File data into sub-folders based on logger and date
# The data should be a data frame with a 'TIMESTAMP' column
# Sort into <yyyy>_<mm>_<logger> folders, splitting apart as needed
# based on the timestamp
write_to_folders <- function(x, root_dir, logger, table, prefix = "", quiet = FALSE) {
    years <- year(x$TIMESTAMP)
    months <- sprintf("%02i", month(x$TIMESTAMP)) # add leading zero if needed

    for(y in unique(years)) {
        for(m in unique(months)) {

            # Construct folder name and create it if necessary
            folder <- file.path(root_dir, paste(y, m, logger, sep = "_"))
            if(!dir.exists(folder)) {
                # Create folder and add a README file
                if(!quiet) message("Creating ", basename(folder))
                if(!dir.create(folder)) {
                    stop("dir.create returned an error")
                }
                cat(paste("#", basename(folder)),
                    "",
                    paste("This folder was automatically created", Sys.time()),
                    sep = "\n",
                    file = file.path(folder, "README.md"))
            }

            # Isolate the data to write
            dat <- x[y == years & m == months,]
            stopifnot(nrow(dat) > 0) # this shouldn't happen

            # Construct filename and write the data
            if(prefix != "") {
                filename <- paste0(paste(prefix, logger, table, y, m, sep = "_"), ".csv")
            } else {
                filename <- paste0(paste(logger, table, y, m, sep = "_"), ".csv")
            }
            if(!quiet) message("Writing ", nrow(dat), "/", nrow(x), " rows of data to ",
                               basename(folder), "/", filename)

            fn <- file.path(folder, filename)
            if(file.exists(fn)) message("NOTE: overwriting existing file")
            readr::write_csv(dat, fn)
        }
    }
}


reset <- function(root = "portage/data") {
    items <- list.files(file.path(root, "L0/"), pattern = "*.csv", full.names = TRUE)
    lapply(items, file.remove)

    items <- list.files(file.path(root, "L1_normalize/"), pattern = "*.csv", full.names = TRUE)
    lapply(items, file.remove)

    items <- list.files(file.path(root, "L1a/"), recursive = TRUE, full.names = TRUE)
    items <- items[items != file.path(root, "L1a//README.md")]
    lapply(items, file.remove)

    items <- list.files(file.path(root, "L1b/"), recursive = TRUE, full.names = TRUE)
    items <- items[items != file.path(root, "L1b//README.md")]
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

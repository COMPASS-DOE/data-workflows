# Helper functions

# TODO: move into compasstools?

library(lubridate)
library(readr)

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

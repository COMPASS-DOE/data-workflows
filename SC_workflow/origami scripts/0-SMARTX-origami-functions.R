## GCREW WARMING SMARTX EXPERIMENT
## Selina Cheng - originated July 26, 2022. Last modified July 26, 2022

# This script writes several functions to reshape raw logger data from the GCReW SMARTX experiment
# The functions are as follows:
# 1) dir_names(mid_path) -- return file paths for important directories
# 2) create_dirs(my_dirs) -- create the actual directories
# 3) file_import(source_dir, raw_dir) -- copy data files from source directory into raw_working
# 4) process(raw_dir, proc_dir) -- writes .dat and .backup files to .csv format
# 5) sorter(proc_dir, sort_dir) -- sorts processed data into appropriate subfolders
# 6) mean_rm(x) -- same thing as mean(x, na.rm = T). Removes NAs before taking the mean.
# 7) norm_files(logger, table) -- returns directories that correspond to the loggers and tables you want to be normalized
# 8) norm_smartx(source_dir, norm_dir, table_type, design_table, plot_names, increment) -- normalizes data tables
# 9) monthly_files(table, year) -- returns files that correspond to the tables and years you want for monthly aggregation
# 10) monthlymanage(source_files, monthly_dir, out_path, logger_role) -- aggregates data into separate monthly files
# 11) bundle(monthly_dir, out_path, logger_role) -- aggregates all monthly data into a yearly data file for a given logger, table, and year
# 12) headercheck(proc_dir, output_dir) -- creates a text file containing all variable names from that table type
# 13) dup_removal(source_dir) -- ensures that a file has no duplicates

## =============================================================================
## Load packages
# Automatically installs packages if not installed and loads them appropriately
if (!require("pacman")) install.packages("pacman")
pacman::p_load(plyr, reshape2, lubridate, data.table, tools, dplyr)

## =============================================================================
# FUNCTION 1: dir_names(path)
# Input:
# path = file path for folder that you want to save intermediary output in

# Output:
# Returns all of the new file paths as a list
# -----------------------------------------------------------------------------
dir_names <- function(mid_path){
  # Create main folders for each step of the process
  raw <- paste0(mid_path, "/0_raw")
  processed <- paste0(mid_path, "/1_processed")
  sorted <- paste0(mid_path, "/2_sorted")
  normal <- paste0(mid_path, "/3_normal")
  monthly <- paste0(mid_path, "/4_monthly_working")

  # Create subfolders for working and archive within each step
  raw_work <- paste0(raw, "/raw_working")
  raw_archive <- paste0(raw, "/raw_archive")

  proc_work <- paste0(processed, "/processed_working")
  proc_archive <- paste0(processed, "/processed_archive")

  sort_work <- paste0(sorted, "/sorted_working")
  sort_archive <- paste0(sorted, "/sorted_archive")

  normal_work <- paste0(normal, "/normal_working")
  normal_archive <- paste0(normal, "/normal_archive")

  # Create list of directories so user can call directories
  my_dirs <- list(raw = raw, raw_work = raw_work, raw_archive = raw_archive,
                  processed = processed, proc_work = proc_work, proc_archive = proc_archive,
                  sorted = sorted, sort_work = sort_work, sort_archive = sort_archive,
                  normal = normal, normal_work = normal_work, normal_archive = normal_archive,
                  monthly = monthly)

  # Return list of dirs
  print("Directory names created as follows: ")
  print(my_dirs)
  return(my_dirs)
}

## =============================================================================
# FUNCTION 2: create_dirs(my_dirs)
# Input:
# my_dirs = list of file paths for directories that you want to create. Made using dir_names() function

# Output:
# Creates a bunch of new directories to save each step of data processing
# -----------------------------------------------------------------------------
create_dirs <- function(my_dirs){
  # For all file paths just named, this loop creates directories at those paths ONLY if they don't exist yet.
  out <- 0
  for(n in 1:length(my_dirs)){
    if(!dir.exists(file.path(my_dirs[n]))){
      dir.create(file.path(my_dirs[n]))
      out <- out + 1
    }
  }
  print(paste(out, "directories have been created."))
}

## =============================================================================
# FUNCTION 3: file_import(source_dir, raw_dir)
# Input:
# source_dir = directory to pull files from
# raw_dir = directory to save raw files to

# Output:
# Copies all files from source directory into raw_working directory
# Does not copy any files that are the wrong file extension or that do not have any data in them
# -----------------------------------------------------------------------------
file_import <- function (source_dir, raw_dir){
  raw_working <- paste0(raw_dir, "/raw_working")
  raw_archive <- paste0(raw_dir, "/raw_archive")
  # List all files from the source directory folder, does NOT recurse into other folders in directory
  files <- list.files(source_dir, pattern = NULL, all.files = FALSE,
                  full.names = TRUE, recursive = FALSE,
                  ignore.case = FALSE)

  # Remove any file paths that are just folders in the directory
  files <- files[!file.info(files)$isdir]

  # Remove files that are not .dat or .backup extensions
  files <- files[file_ext(files) == "dat" | file_ext(files) == "backup"]

  # Create empty vector for the for loop
  keep_files <- vector(mode = "character", length = length(files))

  # Read in data tables so that we can remove any empty data tables
  for (n in 1:length(files)) {
    dat <- read.table(files[n], sep = ",", quote = "\"'", dec = ".", na.strings = "NA",
                      colClasses = "character", nrows = 6, skip = 0, check.names = FALSE, fill=TRUE,
                      strip.white = FALSE, blank.lines.skip = TRUE)

    # Extract table type from data
    type <- dat[1,1]

    # Depending on type of table, remove tables less than a given length
    if(nrow(dat) > 2 & type == "TOACI1"){
      keep_files[n] <- files[n]
    }
    if(nrow(dat) > 4 & type == "TOA5"){
      keep_files[n] <- files[n]
    }
  }

  keep_files <- keep_files[keep_files != ""]

  # Compare list of incoming files with files from raw_archive
  keep_files_short <- strsplit(keep_files, "/")
  keep_files_short <- sapply(keep_files_short, "[[", length(keep_files_short[[1]]))

  # Read in data from raw_archive
  done_files <- list.files(raw_archive, pattern = NULL, all.files = FALSE,
                           full.names = FALSE, recursive = FALSE,
                           ignore.case = FALSE)

  keep_files <- keep_files[keep_files_short %in% done_files == F]

  # Copy all files to raw_working
  file.copy(keep_files, raw_working)
  print(paste0(length(keep_files), " files have been imported."))
}

## ===========================================================================
# FUNCTION 4: process(raw_dir, proc_dir)
# Input:
# raw_dir = directory for raw files (incoming directory)
# proc_dir = directory for processed files (outgoing directory)

# Output:
# Writes TOA5 and TOACI1 .dat and .backup files to .csv format. Also changes headers and renames files.
# After files are processed, they get removed from raw_working and copied into raw_archive
# process() also creates a meta file saved as "[year]_meta.txt", which contains a record of the raw and processed filepaths as well as header names for all files.
# ----------------------------------------------------------------------------
process <- function(raw_dir, proc_dir) {
  raw_working <- paste0(raw_dir, "/raw_working")
  raw_archive <- paste0(raw_dir, "/raw_archive")
  proc_working <- paste0(proc_dir, "/processed_working")

  # Create list of file names in the raw directory
  files <- list.files(raw_working, pattern = NULL, all.files = FALSE,
                full.names = TRUE, recursive = TRUE,
                ignore.case = FALSE)

  if(length(files) == 0){
    stop("No files to process")
  }

  for (n in 1:length(files)) {
    # Read in start of data to determine header and type
    dat_header <- read.table(files[n], sep = ",", quote = "\"",
                             dec = ".", header = FALSE,
                             na.strings = "NA", colClasses = "character", nrows = 6,
                             skip = 0, check.names = FALSE,fill=TRUE,
                             strip.white = FALSE, blank.lines.skip = TRUE)

    # Read in data differently depending on table type
    type <- dat_header[1,1]

    if(type == "TOACI1") dat <- read.table(files[n], sep = ",", quote = "\"'",
                                           dec = ".", na.strings = "NA", colClasses = "character", nrows = -1,
                                           skip = 2, fill=TRUE, strip.white = FALSE, blank.lines.skip = TRUE)

    if(type == "TOA5" ) dat <- read.table(files[n], sep = ",", quote = "\"'",
                                          dec = ".", na.strings = "NA", colClasses = "character", nrows = -1,
                                          skip = 4, fill=TRUE, strip.white = FALSE, blank.lines.skip = TRUE)

    # Get logger info and column names from header
    loggerinfo <- dat_header[1, ]
    datnames <- dat_header[2, 1:ncol(dat)]
    datnames <- c(datnames, "Logger", "Program", "Table")

    # Format data tables depending on type
    if(type == "TOA5") dat <- cbind(dat, loggerinfo[2], loggerinfo[6], loggerinfo[8], row.names = NULL)
    if(type == "TOACI1") dat <- cbind(dat, loggerinfo[2], type, loggerinfo[3], row.names = NULL)

    # Set new column names
    colnames(dat) <- datnames

    # Get date and time for file naming purposes
    date <- substr(dat[nrow(dat), 1], 1, 10)
    time <- substr(dat[nrow(dat), 1], 12, 20)
    time <- chartr(":", "-", time)

    # Create unique file name by pulling data from process file in this order:
    # Table type, date, time, block, number of rows in data frame, type of original file
    # Any file name that could be overwritten would be a duplicate file
    if(type == "TOA5"){
      loggerinfo[1, 8] <- gsub("_", "", loggerinfo[1, 8])
      loggerinfo[1, 2] <- gsub("_", "", loggerinfo[1, 2])
      filename <- paste(tolower(loggerinfo[1,8]), date, time, tolower(loggerinfo[1,2]), nrow(dat), type, ".csv", sep="_")
    }
    if(type =="TOACI1"){
      loggerinfo[1, 3] <- gsub("_", "", loggerinfo[1, 3])
      loggerinfo[1, 2] <- gsub("_", "", loggerinfo[1, 2])
      filename <- paste(tolower(loggerinfo[1,3]), date, time, tolower(loggerinfo[1,2]), nrow(dat), type, ".csv", sep="_")
    }

    # Add the rest of the file path to file name
    filename <- paste(proc_working, "/", filename, sep = "")

    # Create metadata and file path for metadata
    meta_dat <- c(filename, datnames, files[n])
    meta_file <- paste(proc_working, "/", format(Sys.time(), "%Y-%m-%d"), "_meta.txt", sep = "")

    # Save tables including metadata
    write.table(meta_dat, meta_file, append = TRUE, quote = FALSE, sep = ",",
                na = "NA", dec = ".", row.names = FALSE,
                col.names = FALSE, qmethod = c("escape", "double"))

    write.table(dat, filename, append = FALSE, quote = FALSE, sep = ",",
                na = "NA", dec = ".", row.names = FALSE,
                col.names = TRUE, qmethod = c("escape", "double"))

    # Copy files to raw_archive folder
    file.copy(files[n], raw_archive)

    # Remove files from raw_dir
    file.remove(files[n])
  }
  print("Files have been processed.")
}

## ===========================================================================
# FUNCTION 5: sorter(proc_dir, sort_dir)
# Input:
# proc_dir = directory for processed files (incoming directory)
# sort_dir = directory for sorted files (outgoing directory)

# Output:
# Copies all processed data into the appropriate sub-folders in sorted_working and processed_archive
# Removes processed data from processed_working as it's sorted.
# ----------------------------------------------------------------------------
sorter <- function(proc_dir, sort_dir){
  proc_working <- paste0(proc_dir, "/processed_working")
  proc_archive <- paste0(proc_dir, "/processed_archive")
  sort_working <- paste0(sort_dir, "/sorted_working")
  sort_archive <- paste0(sort_dir, "/sorted_archive")

  # Create list of file names from the processed directory.
  # abs_files = absolute file paths, rel_files = relative file paths
  abs_files <- list.files(proc_working, pattern = NULL, all.files = FALSE,
                          full.names = TRUE, recursive = TRUE,
                          ignore.case = FALSE)
  rel_files <- list.files(proc_working, pattern = NULL, all.files = FALSE,
                          full.names = FALSE, recursive = TRUE,
                          ignore.case = FALSE)

  if(length(abs_files) == 0){
    stop("No files to sort")
  }

  # Save file path of metadata
  meta <- abs_files[file_ext(abs_files) == "txt"]

  # Only include files with a .csv extension
  abs_files <- abs_files[file_ext(abs_files) == "csv"]
  rel_files <- rel_files[file_ext(rel_files) == "csv"]

  # Split file names into separate components
  logger_info <- strsplit(rel_files, "_")
  logger_info <- lapply(logger_info, tolower)
  table_type <- vector("list", length(rel_files))
  folder_names <- vector("character", length(rel_files))

  # Extract table type and logger type from file name
  # Create a list of folder names
  for(n in 1:length(rel_files)){
    table_type[[n]][1] <- logger_info[[n]][1]
    table_type[[n]][2] <- logger_info[[n]][4]
    folder_names[n] <- paste(table_type[[n]][2], table_type[[n]][1], sep = "_")
  }

  # Create folders
  for(n in 1:length(folder_names)){
    if(!dir.exists(file.path(paste0(sort_working, "/", folder_names[n])))){
      dir.create(file.path(paste0(sort_working, "/", folder_names[n])))
    }
    if(!dir.exists(file.path(paste0(sort_archive, "/", folder_names[n])))){
      dir.create(file.path(paste0(sort_archive, "/", folder_names[n])))
    }
    # Copy files into corresponding folders in sorted_working
    file.copy(abs_files[n], paste0(sort_working, "/", folder_names[n]))
    # Also copy files to processed_archive once they've been sorted
    file.copy(abs_files[n], proc_archive)

    # Remove files from processed_working
    file.remove(abs_files[n])
  }
  # Copy meta.txt to processed_archive and remove from processed_working
  file.copy(meta, proc_archive)
  file.remove(meta)

  print("Files have been sorted.")
}

## ===========================================================================
# FUNCTION 6: mean_rm(x)
# Input:
# x = vector across which you want to find the mean

# Output:
# Same thing as the mean function, just with the na.rm argument automatically set to TRUE.
# i.e. takes the mean and automatically removes NA values
# ----------------------------------------------------------------------------
mean_rm <- function(x){mean(x, na.rm = T)}

## ===========================================================================
# FUNCTION 7: norm_files(logger, table)
# Input:
# logger = logger(s) that you want to draw data from. Can be single element or character vector
# table = table(s) that you want to draw data from. Can be single element or character vector

# Output:
# Returns directories that correspond to the tables you want, in preparation for normalization
# Using this function ensures that you search for only valid loggers and tables
# ---------------------------------------------------------------------------
norm_files <- function(sort_dir, logger, table) {
  sorted_files <- list.dirs(paste0(sort_dir, "/sorted_working"), full.names = FALSE, recursive = FALSE)
  sorted_files_full <- list.dirs(paste0(sort_dir, "/sorted_working"), full.names = TRUE, recursive = FALSE)

  possible_loggers <- unique(sapply(strsplit(sorted_files, "_"), "[[", 1))
  possible_tables <- unique(sapply(strsplit(sorted_files, "_"), "[[", 2))

  logger <- tolower(logger)
  table <- tolower(table)

  if((min(logger %in% possible_loggers) == 0) & (min(table %in% possible_tables) == 1)){
    print("That logger doesn't exist in the sorted_working folder. Check your input for typos. The possible loggers to choose from are:")
    print(possible_loggers)
    stop("You entered an invalid logger")
  }
  if((min(table %in% possible_tables) == 0) & (min(logger %in% possible_loggers) == 1)){
    print("That table doesn't exist in the sorted_working folder. Check your input for typos. The possible tables to choose from are:")
    print(possible_tables)
    stop("You entered an invalid table")
  }
  if((min(table %in% possible_tables) == 0) & (min(logger %in% possible_loggers) == 0)){
    print("That logger and table don't exist in the sorted_working folder. Check your input for typos. The possible loggers to choose from are:")
    print(possible_loggers)
    print("The possible tables to choose from are:")
    print(possible_tables)
    stop("You entered an invalid logger and table")
  } else {
    dirs_for_norm <- sorted_files_full[grepl(paste(logger, collapse = "|"), sorted_files_full, ignore.case = TRUE) == T
                                       & grepl(paste(table, collapse = "|"), sorted_files_full, ignore.case = TRUE) == T]
  }
  print("The directories you want to normalize are as follows:")
  print(dirs_for_norm)
  return(dirs_for_norm)
}

##  ==================================================================
# FUNCTION 8: norm_smartx(source_dir, norm_dir, table_type, design_table, plot_names, increment)
# The two key files needed for this are:
# 1) design_table: this connects the experimental design to the cr1000 variable name and the scale link for data.
# It has every variable used in datalogger exept raw t_temp data which are store separatetly, and 60 minute data.
# An excel file called R-processing_keys.xlsx has sheets needed to add or change the structure of the data.
# The original source for this is the configuration file and the Sauron Program.
# After the 2012 switchover of to water removal, the program structure became very complex to accommodate the change in experimental design.
# Variables may be coded by original (standard coding or by PPT manipulation coding).
# Caution must be made in properly adding new variables to check code and determine experimental structure of channels.
# 2) plot_names: this contains the experimental design and the treatments in each plot.

# Input:
# source_dir = list of directories in "sorted_working" that you want to read files from. Taken from norm_files function
# norm_dir = directory of normalized files
# design_table = experimental design and link
# plot_names = experimental design
# increment = increment that the data is taken at

# Output:
# Re-Formats raw data files from loggernet processing, and stacks (melts) the data.
# Creates a logger file with all logger/site level data, and creates a plot level file with all plot level data
# Also creates renormalized files for data file
# Creates a normalized data file in a folder called "tabletype_year" in the "normal_working" folder
# Also creates a copy of the normalized data file in the "normal_archive" folder
# ----------------------------------------------------------------------------
norm_gcrew <- function(source_dir, norm_dir, design_table, plot_names, increment){
    # Create path to normal working directory
    norm_working <- file.path(norm_dir, "normal_working")

    # Read in experimental design tables
    design <- fread(design_table, na.strings = "")
    plotname <- fread(plot_names, na.strings = "")

    # Set key for plotname and design tables to speed joining
    merge_key = c("logger","design_link")
    setkeyv(plotname, merge_key)
    setkeyv(design, merge_key)

    # Prepare plotname and design tables for merging (set everything to lowercase and class data.table)
    plotname <- lapply(plotname, tolower)
    plotname <- as.data.table(plotname)
    design <- lapply(design, tolower)
    design <- as.data.table(design)

    # Merge plotname and design to create big table that includes all of the measurements / parameters that apply to each plot
    # SC: I am mildly concerned about the design join here for tables like redox, where plots are split across tables in the logger
    # SC: The design join for redox creates a lot of incorrect variables, but it ends up being okay because we split up site and plot vars
    design <- merge(plotname, design, allow.cartesian = TRUE, by = merge_key)

    # Identify variable types "key", "site", and "plot" level from "var_norm_split" variable in design document
    # Set key variables explicitly here since they're always the same?
    key <- c("rowid", "logger", "time2", "timestamp")

    temp <- design[var_norm_split == "n",]
    site <- unique(as.vector(temp$cr1000_name))

    temp <- design[var_norm_split == "y",]
    plot <- unique(as.vector(temp$cr1000_name))

    # List file paths from source directory. i includes full file path
    i <- list.files(source_dir, pattern = NULL, all.files = FALSE,
                    full.names = TRUE, recursive = TRUE,
                    ignore.case = FALSE)

    # If there are no files to read in the specified directory,
    # tell the user and stop the function from running so it doesn't break down the line.
    if(length(i) == 0){
        stop("No files to normalize in the specified directories.")
    }

    for(n in 1:length(i)){
        # Read in data table
        dt <- fread(i[n])

        # Edit column names for ease of use
        setnames(dt, tolower(names(dt)))
        newnames <- names(dt)
        newnames <- gsub("[(]", "", newnames)
        newnames <- gsub("[)]", "", newnames)
        setnames(dt, newnames)

        # Make all data lowercase
        dt <- lapply(dt, function(x) {gsub("NaN", NA, x, ignore.case = T)})
        dt <- lapply(dt, tolower)
        dt <- as.data.table(dt)

        ## Format time so that minute is rounded to the nearest X-minute increment
        Sys.setenv(TZ = "America/Cancun") ### set for EST all year long
        dt <- subset(dt, grepl("20..-..-.. ..:..:..", dt$timestamp))
        dt$timestamp <- as.POSIXct(dt$timestamp)

        # Create new time variable
        minute_time <- round(minute(dt$timestamp)/increment)*increment
        dt$time2 <- update(dt$timestamp, min = minute_time)

        # Create row id
        dt$rowid <- paste("gcrew", dt$timestamp, dt$logger, sep="_")

        # Separate data into plot level and site level by selecting relevant variables
        plot_name <- c(key, plot)
        site_name <- c(key, site)

        dt_site <- subset(dt, select = names(dt) %in% site_name) # site data for merge
        dt_plot <- subset(dt, select = names(dt) %in% plot_name) # plot level to denormalize

        ### Normalize data
        # Stack block/site data (long form data)
        dt_site2 <- melt(dt_site, id.vars = key, measure.vars = , variable.name = "cr1000_name", na.rm = FALSE)
        dt_site2 <- unique(dt_site2)

        # Stack plot data
        dt_plot2 <- melt(dt_plot, id.vars = key, measure.vars = , variable.name = "cr1000_name", na.rm = FALSE)

        # If there are no plot variables, join just the site data to the design data
        if(("cr1000_name" %in% colnames(dt_plot2)) == F){
            # Merge site level data and design attributes (really just the site name and the type of data)
            merge_key = c("logger", "cr1000_name")
            dt_site2 <- as.data.table(dt_site2)
            setkeyv(dt_site2, merge_key)
            setkeyv(design, merge_key)
            dt_site_merged <- merge(dt_site2, design, allow.cartesian = TRUE, by = merge_key)

            # Select only the relevant columns
            dt_site_merged <- subset(dt_site_merged, select = c("site", "logger", "rowid", "time2", "timestamp",
                                                                "value", "research_name", "cr1000_name", "type"))

            # Get only unique site values
            dt_site_merged <- unique(dt_site_merged)

            # Recast site level data to wide format with each variable as its own column (wide)
            dt_site_wide <- dcast(dt_site_merged, site+logger+rowid+time2+timestamp ~ cr1000_name,
                                  subset = NULL, drop = TRUE, value.var = "value") # note that fill should be allowed to default

            # Normalized dt for output
            dt_normalized <- dt_site_wide
        } else{ # Otherwise, merge plot and site level data
            # Merge plot level values and timestamps with design table
            dt_plot2 <- as.data.table(dt_plot2)
            merge_key = c("logger", "cr1000_name")
            setkeyv(dt_plot2, merge_key)
            setkeyv(design, merge_key)
            dt_plot3 <- merge(dt_plot2, design, allow.cartesian = TRUE, by = merge_key)

            # Select only the variables we want.
            # Include all measurement variables and plot-associated variables
            dt_plot4 <- subset(dt_plot3, select = c("rowid", "time2", "timestamp", "value", "research_name", "cr1000_name", "type",
                                                    colnames(plotname)))
            dt_plot4 <- subset(dt_plot4, select = c(-design_link))

            # Collect numeric data, remove duplicate rows, and set to numeric class
            dt_plot4_num <- dt_plot4[type == "number", ]
            dt_plot4_num <- unique(dt_plot4_num)
            dt_plot4_num$value <- as.double(dt_plot4_num$value)
            setkey(dt_plot4_num, NULL)

            # Filter out variables that are characters
            # But don't include logger as a measurement? It's character type but is a key variable. This can be coded differently....?
            dt_plot4_char <- dt_plot4[type == "character" & research_name != "logger"]
            dt_plot4_char <- unique(dt_plot4_char)

            # Get relevant plot columns
            plot_cols <- colnames(plotname)[colnames(plotname) %in% key == F & colnames(plotname) != "design_link"]

            # Create formula for dcast based on all plot variables and key variables
            f <- as.formula(paste(paste(c(plot_cols, key), collapse = " + "), " ~ research_name"))

            # Recast numeric data to wide format with each variable as its own column. Note that fill should be allowed to default
            dt_plot_num_wide <- dcast(dt_plot4_num, f, fun.aggregate = mean_rm, subset = NULL, drop = TRUE, value.var = "value")

            # If there are character variables, cast character table to wide format and join with numeric data
            if(nrow(dt_plot4_char) > 0){
                dt_plot_char_wide <- dcast(dt_plot4_char, f, fun.aggregate = function(x){unique(x)[1]},
                                           subset = NULL, drop = TRUE, value.var = "value")

                # Combine num and char values
                dt_plot_merge <- merge(dt_plot_num_wide, dt_plot_char_wide,
                                       by = c(plot_cols, key), allow.cartesian = T, all = T)
            }else{ # Otherwise, just use numeric data
                dt_plot_merge <- dt_plot_num_wide
            }

            # Recast site level data to wide format with each variable as its own column
            dt_site_wide <- dcast(dt_site2, rowid+logger+time2+timestamp ~ cr1000_name,
                                  subset = NULL, drop = TRUE, value.var = "value") # note that fill should be allowed to default

            # Merge non normalized variables with data
            dt_plot_merge <- as.data.table(dt_plot_merge)
            dt_site_wide <- as.data.table(dt_site_wide)

            setkeyv(dt_plot_merge, key)
            setkeyv(dt_site_wide, key)

            dt_merged <- merge(dt_site_wide, dt_plot_merge, allow.cartesian = TRUE, by = key)

            # Reorder columns for output
            plot_cols <- colnames(plotname)[colnames(plotname) != "design_link"]
            dt_merged <- subset(dt_merged, select = c(plot_cols, colnames(dt_merged)[(colnames(dt_merged) %in% plot_cols == F)]))

            # Set final data for output
            dt_normalized <- dt_merged
        }

        # Get table name from i
        table_name <- unlist(strsplit(i[n], "/"))
        table_name <- table_name[length(table_name)]

        ### Save table to directory
        path <- paste("norm_", table_name, sep = "")
        out_path <- file.path(norm_working, path)

        # Save table to norm_working
        write.table(dt_normalized, out_path, append = FALSE, quote = FALSE, sep = ",",
                    na = "NA", dec = ".", row.names = FALSE,
                    col.names = TRUE, qmethod = c("escape", "double"))

        # Copy original file to sorted_archive
        sort_dir <- unlist(strsplit(i[n], "/sorted_working/"))
        folder <- unlist(strsplit(sort_dir[2], "/"))[1]
        copy_dir <- paste0(sort_dir[1], "/sorted_archive/", folder)

        file.copy(i[n], copy_dir)
        # Remove original file from sorted_working
        file.remove(i[n])
    }
    print("Files have been normalized.")
}
## ===========================================================================
# FUNCTION 9: monthly_files(table, year)
# Input:
# table = table(s) that you want to draw data from. Can be single element or character vector
# year = year(s) that you want to draw data from. Can be single element or vector.

# Output:
# Returns the files that correspond to the tables you want, in preparation for monthly aggregation
# Using this function ensures that you search for only valid tables and years
# ---------------------------------------------------------------------------
monthly_files <- function(norm_dir, table) {
  norm_files <- list.files(paste0(norm_dir, "/normal_working"), pattern = NULL, all.files = FALSE,
                           full.names = FALSE, recursive = FALSE,
                           ignore.case = FALSE)
  norm_files_full <- list.files(paste0(norm_dir, "/normal_working"), pattern = NULL, all.files = FALSE,
                                full.names = TRUE, recursive = FALSE,
                                ignore.case = FALSE)

  possible_tables <- tolower(unique(sapply(strsplit(norm_files, "_"), "[[", 2)))
  # possible_years <- unique(sapply(strsplit(norm_files, "_"), "[[", 3))
  # possible_years <- substring(possible_years, 1, 4)

  table <- tolower(table)

  if(min(table %in% possible_tables) == 0){
    print("That table doesn't exist in the normal_working folder. Check your input for typos. The possible tables to choose from are:")
    print(possible_tables)
    stop("You entered an invalid table")
  } else {
    dirs_for_month <- norm_files_full[grepl(paste(table, collapse = "|"), norm_files_full) == T]
  }
  # if((min(year %in% possible_years) == 0) & (min(table %in% possible_tables) == 1)){
  #   print("That year doesn't exist in the normal_working folder. Check your input for typos. The possible years to choose from are:")
  #   print(possible_years)
  #   stop("You entered an invalid year")
  # }
  # if((min(table %in% possible_tables) == 0) & (min(year %in% possible_years) == 0)){
  #   print("That table and year don't exist. Check your input for typos. The possible tables to choose from are:")
  #   print(possible_tables)
  #   print("The possible years to choose from are:")
  #   print(possible_years)
  #   stop("You entered an invalid table and year")

  print("The files you want to aggregate by month are as follows:")
  print(dirs_for_month)
  return(dirs_for_month)
}

##  ==================================================================
# FUNCTION 10: monthlymanage(source_files, monthly_dir, out_path, logger_role)
# Input:
# source_files = list of normalized files
# monthly_dir = directory for storage of monthly_working files
# out_path = path for folder where everything is saved for researcher use
# logger_role = document connecting logger or table name with the project it's associated with

# Output:
# Divides data table into separate monthly files. Accounts for duplicates.
# If monthly file already exists, data are appended to the existing file
# ----------------------------------------------------------------------------
monthlymanage <- function(source_files, monthly_dir, out_path, logger_role){
  norm_working <- paste0(unlist(strsplit(source_files[1], "/normal_working/"))[1], "/normal_working")
  norm_archive <- paste0(unlist(strsplit(source_files[1], "/normal_working/"))[1], "/normal_archive")
  logger_role <- read.csv(logger_role)

  if(length(source_files) == 0){
    stop("There are no files to aggregate that match your specified table and year")
  }

  for (n in 1:length(source_files)){
    # Read in data table
    dt <- fread(source_files[n])

    # Format time
    dt$time2 <- as.POSIXct(dt$time2)

    # Create new columns in data set for day of the year, week, minute, hour, year, month
    # dt[ , yday := yday(time)]
    # dt[ , week := week(time)]
    # dt[ , minute := minute(time)]
    # dt[ , hour := hour(time)]
    # dt[ , year := year(time)]
    # dt[ , month := month(time)]

    # Create year, month, and logger variables to create a key data frame
    # Create a different table for every combination of month, year, and logger
    dates <- unique(format(dt$time2, "%Y-%m"))
    dates <- strsplit(dates, "-")
    logger <- unique(tolower(dt$logger))
    y2 <- data.frame(yr = as.integer(sapply(dates, "[[", 1)),
                     mo = as.integer(sapply(dates, "[[", 2)),
                     logger = logger, row.names = NULL)

    # Find output directory
    table <- gsub("_", "", unique(tolower(dt$table)))
    logger <- gsub("_", "", unique(tolower(dt$logger)))
    if(grepl("waterlevel", table, ignore.case = TRUE) == TRUE){
      folder <- "gcrew_waterlevel"
    } else{
      folder <- logger_role$project[which(logger_role$pattern == logger)]
    }

    output_dir <- paste0(out_path, "/", folder, "/monthly")
    if(!dir.exists(output_dir)) dir.create(output_dir)

    for (m in 1:nrow(y2)) {
      # Subset data by month and save each one as a different file
      dtplot <- subset(dt, month(dt$time2) == y2$mo[m] & dt$logger == y2$logger[m] & year(dt$time2) == y2$yr[m])
      path <- paste(gsub("_", "", y2$logger[m]), table, y2$mo[m], y2$yr[m], sep = "_")
      path <- paste(path, "csv", sep = ".")
      path_out <- paste(output_dir, path, sep = "/")
      path_monthly <- paste(monthly_dir, path, sep = "/")

      # If there's already a data table with the same name, bind rows together and overwrite
      if (file.exists(path_out) == TRUE) {
        dt2 <- fread(path_out)
        dt2 <- rbind.fill(dtplot, dt2)
        dt2 <- unique(dt2)
        dt2 <- as.data.table(dt2)

        write.table(dt2, path_out, append = FALSE, quote = FALSE, sep = ",",
                    na = "NA", dec = ".", row.names = FALSE,
                    col.names = TRUE, qmethod = c("escape", "double"))
        write.table(dt2, path_monthly, append = FALSE, quote = FALSE, sep = ",",
                    na = "NA", dec = ".", row.names = FALSE,
                    col.names = TRUE, qmethod = c("escape", "double"))

        # Otherwise, if the data table doesn't exist yet, just save it to the output dir
      } else if (file.exists(path_out) == FALSE) {
        write.table(dtplot, path_out, append = FALSE, quote = FALSE, sep = ",",
                    na = "NA", dec = ".", row.names = FALSE,
                    col.names = TRUE, qmethod = c("escape", "double"))
        write.table(dtplot, path_monthly, append = FALSE, quote = FALSE, sep = ",",
                    na = "NA", dec = ".", row.names = FALSE,
                    col.names = TRUE, qmethod = c("escape", "double"))
      }
    }

    # Copy file to norm_archive
    file.copy(source_files[n], norm_archive)
    # Remove file from norm_working
    file.remove(source_files[n])
  }
}

# ========================================================
# OTHER FUNCTIONS THAT ARE RUN AS NEEDED
# ========================================================
# FUNCTION 11: bundle(monthly_dir, out_path, logger_role)
# Input:
# monthly_dir = directory where monthly files are stored that haven't been aggregated into a yearly file yet
# out_path = path for folder where everything is saved for researcher use
# logger_role = document connecting logger or table name with the project it's associated with

# Output:
# Aggregates all monthly data into one yearly data file
# ----------------------------------------------------------------------------
bundle <- function(monthly_dir, out_path, logger_role){
  logger_role <- read.csv(logger_role)

  # Create list of file paths in source directory
  i <- list.files(monthly_dir, pattern = NULL, all.files = FALSE,
                  full.names = TRUE, recursive = TRUE,
                  ignore.case = FALSE)

  # Obtain relative file paths
  j <- strsplit(i, "/")
  j <- sapply(j, "[[", length(j[[1]]))

  # Separate data based on logger, table type, and year
  info <- strsplit(j, "_")
  logger <- tolower((sapply(info, "[[", 1)))
  table <- tolower((sapply(info, "[[", 2)))
  year <- substr(sapply(info, "[[", 4), 1, 4)

  lt_id <- data.frame(logger = logger,
                      table = table,
                      year = year,
                      id = paste(logger, table, year, sep = "_"))

  lt_id <- unique(lt_id)

  # For each combination of logger, table, and year...
  for(n in 1:nrow(lt_id)){
    # Select files that contain logger name
    group <- i[grepl(lt_id$logger[n], i, ignore.case = TRUE) & grepl(lt_id$table[n], i, ignore.case = TRUE) & grepl(lt_id$year[n], i)]

    # Read in first data table
    dt <- fread(group[1])

    if(length(group) > 1){
      for (m in 2:length(group)){
        # Append all other data tables to the first one
        dt2 <- fread(group[m])
        dt <- rbind.fill(dt, dt2)
        dt <- unique(dt)
      }
    }

    dt <- as.data.table(dt)

    # Create new file path
    if(grepl("waterlevel", lt_id$table[n], ignore.case = TRUE) == TRUE){
      folder <- "gcrew_waterlevel"
    } else{
      folder <- logger_role$project[which(logger_role$pattern == lt_id$logger[n])]
    }

    output_dir <- paste0(out_path, "/", folder, "/yearly")
    if(!dir.exists(output_dir)) dir.create(output_dir)

    newpath <- paste0(output_dir, "/", lt_id$logger[n], "_", lt_id$table[n], "_", lt_id$year[n], ".csv")

    # If file exists already, append it to the existing one and overwrite. If it doesn't exist, save it normally
    if (file.exists(newpath) == TRUE){
      write.table(dt, newpath, append = TRUE, quote = FALSE, sep = ",",
                  na = "NA", dec = ".", row.names = FALSE,
                  col.names = FALSE, qmethod = c("escape", "double"))
    }

    if (file.exists(newpath) == FALSE){
      write.table(dt, newpath, append = FALSE, quote = FALSE, sep = ",",
                  na = "NA", dec = ".", row.names = FALSE,
                  col.names = TRUE, qmethod = c("escape", "double"))
    }
    # Remove old files from the "monthly_working" folder
    file.remove(group)
  }
}

## ===========================================================================
# FUNCTION 12: headercheck(proc_dir, output_dir)
# Input:
# proc_dir = directory for files that you want to read headers from
# output_dir = directory where you want to save header names to

# Output:
# Creates a text file containing all variable names from that table type, saved as "loggernetcol_[logger]_[tabletype].txt"
# ----------------------------------------------------------------------------
headercheck <- function(source_dir, output_dir, logger, table) {
  # Read in file names from the "processed files" directory
  files <- list.files(source_dir, pattern = NULL, all.files = FALSE,
                      full.names = TRUE, recursive = TRUE,
                      ignore.case = FALSE)

  # Only include files with a .csv extension
  files <- files[file_ext(files) == "csv"]

  logger <- tolower(logger)
  table <- tolower(table)

  # Only include files that match the logger and table patterns
  files <- files[grepl(paste(table, collapse = "|"), files) == T
                 & grepl(paste(logger, collapse = "|"), files) == T]

  # Start reading in first data table
  dat <- read.table(files[1], header=TRUE, sep = ",", quote = "\"'",
                    dec = ".", na.strings = "NA", colClasses = "character", nrows = 2,
                    skip = 0, check.names = FALSE, fill=TRUE, strip.white = FALSE, blank.lines.skip = TRUE)

  # Get data names
  names <- names(dat)

  if(length(files) > 1){
  # Read in the rest of the data tables and obtain variable names
    for (n in 2:length(files)) {
      dat2 <- read.table(files[n], header=TRUE, sep = ",", quote = "\"'",
                         dec = ".", na.strings = "NA", colClasses = "character", nrows = 2,
                         skip = 0, check.names = FALSE,fill=TRUE, strip.white = FALSE, blank.lines.skip = TRUE)

      # Create a vector of all the variable names from all the data tables
      names <- c(names, names(dat2))
    }
  }

  # Choose only unique variable names and transpose to wide format
  datnames <- (unique(names))
  datnames <- t(datnames)

  # Create file path for header names
  header_file <- paste0(output_dir, "/loggernetcol_", paste(logger, collapse = "_"), "_", paste(table, collapse = "_"), ".txt")
  # Write list of variable names to new file path
  write.table(datnames, header_file, append = FALSE, quote = FALSE, sep = ",",
              na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))
}

## ==================================================================
# FUNCTION 13: dup_removal(source_dir)
# Input:
# source_dir = directory that you want to take files from and check
#
# Output:
# Returns the same files and overwrites them to really ensure that there are no duplicates
# ----------------------------------------------------------------------------
dup_removal <- function(source_dir){
  # Get absolute file path
  i <-list.files(source_dir, pattern = NULL, all.files = FALSE,
                 full.names = TRUE, recursive = TRUE,
                 ignore.case = FALSE)

  # For each file, read it in, select only the unique rows, and save it back to where it came from
  for (n in 1:length(i)) {
    dt <- fread(i[n])
    dt <- unique(dt)
    write.table(dt, i[n], append = FALSE, quote = FALSE, sep = ",",
                na = "NA", dec = ".", row.names = FALSE,
                col.names = TRUE, qmethod = c("escape", "double"))
  }
}

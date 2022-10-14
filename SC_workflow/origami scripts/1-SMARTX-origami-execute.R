## GCREW WARMING SMARTX EXPERIMENT 
## Selina Cheng - originated July 26, 2022. Last modified July 26, 2022

# This script runs all the functions that were created in "0-SMARTX-origami-functions.R"
# To find the places in this script that the user should edit, "Find" or "Ctrl + F" the phrase "user input".
# These "User input" sections are the only areas where the user should need to edit variables.

## =============================================================================
# Source functions and packages from "0-SMARTX-origami-functions.R"
source("origami scripts/0-SMARTX-origami-functions.R")

## =============================================================================
## USER INPUT SECTION:
# The user should edit the following variables as seen fit

# Set source directory for initial draw of raw data
source_dir <- paste0("C:/Users/", Sys.getenv("USERNAME"), 
                     "/OneDrive - Smithsonian Institution/Documents/GCREW_LOGGERNET_DATA-test")

# File path at which you want to save all intermediary output for this script
mid_path <- paste0("C:/Users/", Sys.getenv("USERNAME"), 
               "/OneDrive - Smithsonian Institution/Documents/SMARTX_processing_test")

# File path where usable data is made available for researchers (monthly/yearly)
out_path <- paste0("C:/Users/", Sys.getenv("USERNAME"), 
                   "/OneDrive - Smithsonian Institution/Documents/GCREW_RESEARCHER_DATA-test")

# File paths for where design documents are saved
# design_table <- "design documents/gcrew schema drafts/design_role_co2log_co2log.csv"
# design_table <- "design documents/gcrew schema drafts/design_role_SMARTX.csv"
# design_table <- "C:/Users/Chengs1/Dropbox (Smithsonian)/Cheng_Projects/gcrew schema drafts/GENX/design_role_genx_ardlog.csv"
design_table <- "design documents/gcrew schema drafts/functional/design_role_waterlevel.csv"
# design_table <- "C:/Users/Chengs1/Dropbox (Smithsonian)/Cheng_Projects/gcrew schema drafts/GENX/design_role_genx_control.csv"
# design_table <- "C:/Users/Chengs1/Dropbox (Smithsonian)/Cheng_Projects/gcrew schema drafts/GENX/design_role_genx_redoxswap.csv"
# design_table <- "C:/Users/Chengs1/Dropbox (Smithsonian)/Cheng_Projects/gcrew schema drafts/GENX/design_role_genx_export.csv"

plot_names <- "design documents/gcrew schema drafts/functional/plot_names_SMARTX.csv"
# plot_names <- "C:/Users/Chengs1/Dropbox (Smithsonian)/Cheng_Projects/gcrew schema drafts/GENX/plot_names_GENX_SC.csv"

logger_role <- "design documents/logger_x_project.csv"

## =============================================================================
# Step 1: Create directory file paths and create the directories
my_dirs <- dir_names(mid_path)
create_dirs(my_dirs)

# Step 2: Copy files from source directory to raw directory
# When running this a second time, file_import should just overwrite files of the same name
file_import(source_dir, my_dirs$raw)

# Step 3: Run process function to convert files to .csv and rename them
process(my_dirs$raw, my_dirs$processed)

# Step 5: Run sorter function, which sorts all files into appropriate subfolders
sorter(my_dirs$processed, my_dirs$sorted)

# Step 6: Normalize data
# ------------------------------------------------------------------------------
# USER INPUT SECTION
# Enter the logger and table that you want to normalize.
# Also enter the increment for how often the data has been collected for that table
# You should only normalize loggers/tables together when they have the same increment. 
# You can enter several loggers or tables as a character vector.
# For example, logger <- c("c3log", "c4log") and table <- c("export", "check")
logger <- c("c4log")
table <- c("waterlevel")
increment <- 15

# Control is only run sometimes if ever
# Increment for control is 1 min
# logger <- c("c3log", "c4log", "gcrewmet", "genx")
# table <- c("waterlevel")
# increment <- 15
# --------------------------------------------------------------------------------
# Step 6a: Obtain directories that will be normalized
dirs_for_norm <- norm_files(my_dirs$sorted, logger, table)

# Step 6b: Normalize the files
# old_norm_smartx(dirs_for_norm, my_dirs$normal, design_table, plot_names, increment)
new_norm_smartx(dirs_for_norm, my_dirs$normal, design_table, plot_names, increment)

# Step 7: Parse data files and save individual data files by month. 
# --------------------------------------------------------------------------------
# USER INPUT SECTION 
# Enter the table that you want to aggregate by month
# You can enter several tables as vectors.
# I added this user input section to make it more manual, but honestly it could just be automated to 
# Aggregate all of the files in the normal_working directory.
# For example, table <- c("export", "check")
table <- c("export")
# ---------------------------------------------------------------------------------
# Step 7a: Obtain directories that will be aggregated by months
dirs_for_month <- monthly_files(my_dirs$normal, table)

# Step 7b: Aggregate the data by month and save to researcher data folder
monthlymanage(dirs_for_month, my_dirs$monthly, out_path, logger_role)
file.copy(out_path, mid_path, recursive = TRUE)

# ====================================================================================
# RUN THESE FUNCTIONS AS NEEDED
# Step 8, bundle: Aggregates monthly data tables together into yearly data by logger
bundle(my_dirs$monthly, out_path, logger_role)
file.copy(out_path, mid_path, recursive = TRUE)

# HEADERCHECK:
# Extracts all variable names from LoggerNet file and saves it in loggernetcol.txt
# USER INPUT SECTION
# Enter the logger and table that you want to extract headers from
# You can enter several loggers or tables as a character vector.
# For example, logger <- c("c3log", "c4log") and table <- c("export", "check")
logger <- c("C3LOG", "c4log")
table <- c("check", "export")
# Enter the directory that you want to take files from
header_dir <- paste0(mid_path, "/1_processed/processed_archive")

headercheck(header_dir, mid_path, logger, table)

# Dup removal
# USER INPUT SECTION
# Set source_dir to whatever you want to read things in from to remove duplicates
source_dir <- out_path
dup_removal(source_dir)


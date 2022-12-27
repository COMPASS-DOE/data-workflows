# Driver script for data workflow
#
# This calls the quarto (*.qmd) files that handle data processing for
# each step (raw data to L0, L0 to L1a, etc).

library(quarto)

# Need to run this script from within portage directory
if(basename(getwd()) != "portage") {
    stop("Working directory needs to be portage/")
}

source("helpers.R")

# Settings ----------------------------------------------------

now_string <- function() format(Sys.time(), "%Y%m%d.%H%M")

ROOT <- "./data_TEST"

RAW <- file.path(ROOT, "Raw/")
RAW_DONE <- file.path(ROOT, "Raw_done/")
L0 <- file.path(ROOT, "L0/")
L1_NORMALIZE <- file.path(ROOT, "L1_normalize/")
L1A <- file.path(ROOT, "L1a/")
L1B <- file.path(ROOT, "L1b/")
LOGS <- file.path(ROOT, "Logs/")

# Main logfile
LOGFILE <- file.path(LOGS, paste0("driver_log_", now_string(), ".txt"))
if(file.exists(LOGFILE)) file.remove(LOGFILE)


# Construct L0 data ---------------------------------------------
# L0 data are raw but in CSV form, and with "Logger" and "Table" columns added

message("Running L0")
new_section("Starting L0")

outfile <- paste0("L0_", now_string(), ".html")
outfile <- file.path(LOGS, outfile)

quarto_render("L0.qmd",
              execute_params = list(raw = RAW,
                                    raw_done = RAW_DONE,
                                    L0 = L0,
                                    html_outfile = outfile,
                                    logfile = LOGFILE))
file.copy("L0.html", outfile, overwrite = TRUE)

# 'Normalize' L0 data -------------------------------------------
# Reshaped to long form and matched with design_link info
# This is an intermediate step, not exposed to data users

message("Running L1_normalize.qmd")
new_section("Starting L1_normalize")

dt <- file.path(ROOT, "design_table.csv")
outfile <- paste0("L1_normalize_", now_string(), ".html")
outfile <- file.path(LOGS, outfile)

quarto_render("L1_normalize.qmd",
              execute_params = list(L0 = L0,
                                    L1_normalize = L1_NORMALIZE,
                                    html_outfile = outfile,
                                    design_table = dt))
file.copy("L1_normalize.html", outfile, overwrite = TRUE)


# Construct L1a data --------------------------------------------
# Unit conversion and bounds checks performed
# L1a data are long form but without any plot (experimental) info

# This step will use a 'units_bounds.csv' file or something like that
# This step also sorts data into folders; see write_to_folders() in helpers.R

message("Running L1a.qmd")
new_section("Starting L1a")

outfile <- paste0("L1a_", now_string(), ".html")
outfile <- file.path(LOGS, outfile)

quarto_render("L1a.qmd",
              execute_params = list(L1_normalize = L1_NORMALIZE,
                                    L1a = L1A,
                                    html_outfile = outfile))
file.copy("L1a.html", outfile, overwrite = TRUE)


# Manual QA/QC step ---------------------------------------------
# A MarineGEO-like Shiny app, allowing technicians to
# flag/annotate observations, would be applied to the output
# from this step


# Summary report data at L1a stage ------------------------------

# Seems like it? So that we move files out of this stage by hand
# (i.e. when month folder is complete)?


# Construct L1b data --------------------------------------------
# L1b data are wide form, matched plot/experimental info, and ready for analysis

# This is tricky because for the first time we need to match info across
# files and dataloggers
# Should data be put into (e.g.) month folders at the end of the previous step?
# This would imply L1a sit in a holding pen (the month folders) until complete?

message("Running L1b.qmd")
new_section("Starting L1b")

pt <- file.path(ROOT, "plot_table.csv")
outfile <- paste0("L1b_", now_string(), ".html")
outfile <- file.path(LOGS, outfile)

quarto_render("L1b.qmd",
              execute_params = list(L1a = L1A,
                                    L1b = L1B,
                                    plot_table = pt,
                                    html_outfile = outfile))
file.copy("L1b.html", outfile, overwrite = TRUE)

message("All done.")

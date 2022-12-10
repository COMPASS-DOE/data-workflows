# Driver script for data workflow
#
# This calls the quarto (*.qmd) files that handle data processing for
# each step (raw data to L0, L0 to L1a, etc).

library(quarto)

source("portage/helpers.R")

# Settings ----------------------------------------------------

now_string <- function() format(Sys.time(), "%Y%m%d.%H%M")

RAW <- "Raw/"
RAW_DONE <- "Raw_done/"
L0 <- "L0/"
L1_NORMALIZE <- "L1_normalize/"
L1A <- "L1a/"
L1B <- "L1b/"
LOGS <- "Logs/"

# Main logfile
LOGFILE <-file.path("portage", LOGS, paste0("driver_log_", now_string(), ".txt"))
if(file.exists(LOGFILE)) file.remove(LOGFILE)

# Small helper function to make the various steps obvious in the log
new_section <- function(name, logfile = LOGFILE) {
    cat("\n===================================================\n",
        now_string(), name, "\n", file = logfile, append = TRUE)
    list_directories(list("portage/Raw/", "portage/L0/",
                          "portage/L1_normalize/", "portage/L1a/",
                          "portage/L1b"), outfile = logfile)
}


# Construct L0 data ---------------------------------------------
# L0 data are raw but in CSV form, and with "Logger" and "Table" columns added

message("Running L0")
new_section("Starting L0")

outfile <- paste0("L0_", now_string(), ".html")
outfile <- file.path("portage", LOGS, outfile)
quarto_render("portage/L0.qmd",
              output_file = outfile,
              execute_params = list(raw = RAW,
                                    raw_done = RAW_DONE,
                                    L0 = L0,
                                    html_outfile = outfile))


# 'Normalize' L0 data -------------------------------------------
# Reshaped to long form and matched with design_link info
# This is an intermediate step, not exposed to data users

message("Running L1_normalize.qmd")
new_section("Starting L1_normalize")

outfile <- paste0("L1_normalize_", now_string(), ".html")
outfile <- file.path("portage", LOGS, outfile)

quarto_render("portage/L1_normalize.qmd",
              output_file = outfile,
              execute_params = list(L0 = L0,
                                    L1_normalize = L1_NORMALIZE,
                                    html_outfile = outfile,
                                    design_table = "design_table.csv"))


# Construct L1a data --------------------------------------------
# Unit conversion and bounds checks performed
# L1a data are long form but without any plot (experimental) info

# This step will use a 'units_bounds.csv' file or something like that
# This step also sorts data into folders; see write_to_folders() in helpers.R

message("Running L1a.qmd")
new_section("Starting L1a")

outfile <- paste0("L1a_", now_string(), ".html")
outfile <- file.path("portage", LOGS, outfile)

quarto_render("portage/L1a.qmd",
              output_file = outfile,
              execute_params = list(L1_normalize = L1_NORMALIZE,
                                    L1a = L1A,
                                    html_outfile = outfile))


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

outfile <- paste0("L1b_", now_string(), ".html")
outfile <- file.path("portage", LOGS, outfile)

quarto_render("portage/L1b.qmd",
              output_file = outfile,
              execute_params = list(L1a = L1A,
                                    L1b = L1B,
                                    plot_table = "plot_table.csv",
                                    html_outfile = outfile))

message("All done.")

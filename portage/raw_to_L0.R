# Example driver script

library(quarto)

RAW <- "Raw/"
RAW_DONE <- file.path(RAW, "done/")
L0 <- "L0/"
L1_NORMALIZE <- "L1_normalize/"

now_string <- function() format(Sys.time(), "%Y%m%d%H%M%S")

# Move files from raw to L0

outfile <- paste0("raw_to_L0_", now_string(), ".html")
outfile_fq <- file.path("portage", L0, outfile)
quarto::quarto_render("portage/raw_to_L0.qmd",
                      output_file = outfile_fq,
                      execute_params = list(raw = RAW,
                                            raw_done = RAW_DONE,
                                            L0 = L0,
                                            html_outfile = outfile))

# Normalize L0 files

outfile <- paste0("L0_to_L1_norm_", now_string(), ".html")
outfile_fq <- file.path("portage", L1_NORMALIZE, outfile)

quarto::quarto_render("portage/L1_normalize.qmd",
                      output_file = outfile_fq,
                      execute_params = list(L0 = L0,
                                            L1_normalize = L1_NORMALIZE,
                                            html_outfile = outfile,
                                            design_table = "design_table_ex.csv"))

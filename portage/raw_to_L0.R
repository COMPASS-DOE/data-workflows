# Example driver script

library(quarto)

RAW <- "Raw/"
L0 <- "L0/"

# Move files from raw to L0
outfile <- paste0("raw_to_L0_", format(Sys.time(), "%Y%m%d%H%M%S"), ".html")
outfile_fq <- file.path("portage", "L0", outfile)
quarto::quarto_render("portage/raw_to_L0.qmd",
                      output_file = outfile_fq,
                      execute_params = list(raw = RAW,
                                            L0 = L0,
                                            outfile = outfile))

# Create a time series of a single variable at a site

site <- "CRC" # change this to e.g. "CRC_W" if you want a particular plot
variable <- "soil_temp_10cm"

# Construct a "regular expression" to find the files we want: in this case,
# CSV files starting with the site code above
pat <- paste0("^", site, ".*csv$")

# Get the names of the files we need. Note this assumes that your
# working directory is the main directory of the L1 data
files <- list.files("./", pattern = pat, recursive = TRUE, full.names = TRUE)

# Helper function to read the files and filter to just the variable we want
# We use readr::read_csv for easy timestamp handling
library(readr)
f <- function(f) {
    message("Reading ", basename(f))
    x <- read_csv(f, col_types = "ccTccccdccii")
    x[x$research_name == variable,]
}

# Read the files
dat <- lapply(files, f)
dat <- do.call("rbind", dat)

# Plot the data
library(ggplot2)
p <- ggplot(dat, aes(TIMESTAMP, Value, color = Sensor_ID)) +
    geom_line() +
    facet_grid(Plot~.) +
    ggtitle(paste(site, variable, paste0("(n=", nrow(dat), ")")))
print(p)

# All done!

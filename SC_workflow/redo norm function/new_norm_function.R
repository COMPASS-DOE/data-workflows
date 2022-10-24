# Rewriting our join function based on BBL's suggestions
# Selina Cheng, last modified 24 October 2022.

# Load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(reshape2, lubridate, data.table, tools, here, plyr, tidyr)

# Wrote a function for mean() with na.rm = TRUE 
mean_rm <- function(x){mean(x, na.rm =T)}

# Load design documents

# Testing CO2_LOG table (plot and site level variables)
# design <- fread(here("redo norm function/TEST_design_co2log.csv"))
# plotname <- fread(here("redo norm function/TEST_plot_co2log.csv"))
# Testing Waterlevel tables (multiple loggers, only site level variables)
design <- fread(here("redo norm function/design_role_waterlevel_TEST.csv"))
plotname <- fread(here("redo norm function/plot_names_SMARTX_TEST.csv"), na.strings = "")
# Specify time increment for data
increment <- 15

# Join design with plot info
design_plot <- merge(plotname, design, allow.cartesian = T, by = "design_link")
# Make all character data lowercase 
design_plot <- lapply(design_plot, tolower)
design_plot <- as.data.table(design_plot)

# Read in LoggerNet data
# dt <- fread(here("redo norm function/co2log_2021-12-01_01-55-00_co2log_4032_TOA5_.csv"))
dt <- fread(here("redo norm function/waterlevel_2020-08-12_01-45-00_c3log_1344_TOA5_.csv"))

# Reformat column names to lowercase
setnames(dt, tolower(names(dt)))
dt <- lapply(dt, tolower)
dt <- as.data.table(dt)

# Reformat timestamps and make sure they're all set to the same interval
Sys.setenv(TZ = "America/Cancun") ### set for EST all year long
dt <- subset(dt, grepl("20..-..-.. ..:..:..", dt$timestamp))
dt$timestamp <- as.POSIXct(dt$timestamp)
# Round timestamps to the correct increment
minute_time <- round(minute(dt$timestamp)/increment)*increment
dt$time2 <- update(dt$timestamp, min = minute_time)

# Add a rowid
dt$rowid <- paste("gcrew", dt$timestamp, dt$logger, sep="_")

# Pivot to longer dt
dt_long <- melt(dt, id.vars = c("timestamp", "time2", "logger", "rowid"), 
                measure.vars = , variable.name = "loggernet_variable", na.rm = F)

# Join with design information
dt_merged <- merge(design_plot[,-c("design_link")],
                   dt_long, by = c("loggernet_variable", "logger"), allow.cartesian = T)

# Drop plot columns that are empty
# This is useful when processing only site level variables, when we don't care about individual plot info
drop <- rep(TRUE, ncol(dt_merged))

for(n in (which(colnames(dt_merged) %in% colnames(plotname)))){
  drop[n] <- (sum(sapply(subset(dt_merged, select = n), is.na)) != nrow(dt_merged))
}

dt_merged <- dt_merged[,..drop]

# Separate into num and character values
dt_merged_num <- dt_merged[type == "number",]
dt_merged_num <- unique(dt_merged_num)
dt_merged_num$value <- as.double(dt_merged_num$value)

# Separate into char
dt_merged_char <- dt_merged[type == "character" & research_name != "logger",]

# Create formula for casting numeric and character data to wide format
f <- as.formula(paste(paste(colnames(dt_merged)[colnames(dt_merged) %in% colnames(plotname)],
                            collapse = " + "), "+ logger + rowid + timestamp + time2 ~ research_name"))

# Aggregate numeric data by mean (if there are duplicates, it will eliminate them)
dt_num_wide <- dcast(dt_merged_num, f, subset = NULL, drop = TRUE, value.var = "value", fun.aggregate = mean_rm)

# Aggregate character by unique (if there are duplicates, it will just choose the first unique value)
# Which should be good enough for now...but this may need to be changed...
dt_char_wide <- dcast(dt_merged_char, f, subset = NULL, drop = TRUE, value.var = "value", 
                      fun.aggregate = function(x){unique(x)[1]})

# Join numeric and character data
dt_norm <- merge(dt_num_wide, dt_char_wide, 
                 by = c(colnames(dt_merged)[colnames(dt_merged) %in% colnames(plotname)], 
                        "logger", "rowid", "time2", "timestamp"))

# TESTING TO COMPARE TABLES ------------------------------------------------ For Selina
# Make sure tables created using new normalization function are same as previous data tables
write.csv(dt_norm, here("redo norm function/test_norm_waterlevel.csv"), row.names=F)

# compare_co2log <- read.csv(here("redo norm function/norm_co2log_2021-12-01_01-55-00_co2log_4032_TOA5_.csv"))
compare_co2log <- read.csv(here("redo norm function/norm_waterlevel_2020-08-12_01-45-00_c3log_1344_TOA5_.csv"))
compare_co2log <- compare_co2log[,order(names(compare_co2log))]
compare_co2log <- compare_co2log[order(compare_co2log$time2),]

# dt_norm_test <- read.csv(here("redo norm function/test_norm_co2log.csv"))
dt_norm_test <- read.csv(here("redo norm function/test_norm_waterlevel.csv"))
dt_norm_test <- dt_norm_test[, order(names(dt_norm_test))]
dt_norm_test <- dt_norm_test[order(dt_norm_test$time2),]

dt_norm_test <- dt_norm_test[, -c(5, 12)]
identical(compare_co2log, dt_norm_test)

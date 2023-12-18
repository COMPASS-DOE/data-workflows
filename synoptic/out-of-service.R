# out-of-service.R

library(readr)
library(lubridate)
library(tidyr)


troll <- read.csv("data_TEST/out-of-service/troll_maintenance.csv")

# ===========================================================
# This code will be in L1_normalize?

# If no time_pulled given, assume 6 AM
tp <- troll$Time_pulled
tp[is.na(tp) | tp == ""] <- "06:00"

# If no time_replaced given, assume 6 PM
tr <- troll$Time_replaced
tr[is.na(tr) | tr == ""] <- "18:00"

# If no date_replaced given, assume ongoing
dr <- troll$Date_replaced
dr[is.na(dr) | dr == ""] <- "12/31/2999"

# Calculate out of service windows
troll$oos_begin <- mdy(troll$Date_pulled, tz = "EST") + hm(tp)
troll$oos_end <- mdy(dr, tz = "EST") + hm(tr)
# Per Peter R., we throw out all data for 24 hours after replacement
troll$oos_end <- troll$oos_end + 60 * 60 * 24
# ===========================================================



# We pass an oos_df data frame to oos()
# This is a table of out-of-service windows
# It MUST have `oos_begin` and `oos_end` timestamps; optionally,
# it can have other columns that must be matched as well
# (e.g., site, plot, etc)

# The second thing we pass is the observation data frame

# This function returns a logical vector that becomes F_OOS

oos <- function(oos_df, data_df) {

    # Make sure that any 'extra' condition columns (in addition to the
    # oos window begin and end) are present in the data d.f.
    non_ts_fields <- setdiff(colnames(oos_df), c("oos_begin", "oos_end"))
    if(!all(non_ts_fields %in% colnames(data_df))) {
        stop("Not all out-of-service condition columns are present in data!")
    }
    oos_final <- rep(FALSE, nrow(data_df))

    for(i in seq_len(nrow(oos_df))) {
        # First check: are any timestamps within the oos window?
        oos <- data_df$TIMESTAMP >= oos_df$oos_begin[i] &
            data_df$TIMESTAMP <= oos_df$oos_end[i]
        if(any(oos)) {
            # There are timestamp matches, so check other (optional)
            # conditions in the oos_df; they must match exactly
            # For example, if there's a "Site" column it must
            for(f in non_ts_fields) {
                oos <- oos & data_df[,f] == oos_df[,f][i]
            }
            # The out-of-service flags for this row of the oos_df table
            # are OR'd with the overall flags that will be returned below
            oos_final <- oos_final | oos
        }
    }
    return(oos_final)
}

# Test code for oos()

data_df <- data.frame(TIMESTAMP = 1:3, x = letters[1:3], y = 4:6)

# No other conditions beyond time window
oos_df <- data.frame(oos_begin = 1, oos_end = 1)
stopifnot(oos(oos_df, data_df) == c(TRUE, FALSE, FALSE))
oos_df <- data.frame(oos_begin = 4, oos_end = 5)
stopifnot(oos(oos_df, data_df) == c(FALSE, FALSE, FALSE))
oos_df <- data.frame(oos_begin = 0, oos_end = 2)
stopifnot(oos(oos_df, data_df) == c(TRUE, TRUE, FALSE))
oos_df <- data.frame(oos_begin = 0, oos_end = 3)
stopifnot(oos(oos_df, data_df) == c(TRUE, TRUE, TRUE))

# x condition - doesn't match even though timestamp does
oos_df <- data.frame(oos_begin = 1, oos_end = 1, x = "b")
stopifnot(oos(oos_df, data_df) == c(FALSE, FALSE, FALSE))
# x condition - matches and timestamp does
oos_df <- data.frame(oos_begin = 1, oos_end = 1, x = "a")
stopifnot(oos(oos_df, data_df) == c(TRUE, FALSE, FALSE))
# x condition - some match, some don't
oos_df <- data.frame(oos_begin = 1, oos_end = 2, x = "b")
stopifnot(oos(oos_df, data_df) == c(FALSE, TRUE, FALSE))
# x and y condition
oos_df <- data.frame(oos_begin = 1, oos_end = 2, x = "b", y = 5)
stopifnot(oos(oos_df, data_df) == c(FALSE, TRUE, FALSE))
oos_df <- data.frame(oos_begin = 1, oos_end = 2, x = "a", y = 5)
stopifnot(oos(oos_df, data_df) == c(FALSE, FALSE, FALSE))

# Error thrown if condition column(s) not present
oos_df <- data.frame(oos_begin = 1, oos_end = 2, z = 1)
out <- try(oos(oos_df, data_df), silent = TRUE)
stopifnot(class(out) == "try-error")

# out-of-service.R

library(readr)
library(lubridate)
library(tidyr)


troll <- read.csv("data_TEST/out-of-service/troll_maintenance.csv")

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



troll_oos <- troll

# oos (out of service)
# Flag observations that match out-of-service entries
# oos_dir: out-of-service file directory

# We'd like L1_normalize to pass oos_dir, timestamps, site, and
# design links, and be ignorant of everything else

# We will hardcode things for AquaTroll, etc.?

# Probably we will read in Aquatroll oos file in L1_normalize,
# do what we need to do to compute begin_oos and end_oos timestamps,
# and then pass in to this function?

oos <- function(data_design_links, data_ts,
                oos_sites, oos_plots, oos_dl_pattern,
                oos_out_ts, oos_in_ts) {

    # The oos_table has
    #   sensor, sites (vector), plots (vector),
    #   which_sensor (optional vector),
    #   oos_begin (vector), oos_end (vector)

    # For trolls, we need site + plot + dates is all
    # Troll o.o.s. knocks out entire group of sensors on it

    # Read troll d.f.
    # Missing times get 12:00
    # Missing Date_replaced get MAX_DATE
    # Date_replaced + 1 day (per)
    #
    # loop through troll d.f.
    timestamps <- ymd_hms(c("2022-03-06 12:34:56", "2022-03-07 12:34:45", "2022-03-08 12:34:45", "2022-03-09 12:34:45"))
    sensor <- "GW"
    sites <- c("OWC", "OWC", "OWC", "OWC")
    plots <- c("W", "W", "W", "W")

    timestamp_dates <- as.Date(timestamps)

    troll_flags <- rep(FALSE, length(timestamps))
    for(i in seq_len(nrow(troll_oos))) {
        troll_flags <- sensor == "GW" &
            troll_flags | sites == oos_table$Site[i] &
            plots == oos_table$Location[i] &
            # per Peter R.: it takes 24 hours after troll replacement
            # for a sensor to be considered 'good'
            timestamp_dates >= oos_table$Date_pulled[i] &
            timestamp_dates <= (oos_table$Date_replaced[1] + 1)
    }


    troll_flags <- sites == troll$Site[i] &
        timestamp_dates >= troll$Date_pulled[i] &
        timestamp_dates <= (troll$Date_replaced[1] + 1)

    # For Aquatroll, return
    #   SITE == site &
    #   sensor == sensor &
    #   As.Date(timestamp) >= troll$Date_pulled &
    #   as.Date(timestamp) <= troll$Date_replaced + 1 day
}




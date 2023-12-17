# out-of-service.R

library(readr)
library(lubridate)

# A date way far in the future, used by oos()
MAX_DATE <- ymd_hms("2999-12-31 11:59:00")


troll <- read_csv("data_TEST/out-of-service/troll_maintenance.csv")

troll$Date_pulled <- mdy(troll$Date_pulled)
troll$Date_replaced <- mdy(troll$Date_replaced)
troll$Date_replaced[is.na(troll$Date_replaced)] <- MAX_DATE

troll_oos <- troll

# oos (out of service)
# Flag observations that match out-of-service entries
# oos_dir: out-of-service file directory

# We'd like L1_normalize to pass oos_dir, timestamps, site, and
# design links, and be ignorant of everything else

# We will hardcode things for AquaTroll, etc.?

oos <- function(timestamps, design_links, oos_table) {

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




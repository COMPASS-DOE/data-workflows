# Quick sapflow checks for Lake Erie Synoptic
# COMPASS | Stephanie Pennington 2022-06-15

# Load packages
pacman::p_load(lubridate, readr, dplyr, tidyr, ggplot2, cowplot)
theme_set(theme_minimal())
dat_cutoff <- "2022-06-06"


# OWC
#upland

sdat <- readLines("~/Desktop/sapflow_current/Compass_OWC_UP_323_SapflowA.dat")
owc_raw_up <- read_csv(I(sdat), skip = 1)

owc_raw_up <- owc_raw_up[-c(1:2), ]
owc_raw_up$TIMESTAMP <- ymd_hms(owc_raw_up$TIMESTAMP)

owc_raw_up %>%
    filter(TIMESTAMP > dat_cutoff) %>%
    pivot_longer(cols = starts_with("DiffVoltA_Avg"),
                 names_to = "Port", values_to = "Value") %>%
    rename(Timestamp = TIMESTAMP,
           Record = RECORD) %>%
    mutate(Port = parse_number(Port),
           Value = as.numeric(Value)) %>%
    ggplot(aes(x = Timestamp, y = Value, color = as.factor(Port))) +
    geom_line() + labs(title = "OWC Upland",
                       subtitle = "Raw data as of: June 7, 2022") -> owc_up_g

#transition
sdat <- readLines("~/Desktop/sapflow_current/Compass_OWC_TR_322_SapflowB.dat")
owc_raw_tr <- read_csv(I(sdat), skip = 1)

owc_raw_tr <- owc_raw_tr[-c(1:2), ]
owc_raw_tr$TIMESTAMP <- ymd_hms(owc_raw_tr$TIMESTAMP)

owc_raw_tr %>%
    filter(TIMESTAMP > dat_cutoff) %>%
    pivot_longer(cols = starts_with("DiffVoltB_Avg"),
                 names_to = "Port", values_to = "Value") %>%
    rename(Timestamp = TIMESTAMP,
           Record = RECORD) %>%
    mutate(Port = parse_number(Port),
           Value = as.numeric(Value)) %>%
    filter(Timestamp > "2022-04-22 13:45:00") %>%
    ggplot(aes(x = Timestamp, y = Value, color = as.factor(Port))) +
    geom_line() + labs(title = "OWC Transition", color = "Port",
                       subtitle = "Raw data as of: June 7, 2022") +
    theme(legend.position = "none") -> owc_tr_g

# CRC
#upland
sdat <- readLines("~/Desktop/sapflow_current/Compass_CRC_UP_303_SapflowA.dat")
crc_raw_up <- read_csv(I(sdat), skip = 1)

crc_raw_up <- crc_raw_up[-c(1:2), ]
crc_raw_up$TIMESTAMP <- ymd_hms(crc_raw_up$TIMESTAMP)

crc_raw_up %>%
    filter(TIMESTAMP > dat_cutoff) %>%
    pivot_longer(cols = starts_with("DiffVoltA_Avg"),
                 names_to = "Port", values_to = "Value") %>%
    rename(Timestamp = TIMESTAMP,
           Record = RECORD) %>%
    mutate(Port = parse_number(Port),
           Value = as.numeric(Value)) %>%
    ggplot(aes(x = Timestamp, y = Value, color = as.factor(Port))) +
    geom_line() + labs(title = "CRC Upland", color = "Port",
                       subtitle = "Raw data as of: June 7, 2022")  -> crc_up_g

#transition
sdat <- readLines("~/Desktop/sapflow_current/Compass_CRC_TR_302_SapflowB.dat")
crc_raw_tr <- read_csv(I(sdat), skip = 1)

crc_raw_tr <- crc_raw_tr[-c(1:2), ]
crc_raw_tr$TIMESTAMP <- ymd_hms(crc_raw_tr$TIMESTAMP)

crc_raw_tr %>%
    filter(TIMESTAMP > dat_cutoff) %>%
    pivot_longer(cols = starts_with("DiffVoltB_Avg"),
                 names_to = "Port", values_to = "Value") %>%
    rename(Timestamp = TIMESTAMP,
           Record = RECORD) %>%
    mutate(Port = parse_number(Port),
           Value = as.numeric(Value)) %>%
    ggplot(aes(x = Timestamp, y = Value, color = as.factor(Port))) +
    geom_line() + labs(title = "CRC Transition", color = "Port",
                       subtitle = "Raw data as of: June 7, 2022") -> crc_tr_g



#PTR

#upland
sdat <- readLines("~/Desktop/sapflow_current/Compass_PTR_UP_313_SapflowA.dat")
ptr_raw_up <- read_csv(I(sdat), skip = 1)

ptr_raw_up <- ptr_raw_up[-c(1:2), ]
ptr_raw_up$TIMESTAMP <- ymd_hms(ptr_raw_up$TIMESTAMP)

ptr_raw_up %>%
    filter(TIMESTAMP > dat_cutoff) %>%
    pivot_longer(cols = starts_with("DiffVoltA_Avg"),
                 names_to = "Port", values_to = "Value") %>%
    rename(Timestamp = TIMESTAMP,
           Record = RECORD) %>%
    mutate(Port = parse_number(Port),
           Value = as.numeric(Value)) %>%
    ggplot(aes(x = Timestamp, y = Value, color = as.factor(Port))) +
    geom_line() + labs(title = "PTR Upland", color = "Port",
                       subtitle = "Raw data as of: June 7, 2022")  +
    theme(legend.position = "bottom") -> ptr_up_g

#transition
sdat <- readLines("~/Desktop/sapflow_current/Compass_PTR_TR_312_SapflowB.dat")
ptr_raw_tr <- read_csv(I(sdat), skip = 1)

ptr_raw_tr <- ptr_raw_tr[-c(1:2), ]
ptr_raw_tr$TIMESTAMP <- ymd_hms(ptr_raw_tr$TIMESTAMP)

ptr_raw_tr %>%
    filter(TIMESTAMP > dat_cutoff) %>%
    pivot_longer(cols = starts_with("DiffVoltB_Avg"),
                 names_to = "Port", values_to = "Value") %>%
    rename(Timestamp = TIMESTAMP,
           Record = RECORD) %>%
    mutate(Port = parse_number(Port),
           Value = as.numeric(Value)) %>%
    ggplot(aes(x = Timestamp, y = Value, color = as.factor(Port))) +
    geom_line() + labs(title = "PTR Transition", color = "Port",
                       subtitle = "Raw data as of: June 7, 2022")  +
    theme(legend.position = "bottom") -> ptr_tr_g


owc_up_g
owc_tr_g
crc_up_g
crc_tr_g
ptr_up_g
ptr_tr_g

cowplot::plot_grid(owc_up_g, owc_tr_g, crc_up_g, crc_tr_g, ptr_up_g, ptr_tr_g, ncol = 2)


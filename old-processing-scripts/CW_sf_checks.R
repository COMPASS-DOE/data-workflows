
pacman::p_load(lubridate, readr, dplyr, tidyr, ggplot2, cowplot, plotly)
theme_set(theme_minimal())
dat_cutoff <- "2022-06-02"


# MSM
#upland

sdat <- readLines("~/Desktop/sapflow_current/Compass_MSM_UP_403_SapflowA.dat")
msm_raw_up <- read_csv(I(sdat), skip = 1)

msm_raw_up <- msm_raw_up[-c(1:2), ]
msm_raw_up$TIMESTAMP <- ymd_hms(msm_raw_up$TIMESTAMP)

msm_raw_up %>%
    filter(TIMESTAMP > dat_cutoff) %>%
    pivot_longer(cols = starts_with("DiffVoltA_Avg"),
                 names_to = "Port", values_to = "Value") %>%
    rename(Timestamp = TIMESTAMP,
           Record = RECORD) %>%
    mutate(Port = parse_number(Port),
           Value = as.numeric(Value)) %>%
    ggplot(aes(x = Timestamp, y = Value, color = as.factor(Port))) +
    geom_line() + labs(title = "MSM Upland",
                       subtitle = "Raw data as of: June 7, 2022") +
    theme(legend.position = "bottom") -> msm_up_g

#transition
sdat <- readLines("~/Desktop/sapflow_current/Compass_MSM_TR_402_SapflowB.dat")
msm_raw_tr <- read_csv(I(sdat), skip = 1)

msm_raw_tr <- msm_raw_tr[-c(1:2), ]
msm_raw_tr$TIMESTAMP <- ymd_hms(msm_raw_tr$TIMESTAMP)

msm_raw_tr %>%
    filter(TIMESTAMP > dat_cutoff) %>%
    pivot_longer(cols = starts_with("DiffVoltB_Avg"),
                 names_to = "Port", values_to = "Value") %>%
    rename(Timestamp = TIMESTAMP,
           Record = RECORD) %>%
    mutate(Port = parse_number(Port),
           Value = as.numeric(Value)) %>%
    ggplot(aes(x = Timestamp, y = Value, color = as.factor(Port))) +
    geom_line() + labs(title = "MSM Transition",
                       subtitle = "Raw data as of: June 7, 2022") +
    theme(legend.position = "bottom") -> msm_tr_g

# GWI
#upland

sdat <- readLines("~/Desktop/sapflow_current/Compass_GWI_UP_413_SapflowA.dat")
gwi_raw_up <- read_csv(I(sdat), skip = 1)

gwi_raw_up <- gwi_raw_up[-c(1:2), ]
gwi_raw_up$TIMESTAMP <- ymd_hms(gwi_raw_up$TIMESTAMP)

gwi_raw_up %>%
    filter(TIMESTAMP > dat_cutoff) %>%
    pivot_longer(cols = starts_with("DiffVoltA_Avg"),
                 names_to = "Port", values_to = "Value") %>%
    rename(Timestamp = TIMESTAMP,
           Record = RECORD) %>%
    mutate(Port = parse_number(Port),
           Value = as.numeric(Value)) %>%
    ggplot(aes(x = Timestamp, y = Value, color = as.factor(Port))) +
    geom_line() + labs(title = "GWI Upland",
                       subtitle = "Raw data as of: June 7, 2022")  -> gwi_up_g

#transition

sdat <- readLines("~/Desktop/sapflow_current/Compass_GWI_TR_412_SapflowB.dat")
gwi_raw_tr <- read_csv(I(sdat), skip = 1)

gwi_raw_tr <- gwi_raw_tr[-c(1:2), ]
gwi_raw_tr$TIMESTAMP <- ymd_hms(gwi_raw_tr$TIMESTAMP)

gwi_raw_tr %>%
    filter(TIMESTAMP > dat_cutoff) %>%
    pivot_longer(cols = starts_with("DiffVoltB_Avg"),
                 names_to = "Port", values_to = "Value") %>%
    rename(Timestamp = TIMESTAMP,
           Record = RECORD) %>%
    mutate(Port = parse_number(Port),
           Value = as.numeric(Value)) %>%
    ggplot(aes(x = Timestamp, y = Value, color = as.factor(Port))) +
    geom_line() + labs(title = "GWI Transition",
                       subtitle = "Raw data as of: June 7, 2022")-> gwi_tr_g

cowplot::plot_grid(gwi_up_g, gwi_tr_g, msm_up_g, msm_tr_g, ncol = 2)


################################################################################
#Metadata for files, including SERC site names and properties, and well measurements
# used to correct water depths are originally from
# smb://pnl.gov/Projects/Coastal_TAI/SULI Work/Madison Bowe (fall 2020)/TEMPEST_GW/SERC_TEMPEST_Trolls_Depths_Nov2019.xlsx
# I've copied them over to
# /Users/regi350/OneDrive - PNNL/Documents/projects/tempest/data_tempest/210506_SERC_TEMPEST_Troll_Depths.csv
# Depths have not necessarily been updated to the new depths of wells dug ~3/2021

# Bring in TEMPEST troll data, compile, and plot
################################################################################


################################################################################
### General setup
################################################################################

# clean up workspace
rm(list=ls())

# load pacman package
if(!require(pacman)){
  install.packages("pacman")
  library(pacman)}

# load packages
pacman::p_load(tidyverse,
               cowplot,
               lubridate,
               parsedate,
               padr,
               RColorBrewer)

# set path for exporting data
path_write_csv <- "data_tempest/"

# set paths for reading in water quality data
path_wq_archive <- "~/Dropbox/TEMPEST_PNNL_Data/Loggernet_Rawdata_Archive/" #archive path
path_wq_current <- "~/Dropbox/TEMPEST_PNNL_Data/Current_data" #current data path

# set paths for reading in barometric data
path_baro_archive <- "~/Dropbox/SERC Dock/SERC_DOCK_Rawdata_Loggernet"
path_baro_current <- "~/Dropbox/SERC Dock/SERC_DOCK_Rawdata_Loggernet/SERC_DOCK_current_data/MGEO_SERC_MetTable.dat"

# read in metadata - stored locally, can put wherever it's helpful
metadata <- read_csv("data_tempest/210506_SERC_TEMPEST_Troll_Depths.csv")

# Set up ORP correction to eH
# Troll Ag/AgCl potentials are listed for the particular molarity used in the In-Situ Aqua Troll 600 here:
# https://in-situ.com/us/blog/orp-field-measurements-reporting-redox-potential-eh-correctly/?___store=us_en&___from_store=global_en
Temp.C=c(5,10,15,20)
Potential.mV=c(219,211,207,202)
orp_cal_curve <- lm(Potential.mV~Temp.C)
dV.dT <- coef(orp_cal_curve)[2]
V0 <- coef(orp_cal_curve)[1]

####################################END#########################################


################################################################################
### Functions
################################################################################

# read in barometric data for correcting water levels
read_met <- function(filepaths) {
  df_names <- read_delim(filepaths, delim = ",", skip = 1, n_max = 0) %>% names()

  read_delim(filepaths, delim = ",", skip = 4, col_names = df_names) %>%
    dplyr::mutate(datetime = round_date(force_tz(parsedate::parse_date(TIMESTAMP), tzone ="GMT"), "15 min")) %>%
    dplyr::group_by(datetime) %>%
    dplyr::summarize(bp_mbar = mean(Barometric_Pressure_PB110B),
              rain_int = mean(Rain_Intensity))
}

#list all troll files (combine current and archive file paths into one list)
list_all_files <- function(pattern_to_find) {
  c( c(list.files(path=, path_wq_archive, pattern=pattern_to_find,full.names=T),
       list.files(path=, path_wq_current, pattern=pattern_to_find,full.names=T)))
}

# read in AquaTroll 200 data the manual way
read_200 <- function(filepaths) {
  read_delim(filepaths,
             skip=1, delim=",", col_names=T) %>%
    slice(3:n()) %>% #remove junk header
    dplyr::mutate(datetime = force_tz(parsedate::parse_date(TIMESTAMP), tzone ="GMT"),
                  depth_cm = as.numeric(Depth),
                  temp_c = as.numeric(Temperature),
                  sp_cond_uScm = as.numeric(Specific_Conductivity),
                  salinity = as.numeric(Salinity),
                  density_gcm3 = as.numeric(Water_Density),
                  pressure_psi = as.numeric(Pressure),
                  pressure_mbar = pressure_psi * 68.948,
                  resistivity_ohmcm = as.numeric(Resistivity),
                  sensor = "TROLL200") %>%
    dplyr::select(datetime, depth_cm, temp_c, sp_cond_uScm, salinity,
           density_gcm3, pressure_mbar, sensor)
}

# read in AquaTroll 600 data the manual way
read_600 <- function(filepaths) {
  read_delim(filepaths,
             skip=1, delim=",", col_names=T) %>%
    dplyr::slice(3:n())%>% #remove junk header
    dplyr::mutate(datetime = force_tz(parsedate::parse_date(TIMESTAMP), tzone ="GMT"),
                  depth_cm = as.numeric(Depth600),
                  temp_c = as.numeric(Temperature600),
                  sp_cond_uScm = as.numeric(Specific_Conductivity600),
                  salinity = as.numeric(Salinity600),
                  do_sat = as.numeric(RDO_perc_sat600),
                  do_mgl = as.numeric(RDO_concen600),
                  pH = as.numeric(pH600),
                  orp = as.numeric(pH_ORP600),
                  eH = orp + V0 + dV.dT*temp_c,
                  density_gcm3 = as.numeric(Water_Density600),
                  pressure_psi = as.numeric(Pressure600),
                  pressure_mbar = pressure_psi * 68.948,
                  resistivity_ohmcm = as.numeric(Resistivity600),
                  sensor = "TROLL600") %>%
    dplyr::select(datetime, depth_cm, temp_c, sp_cond_uScm, salinity, do_sat,
           do_mgl, pH, eH, density_gcm3, pressure_mbar, sensor)
}

# Format troll data and calculate WL
#Convert to water depth (h = P [mbar] * 100 [Pa/mbar])/(rho [g/cm3]*1000 [kg/m3//g/cm3]*g [m/s2]) where [Pa]=[kgm/s2m2]
format_troll <- function(df, site_ID) {
  # find sensor deployment depth and convert to m
  Dist_PressureSensor_belowground_m <- metadata$Dist_PressureSensor_belowground_cm[str_which(site_ID, metadata$Site_PNNL)] / 100

  # find site elevation for pressure head calculation, and conver to m
  Elevation <- metadata$Elevation[str_which(site_ID, metadata$Site_PNNL)] / 100

  # merge barometric and wq data and calculate depth metrics
  x <- left_join(df, baro_data %>% select(datetime, bp_mbar, rain_int), by="datetime") %>%
    dplyr::mutate(pressure_cor_mbar = pressure_mbar - bp_mbar,
                  density_gcm3_cor = ifelse(density_gcm3 > 0.95, density_gcm3, 1),
                  pressurehead.m = (pressure_cor_mbar * 100) / (density_gcm3_cor * 1000 * 9.80665),
                  WLfromsurface.m = pressurehead.m - Dist_PressureSensor_belowground_m,
                  head.m = WLfromsurface.m + Elevation,
                  site = site_ID)
  return(x)
}

# assign L1 QC flags to AquaTroll data (flag data outside sensor range limits)
qc_troll <- function(df) {
  sensor <- first(df$sensor)

  # different parameter sets need flagging for 600s and 200s
  ifelse(first(df$sensor) == "TROLL600",
         x <- df %>%
           dplyr::mutate(f_spc = ifelse(sp_cond_uScm > 350000 | sp_cond_uScm < 0, TRUE, FALSE),
                         f_sal = ifelse(salinity > 350 | salinity < 0, TRUE, FALSE),
                         f_wl = ifelse(pressurehead.m < 0.1, TRUE, FALSE),
                         f_do.sat = ifelse(do_sat > 200 | do_sat < 0, TRUE, FALSE),
                         f_do.mgl = ifelse(salinity > 20 | salinity < 0, TRUE, FALSE),
                         f_ph = ifelse(pH > 14 | pH < 0, TRUE, FALSE),
                         f_eh = ifelse(eH > 1400 | eH < -1400, TRUE, FALSE),
                         flag = ifelse(f_spc == TRUE | f_sal == TRUE | f_wl == TRUE | f_do.sat == TRUE |
                                         f_do.mgl== TRUE | f_ph == TRUE | f_eh == TRUE, TRUE, FALSE)),
         x <- df %>%
           dplyr::mutate(f_spc = ifelse(sp_cond_uScm > 350000 | sp_cond_uScm < 0, TRUE, FALSE),
                         f_sal = ifelse(salinity > 350 | salinity < 0, TRUE, FALSE),
                         f_wl = ifelse(pressurehead.m < 0.1, TRUE, FALSE),
                         flag = ifelse(f_spc == TRUE | f_sal == TRUE | f_wl == TRUE,  TRUE, FALSE)))

  return(x)
}
####################################END#########################################


################################################################################
### Read in / format barometric pressure data
################################################################################

# Gather file names from archive and current folders
filepaths_baro <- c(list.files(path = path_baro_archive,
                               pattern = "*MetTable",full.names = T),
                    path_baro_current)

# read all barometric data after 5/30/19 into a dataframe
baro_data <- lapply(filepaths_baro, read_met) %>%
  bind_rows(.) %>% filter(datetime > "2019-05-30 14:45:00")
####################################END#########################################


################################################################################
### Read in / format troll data
################################################################################
# Site names
# Site_SERC	Site_PNNL	Instrument	Probe_Name
# PNNL_23	  GW1	      TROLL200	  TEMPEST_SF_200_GW1
# PNNL_31	  GW2	      TROLL200	  TEMPEST_S_200_GW2
# PNNL_21	  GW3	      TROLL200	  TEMPEST_F_200_GW3
# PNNL_12	  GW4	      TROLL200	  TEMPEST_C_200_GW4
# PNNL_32	  GW5	      TROLL600	  TEMPEST_S_600_GW5
# PNNL_23	  GW6	      TROLL600	  TEMPEST_F_600_GW6
# PNNL_13	  GW7	      TROLL600	  TEMPEST_C_600_GW7
# PNNL_41	  GW8	      TROLL200	  TEMPEST_L_200_GW8

# set up file names for each sensor
filepaths_gw1 <- list_all_files("*PNNL_23_WaterLevel200")
filepaths_gw2 <- list_all_files("*PNNL_31_WaterLevel200")
filepaths_gw3 <- list_all_files("*PNNL_21_WaterLevel200")
filepaths_gw4 <- list_all_files("*PNNL_12_WaterLevel200")
filepaths_gw5 <- list_all_files("*PNNL_32_WaterLevel600")
filepaths_gw6 <- list_all_files("*PNNL_23_WaterLevel600")
filepaths_gw7 <- list_all_files("*PNNL_13_WaterLevel600")
filepaths_gw8 <- list_all_files("*PNNL_41_WaterLevel200")

# read, format, and qc troll data
gw1 <- lapply(filepaths_gw1, read_200) %>% bind_rows(.) %>% format_troll(., "GW1") %>% qc_troll(.)
gw2 <- lapply(filepaths_gw2, read_200) %>% bind_rows(.) %>% format_troll(., "GW2") %>% qc_troll(.)
gw3 <- lapply(filepaths_gw3, read_200) %>% bind_rows(.) %>% format_troll(., "GW3") %>% qc_troll(.)
gw4 <- lapply(filepaths_gw4, read_200) %>% bind_rows(.) %>% format_troll(., "GW4") %>% qc_troll(.)
gw5 <- lapply(filepaths_gw5, read_600) %>% bind_rows(.) %>% format_troll(., "GW5") %>% qc_troll(.)
gw6 <- lapply(filepaths_gw6, read_600) %>% bind_rows(.) %>% format_troll(., "GW6") %>% qc_troll(.)
gw7 <- lapply(filepaths_gw7, read_600) %>% bind_rows(.) %>% format_troll(., "GW7") %>% qc_troll(.)
gw8 <- lapply(filepaths_gw8, read_200) %>% bind_rows(.) %>% format_troll(., "GW8") %>% qc_troll(.)

# combine into a single dataframe
df <- bind_rows(gw1, gw2, gw3, gw4, gw5, gw6, gw7, gw8)

# write out dataset
write_csv(df, paste0(path_inputs, "raw_troll_data.csv"))
####################################END#########################################

# quick look at depths, flagged points in red
theme_set(theme_bw())
a_week_ago <- max(df$datetime) - 60*60*24*7

plot_grid(ggplot(df, aes(datetime, WLfromsurface.m, color = site)) + geom_line() +
            geom_point(data = df %>% filter(flag == "TRUE"), color = "red"),
                ggplot(df %>% filter(datetime > a_week_ago),
                       aes(datetime, WLfromsurface.m, color = site)) + geom_line() +
            geom_point(data = df %>% filter(datetime > a_week_ago & flag == "TRUE"), color = "red"),
                ncol = 1)

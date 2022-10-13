# Functions for AquaTroll processing
# Functions written by Peter Regier
# Script created by Stephanie Pennington 2022-01-18

#write troll data with standard naming
write_troll <- function(x) {

    filename <- paste0("TMP_", x$Instrument[1], "_",
                       x$Well_Name[1], "_",
                       gsub("-","", date(min((x$Timestamp)))), "-",
                       gsub("-","", date(max((x$Timestamp)))), "_L0A.csv")
    cat("Writing...", filename, "\n")
    write_csv(x, filename)
}

# read in barometric data for correcting water levels
read_met <- function(filepaths) {
    df_names <- read_delim(filepaths, delim = ",", skip = 1, n_max = 0) %>% names()

    read_delim(filepaths, delim = ",", skip = 4, col_names = df_names) #%>%

        # dplyr::mutate(Timestamp = round_date(parsedate::parse_date(TIMESTAMP, default_tz = "GMT"), "15 min")) %>%
        # dplyr::group_by(Timestamp) %>%
        # dplyr::summarize(bp_mbar = mean(Barometric_Pressure_PB110B),
        #                  rain_int = mean(Rain_Intensity))
}

#
# #list all troll files (combine current and archive file paths into one list)
# list_all_files <- function(pattern_to_find) {
#     c( c(list.files(path=, path_wq_archive, pattern=pattern_to_find,full.names=T),
#          list.files(path=, path_wq_current, pattern=pattern_to_find,full.names=T)))
# }

# read in AquaTroll 200 data the manual way
read_200 <- function(filepaths) {
    read_delim(filepaths,
               skip=1, delim=",", col_names=T) %>%
        slice(3:n()) %>% #remove junk header
        dplyr::mutate(Timestamp = force_tz(parsedate::parse_date(TIMESTAMP), tzone ="GMT"),
                      Depth = as.numeric(Depth),
                      Temp = as.numeric(Temperature),
                      Specific_Conductivity = as.numeric(Specific_Conductivity),
                      Salinity = as.numeric(Salinity),
                      Density = as.numeric(Water_Density),
                      Pressure_psi = as.numeric(Pressure),
                      Pressure_mbar = Pressure_psi * 68.948,
                      Resistivity = as.numeric(Resistivity),
                      Instrument = "TROLL200") %>%
        rename(Logger_ID = Statname) %>%
        dplyr::select(Timestamp, Depth, Logger_ID, Temp, Specific_Conductivity, Salinity, Density, Pressure_mbar, Resistivity, Instrument)
}

# read in AquaTroll 600 data the manual way
read_600 <- function(filepaths) {
    read_delim(filepaths,
               skip=1, delim=",", col_names=T) %>%
        dplyr::slice(3:n())%>% #remove junk header
        dplyr::mutate(Timestamp = force_tz(parsedate::parse_date(TIMESTAMP), tzone ="GMT"),
                      Depth = as.numeric(Depth600),
                      Temp = as.numeric(Temperature600),
                      Specific_Conductivity = as.numeric(Specific_Conductivity600),
                      Salinity = as.numeric(Salinity600),
                      DO_sat = as.numeric(RDO_perc_sat600),
                      DO_mgl = as.numeric(RDO_concen600),
                      pH = as.numeric(pH600),
                      ORP = as.numeric(pH_ORP600),
                      eH = ORP + V0 + dV.dT * Temp,
                      Density = as.numeric(Water_Density600),
                      Pressure_psi = as.numeric(Pressure600),
                      Pressure_mbar = Pressure_psi * 68.948,
                      Resistivity = as.numeric(Resistivity600),
                      Instrument = "TROLL600") %>%
        rename(Logger_ID = Statname) %>%
        dplyr::select(Timestamp, Depth, Temp, Specific_Conductivity, Salinity, DO_sat, DO_mgl,
                      pH, ORP, eH, Density, Pressure_mbar, Resistivity, Instrument, Logger_ID)
}


join_troll <- function(df, troll_inventory) {

    change_date <- "2021-03-10 00:00:00"

    change_IDs <- c("PNNL_13", "PNNL_23", "PNNL_32")
    change_instrument <- "TROLL600"

    df %>%
        mutate(Install = if_else(Timestamp >= change_date & Logger_ID %in% change_IDs & Instrument == change_instrument, 2, 1)) %>%
        left_join(troll_inventory, by = c("Logger_ID", "Instrument", "Install"))

}

# Format troll data and calculate WL
#Convert to water depth (h = P [mbar] * 100 [Pa/mbar])/(rho [g/cm3]*1000 [kg/m3//g/cm3]*g [m/s2]) where [Pa]=[kgm/s2m2]
format_troll <- function(df) {

    df %>%
        group_by(Probe_Name) %>%
        distinct() %>%
        # convert deployment depth and elevation to meters
        mutate(Dist_PressureSensor_belowground_m = Dist_pressure_sensor_belowground_calc / 100,
               Elevation = Elevation / 100) %>%
        # merge barometric and wq data and calculate depth metrics
        left_join(baro_data, by="Timestamp") %>%
        dplyr::mutate(Year = year(Timestamp),
                      Pressure_cor_mbar = Pressure_mbar - bp_mbar,
                      Density_gcm3_cor = ifelse(Density > 0.95, Density, 1),
                      Pressurehead.m = (Pressure_cor_mbar * 100) / (Density_gcm3_cor * 1000 * 9.80665),
                      WLfromsurface.m = Pressurehead.m - Dist_PressureSensor_belowground_m,
                      Head.m = WLfromsurface.m + Elevation)

}

# assign L1 QC flags to AquaTroll data (flag data outside sensor range limits)
qc_troll <- function(df, troll_type) {
    # for both models

    if(troll_type == "TROLL200") {

        print("Running QA/QC on AquaTroll200 data")
        df %>%
            mutate(
                f_spc = Specific_Conductivity > 350000 | Specific_Conductivity < 0,
                f_sal = Salinity > 350 | Salinity < 0,
                f_wl = Pressurehead.m < 0.1) %>%
            mutate(Flag = f_spc | f_sal | f_wl) -> df1
    }

    if(troll_type == "TROLL600") {

        print("Running QA/QC on AquaTroll600 data")
        df %>%
            mutate(
                f_spc = Specific_Conductivity > 350000 | Specific_Conductivity < 0,
                f_sal = Salinity > 350 | Salinity < 0,
                f_wl = Pressurehead.m < 0.1,
                f_do.sat = DO_sat > 200 | DO_sat < 0,
                f_do.mgl = Salinity > 20 | Salinity < 0, # needs to be DO_sat
                f_ph = pH > 14 | pH < 0,
                f_eh = eH > 1400 | eH < -1400) %>%
            mutate(Flag = f_spc | f_sal | f_wl | f_do.sat | f_do.mgl | f_ph | f_eh) -> df1
    }

    return(df1)
}


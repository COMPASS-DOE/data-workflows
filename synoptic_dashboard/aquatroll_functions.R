

## This function reads in and cleans up each file
read_the_troll <- function(data) {
  
  cat(paste("Downloading file:", data))
  # Download files to local (don't worry, we'll delete em later)
  drive_download(data, overwrite = T, path = "tempfile.dat")
  
  # Extract site and location information from filename
  site <- str_split(data, "_", simplify = T)[,2]
  location <- str_split(data, "_", simplify = T)[,3]
  
  # Read in files, clean up columnn names, and select
  aq_primitive <- read_delim(file = "tempfile.dat", skip = 1) 
  
  # Print in the console which file we are removing
  cat(paste("Removing the tempfile.dat for:", data, "from local..."))
  unlink("tempfile.dat") # delete tempfile from local
  
  aq_primitive %>% 
    slice(3:n()) %>% 
    clean_names() %>% 
    mutate(datetime = parsedate::parse_date(timestamp)) %>% 
    filter(datetime > "2022-03-01") %>% 
    mutate_at(vars(contains("600")), as.numeric) %>% 
    rename_with(~str_remove(., '600[a-z]')) %>% 
    rename("pressure_psi" = pressure) %>% 
    mutate(pressure_mbar = pressure_psi * 68.948) %>% 
    mutate(site = site, 
           location = location) %>% 
    select(datetime, site, location, temperature, salinity, rdo_concen, p_h, p_h_orp,
           depth, water_density, pressure_mbar, pressure_psi)
}

process_the_troll <- function(){
 
  # Create a list of files
  cat("Accessing drive..")
  aquatroll_files <- gdrive_files %>% 
    filter(grepl("WaterLevel", name))
  
  
  ## Read in well dimensions
  well_dimensions <- read_csv("aquatroll_inventory.csv") %>% 
    mutate(location = case_when(transect_location == "Upland" ~ "UP", 
                                transect_location == "Transition" ~ "TR", 
                                transect_location == "Wetland" ~ "W"), 
           ground_to_sensor_cm = ring_to_pressure_sensor_cm - (well_top_to_ground_cm - bolt_to_cap_cm)) %>% 
    dplyr::select(site, location, ground_to_sensor_cm)
  
  
  aquatroll_files$name %>% 
    map(read_the_troll) %>% 
    bind_rows() %>%
    inner_join(well_dimensions, by = c("site", "location")) %>% 
    mutate(density_gcm3_cor = ifelse(water_density > 0.95, water_density, 1), 
           pressurehead_m = (pressure_mbar * 100) / (density_gcm3_cor * 1000 * 9.80665), 
           wl_below_surface_m = pressurehead_m - (ground_to_sensor_cm / 100))
  
}





# Function setup
read_sapflow <- function(data) {
  
  # Download files to local (don't worry, we'll delete em later)
  drive_download(data, overwrite = T, path = "tempfile.dat")
  
  sf_primitive <- readLines("tempfile.dat") #temp file goes here
    sf_primitive <- sf_primitive[-3:-4] # remove lines 3 and 4 with unneeded information
  
  unlink("tempfile.dat")
  
  # parse line one to extract logger name
  # pnnl_x <- gregexpr("PNNL_", sf_primitive[1])[[1]][1]
  # logger_name <- substr(sf_primitive[1], start = pnnl_x, stop = pnnl_x + 6)
  
  # The "I()" notation is how to read from a string; see help page
  read_csv(I(sf_primitive), skip = 1, col_types = "Tddcddddddddddddd") #%>%
#    mutate(Logger = logger_name)
}

process_sapflow <- function() {

  # Create a list of files
  cat("Accessing drive..")
  sapflow_files <- gdrive_files %>% 
    filter(grepl("Sapflow", name))

  #NEED TO ADD INVENTORY LATER
  
  sapflow_files$name %>% 
    map(read_sapflow) %>% 
    bind_rows() %>% 
    separate(Statname, into = c("project", "Site", "Location") ) %>% 
    select(-project) %>% 
    filter(Location != "W") %>% 
    # extract number form former col name "DiffVolt_Avg(1)" would become "1"
    # join with ports dataframe and bring in tree codes
    pivot_longer(cols = starts_with("DiffVolt"),
                 names_to = "Port", values_to = "Value") %>%
    rename(Timestamp = TIMESTAMP,
           Record = RECORD) %>%
    mutate(Port = parse_number(Port),
           Value = as.numeric(Value))

}


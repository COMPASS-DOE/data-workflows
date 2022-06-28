# Sapflow functions


read_sapflow <- function(filename) {

    sdat <- readLines(filename) #temp file goes here
    sdat <- sdat[-3:-4] # remove lines 3 and 4 with unneeded information

    # parse line one to extract logger name
    pnnl_x <- gregexpr("PNNL_", sdat[1])[[1]][1]
    logger_name <- substr(sdat[1], start = pnnl_x, stop = pnnl_x + 6)

    read_csv(I(sdat), skip = 1) %>%
        mutate(Logger = logger_name)
}

format_sapflow <- function(sf_primitive, sf_inventory) {
    sf_primitive %>%
        distinct() %>%
        # extract number form former col name "DiffVolt_Avg(1)" would become "1"
        # join with ports dataframe and bring in tree codes
        pivot_longer(cols = starts_with("DiffVolt_Avg"),
                     names_to = "Port", values_to = "Value") %>%
        rename(Timestamp = TIMESTAMP,
               Record = RECORD) %>%
        mutate(Timestamp = ymd_hms(Timestamp, tz = "EST"),
               Port = parse_number(Port),
               Logger = parse_number(Logger)) -> sf_raw

    sf_raw %>%
        left_join(sf_inventory, by = c("Logger", "Port")) %>%
        filter(!is.na(Tree_Code)) %>% #remove ports that dont have any sensors
        select(Timestamp, Record, BattV_Avg, Port, Value, Logger, Tree_Code, Grid_Square, Species, Installation_Date) %>%
        mutate(Deep_Sensor = grepl("D", Tree_Code),
               Grid_Letter = substring(Grid_Square, 1, 1),
               Grid_Number = substring(Grid_Square, 2, 2)) ->> sapflow

    nomatch_ports <<- anti_join(sf_raw, sf_inventory, by = c("Logger", "Port"))

    # if(nrow(nomatch_ports) > 0) {
    #     warning("There were logger/port combinations that I couldn't find in sapflow_inventory.csv:")
    #     nomatch_ports %>%
    #         distinct(Logger, Port) %>%
    #         kable()
    # }
}

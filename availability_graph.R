
library(tidyverse)

fls <- list.files("~/Documents/v1-1/", pattern = "*.csv$", full.names = TRUE, recursive = TRUE)

results <- list()

for(i in seq_along(fls)) {
    message(basename(fls[i]))

    results[[basename(fls[i])]] <- readr::read_csv(fls[i]) %>% group_by(Site, Instrument, year(TIMESTAMP), month(TIMESTAMP)) %>% summarise(n = n())
}

bind_rows(results) %>%
    rename(Year = `year(TIMESTAMP)`, Month = `month(TIMESTAMP)`) %>%
    arrange(Site, Year, Month) -> r

r %>% group_by(Site, Instrument, Year, Month) %>%
    summarise(n = sum(n)) %>%
    filter(Site =="GCW", !is.na(Instrument)) %>%
    mutate(data_present = ifelse(n > 0, "Yes", "No"),
           Month = month.abb[Month],
           date = ym(paste(Year, Month))) %>%
    arrange(date) %>%
    mutate(
           ts_str = format(date, "%b-%Y"),
           ts_fct = factor(ts_str, levels = unique(ts_str))) %>%
    select(-n) %>%
    ggplot(aes(x = ts_fct, y = Instrument, fill = Instrument)) +
    geom_raster(colour = "white", hjust = 0, vjust = 0) +
    facet_wrap(~Site, ncol = 1, strip.position = "left") +
    theme_minimal() +
    scale_y_discrete(position = "right") +
    theme(axis.text.x = element_text(angle = 90, vjust = -0.25, hjust = 1),
          axis.text.y = element_text(vjust = 1.25),
          axis.ticks.y=element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid = element_line(color="darkgrey"),
          panel.border = element_rect(color = "darkgrey", fill = NA, size = 1)) +
    labs(x = "", y = "") +
    scale_fill_viridis_d(option = "rocket", begin = 0, end = 0.85) +
    theme(axis.title = element_text(size = 20),
          plot.title = element_text(size=20),
          axis.text.y = element_text(size = 16),
          axis.text.x = element_text(size = 16),
          strip.text = element_text(size = 18),
          legend.position="bottom",
          legend.text = element_text(size=16),
          legend.title = element_text(size=18)) -> p

ggsave("~/Documents/synoptic_avail_GCW.png", height = 6, width = 15)

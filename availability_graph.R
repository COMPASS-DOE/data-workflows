
library(tidyverse)

fls <- list.files("~/Documents/v1-1/", pattern = "*.csv$", full.names = TRUE, recursive = TRUE)

results <- list()

for(f in fls[1:3]) {
    message(basename(f))

    results[[f]] <- readr::read_csv(f, col_types = "ccTccccdccii") %>%
        mutate(ts_str = format(TIMESTAMP, "%b-%Y")) %>%
        group_by(Site, Instrument, ts_str) %>%
            summarise(TIMESTAMP = mean(TIMESTAMP),
                      n = n(),
                      .groups = "drop")
}

bind_rows(results) %>%
    # each file is a site and plot; sum by site
    group_by(Site, Instrument, ts_str) %>%
    summarise(TIMESTAMP = mean(TIMESTAMP),
              n = sum(n),
              .groups = "drop") %>%
    # not sure why this next line
#    filter(Site == "GCW", !is.na(Instrument)) %>%
    mutate(data_present = if_else(n > 0, "Yes", "No")) %>%
    # create the factor month-year
    arrange(TIMESTAMP) %>%
    mutate(ts_fct = factor(ts_str, levels = unique(ts_str))) %>%
    # plot
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

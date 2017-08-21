# Packages
library(tidyverse)
library(here)
library(stringr)
library(hrbrthemes)
library(ggjoy)

# Data munging
wi <- unique(read_csv(here("data/watershed_integrity_new_2.csv")))

index_vars <- c("iwi", "ici","wchem","cchem","whabt","chabt","wsed","csed","whyd",
                "chyd","wtemp","ctemp", "wconn", "cconn")

# Joy plots of distribution of all index components by study area
wi_index <- wi %>%
  filter(variable %in% index_vars)

index_all_ws_gg <- ggplot(wi_index, aes(x = value, y = variable)) +
  geom_joy(rel_min_height = 0.001, scale = 0.9, bandwidth = 0.04) +
  facet_grid(. ~ watershed) +
  theme_ipsum() +
  labs(x = "", y = "")
index_all_ws_gg

ggsave(filename = here("figures/iwi_ici_joy_ws_fig.jpg"), plot = index_all_ws_gg, width = 7, height = 5, 
       units = "in", dpi = 300)

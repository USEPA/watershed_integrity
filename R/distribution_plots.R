# Packages
library(tidyverse)
library(here)
library(stringr)
library(hrbrthemes)
library(ggridges)
library(forcats)

# Data munging
wi <- unique(read_csv(here("data/watershed_integrity_new_2.csv")))
landscape <- read_csv(here("data/raw/lulc_4wtsheds.csv")) %>%
  select(-watershed) %>%
  gather(variable, value, 2:7) %>%
  left_join(select(wi,comid,watershed,site,waterbody_type)) %>%
  select(comid, watershed, site, waterbody_type, variable, value) %>%
  unique() %>%
  mutate(value = value/100)

wi <- wi %>%
  rbind(landscape) %>%
  mutate(watershed = factor(watershed, labels = c("Calapooia", "Choptank", 
                                                  "East Fork\nLittle Miami", 
                                                  "Narragansett\nBay")))

index_vars <- c("iwi", "wtemp","wsed","whyd","whabt","wconn","wchem",
                "pctforestws","pctagws","pcturbanws",
                "ici","ctemp","csed","chyd","chabt","cconn","cchem",
                "pctforestcat","pctagcat","pcturbancat")
index_vars <- index_vars[length(index_vars):1]


# Joy plots of distribution of all index components by study area
wi_index <- wi %>%
  filter(toupper(variable) %in% toupper(index_vars)) %>%
  mutate(variable = fct_relevel(toupper(variable), toupper(index_vars)))

index_all_ws_gg <- ggplot(wi_index, aes(x = value, y = variable)) +
  geom_density_ridges(rel_min_height = 0.001, scale = 0.9, bandwidth = 0.04) +
  facet_grid(. ~ watershed) +
  theme_ipsum(base_size = 12) +
  labs(x = "", y = "") + 
  scale_x_continuous(breaks = c(0,0.5,1)) +
  theme(strip.text.x = element_text(size = 12, hjust = 0.5),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.spacing.x = unit(0.25, "cm"),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
index_all_ws_gg

ggsave(filename = here("figures/iwi_ici_joy_ws_fig.tiff"), plot = index_all_ws_gg, width = 6, height = 5, 
       units = "in", dpi = 300)

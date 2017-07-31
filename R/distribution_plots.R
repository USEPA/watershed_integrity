# Packages
library(tidyverse)
library(here)
library(stringr)
library(hrbrthemes)
library(ggjoy)

wi <- unique(read_csv(here("data/watershed_integrity_new_2.csv")))

wi_joy_data <- wi %>%
  select(comid, watershed, variable, value) %>%
  filter(variable %in% c("iwi","ici")) %>%
  unique #%>%
  #spread(key = variable, value = value)
  
# Joy plots of iwi and ici distribution by study area

distro_fig(wi, c("iwi", "ici"), watershed ~ .)

iwi_ici_joy_ws <- ggplot(wi_joy_data, aes(x = value, y = watershed)) +
  geom_joy(rel_min_height = 0.001, scale = 0.9, bandwidth = 0.04) +
  theme_ipsum() +
  labs(x = "", y = "") + 
  facet_grid(. ~ variable)
iwi_ici_joy_ws

#ici_joy_ws <- ggplot(wi_joy_data, aes(x = ici, y = watershed)) +
#  geom_joy(rel_min_height = 0.001, scale = 0.9, bandwidth = 0.04) +
#  theme_ipsum() +
#  labs(x = "", y = "")

#iwi_joy_ws
#ici_joy_ws

index_x <- c("iwi", "ici","wchem","cchem","whabt","chabt","wsed","csed","whyd",
           "chyd","wtemp","ctemp")

index_joy_data <- wi %>%
  select(comid, watershed, variable, value) %>%
  filter(variable %in% index_x) %>%
  unique

# Joy plots of distribution of all index components by study area
all_index_joy <- ggplot(index_joy_data, aes(x = value, y = variable)) +
  geom_joy(rel_min_height = 0.001, scale = 0.9, bandwidth = 0.04) +
  facet_grid(. ~ watershed) +
  theme_ipsum() +
  labs(x = "", y = "")
all_index_joy


# Joy plots of distribution of y's per study area
ws <- unique(wi$watershed)


ggsave(filename = here("figures/iwi_ici_joy_ws_fig.jpg"), plot = iwi__ici_joy_ws, width = 7, height = 5, 
       units = "in", dpi = 300)
ggsave(filename = here("figures/ici_joy_ws_fig.jpg"), plot = ici_joy_ws, width = 7, height = 5, 
       units = "in", dpi = 300)
ggsave(filename = here("figures/all_index_joy_fig.jpg"), plot = all_index_joy, width = 7, height = 9, 
       units = "in", dpi = 300)


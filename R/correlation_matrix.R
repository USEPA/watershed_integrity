# Packages
library(tidyverse)
library(here)
library(stringr)
library(hrbrthemes)
source("R/corr_fig.R")

wi <- unique(read_csv(here("data/watershed_integrity_new_2.csv")))

x <- c("iwi", "ici","wchem","cchem","whabt","chabt","wsed","csed","whyd",
           "chyd","wtemp","ctemp","wconn","cconn")

watersheds <- unique(wi$watershed)

for(i in watersheds){
  ws_abb <- abbreviate(paste(i,collapse = ""))
  wi %>%
    filter(!str_detect(wi$variable, paste(x,collapse = "|"))) %>%
    filter(!grepl("_old",variable)) %>%
    filter(watershed == i) %>%
    pull(variable) %>%
    unique() %>%
    corr_fig(data = wi, ws = i, x = x, y = ., 
             output_csv = paste0("data/", ws_abb, "_cor.csv"),
             filename = paste0("figures/", ws_abb, "_cor_fig.jpg"), 
             width = 7,height = 5, units = "in", dpi = 300)
}

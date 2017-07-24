# Packages
library(tidyverse)
library(here)
library(stringr)
library(hrbrthemes)
source("R/corr_fig.R")

wi <- unique(read_csv(here("data/watershed_integrity_new.csv")))

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



cal_y <- c("logNdif","dN15chironimid","total_in","xcmgw","xembed","fishMMI",
           "max_tempC_summer","phase")

chop_y <- c("WetPercentage","WetCntAll", "WetCntWhole", "WetCntPartial")

eflr_y <- c("log10TNOxdif",)

nb_y <- c("log10no3","log10chloride", "pN15", "dN15BOM")

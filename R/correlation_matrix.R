# Packages
library(tidyverse)
library(here)

wi <- unique(read_csv(here("data/watershed_integrity_new.csv")))

# Calapooia
cal_id <- names(wi)[1:4]
cal_y <- c("logNdif","dN15chironimid","total_in","xcmgw","xembed","fishMMI",
           "max_tempC_summer","phase")

cal_x <- c("iwi", "ici","wchem","cchem","whabt","chabt","wsed","csed","whyd",
           "chyd","wtemp","ctemp")

cal <- wi %>%
  filter(watershed == "calapooia") %>%
  spread(key = variable, value = value) %>%
  select(c(cal_id,cal_y,cal_x))

cor(cal[,5:24],use = "pairwise.complete.obs")



# Packages
library(tidyverse)
library(here)
library(stringr)
library(corrplot)

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

cal_cor <- cor(cal[,5:24],use = "pairwise.complete.obs") %>%
  data.frame() %>%
  rownames_to_column("index") %>% 
  filter(str_detect(index, 'iwi|ici|whyd|chyd|wchem|cchem|wsed|csed|wconn|cconn|wtemp|ctemp|whabt|chabt')) %>%
  select(c("index",cal_y)) %>%
  gather("variable", "value", -1)

cal_cor_gg <- ggplot(cal_cor, aes(x = index, y = variable)) +
  geom_point(aes(size = abs(value)/max(abs(value)), color = value))
cal_cor_gg


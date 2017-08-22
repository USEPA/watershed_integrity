library(tidyverse)
library(here)

wi <- unique(read_csv(here("data/watershed_integrity_new_2.csv")))

# variables to filter out after initial review
y_out <- c("total_out, Aag_frt_in, winter_frt, harvest_rmvl, res_N, v1w_msq, sddepth, xfc_nat, pcturb2011cat, pctcrop2011cat, amplitude, CatAreaSqKM, Areasqkm, forspct, agpct, devpct, pcticnlcd, pcticstate, logwspop2010, logwspopdens2010, urb200buf, agr200buf, wetland200buf, lakeimp200buf, forest200buf, househec200buf, urbwshed, agrwshed, wetlandshed, lakeimpwshed, forestwshed, househecwshed")
# Is Aag_frt_in same as ag_frt?
y_out <-strsplit(y_out,", ")[[1]]

wi <- wi %>%
  filter(!variable %in% y_out)

write_csv(wi, here("data/watershed_integrity_new_2.csv"))
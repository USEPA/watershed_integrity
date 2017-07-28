library(tidyverse)
library(here)

wi <- unique(read_csv(here("data/watershed_integrity_new.csv")))
nar_miss <- read_csv(here("data/raw/NAR_missing streamsites.csv"))
nar_stream_sites <- read_csv("data/raw/NAR_comid_stream siteID.csv")

nar_streams_long2 <- nar_miss %>%
  left_join(nar_stream_sites, by = c("comid")) %>%
  select(comid,watershed,site, everything()) %>%
  gather(variable, value, 5:30) %>%
  rename(waterbody_type = `waterbody type`) %>%
  mutate(watershed = "narragansett bay")



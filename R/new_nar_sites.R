library(tidyverse)
library(here)

wi <- unique(read_csv(here("data/watershed_integrity_new.csv")))
nar_miss <- read_csv(here("data/raw/NAR_missing streamsites.csv")) %>%
  mutate(comid = as.integer(comid))
nar_stream_sites <- readxl::read_excel("data/raw/NAR_missing streamsites.xlsx") %>%
  select(comid, site) %>%
  mutate(comid = as.integer(comid))

nar_streams_long2 <- nar_miss %>%
  left_join(nar_stream_sites, by = c("comid")) %>%
  select(comid,watershed,site, everything()) %>%
  gather(variable, value, 5:30) %>%
  rename(waterbody_type = `waterbody type`) %>%
  mutate(watershed = "narragansett bay") %>%
  na.omit()

wi <- bind_rows(wi, nar_streams_long2)
write_csv(wi, here("data/watershed_integrity_new_2.csv"))

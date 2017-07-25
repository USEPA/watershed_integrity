# Tidy-rific
library("tidyverse")
library("readxl")

# Write out copies of raw as csv

#1
if(!file.exists("data/calapooia_fish_mmi.csv")){
  read_csv("data/raw/CAL IWI_ICI_FMMI_Temp_Habt.csv") %>%
    write_csv("data/calapooia_fish_mmi.csv")
}

#2
if(!file.exists("data/calapooia_n_isotopes.csv")){
  read_csv("data/raw/CAL IWI_ICI_N_iso.csv") %>%
    write_csv("data/calapooia_n_isotopes.csv")
}

#3
if( !file.exists("data/calapooia_n.csv")){
  read_csv("data/raw/CAL IWI_ICI_N.csv") %>%
    write_csv("data/calapooia_n.csv")
}

#4
if(!file.exists("data/calapooia_n_budget.csv")){
  read_csv("data/raw/CAL IWI_ICI_wsNbudget.csv") %>%
    write_csv("data/calapooia_n_budget.csv")
}

#5
#if(!file.exists("data/choptank_lidar.csv")){
#read_csv("data/raw/CHOP_IWI_ICI_LiDAR_indices.csv") %>%
#    write_csv("data/choptank_lidar.csv")
#}

#6
if(!file.exists("data/choptank_nhd.csv")){
read_csv("data/raw/CHOP_IWI_ICI_NHDv2_indices.csv") %>%
    write_csv("data/choptank_nhd.csv")
}

#7
if(!file.exists("data/east_fork_little_miami_n.csv")){
  read_csv("data/raw/EFLMR_IWI_ICI_N.csv") %>%
    write_csv("data/east_fork_little_miami_n.csv")
}

#8
if(!file.exists("data/narragansett_lakes.csv")){
read_csv("data/raw/NAR Lake IWI_ICI indices.csv") %>%
    write_csv("data/narragansett_lakes.csv")
}

#9
if(!file.exists("data/narragansett_streams.csv")){
read_csv("data/raw/NAR Stream IWI_ICI indices.csv") %>%
    write_csv("data/narragansett_streams.csv")
}

# Read in csv
calapooia_fish <- read_csv("data/calapooia_fish_mmi.csv")
calapooia_n_iso <- read_csv("data/calapooia_n_isotopes.csv")
calapooia_n <- read_csv("data/calapooia_n.csv")
calapooia_n_budg <- read_csv("data/calapooia_n_budget.csv")
#choptank_lidar <- read_csv("data/choptank_lidar.csv")
choptank_nhd <- read_csv("data/choptank_nhd.csv")
lil_miami_n <- read_csv("data/east_fork_little_miami_n.csv")
nar_lakes <- read_csv("data/narragansett_lakes.csv")
nar_streams <- read_csv("data/narragansett_streams.csv")
# Add in Site Names for Nar streams and lakes
nar_stream_sites <- read_csv("data/raw/NAR_comid_stream siteID.csv")
nar_lake_sites <- read_csv("data/raw/NAR_comid_lake_siteid.csv")

# Standardize data frames
calapooia_fish_long <- calapooia_fish %>%
  gather(variable, value, 5:29) %>%
  rename(site = aliasid, waterbody_type = `waterbody type`) %>%
  mutate(watershed = "calapooia")

calapooia_n_iso_long <- calapooia_n_iso %>%
  gather(variable, value, 5:19) %>%
  rename(site = aliasid, waterbody_type = `waterbody type`) %>%
  mutate(watershed = "calapooia")

calapooia_n_long <- calapooia_n %>%
  select(-catchmentid) %>%
  gather(variable, value, 5:20) %>%
  rename(site = aliasid, waterbody_type = `waterbody type`) %>%
  mutate(watershed = "calapooia")
  
calapooia_n_budg_long <- calapooia_n_budg %>%
  select(-catchmentid) %>%
  gather(variable, value, 5:24) %>%
  rename(site = aliasid, waterbody_type = `waterbody type`) %>%
  mutate(watershed = "calapooia")

#choptank_lidar_long <- choptank_lidar %>%
#  select(comid, watershed,site = huc10name, waterbody_type = waterbody, 
#         everything()) %>%
#  gather(variable, value, 5:25) %>%
#  mutate(watershed = "choptank")

choptank_nhd_long <- choptank_nhd %>%
  select(-`stream method`) %>%
  gather(variable, value, 5:24) %>%
  rename(site = huc10name, waterbody_type = waterbody) %>%
  mutate(watershed = "choptank")

lil_miami_n_long <- lil_miami_n %>%
  gather(variable, value, 5:29) %>%
  rename(site = aliasid, waterbody_type = `waterbody type`) %>%
  mutate(watershed = "east fork little miami river")

nar_lakes_deep <- nar_lakes %>%
  filter(comid == 6141286 & near(d15NBOM, 2.05110274)) %>%
  mutate(site = "Deep") %>%
  select(comid, watershed, site, everything()) 

nar_lakes_school <- nar_lakes %>%
  filter(comid == 6141286 & near(d15NBOM, 2.58016230)) %>%
  mutate(site = "Schoolhouse") %>%
  select(comid, watershed, site, everything())

nar_lakes_long <- nar_lakes %>%
  filter(comid != 6141286) %>%
  left_join(nar_lake_sites, by = c("comid")) %>%
  select(comid, watershed, site, everything()) %>%
  bind_rows(nar_lakes_deep, nar_lakes_school) %>%
  gather(variable, value, 5:31) %>%
  rename(waterbody_type = `waterbody type`) %>%
  mutate(watershed = "narragansett bay") 

nar_streams_long <- nar_streams %>%
  left_join(nar_stream_sites, by = c("comid")) %>%
  select(comid,watershed,site, everything()) %>%
  gather(variable, value, 5:30) %>%
  rename(waterbody_type = `waterbody type`) %>%
  mutate(watershed = "narragansett bay")


# Combine data frames
dfs <- as.list(ls(pattern = "_long")) %>%
  lapply(get)
ws_integ_df <- bind_rows(dfs) %>%
  arrange(watershed, comid) %>%
  unique()  %>% #Original spreadsheets contained some duplicate rows
  na.omit() # Had some extra rows in original spreadsheets
write_csv(ws_integ_df, "data/watershed_integrity_orig.csv")

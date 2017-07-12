library("tidyverse")
library("here")
library("stringr")

# Read in watershed_integrity.csv
ws_integ_df <- read_csv(here("data/watershed_integrity_orig.csv")) %>%
  mutate(variable = case_when(
    variable == "iwi" ~ "iwi_old",
    variable == "ici" ~ "ici_old",
    variable == "whyd" ~ "whyd_old",
    variable == "chyd" ~ "chyd_old",
    variable == "wchem" ~ "wchem_old",
    variable == "cchem" ~ "cchem_old",
    variable == "wsed" ~ "wsed_old",
    variable == "csed" ~ "csed_old",
    variable == "wconn" ~ "wconn_old",
    variable == "cconn" ~ "cconn_old",
    variable == "wtemp" ~ "wtemp_old",
    variable == "ctemp" ~ "ctemp_old",
    variable == "whabt" ~ "whabt_old",
    variable == "chabt" ~ "chabt_old",
    TRUE ~ variable)
  )

# Read in updated IWI etc
july7_iwi <- read_csv(here("data/raw/revised IWI_ICI_07072017.csv")) %>%
  unique() %>%
  gather(variable, value, 3:16) %>%
  rename(comid = COMID) %>%
  mutate(variable = tolower(variable)) %>%
  select(comid,variable,value)

comid_info <- select(ws_integ_df, comid, watershed, site, waterbody_type) %>%
  unique()

july7_iwi <- july7_iwi %>%
  full_join(comid_info) %>%
  select(comid, watershed, site, waterbody_type, variable, value) %>%
  na.omit


# Add new
ws_integ_df <- ws_integ_df %>%
  bind_rows(july7_iwi)

# Write new
write_csv(ws_integ_df,here("data/watershed_integrity_new.csv"))

# Compare
ws_integ_df_index_only <- ws_integ_df %>%
  filter(str_detect(variable, 'iwi|ici|whyd|chyd|wchem|cchem|wsed|csed|wconn|cconn|wtemp|ctemp|whabt|chabt')) %>%
  spread(variable, value)

plot(ws_integ_df_index_only$ici,ws_integ_df_index_only$ici_old)
abline(0,1)

plot(ws_integ_df_index_only$iwi,ws_integ_df_index_only$iwi_old)
abline(0,1)

ws_integ_df_index_only %>%
  group_by(watershed) %>%
  summarize(mean(iwi,na.rm=T),
            median(iwi,na.rm=T),
            min(iwi,na.rm=T),
            max(iwi,na.rm=T))
ws_integ_df_index_only %>%
  group_by(watershed) %>%
  summarize(mean(iwi_old,na.rm=T),
            median(iwi_old,na.rm=T),
            min(iwi_old,na.rm=T),
            max(iwi_old,na.rm=T))




library(tidyverse)
library(here)

wi <- unique(read_csv(here("data/watershed_integrity_new_2.csv")))

# cal - remove PctCrop2011Cat, corrrect chironomid spell, change xembed to
# sedembed, change xcmgw to vegcovrip

# chop - lower case all

# east fork - log10DIavg change to log10INavg, log10fDIavg change to
# log10fINavg, log10DINdif change to log10INdif. log10fDINdif change to
# log10fINdif

# Narr bay - remove forest200buf 

wi <- wi %>%
  filter(variable != "PctCrop2011cat",
         variable != "Forest200buf") %>%
  mutate(variable = case_when(variable == "dN15chironimid" ~ "dN15chironomid",
                              variable == "xembed" ~ "sedembed",
                              variable == "xcmgw" ~ "vegcovrip",
                              watershed == "choptank" ~ tolower(variable),
                              variable == "log10DIavg" ~ "log10INavg",
                              variable == "log10fDIavg" ~ "log10fINavg",
                              variable == "log10DINdif" ~ "log10INdif",
                              variable == "log10fDINdif" ~ "log10fINdif",
                              TRUE ~ variable))

write_csv(wi, here("data/watershed_integrity_new_2.csv"))

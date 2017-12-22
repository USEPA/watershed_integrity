library(tidyverse)
library(here)

wi <- unique(read_csv(here("data/watershed_integrity_new_2.csv")))

# cal - remove PctCrop2011Cat, corrrect chironomid spell, change xembed to
# sedembed, change xcmgw to vegcovrip

# chop - lower case all

# east fork - log10DIavg change to log10INavg, log10fDIavg change to
# log10fINavg, log10DINdif change to log10INdif. log10fDINdif change to log10fINdif

# Narr bay - remove forest200buf
wi <- wi %>%
  filter(variable != "PctCrop2011cat")


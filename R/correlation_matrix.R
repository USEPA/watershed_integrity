# Packages
library(tidyverse)
library(here)
library(stringr)
library(hrbrthemes)
source("R/corr_fig.R")

wi <- unique(read_csv(here("data/watershed_integrity_new_2.csv")))

x <- c("iwi", "wtemp","wsed","whyd","whabt","wconn","wchem","ici",
                  "ctemp","csed","chyd","chabt","cconn","cchem")

#Figure 6. Calapooia (CRW)
#Figure 7. Choptank (CHOP)
#Figure 8. East Fork Little Miami (EFLMR) 
#Figure 9. Narragansett Bay (NBW)


watersheds <- unique(wi$watershed)
fig_num <- 5
for(i in watersheds){
  ws_abb <- abbreviate(paste(i,collapse = ""))
  
  if(i == "choptank"){
    mtd <- "spearman"
  } else {
    mtd <- "pearson"
  }
  fig_num <- fig_num + 1 

  
  wi %>%
    filter(!str_detect(wi$variable, paste(x,collapse = "|"))) %>%
    filter(!grepl("_old",variable)) %>%
    filter(watershed == i) %>%
    pull(variable) %>%
    unique() %>%
    corr_fig(data = wi, ws = i, x = x, y = ., 
             output_csv = paste0("data/", ws_abb, "_cor.csv"), method = mtd,
             filename = paste0("figures/figure",fig_num,".tiff"), 
             width = 6.5, height = 6.5, units = "in", pointsize = 12, dpi = 1000, 
             compression = "lzw")
  #tiff()
  #gg
  #dev.off()
}


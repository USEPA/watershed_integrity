# Packages
library(tidyverse)
library(here)
library(stringr)
library(hrbrthemes)

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

cal_cor_df <- cor(cal[,5:24],use = "pairwise.complete.obs") %>%
  data.frame() %>%
  rownames_to_column("index") %>% 
  filter(str_detect(index, 'iwi|ici|whyd|chyd|wchem|cchem|wsed|csed|wconn|cconn|wtemp|ctemp|whabt|chabt'))  %>%
  select(c("index",cal_y))
         
write_csv(data.frame(index = cal_cor_df[,1],round(cal_cor_df[,-1], 3)), path = "data/cal_correlations.csv")

cal_cor <- cal_cor_df  %>%
  gather("variable", "value", -1) %>%
  mutate(value = round(value,2))

cal_cor_gg <- ggplot(cal_cor, aes(x = index, y = variable)) +
  geom_point(aes(size = abs(value), color = value)) + 
  scale_color_viridis_c(name = "Pearson\nCorrelation", 
                        limits = round(range(cal_cor$value),2), 
                        breaks = round(c(max(cal_cor$value),0.5,0,-0.5,min(cal_cor$value)),2),
                        guide = guide_legend(
                          override.aes = list(#color = viridis::viridis(5,direction = -1), 
                                              size = c(5,2.5,1,2.5,5)),
                          reverse = FALSE)) +
  scale_size(range = c(1,5), guide = FALSE) +
  theme_ipsum() +
  scale_x_discrete(position = "top") +
  labs(x = "", y = "") +
  theme(legend.text = element_text(size = 10),
        axis.text.x = element_text(angle= 45, hjust = 0))

cal_cor_gg
ggsave(filename = "cal_cor_fig.jpg", plot = cal_cor_gg, width = 7, height = 5, 
       units = "in", dpi = 300)


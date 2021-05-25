rm(list=ls())
library(data.table)
library(tidyverse)

all_nh <- read_csv("intermediate/nh_geocode.csv")
lodes_wac <- read_csv("intermediate/nh_lodes_blocks.csv") 
tractsum <- read_csv("intermediate/nh_res_tracts_char.csv")

nb_merged <- all_nh %>%
  left_join(lodes_wac)  %>%
  left_join(tractsum, by=c("PROVNUM"= "provnum"))
 
nb_merged <- nb_merged %>%
  mutate(PROVNUM=str_pad(PROVNUM, 6, "left", "0"),
         share_white=CR01/C000,
         share_black=CR02/C000,
         share_native=CR03/C000,
         share_asianpi=(CR04+CR05)/C000,
         share_hisplatino=CT02/C000) %>%
  rename(total_service_emp=C000,
         hc_emp=CNS16) %>%
  arrange(PROVNUM)


write.csv(nb_merged, "output/nh_staff_nbs.csv", row.names=FALSE)

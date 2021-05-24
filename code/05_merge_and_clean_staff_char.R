rm(list=ls())
library(data.table)
library(tidyverse)

lodes_wac <- fread("intermediate/nh_lodes_clean.csv") 
tractsum <- fread("intermediate/nh_res_tracts_char.csv")

nb_merged <- merge(lodes_wac, tractsum, by.x="PROVNUM", by.y="provnum")
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

## Undo scientific notation for provider numbers
nb_merged <- nb_merged %>%
  separate(PROVNUM, c("a", "b", "c")) %>% 
  mutate(b=ifelse(is.na(b), "", b),
         c=ifelse(is.na(c), "", c))

nb_merged <- nb_merged %>%
  mutate(c = as.character(as.numeric(nb_merged$c) - 2),
         c=ifelse(is.na(c), "", c))

nb_merged$provnum <- paste0(nb_merged$a, nb_merged$b, nb_merged$c)

# Output
nb_merged <- nb_merged %>% select(provnum, tract_med_income, share_white, 
                                  share_black, share_native, share_asianpi, share_hisplatino,
                                  total_service_emp, hc_emp)

write.csv(nb_merged, "output/nh_staff_nbs.csv", row.names=FALSE)

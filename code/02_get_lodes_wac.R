rm(list=ls())
library(data.table)
library(tidyverse)
library("RCurl")
library("rjson")
library("mgcv")
library(lehdr)
library(censusxy)
library(data.table)
library(tidyverse)
library(tigris)

nhcompare <- fread("intermediate/nh_geocode.csv")
allemp = list()
us <- unique(fips_codes %>% select(state, state_code))[1:51, ]
statelist <- unique(fips_codes$state)
statelist <- setdiff(statelist,  c("AS", "GU", "MP", "PR", "UM", "VI"))
for(i in 1:length(statelist)) {
  s = statelist[[i]]
  print(s)
  nhblocklist = unique((nhcompare %>% filter(STATE == s))$censusblock)
  if(s=="AK") {
    year = 2016
  }
  else {
    year = 2018
  }
  data1 <- grab_lodes(s, year,
                      lodes_type = c("wac"),
                      job_type = c("JT00"),
                      segment = c("SI03"),
                      agg_geo = c("block"),
                      state_part = c("main"),
                      download_dir = "temp/lodes"
  )
  data2 <- grab_lodes(s, year,
                      lodes_type = c("wac"),
                      job_type = c("JT00"),
                      segment = c("SI03"),
                      agg_geo = c("block"),
                      state_part = c("aux"),
                      download_dir = "temp/lodes"
  )  
  data <- rbindlist(list(data1, data2))
  data <- data %>% 
    filter(w_geocode %in% nhblocklist)
  empsum <- data %>% 
    group_by(w_geocode) %>% 
    summarise_at(c("C000", "CA01", "CA02", "CA03", "CNS16", "CR01", "CR02", "CR03", "CR04", "CR05", "CR07", "CT01", "CT02", "CD01", "CD02", "CD03", "CD04", "CS01", "CS02"), sum, na.rm=TRUE)
  allemp[[i]] <- empsum
}
empmerge <- rbindlist(allemp)
nhmerged <- merge(nhcompare, empmerge, by.x="censusblock", by.y="w_geocode", all.x=TRUE, allow.cartesian=TRUE)

##########################
# If no employment on the block, get entire block group instead
nhunmatched <- nhmerged %>% filter(is.na(C000)) %>% mutate(nh_bg = substr(censusblock, 1, 12))
allemp = list()
for(i in 1:length(statelist)) {
  s = statelist[[i]]
  stfips = as.data.table(us)[state==s,]$state_code
  nhblockgroups = unique((nhunmatched %>% filter(STATE==s))$nh_bg)
  if(s=="AK") {
    year = 2016
  }
  else {
    year = 2018
  }
  data1 <- grab_lodes(s, year,
                      lodes_type = c("wac"),
                      job_type = c("JT00"),
                      segment = c("SI03"),
                      agg_geo = c("block"),
                      state_part = c("main"),
                      download_dir = "~/Google Drive/proj/covid_nh/covid_nh_data/raw/lodes"
  )
  data2 <- grab_lodes(s, year,
                      lodes_type = c("wac"),
                      job_type = c("JT00"),
                      segment = c("SI03"),
                      agg_geo = c("block"),
                      state_part = c("aux"),
                      download_dir = "~/Google Drive/proj/covid_nh/covid_nh_data/raw/lodes"
  )  
  data <- rbindlist(list(data1, data2)) %>% mutate(w_bg=substr(w_geocode, 1, 12))
  data <- data %>% filter(w_bg %in% nhblockgroups)
  empsum <- data %>% group_by(w_bg) %>% summarise_at(c("C000", "CA01", "CA02", "CA03", "CNS16", "CR01", "CR02", "CR03", "CR04", "CR05", "CR07", "CT01", "CT02", "CD01", "CD02", "CD03", "CD04", "CS01", "CS02"), sum, na.rm=TRUE)
  allemp[[i]] <- empsum
}
empmerge <- rbindlist(allemp)
nhmerged1 <- merge(nhunmatched %>% select(PROVNUM, PROVNAME, ADDRESS, CITY, STATE, ZIP, RESTOT, cxy_lon, cxy_lat, match_type, censusblock, nh_bg),
                   empmerge,  by.x="nh_bg", by.y="w_bg", all.x=TRUE, allow.cartesian=TRUE)
nhmerged1$bg_match=1
nhall <- rbindlist(list(nhmerged %>% filter(!is.na(C000)), nhmerged1), use.names=TRUE, fill=TRUE)
nhall <- nhall %>%
  mutate(bg_match=ifelse(is.na(bg_match), 0, bg_match))

write.csv(nhall, "intermediate/nh_lodes_blocks.csv", row.names=FALSE )




rm(list=ls())
library(data.table)
library(tidyverse)
library(lehdr)
TEMP_DIR = "temp/lodes"

nhcompare <- read_csv("intermediate/nh_geocode.csv")
pbj_emp <- read_csv("intermediate/pbj_counts.csv")
nhcompare <- merge(nhcompare, pbj_emp, by.x="PROVNUM", by.y="prov_id")

nhblocks <- nhcompare %>% select(PROVNUM, STATE, RESTOT, pbj_emp, cxy_lon, cxy_lat, match_type, censusblock)

us <- unique(fips_codes %>% select(state, state_code))[1:51, ]
statelist <- unique(fips_codes$state)
statelist <- setdiff(statelist,  c("AS", "GU", "MP", "PR", "UM", "VI"))

allemp = list()
for(i in 1:length(statelist)) {
  s = statelist[[i]]
  print(s)
  nhblocklist = unique((nhblocks %>% filter(STATE == s))$censusblock)
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
                      download_dir = TEMP_DIR
  )
  data2 <- grab_lodes(s, year,
                      lodes_type = c("wac"),
                      job_type = c("JT00"),
                      segment = c("SI03"),
                      agg_geo = c("block"),
                      state_part = c("aux"),
                      download_dir = TEMP_DIR
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
empmerge <- empmerge %>% 
  mutate(w_geocode  =str_pad(w_geocode, 15, "left", "0"))

nhmerged <- merge(nhblocks, empmerge, by.x="censusblock", by.y="w_geocode", all.x=TRUE, allow.cartesian=TRUE)

##########################
# If no or very low health care employment on the block (2664 observations), get closest block in block group
nhmatched <- nhmerged %>% 
  filter(!is.na(C000) & CNS16 >= .5 * pbj_emp)

nhunmatched <- nhmerged %>% 
  filter(is.na(C000) | CNS16 < .5 * pbj_emp) %>% 
  mutate(nh_bg = substr(censusblock, 1, 12)) %>%
  select(PROVNUM, STATE, RESTOT, pbj_emp, cxy_lon, cxy_lat, match_type, censusblock, nh_bg)

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
                      download_dir = TEMP_DIR
  )
  data2 <- grab_lodes(s, year,
                      lodes_type = c("wac"),
                      job_type = c("JT00"),
                      segment = c("SI03"),
                      agg_geo = c("block"),
                      state_part = c("aux"),
                      download_dir = TEMP_DIR
  )  
  data <- rbindlist(list(data1, data2)) %>% 
    mutate(w_bg=substr(w_geocode, 1, 12))
  data <- data %>% 
    filter(w_bg %in% nhblockgroups)
  empsum <- data %>% 
    group_by(w_geocode, w_bg) %>% 
    summarise_at(c("C000", "CA01", "CA02", "CA03", "CNS16", "CR01", "CR02", "CR03", "CR04", "CR05", "CR07", "CT01", "CT02", "CD01", "CD02", "CD03", "CD04", "CS01", "CS02"), sum, na.rm=TRUE)
  allemp[[i]] <- empsum
}
empmerge <- rbindlist(allemp)
nhmerged1 <- merge(nhunmatched,
                   empmerge,  by.x="nh_bg", by.y="w_bg", all.x=TRUE, allow.cartesian=TRUE)
nhmerged1 <- nhmerged1 %>% 
  filter(CNS16 >= .5 * pbj_emp) %>%
  mutate(block_diff=as.numeric(w_geocode)-as.numeric(censusblock),
         abs_block_diff=abs(block_diff)) %>%
  group_by(PROVNUM) %>%
  slice_min(abs_block_diff, order_by=block_diff,n= 1, with_ties=FALSE)

nhmerged1$bg_match <- 1
nhall <- rbindlist(list(nhmatched, nhmerged1), use.names=TRUE, fill=TRUE)
nhall <- nhall %>%
  mutate(bg_match=ifelse(is.na(bg_match), 0, bg_match),
         w_geocode=ifelse(is.na(w_geocode),censusblock,w_geocode)) %>%
  select(PROVNUM, w_geocode, bg_match, C000, CA01, CA02, CA03, CNS16, CR01, CR02, CR03, CR04, CR05, CR07, CT01, CT02, CD01, CD02, CD03, CD04, CS01, CS02)

nh_lodes <- merge(nhcompare, nhall, by="PROVNUM", all.x=TRUE)
write.csv(nh_lodes, "intermediate/nh_lodes_blocks.csv", row.names=FALSE )

# Did not find LODES match
nh_lodes %>% filter(is.na(w_geocode))




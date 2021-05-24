library(lehdr)
library(data.table)
library(tidyverse)
library(tigris)


nhblocks <- fread("intermediate/nh_lodes_blocks.csv")

us <- unique(fips_codes %>% select(state, state_code))[1:51, ]
nhblocks$censusblock <- str_pad(nhblocks$censusblock, 15, "left", "0")
nhblocks$nh_bg <- str_pad(nhblocks$censusblock, 12, "left", "0")

nhblocks <- nhblocks %>% mutate(statefip = substr(censusblock, 1, 2))
allemp = list()
allemp_bg = list()
statelist = us[[1]]
for(i in 1:length(statelist)) {
  s = statelist[[i]]
  stfips = as.data.table(us)[state==s,]$state_code
  nhblocklist = (nhblocks %>% filter(statefip == stfips))$censusblock
  nhbglist = (nhblocks %>% filter(statefip==stfips))$nh_bg
  if(s=="AK") {
    year = 2016
  }
  else {
    year = 2018
  }
  data1 <- grab_lodes(s, year,
                        lodes_type = c("od"),
                        job_type = c("JT00"),
                        segment = c("S000"),
                        agg_geo = c("block"),
                        state_part = c("main"),
                        download_dir = "temp/lodes")
  data2 <- grab_lodes(s, year,
                      lodes_type = c("od"),
                      job_type = c("JT00"),
                      segment = c("S000"),
                      agg_geo = c("block"),
                      state_part = c("aux"),
                      download_dir = "temp/lodes")
  data <- rbindlist(list(data1, data2))
  data <- data %>% mutate(h_tract = substr(h_geocode, 1, 11))
  data <- data %>% mutate(w_bg = substr(w_geocode, 1, 12))
  # Get data for all blocks
  data_block <- data %>% filter(w_geocode %in% nhblocklist)
  # Get data for block groups
  data_bg <- data %>% filter(w_bg %in% nhbglist)
  
  empsum <- data_block %>% group_by(w_geocode, h_tract) %>% summarise_at(c("S000", "SE01", "SE02", "SE03", "SI03"), sum)
  empsum_bg <- data_bg %>% group_by(w_bg, h_tract) %>% summarise_at(c("S000", "SE01", "SE02", "SE03", "SI03"), sum)
  allemp[[i]] <- empsum
  allemp_bg[[i]] <- empsum_bg
}

empmerge <- rbindlist(allemp)
empmerge_bg <- rbindlist(allemp_bg)

nhmerged1 <- merge(nhblocks %>% filter(bg_match==0), empmerge, by.x="censusblock", by.y="w_geocode", all.x=TRUE, allow.cartesian=TRUE)
nhmerged2 <- merge(nhblocks %>% filter(bg_match==1), empmerge_bg, by.x="nh_bg", by.y="w_bg", all.x=TRUE, allow.cartesian=TRUE)
nhmerged <- rbindlist(list(nhmerged1, nhmerged2), use.names=TRUE, fill=TRUE)
write.csv(nhmerged, "intermediate/nh_res_tracts.csv", row.names=FALSE )



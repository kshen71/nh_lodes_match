library(lehdr)
library(data.table)
library(tidyverse)
library(tigris)


nhblocks <- read_csv("intermediate/nh_lodes_blocks.csv")

us <- unique(fips_codes %>% select(state, state_code))[1:51, ]
statelist <- unique(fips_codes$state)
statelist <- setdiff(statelist,  c("AS", "GU", "MP", "PR", "UM", "VI"))
#us <- unique(fips_codes %>% select(state, state_code))[1:51, ]
#nhblocks <- nhblocks %>% mutate(statefip = substr(censusblock, 1, 2))
allemp = list()
#statelist = us[[1]]
for(i in 1:length(statelist)) {
  s = statelist[[i]]
  print(s)
  nhblocklist = unique((nhblocks %>% filter(STATE == s))$w_geocode)
  
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

  empsum <- data_block %>% group_by(w_geocode, h_tract) %>% summarise_at(c("S000", "SE01", "SE02", "SE03", "SI03"), sum)
  allemp[[i]] <- empsum
}

empmerge <- rbindlist(allemp)

nhmerged <- merge(nhblocks %>% filter(bg_match==0), empmerge, by="w_geocode", all.x=TRUE, allow.cartesian=TRUE)
write.csv(nhmerged, "intermediate/nh_res_tracts.csv", row.names=FALSE )



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


getLL <- function(address,  state) {
  #print(loc)
  geoc <- getURL(paste0("https://maps.googleapis.com/maps/api/geocode/json?address=", address, 
                        "&components:state=", state,
                        "&key=", Sys.getenv("GMAPS_KEY")))
  tryCatch({
    geoc_parse <- fromJSON(geoc)
    if(length(geoc_parse[[1]]) > 0) {
      lat <- geoc_parse[[1]][[1]]$geometry$location$lat
      lng <- geoc_parse[[1]][[1]]$geometry$location$lng
      match <- 1
      print(match)
      return(list( lng=lng, lat=lat, match=match))
    }
  }, error = function(e) {
    lat <- 0
    lng <- 0
    match <- 0
    return(list( lng=lng, lat=lat, match=match))
  }
  
  )
  
}


get_cblock <- function(lng, lat) {
  if(!is.na(lat) & !is.na(lng) & (lat != 0 | lng != 0)) {
    tryCatch({
      cb = as.character(cxy_geography(lng, lat, vintage="Census2010_Current")$Census.Blocks.GEOID)
      #print(cb)
      return(censusblock=cb)
    },
    error = function(e) {
      return(censusblock="")
    })
  } else {
    return(censusblock="")
  }
}



## Get coordinates
nhcompare <- fread("raw/ProviderInfo_2018.csv")
nhcompare <- nhcompare %>%  
  select(PROVNUM, PROVNAME, ADDRESS, CITY, STATE, ZIP, RESTOT) %>%
  mutate(fulladdress = paste0(ADDRESS, ",", CITY,",", STATE, ",", ZIP)) %>%
  mutate(fulladdress = str_squish(str_replace_all(fulladdress, "#", " "))) %>%
  mutate(fulladdress = str_replace_all(fulladdress, " ", "+")) 
  

# Use Census Geocoder
nhcompare <- cxy_geocode(nhcompare,  street = 'ADDRESS', city = 'CITY', state = 'STATE', zip = 'ZIP',
                         return = 'locations', class = 'dataframe', output = 'full')

exact_matches <- nhcompare %>% filter(cxy_quality=="Exact")

# If Census Geocoder returns non-exact match, use GMaps API
unmatched <- as.data.table(nhcompare %>% filter(cxy_quality != "Exact" | is.na(cxy_quality)))
unmatched[,  c("cxy_lon", "cxy_lat", "gmaps_match") := getLL(fulladdress, STATE), by=1:nrow(unmatched)]

# Combine all LL coords
all_matches <- rbindlist(list(exact_matches, unmatched), fill=TRUE, use.names=TRUE)
all_matches <- all_matches %>% 
  mutate(match_type = case_when(cxy_quality=="Exact" ~ 1, gmaps_match==1 ~ 2, TRUE ~ 3))

## Get census blocks associated with each lon-lat
nhcompare <- all_matches %>% select(PROVNUM, PROVNAME, ADDRESS, CITY, STATE, ZIP, RESTOT, cxy_lon, cxy_lat, match_type)
nhcompare[, censusblock:=get_cblock(cxy_lon, cxy_lat), by=1:nrow(nhcompare)]

nhcompare$censusblock <- str_pad(nhcompare$censusblock, 15, "left", "0")
write.csv(nhcompare, "intermediate/nh_geocode.csv", row.names=FALSE)
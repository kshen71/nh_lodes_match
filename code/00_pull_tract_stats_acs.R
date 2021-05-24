library(tidycensus)
library(tidyverse)
library(data.table)
library(tigris)

us <- unique(fips_codes$state)[1:51]

##### Tract estimates of use of pub trans and housing units

total_pop <- c(
  paste0("B08119_",str_pad(as.character(seq(1,9,1)),width = 3,side = "left",pad = "0"))
)
totalpt_pop <- c(
  paste0("B08119_",str_pad(as.character(seq(28,36,1)),width = 3,side = "left",pad = "0"))
)

alltractpt <- map_df(us, function(x) {
  get_acs(geography = "tract", c(total_pop, totalpt_pop, "B25001_001", "B01003_001", "B06011_001", 
                                 "B05010_001", "B05010_002", "B08126_041", "B08126_011", "B25002_002", "B25008_001", 
                                 "B02001_001", "B02001_002", "B03002_003", "B02001_003"), 
          state = x, year=2018, output="wide")
})


tractstats <- alltractpt %>%   rename(total_pop=B08119_001E, total_pt = B08119_028E, 
                                      total_pop2 = B01003_001E, race_pop = 	B02001_001E, white_pop=B02001_002E, white_nonhisp_pop =	B03002_003E,
                                      total_housing_units = B25002_002E, black_pop=B02001_003E,
                                      total_pop_housing=B25008_001E,
                                      med_income=B06011_001E, pov_total = B05010_001E, below_fpl_pop=B05010_002E, pt_i62 = B08126_041E,
                                      tract_i62 = B08126_011E) %>%
  mutate(total_u50k = B08119_002E + B08119_003E + B08119_004E + B08119_005E + B08119_006E, 
         pt_u50k = B08119_029E + B08119_030E + B08119_031E + B08119_032E + B08119_032E) %>% 
  select(GEOID, NAME, total_pop, total_pt, total_u50k, pt_u50k, total_pop2, race_pop, white_pop, white_nonhisp_pop, 
         black_pop, total_housing_units,
         med_income, pov_total, below_fpl_pop, total_pop_housing, pt_i62, tract_i62)
write.csv(tractstats, "intermediate/tract_acs_stats.csv", row.names=FALSE)

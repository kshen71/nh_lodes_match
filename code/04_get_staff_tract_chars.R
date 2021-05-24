rm(list=ls())
library(data.table)
library(tidyverse)

nhtracts <- fread("intermediate/nh_res_tracts.csv")
setnames(nhtracts, c("PROVNUM"), c("provnum"))

acsdata <- fread("intermediate/tract_acs_stats.csv")
nhtracts$h_tract <- as.numeric(nhtracts$h_tract)
nhtracts <- merge(nhtracts,acsdata, by.x="h_tract", by.y="GEOID", all.x=TRUE)

nhsum <- nhtracts[, .(tract_med_income = weighted.mean(med_income, w=SI03, na.rm=TRUE)),
                  by=c("provnum")]

write.csv(nhsum, "intermediate/nh_res_tracts_char.csv", row.names=FALSE)

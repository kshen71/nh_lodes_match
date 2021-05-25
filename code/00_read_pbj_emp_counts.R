rm(list=ls())
library(data.table)
library(tidyverse)


pbj <- read_csv("raw/pbj_unique_employees.csv")
pbj <- pbj %>%
  filter(dateq=="201801") %>%
  filter(emp_cat <= 6) %>%
  group_by(prov_id) %>%
  summarise(pbj_emp=sum(n_emp, na.rm=TRUE))

write_csv(pbj, "intermediate/pbj_counts.csv")

  
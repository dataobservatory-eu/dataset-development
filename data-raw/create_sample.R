

library(eurostat)
library(tidyverse)

sample_raw <- eurostat::get_eurostat("tepsr_sp410") 

sample_wide <- sample_raw %>%
  filter ( geo %in% c("IT", "NL")) %>%
  rowid_to_column() %>%
  select ( rowid, indic_is, ind_type, unit, geo, time, values) %>%
  mutate ( rowid = paste0("example", rowid))

write.csv(sample_wide, "wide_sample.csv", row.names = F)

long_sample <- sample_wide %>% mutate ( time  = as.character(time), 
                         decimal = paste0('"', as.character(values), '"^^xsd::decimal')) %>% 
  select (-values) %>%
  tidyr::pivot_longer( c("geo", "time",  "indic_is", "ind_type", "unit", "decimal"),
                       names_to = "p", 
                       values_to = "o") %>%
  mutate ( p = case_when (
    p == "geo" ~ "http://dd.eionet.europa.eu/vocabulary/eurostat/geo/",
    p == "ind_type" ~ "http://dd.eionet.europa.eu/vocabulary/eurostat/ind_type/", 
    p == "indic_is" ~ "http://dd.eionet.europa.eu/vocabulary/eurostat/indic_is/",
    p == "time" ~ "http://dd.eionet.europa.eu/vocabulary/eurostat/time/",
    p == "unit" ~ "http://dd.eionet.europa.eu/vocabulary/eurostat/unit/",
    p == "decimal" ~ "eg:tepsr_sp410", 
    .default = as.character(p)
  ))

write.csv(long_sample, "long_sample.csv", row.names = F)


# CL AREA

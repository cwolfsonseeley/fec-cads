## get bio info from cdw
library(getcdw)
source("r/sql_queries.R")

cads_names <- get_cdw(cads_names_query)
cads_address <- get_cdw(cads_address_query)
cads_employment <- get_cdw(cads_employment_query)

# get names into the same format as fec names from part 1
cads_names %<>%
    mutate_each(funs(toupper), -entity_id) %>%
    mutate(first_name = str_replace_all(first_name, "[^A-Z]", ""),
           last_name  = str_replace_all(last_name, "[^A-Z]", "")) %>%
    mutate(middle_initial = str_trim(str_sub(middle_name, 1, 1)))

cads_address %<>%
    mutate(city = toupper(city))

cads_employment %<>%
    mutate(job_title = toupper(job_title),
           employer  = toupper(employer))

# frequency tables for frequency-based match scores
cads_frequency_first <- cads_names %>%
    select(entity_id, first_name) %>%
    distinct %>%
    group_by(first_name) %>%
    summarise(cads_first = n_distinct(entity_id)) %>%
    mutate(n_cads_first = sum(cads_first))

cads_frequency_last <- cads_names %>%
    select(entity_id, last_name) %>%
    distinct %>%
    group_by(last_name) %>%
    summarise(cads_last = n_distinct(entity_id)) %>%
    mutate(n_cads_last = sum(cads_last))

saveRDS(cads_frequency_first, file = "temp/cads_frequency_first.rds")
saveRDS(cads_frequency_last, file = "temp/cads_frequency_last.rds")
rm(cads_frequency_first, cads_frequency_last)

saveRDS(cads_address, file = "temp/cads_address.rds")
saveRDS(cads_employment, file = "temp/cads_employment")
saveRDS(cads_names, file = "temp/cads_names.rds")
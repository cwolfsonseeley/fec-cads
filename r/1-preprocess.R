source("r/dl-functions.R")
library(dplyr) # otherwise some painful printing
library(stringr)
library(magrittr)

# download the data and load the individual contributions file into memory
fec_year <- 2018
get_fec(fec_year)
fec_ind <- individuals(fec_year)

fec_db <- src_sqlite("temp_fec.sqlite", create = TRUE)
fec_ind <- copy_to(fec_db, fec_ind, name = "fec_ind")

fec_ind <- fec_ind %>%
    filter(entity_tp %in% c("CAN", "IND")) %>%
    mutate(full_last = SUBSTR(name, 1, INSTR(name, ',') - 1L)) %>%
    mutate(last = SUBSTR(name, 1, INSTR(name, ' ') - 1L)) %>%
    mutate(first_name = SUBSTR(name, INSTR(name, ',') + 1L)) %>%
    mutate(first_name = TRIM(first_name)) %>%
    mutate(first = SUBSTR(first_name, 1, INSTR(first_name, ' ') - 1L)) %>%
    mutate(first = ifelse(first == "", first_name, first)) %>%
    mutate(middle = SUBSTR(first_name, INSTR(first_name, ' ') + 1L)) %>%
    mutate(middle = ifelse(middle == first, NA, middle)) %>%
    mutate(first = REPLACE(first, '.', '')) %>%
    mutate(middle = REPLACE(middle, '.', ''))

fec_ind %<>% compute

fec_ind %<>%
    mutate(middle = ifelse(middle %in% c("MS", "MR", "MRS", "DR", "DRS"), "", middle))

fec_ind %<>% compute

fec_ind %<>%
    mutate(last = REPLACE(last, ",", "")) %>%
    mutate(shortzip = SUBSTR(zip_code, 1, 5))

fec_ind %<>% 
    mutate(last = ifelse(last %in% c("VAN", "DE", "MC", "VON", "ST", "DEL", "LA",
                                     "VANDER", "DI", "O", "LE", "TE", "SAN", "VANDEN",
                                     "MAC", "DU", "D", "VANDE", "LO", "SANTA"),
                         full_last, last)) %>%
    compute

fec_ind %<>%
    mutate(last = TRIM(last), 
           middle = TRIM(middle), 
           first = TRIM(first),
           full_last = TRIM(full_last),
           middle_initial = ifelse(is.na(middle) | length(middle) < 1,
                                   NA, substr(middle, 1, 1)))
fec_ind <- compute(fec_ind)

# need to be able to identify truly distinct individuals in the dataset, but 
# there aren't any identifiers. this is an approximation, but there will def. 
# be distinct people in the same zip code with the same first/last name. 
unique_individuals <- fec_ind %>%
    select(first, last, shortzip) %>%
    distinct %>%
    compute %>%
    mutate(fec_id = rowid)

fec_ind %<>%
    inner_join(unique_individuals, by = c("first", "last", "shortzip")) %>%
    compute

unique_individuals <- collect(unique_individuals, n = Inf)

# make name frequency tables for frequency based match scores
fec_frequency_first <- unique_individuals %>%
    group_by(first) %>%
    summarise(fec_first = n()) %>%
    mutate(n_fec_first = sum(fec_first))

fec_frequency_last <- unique_individuals %>%
    group_by(last) %>%
    summarise(fec_last = n()) %>%
    mutate(n_fec_last = sum(fec_last))

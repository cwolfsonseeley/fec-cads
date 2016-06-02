source("r/dl-functions.R")
library(dplyr) # otherwise some painful printing
library(stringr)
library(magrittr)

# download the data and load the individual contributions file into memory
fec_year <- 2016
get_fec(fec_year)
fec_ind <- individuals(fec_year)

# first clean up names a little bit
# and filter out non-individuals
fec_ind %<>% 
    # only "individuals" and "candidates"
    filter(entity_tp %in% c("CAN", "IND")) %>%
    # get first/middle/last from single name field
    mutate(last = str_extract(name, "^[^\\s,]+")) %>%
    mutate(full_last = str_extract(name, "^[^,]+")) %>%
    mutate(first_name = str_match(name, "(^[^,]+), (.*)$")[,3]) %>%
    mutate(first_name = str_trim(first_name)) %>%
    mutate(first = str_extract(first_name, "^[^\\s]+")) %>%
    mutate(middle = str_extract(first_name, "(\\s+[^\\s]+)+$")) %>%
    mutate(middle = str_replace(middle, "((^|[\\s]+)MS\\.?(\\s|$))|((^|[\\s]+)MRS\\.?(\\s|$))|((^|[\\s]+)MS\\.?(\\s|$))|((^|[\\s]+)MR\\.?(\\s|$))", "")) %>%
    mutate(middle = str_trim(middle)) %>%
    mutate(first     = str_replace_all(first, "[^A-Z]", ""),
           middle    = str_replace_all(middle, "[^A-Z]", ""),
           last      = str_replace_all(last, "[^A-Z]", ""),
           full_last = str_replace_all(full_last, "[^A-Z]", "")) %>%
    # clean up the zip code
    mutate(shortzip = str_sub(zip_code, 1, 5)) %>%
    ## detailed last-name cleanup!
    # by default, last name is considered to have ended at the first space,
    # rather than the comma, because there are a lot of "Jr." and "II/III" type 
    # last name additions which we won't use. but some last names actually have 
    # a space in them. here is where i try to keep those
    mutate(last = ifelse(last %in% c("VAN", "DE", "MC", "VON", "ST", "DEL", "LA",
                                     "VANDER", "DI", "O", "LE", "TE", "SAN", "VANDEN",
                                     "MAC", "DU", "D", "VANDE", "LO", "SANTA"),
                         full_last, last)) %>%
    mutate(last = str_trim(last), 
           middle = str_trim(middle), 
           first = str_trim(first),
           full_last = str_trim(full_last),
           middle_initial = ifelse(is.na(middle) | str_length(middle) < 1,
                                   NA, str_sub(middle, 1, 1)))

# need to be able to identify truly distinct individuals in the dataset, but 
# there aren't any identifiers. this is an approximation, but there will def. 
# be distinct people in the same zip code with the same first/last name. 
unique_individuals <- fec_ind %>%
    select(first, last, shortzip) %>%
    distinct

## an ad-hoc ID field
unique_individuals %<>%
    mutate(fec_id = seq(nrow(.)))

fec_ind %<>%
    inner_join(unique_individuals, by = c("first", "last", "shortzip"))

# make name frequency tables for frequency based match scores
fec_frequency_first <- unique_individuals %>%
    group_by(first) %>%
    summarise(fec_first = n()) %>%
    mutate(n_fec_first = sum(fec_first))

fec_frequency_last <- unique_individuals %>%
    group_by(last) %>%
    summarise(fec_last = n()) %>%
    mutate(n_fec_last = sum(fec_last))
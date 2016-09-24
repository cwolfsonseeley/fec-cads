# create candidates by: 
# any match on zip+4, regardless of name
# any exact match on first & last name
library(tidyr)
library(stringdist)

# cads 9-digit zips
cads_address %>%
    mutate(zipcode = str_replace_all(zipcode, "[^0-9]", "")) %>%
    filter(str_length(zipcode) == 9) -> longzips

# fec 9-digit zips
fec_longzips <- fec_ind %>% 
    select(fec_id, zip_code) %>% 
    filter(length(zip_code) == 9) %>% 
    distinct %>%
    collect(n = Inf)

zip_candidates <- fec_longzips %>%
    inner_join(longzips, by = c("zip_code" = "zipcode")) %>%
    select(fec_id, entity_id) %>%
    distinct

name_candidates <- unique_individuals %>%
    inner_join(cads_names, by = c("first" = "first_name",
                                  "last"  = "last_name")) %>%
    select(fec_id, entity_id) %>%
    distinct

all_candidates <- name_candidates %>% 
    dplyr::union(zip_candidates)

fec_to_match <- fec_ind %>% 
    select(fec_id, 
           fec_first = first, 
           fec_mi = middle_initial, 
           fec_last = last, 
           fec_city = city, 
           fec_zip = zip_code, 
           fec_zip5 = shortzip,
           fec_employer = employer, 
           fec_occupation = occupation) %>%
    distinct %>%
    collect(n = Inf)

fec_to_match %>%
    inner_join(all_candidates, by = "fec_id") %>%
    inner_join(cads_names, by = "entity_id") %>%
    left_join(cads_address, by = "entity_id") %>%
    left_join(cads_employment, by = "entity_id") -> candidate_matrix

candidate_matrix %<>%
    rename(cads_first = first_name,
           cads_mi = middle_initial,
           cads_last = last_name,
           cads_city = city,
           cads_zip = zipcode,
           cads_zip5 = zipcode5,
           cads_employer = employer,
           cads_occupation = job_title)

fec_occupation <- candidate_matrix$fec_occupation
cads_occupation <- candidate_matrix$cads_occupation
fec_employer <- candidate_matrix$fec_employer
cads_employer <- candidate_matrix$cads_employer

occupation <- stringdist(fec_occupation,
                         cads_occupation,
                         method = "cosine", q = 3,
                         nthread = 1)

rm(fec_occupation, cads_occupation)

employer <- stringdist(fec_employer,
                       cads_employer,
                       method = "cosine", q = 3, 
                       nthread = 1)
rm(fec_employer, cads_employer)

candidate_matrix %>%
    transmute(fec_id, entity_id,
              first = fec_first == cads_first,
              mi = fec_mi == cads_mi,
              last = fec_last == cads_last,
              geo = fec_city == cads_city | fec_zip5 == cads_zip5,
              occupation = occupation < .5,
              employer = employer < .5) %>%
    replace_na(list(first = FALSE, mi = FALSE, last = FALSE, geo = FALSE, 
                    occupation = FALSE, employer = FALSE)) %>%
    group_by(fec_id, entity_id) %>%
    summarise_each(funs(max)) %>% ungroup -> gamma_matrix

#####
source("r/fs-model-functions.R")
preds = names(gamma_matrix)[6:8]
guess_u <- rep(.1, length(preds))
names(guess_u) <- preds
guess_p <- .1
guess_m <- rep(.9, length(preds))
names(guess_m) <- preds

df <- gamma_matrix %>%
    select_(.dots=preds) %>%
    group_by_(.dots=preds) %>%
    summarise(count=n()) %>%
    ungroup

ans <- fs_weights(df, preds, "count", guess_p, guess_m, guess_u)

m <- ans$m
u <- ans$u
p <- ans$p
m[m>.999] <- .999
u <- pmin(u, random_agreement(df, preds, "count"))
u[u<.005] <- .005
agree_weight <- log2(m/u)
disagree_weight <- log2((1-m)/(1-u))

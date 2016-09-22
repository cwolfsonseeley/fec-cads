# create candidates by: 
# any match on zip+4, regardless of name
# any exact match on first & last name
library(tidyr)
library(stringdist)

cads_address %>%
    mutate(zipcode = str_replace_all(zipcode, "[^0-9]", "")) %>%
    filter(str_length(zipcode) == 9) -> longzips

fec_ind <- readRDS("temp/fec_ind.rds")
fec_ind %<>% select(fec_id, zip_code)

zip_candidates <- fec_ind %>%
    filter(str_length(zip_code) == 9) %>%
    inner_join(longzips, by = c("zip_code" = "zipcode")) %>%
    select(fec_id, entity_id) %>%
    distinct

rm(fec_ind)
unique_individuals <- readRDS("temp/unique_individuals.rds")

name_candidates <- unique_individuals %>%
    inner_join(cads_names, by = c("first" = "first_name",
                                  "last"  = "last_name")) %>%
    select(fec_id, entity_id) %>%
    distinct
rm(unique_individuals)

all_candidates <- name_candidates %>% 
    dplyr::union(zip_candidates)

# some clean up
saveRDS(all_candidates, file = "temp/all_candidates.rds")
saveRDS(zip_candidates, file = "temp/zip_candidates")
saveRDS(name_candidates, file = "temp/name_candidates")
saveRDS(longzips, file = "temp/longzips.rds")
rm(zip_candidates, name_candidates, longzips)

fec_ind <- readRDS("temp/fec_ind.rds")

fec_ind %<>% 
    select(fec_id, 
           fec_first = first, 
           fec_mi = middle_initial, 
           fec_last = last, 
           fec_city = city, 
           fec_zip = zip_code, 
           fec_zip5 = shortzip,
           fec_employer = employer, 
           fec_occupation = occupation)
fec_ind %<>% distinct

fec_ind %>%
    inner_join(all_candidates, by = "fec_id") %>%
    inner_join(cads_names, by = "entity_id") %>%
    left_join(cads_address, by = "entity_id") %>%
    left_join(cads_employment, by = "entity_id") -> candidate_matrix

# more clean up
saveRDS(candidate_matrix, file = "temp/candidate_matrix.rds")
rm(fec_ind, cads_address, cads_employment, cads_names, all_candidates)

candidate_matrix %<>%
    rename(cads_first = first_name,
           cads_mi = middle_initial,
           cads_last = last_name,
           cads_city = city,
           cads_zip = zipcode,
           cads_zip5 = zipcode5,
           cads_employer = employer,
           cads_occupation = job_title)
saveRDS(candidate_matrix, file = "temp/candidate_matrix.rds")

fec_occupation <- candidate_matrix$fec_occupation
cads_occupation <- candidate_matrix$cads_occupation
fec_employer <- candidate_matrix$fec_employer
cads_employer <- candidate_matrix$cads_employer
rm(candidate_matrix)

occupation <- stringdist(fec_occupation,
                         cads_occupation,
                         method = "cosine", q = 3,
                         nthread = 1)
saveRDS(occupation, file = "temp/occupation.rds")
rm(occupation, fec_occupation, cads_occupation)

cutoff <- length(cads_employer) %/% 2

cads_employer1 <- cads_employer[1:cutoff]
fec_employer1 <- fec_employer[1:cutoff]
cads_employer2 <- cads_employer[(cutoff + 1):length(cads_employer)]
fec_employer2 <- fec_employer[(cutoff + 1):length(fec_employer)]
rm(cads_employer, fec_employer)

employer1 <- stringdist(fec_employer1,
                       cads_employer1,
                       method = "cosine", q = 3, 
                       nthread = 1)
rm(cads_employer1, fec_employer1)

employer2 <- stringdist(fec_employer2,
                        cads_employer2,
                        method = "cosine", q = 3, 
                        nthread = 1)

employer <- c(employer1, employer2)
rm(employer1, employer2, cads_employer2, fec_employer2)
saveRDS(employer, file = "temp/employer.rds")

occupation <- readRDS("temp/occupation.rds")
candidate_matrix <- readRDS("temp/candidate_matrix.rds")

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
rm(candidate_matrix, employer, occupation)
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

saveRDS(gamma_matrix, file = "temp/gamma_matrix.rds")
rm(list = ls()[!ls() %in% c("agree_weight", "disagree_weight")])